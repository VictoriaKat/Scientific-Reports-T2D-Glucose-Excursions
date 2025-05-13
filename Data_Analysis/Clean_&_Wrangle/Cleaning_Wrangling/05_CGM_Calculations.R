# =============================================================================
# CGM Data Analysis and Calculations
# =============================================================================
# This script calculates various CGM measures including:
# - 24-hour CGM metrics
# - Postprandial glucose values
# - Incremental AUC calculations
# - Rolling food intake metrics

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(lubridate)  # For date/time handling
library(dplyr)      # For data manipulation
library(iglu)       # For glucose analysis
library(slider)     # For rolling calculations
library(tidyr)      # For data tidying

# =============================================================================
# 2. Load and Prepare Data
# =============================================================================
# Read the processed dataset
data <- read.csv("Data_Analysis/Data/Data_total_04.csv")

# Convert and format date/time variables
data$Date <- ymd_hms(data$Date)
data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract temporal components
data$day <- format(data$Date, "%d")
data$time <- format(data$Date, "%H:%M:%S")
data$month <- format(data$Date, "%m")

# Add minimum date for each ID
data <- data %>%
  group_by(ID) %>%
  dplyr::mutate(min_Date = min(Date)) %>%
  ungroup()

# Remove records with missing timestamps
data <- data[!with(data, is.na(Date)),]

# =============================================================================
# 3. Pre-Meal Interval Calculations
# =============================================================================
# Identify meal times and calculate pre-meal intervals
data <- data %>% 
  mutate(Meal_time = case_when(Meal == 1 ~ Date))
data$Meal_time <- ymd_hms(data$Meal_time)

# Calculate 1-hour pre-meal interval
data$Pre_Meal_start <- format(with(data, Meal_time - 60*60))
data$Pre_Meal_start <- ymd_hms(data$Pre_Meal_start)
data$Pre_meal_inter <- interval(data$Pre_Meal_start, data$Meal_time)

# Fill pre-meal intervals
data <- data %>% 
  dplyr::group_by(ID) %>% 
  arrange(Date) %>% 
  do(fill(., Pre_meal_inter, .direction = 'up')) %>% 
  dplyr::ungroup()

# Calculate pre-meal glucose values
data$premeal_glu <- ifelse(
  data$Date %within% data$Pre_meal_inter == TRUE, 
  data$CGM..mg...dl., 
  "No_PreMeal"
)

# Calculate mean pre-meal glucose
data <- data %>%
  group_by(ID, Pre_meal_inter) %>%
  mutate(premeal_1h_mean_glu = mean(CGM..mg...dl., na.rm = TRUE)) %>%
  ungroup()

# Clean up temporary columns
data$Meal_time <- NULL
data$Pre_Meal_start <- NULL
data$Pre_meal_inter <- NULL

# =============================================================================
# 4. Postprandial Calculations
# =============================================================================
# Identify postprandial periods
data <- data %>% 
  mutate(postpranidal = case_when(Meal == 1 ~ Date))
data$postpranidal <- ymd_hms(data$postpranidal)

# Calculate 2-hour postprandial interval
data$postpranidal_end <- format(with(data, postpranidal + 120*60))
data$postpranidal_end <- ymd_hms(data$postpranidal_end)
data$time_inter <- interval(data$postpranidal, data$postpranidal_end)

# Fill postprandial intervals
data <- data %>% 
  dplyr::group_by(ID) %>% 
  arrange(Date) %>% 
  do(fill(., time_inter, .direction = 'down')) %>% 
  dplyr::ungroup()

# Identify postprandial glucose values
data$postprandial_glu <- ifelse(
  data$Date %within% data$time_inter == TRUE, 
  data$CGM..mg...dl., 
  "No_postpran"
)

# Create postprandial dataset
data_postprandial <- data %>% 
  filter(postprandial_glu != "No_postpran")

# Calculate postprandial means
data_postprandial <- data_postprandial %>% 
  group_by(ID, time_inter) %>% 
  mutate(mean_postprandial = mean(CGM..mg...dl.)) %>% 
  ungroup()

# Create meal-specific dataset
data_post_meal <- data_postprandial %>% 
  filter(Meal == 1)

# =============================================================================
# 5. Rolling Postprandial Calculations
# =============================================================================
# Sort data for rolling calculations
data_post_meal <- data_post_meal %>%
  arrange(ID, Date)

# Calculate 24-hour rolling postprandial mean
data_post_meal <- data_post_meal %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(
    mean_postprandial_previous_24h = slider::slide_index_dbl(
      .x = mean_postprandial,
      .i = Date,
      .f = ~ if(length(.x) > 1) mean(head(.x, -1), na.rm = TRUE) else NA,
      .before = ~.x - hours(24),
      .complete = TRUE
    )
  ) %>%
  ungroup()

# Calculate all previous postprandial means
data_post_meal <- data_post_meal %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(
    mean_postprandial_all_previous = slider::slide_index_dbl(
      .x = mean_postprandial,
      .i = row_number(),
      .f = ~ mean(.x[-length(.x)]),
      .before = Inf,
      .complete = TRUE
    )
  ) %>%
  ungroup()

# =============================================================================
# 6. Incremental AUC Calculation
# =============================================================================
# Define iAUC calculation function
iauc_fn <- function(x, y) {
  # Initialize auc and seg.type vectors
  auc <- numeric(length(x) - 1)
  seg.type <- integer(length(x) - 1)
  
  # Check if there are sufficient points to calculate AUC
  if (length(x) < 2) {
    return(list(auc = NA, segments = auc, seg.type = seg.type)[[1]])
  }
  
  # Calculate the initial segment
  auc[1] <- ifelse(
    !is.na(y[2]) & !is.na(y[1]) & y[2] > y[1], 
    (y[2] - y[1]) * (x[2] - x[1]) / 2, 
    0
  )
  seg.type[1] <- ifelse(!is.na(y[2]) & !is.na(y[1]) & y[2] > y[1], 1, 0)
  
  # Loop through remaining points
  for (i in 3:length(x)) {
    if (is.na(y[i]) | is.na(y[i-1])) {
      auc[i-1] <- NA
      seg.type[i-1] <- NA
    } else if (y[i] >= y[1] & y[i-1] >= y[1]) {
      auc[i-1] <- (((y[i] - y[1]) / 2) + (y[i-1] - y[1]) / 2) * (x[i] - x[i-1]) / 2
      seg.type[i-1] <- 1
    } else if (y[i] >= y[1] & y[i-1] < y[1]) {
      auc[i-1] <- ((y[i] - y[1])^2 / (y[i] - y[i-1])) * (x[i] - x[i-1]) / 2
      seg.type[i-1] <- 2
    } else if (y[i] < y[1] & y[i-1] >= y[1]) {
      auc[i-1] <- ((y[i-1] - y[1])^2 / (y[i-1] - y[i])) * (x[i] - x[i-1]) / 2
      seg.type[i-1] <- 3
    } else if (y[i] < y[1] & y[i-1] < y[1]) {
      auc[i-1] <- 0
      seg.type[i-1] <- 4
    } else {
      stop(paste("Error at i:", i))
    }
  }
  
  return(list(auc = sum(auc, na.rm = TRUE), segments = auc, seg.type = seg.type)[[1]])
}

# Convert postprandial glucose to numeric
data_postprandial$postprandial_glu <- as.numeric(data_postprandial$CGM..mg...dl.)

# Calculate iAUC values
AUC_match <- data_postprandial %>%
  group_by(ID, time_inter) %>%
  do(data.frame(iAUC = iauc_fn(.$Date, .$postprandial_glu)))

# Merge iAUC values with main dataset
data_post_meal <- merge(
  data_post_meal, 
  AUC_match, 
  by = c("ID", "time_inter"),
  all.x = TRUE, 
  all.y = FALSE
)

# =============================================================================
# 7. Rolling Food Intake Calculations
# =============================================================================
# Define rolling mean function
Rolling_mean <- function(data, new_colname, colname, time_hours) {
  col_sym = rlang::sym(colname)
  new_col_sym = rlang::sym(new_colname)
  
  data %>%
    group_by(ID) %>%
    mutate(!!new_col_sym := slide_index_dbl(
      .x = !!col_sym, 
      .i = Date, 
      .f = ~ if(length(.x) > 1) sum(.x[-length(.x)], na.rm = TRUE) else NA, 
      .before = ~.x - hours(time_hours), 
      .complete = TRUE
    )) %>%
    ungroup()
}

# Calculate rolling means for each food group
food_groups <- c(
  "Staples_g", "Vegetables_g", "Fruits_g", "Animal_Foods_g",
  "Dairy_Products_g", "Legumes_Nuts_g", "Sweets_g", "Other_Food_g",
  "Mixed_g", "total_g"
)

# Calculate 8-hour and 24-hour rolling means for each food group
for (group in food_groups) {
  data_post_meal <- Rolling_mean(data_post_meal, paste0(group, "_8h"), group, 8)
  data_post_meal <- Rolling_mean(data_post_meal, paste0(group, "_24h"), group, 24)
}

## Rolling Meal Intake --------------------------------------------------
Rolling_mean <- function(data, new_colname, colname, time_hours){
  # Convert string inputs to symbols
  col_sym = rlang::sym(colname)
  new_col_sym = rlang::sym(new_colname)
  
  data %>%
    group_by(ID) %>%
    mutate(!!new_col_sym := slide_index_dbl(
      .x = !!col_sym, 
      .i = Date, 
      .f = ~ if(length(.x) > 1) sum(.x[-length(.x)], na.rm = TRUE) else NA, 
      .before = ~.x - hours(time_hours), 
      .complete = TRUE
    )) %>%
    ungroup()
}

### Staples 
data_post_meal <- Rolling_mean(data_post_meal,"Staples_g_8h", "Staples_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Staples_g_24h", "Staples_g", 24)

### Vegetables
data_post_meal <- Rolling_mean(data_post_meal,"Vegetables_g_8h", "Vegetables_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Vegetables_g_24h", "Vegetables_g", 24)

### Fruits
data_post_meal <- Rolling_mean(data_post_meal,"Fruits_g_8h", "Fruits_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Fruits_g_24h", "Fruits_g", 24)

### Animal Protein
data_post_meal <- Rolling_mean(data_post_meal,"Animal_Foods_g_8h", "Animal_Foods_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Animal_Foods_g_24h", "Animal_Foods_g", 24)

### Dairy
data_post_meal <- Rolling_mean(data_post_meal,"Dairy_Products_g_8h", "Dairy_Products_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Dairy_Products_g_24h", "Dairy_Products_g", 24)

### Legumes & Nuts
data_post_meal <- Rolling_mean(data_post_meal,"Legumes_Nuts_g_8h", "Legumes_Nuts_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Legumes_Nuts_g_24h", "Legumes_Nuts_g", 24)

### Sweets
data_post_meal <- Rolling_mean(data_post_meal,"Sweets_g_8h", "Sweets_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Sweets_g_24h", "Sweets_g", 24)

### Other_Foods_g
data_post_meal <- Rolling_mean(data_post_meal,"Other_Food_g_8h", "Other_Food_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Other_Food_g_24h", "Other_Food_g", 24)

### Mixed_g
data_post_meal <- Rolling_mean(data_post_meal,"Mixed_g_8h", "Mixed_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Mixed_g_24h", "Mixed_g", 24)

### Total_g 
data_post_meal <- Rolling_mean(data_post_meal,"Total_g_8h", "total_g", 8)
data_post_meal <- Rolling_mean(data_post_meal,"Total_g_24h", "total_g", 24)

### Rolling presence 
### For binary variables indicate if they consumed the FG in the previous 8h or 24h
Rolling_presence <- function(data, new_colname, colname, time_hours) {
  # Convert string inputs to symbols
  col_sym <- rlang::sym(colname)
  new_col_sym <- rlang::sym(new_colname)
  
  data %>%
    group_by(ID) %>%
    mutate(!!new_col_sym := slide_index_dbl(
      .x = !!col_sym, 
      .i = Date, 
      .f = ~ if(any(head(.x, -1) == 1)) 1 else 0, 
      .before = ~.x - hours(time_hours),
      .complete = TRUE
    )) %>%
    ungroup()
}

### Staples 
data_post_meal <- Rolling_presence(data_post_meal,"Staples_bin_8h", "Staples_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Staples_bin_24h", "Staples_bin", 24)

### Vegetables
data_post_meal <- Rolling_presence(data_post_meal,"Vegetables_bin_8h", "Vegetables_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Vegetables_bin_24h", "Vegetables_bin", 24)

### Fruits
data_post_meal <- Rolling_presence(data_post_meal,"Fruits_bin_8h", "Fruits_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Fruits_bin_24h", "Fruits_bin", 24)

### Animal Protein
data_post_meal <- Rolling_presence(data_post_meal,"Animal_Foods_bin_8h", "Animal_Foods_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Animal_Foods_bin_24h", "Animal_Foods_bin", 24)

### Dairy
data_post_meal <- Rolling_presence(data_post_meal,"Dairy_Products_bin_8h", "Dairy_Products_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Dairy_Products_bin_24h", "Dairy_Products_bin", 24)

### Legumes & Nuts
data_post_meal <- Rolling_presence(data_post_meal,"Legumes_Nuts_bin_8h", "Legumes_Nuts_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Legumes_Nuts_bin_24h", "Legumes_Nuts_bin", 24)

### Sweets
data_post_meal <- Rolling_presence(data_post_meal,"Sweets_bin_8h", "Sweets_bin", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Sweets_bin_24h", "Sweets_bin", 24)

### Meal yes or no 
data_post_meal <- Rolling_presence(data_post_meal,"Meal_bin_8h", "Meal", 8)
data_post_meal <- Rolling_presence(data_post_meal,"Meal_bin_24h", "Meal", 24)

## Add further variables---------------
data_post_meal$Date <- as.POSIXct(data_post_meal$Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

## Add further variables----------------------
# Extract hours and categorize
data_post_meal <- data_post_meal %>% 
  dplyr::mutate(
    Hour = as.integer(format(Date, "%H")),
    Meal_time = case_when(
      Hour >= 5 & Hour < 10 ~ 1, #Morning
      Hour >= 10 & Hour < 14 ~ 2, #Lunch
      Hour >= 14 & Hour < 18 ~ 3,#Afternoon
      Hour >= 18 & Hour < 21 ~ 4,#Dinner
      TRUE ~ 5  # Else, Midnight
    )
  )

#### Average Mean Postprandial Overall -------------------
data_post_meal <- data_post_meal %>% group_by(ID) %>% 
  mutate(mean_postprandial_ID = mean(mean_postprandial)) %>% ungroup()

#### Binary Postprandial Indication -------------------
##### Constant over overall postprandial mean per ID -----------------
data_post_meal <- data_post_meal %>% group_by(ID) %>% 
  mutate(postprandial_constant = 
           ifelse(mean_postprandial>mean_postprandial_ID, 1, 0)) %>% ungroup()

##### Rolling 24h mean postprandial per ID -----------------
data_post_meal <- data_post_meal %>% group_by(ID) %>% 
  mutate(postprandial_rolling = 
           ifelse(mean_postprandial>mean_postprandial_previous_24h, 1, 0)) %>% ungroup()

data_post_meal <- data_post_meal %>% group_by(ID) %>% 
  mutate(postprandial_rolling_all = 
           ifelse(mean_postprandial>mean_postprandial_all_previous, 1, 0)) %>% ungroup()


### Remove all additional columns which aren't needed anymore
postprandial_df <- data_post_meal
postprandial_df <- postprandial_df %>% 
  dplyr::select(-c(postpranidal, postpranidal_end, time_inter, postprandial_glu, min_Date, CGM..mg...dl.))

### Calculate time difference
colnames(postprandial_df)
postprandial_df$Date <- as.POSIXct(postprandial_df$Date, format = "%Y-%m-%d %H:%M:%S")

Meal_diff <- postprandial_df %>%
  arrange(ID, Meal_time, Date) %>%
  group_by(ID, Meal_time) %>%
  mutate(Meal_time_diff = difftime(Date, lag(Date), units = "hours")) %>%
  ungroup()

Meal_diff$Meal_time_diff <- Meal_diff$Meal_time_diff-24 
hist(as.numeric(Meal_diff$Meal_time_diff))

average_meal_time_diff <- Meal_diff %>%
  group_by(ID) %>%
  summarize(Average_Meal_time_diff = mean(Meal_time_diff, na.rm = TRUE))

hist(as.numeric(average_meal_time_diff$Average_Meal_time_diff))

write.csv(average_meal_time_diff, "Data_Analysis/Data/DF_Average_Meal_Time_Diff.csv")

columns_g <- grep("_g$", names(postprandial_df), value = TRUE)


# Calculate the standard deviation for each selected column grouped by ID
sd_per_group <- postprandial_df %>%
  group_by(ID) %>%
  summarize(across(all_of(columns_g), sd, na.rm = TRUE, .names = "sd_{col}"))

write.csv(sd_per_group, "Data_Analysis/Data/DF_Average_Food_Group_sd.csv")

summary(postprandial_df)
## Remove Data Not Available Meals ---------------------
postprandial_df <- postprandial_df %>% filter(Dietary_intake != "data not available" &
                                                     Dietary_intake != "Data not available" & 
                                                     !is.na(Dietary_intake))

postprandial_df <- postprandial_df %>% filter(!is.na(postprandial_rolling_all))


write.csv(postprandial_df, "Data_Analysis/Data/Postprandial_05.csv", row.names = FALSE)





