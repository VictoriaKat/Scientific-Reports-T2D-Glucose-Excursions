# =============================================================================
# Data Formatting and Final Preparation
# =============================================================================
# This script performs final data formatting and selection for analysis:
# - Removes unnecessary columns
# - Converts categorical variables to numeric
# - Adds temporal features
# - Filters data for analysis

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(dplyr)  # For data manipulation

# =============================================================================
# 2. Load and Prepare Data
# =============================================================================
# Read the processed postprandial dataset
data <- read.csv("Data_Analysis/Data/Postprandial_05.csv")


# Debug: Check initial data structure
print("Initial data structure:")
str(data)
print("Summary of initial data:")
summary(data)

# Debug: Check for missing values
print("Missing values in key columns:")
print(colSums(is.na(data)))

# =============================================================================
# 3. Remove Unnecessary Columns
# =============================================================================
# Remove columns not needed for analysis
columns_to_remove <- c(
  "Blood.Ketone..mmol...L.",
  "Dietary.intake",
  "Food_checked",
  "Data_not_availabl_bin",
  "Complications",
  "Hypoglycemia_agent_effect",
  "help_bolus_insulin_dose_time"
)

# Debug: Check if columns exist before removal
print("Columns to be removed:")
print(columns_to_remove[columns_to_remove %in% names(data)])

# Remove columns
data <- data[, !names(data) %in% columns_to_remove]

# Debug: Check for duplicates after removing columns
print("Checking for duplicates after removing columns:")
print(paste("Total rows:", nrow(data)))
print(paste("Unique rows:", nrow(unique(data))))
print(paste("Number of duplicates:", nrow(data) - nrow(unique(data))))

# =============================================================================
# 4. Convert Categorical Variables to Numeric
# =============================================================================
# Convert binary categorical variables to 0/1 for XGBoost compatibility

# Debug: Check unique values before conversion
print("Unique values in categorical columns:")
print(unique(data$Alcohol.Drinking.History..drinker.non.drinker.))
print(unique(data$Other_Agents))
print(unique(data$Hypoglycemia))
print(unique(data$Macrovascular_Complications))
print(unique(data$Microvascular_Complications))
print(unique(data$Smoking_history))

# Alcohol drinking history
data$Alcohol.Drinking.History..drinker.non.drinker. <- ifelse(
  data$Alcohol.Drinking.History..drinker.non.drinker. == "drinker", 1,
  ifelse(data$Alcohol.Drinking.History..drinker.non.drinker. == "non-drinker", 0, NA)
)

# Other binary variables
data$Other_Agents <- ifelse(data$Other_Agents == "yes", 1, 0)
data$Hypoglycemia <- ifelse(data$Hypoglycemia == "yes", 1, 0)
data$Macrovascular_Complications <- ifelse(data$Macrovascular_Complications == "yes", 1, 0)
data$Microvascular_Complications <- ifelse(data$Microvascular_Complications == "yes", 1, 0)
data$Smoking_history <- ifelse(data$Smoking_history == "smoker", 1, 0)

# Debug: Verify conversions
print("Summary after categorical conversion:")
print(summary(data[, c("Alcohol.Drinking.History..drinker.non.drinker.", 
                      "Other_Agents", "Hypoglycemia", 
                      "Macrovascular_Complications", 
                      "Microvascular_Complications", 
                      "Smoking_history")]))

# Debug: Check for duplicates after categorical conversion
print("Checking for duplicates after categorical conversion:")
print(paste("Total rows:", nrow(data)))
print(paste("Unique rows:", nrow(unique(data))))
print(paste("Number of duplicates:", nrow(data) - nrow(unique(data))))

# =============================================================================
# 5. Filter Data for Analysis
# =============================================================================
# Debug: Check postprandial_rolling_all before filtering
print("Summary of postprandial_rolling_all before filtering:")
print(summary(data$postprandial_rolling_all))

# Remove records with missing postprandial rolling values
data <- data %>% filter(!is.na(postprandial_rolling_all))

# Debug: Check number of observations per ID
print("Number of observations per ID before filtering:")
print(table(data$ID))

# Keep only IDs with more than 20 observations
data <- data %>% 
  group_by(ID) %>% 
  filter(n() >= 20)

unique(data$ID)
# =============================================================================
# 6. Add Temporal Features
# =============================================================================
# Debug: Check date and time columns before processing
print("Date column type before conversion:")
print(class(data$date))
print("Time column type before conversion:")
print(class(data$time))

# Add previous postprandial glucose spike
data <- data %>% 
  group_by(ID) %>% 
  arrange(ID, Date) %>%
  mutate(PPG_spike_prev = lag(postprandial_rolling_all)) %>%
  ungroup()

# Debug: Check PPG_spike_prev
print("Summary of PPG_spike_prev:")
print(summary(data$PPG_spike_prev))

# Extract hour from time
data$time <- substr(data$time, 1, 2) %>% as.numeric(.)

# Debug: Check time conversion
print("Summary of time after conversion:")
print(summary(data$time))

# Add weekday information
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
data$date <- as.Date(data$date)
data$weekday_num <- match(weekdays(data$date), week_days)

# Debug: Check weekday conversion
print("Summary of weekday_num:")
print(summary(data$weekday_num))

# Debug: Check for duplicates after adding temporal features
print("Checking for duplicates after adding temporal features:")
print(paste("Total rows:", nrow(data)))
print(paste("Unique rows:", nrow(unique(data))))
print(paste("Number of duplicates:", nrow(data) - nrow(unique(data))))

# If duplicates are found, identify the key columns causing duplication
if(nrow(data) != nrow(unique(data))) {
  print("Identifying key columns causing duplication:")
  # Check for duplicates based on ID and Date
  duplicates_by_id_date <- data[duplicated(data[, c("ID", "Date")]) | 
                               duplicated(data[, c("ID", "Date")], fromLast = TRUE),]
  print(paste("Number of duplicates by ID and Date:", nrow(duplicates_by_id_date)))
  
  # Check for duplicates based on ID, Date, and time
  duplicates_by_id_date_time <- data[duplicated(data[, c("ID", "Date", "time")]) | 
                                     duplicated(data[, c("ID", "Date", "time")], fromLast = TRUE),]
  print(paste("Number of duplicates by ID, Date, and time:", nrow(duplicates_by_id_date_time)))
}

# =============================================================================
# 7. Save Final Dataset
# =============================================================================
# Remove any remaining duplicates before saving
data <- unique(data)
 Debug: Final data check
print("Final data structure:")
str(data)
print("Final data summary:")
summary(data)

write.csv(data, "Data_Analysis/Data/Data_total_06.csv", row.names = FALSE)








