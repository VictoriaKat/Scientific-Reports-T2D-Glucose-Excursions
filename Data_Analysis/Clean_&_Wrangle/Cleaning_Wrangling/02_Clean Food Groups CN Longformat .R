# =============================================================================
# Food Groups Classification and Analysis
# =============================================================================

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(rlang)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
setwd("/Users/victoriabrugger/Documents/GitHub/Cursor-Repo/Scientific-Reports-T2D-Glucose-Excursions")

# =============================================================================
# 2. Load and Prepare Data
# =============================================================================
# Read the combined CGM and food data
df_CGM_Food <- read.csv("Data_Analysis/Data/T2D_CGM_Food_01.csv")

# Split dietary intake into separate food items
df_CGM_Foods <- df_CGM_Food %>%
  separate(
    col = Dietary_intake, 
    into = c("Food_1", "Food_2", "Food_3", "Food_4", "Food_5", 
             "Food_6", "Food_7", "Food_8", "Food_9", "Food_10"), 
    sep = "g\\n|ml\\n", 
    remove = FALSE
  )

# Convert to long format for easier processing
df_CGM_Foods_long <- df_CGM_Foods %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = starts_with("Food_"),
    names_to = "Food_Item",
    values_to = "Food"
  ) %>%
  filter(!(Food_Item != "Food_1" & is.na(Food))) %>%
  dplyr::select(-Food_Item, -row_id)

df_CGM_Foods_long$Food_checked <- df_CGM_Foods_long$Food

# =============================================================================
# 3. Define Food Classification Function
# =============================================================================
# Function to classify foods into specific groups
Classify_Food_Groups <- function(df, food_group, food_group_char) {
  food_group_col <- sym(food_group)
  
  df <- df %>%
    mutate(!!food_group_col := case_when(
      grepl(food_group_char, Food, ignore.case = TRUE) ~ Food,
      TRUE ~ NA_character_
    ))
  
  df <- df %>%
    mutate(
      Food_checked = ifelse(!is.na(!!food_group_col) & !!food_group_col == Food_checked, 
                           "checked", 
                           Food_checked)
    )
  
  return(df)
}

# =============================================================================
# 4. Define Food Group Patterns
# =============================================================================
# Staples pattern (excluding water, coffee, tea, green tea)
Staples_char <- "bread|noodle|dumpling|corn|grain|potato|bun|bean|pretzel|pancake|wonton|taro|porridge|biscuit|
puff pastry|dough|croissants|roll|cake|cereal|oat|amaranth|Vermicelli|Coix seed|toast|rice|staple food|steamed gluten|Chinese chive pockets|fried gluten puff|Japanese food|Millet|Oil pastry|Pastry|Pizza|Puff|Puff pastry|Sago|Sandwich|Instant spicy steampot|Sorghum|Spaghetti|Sushi|Sheet jelly|yam"

# Vegetables pattern
Vegetables_char <- "vegetable|carrot|green onions|green pepper|spinach|celery|pumpkin|tomato|lettuce|cabbage|turnip|
cucumber|onion|cauliflower|lotus|eggplant|broccoli|mushroom|crown daisy|gourd|radish|sauerkraut|bean sprout|
fungus|asparagus|garlic|bamboo|kelp|zucchini|pickle|Laver|leek|Agrocybe cylindracea|Cucumber|black fungus|ginger|fungus|
day lily and fungus|Dendrobium candidum|Matsutake|pea seedling|Medicago polymorpha|Fried spinage|Vegtable|Sea weed|Seaweed|Spinage|Raddish|
Scallion oil fried wax gourd|Scallion oil fried wax goud|Pleurotus Eryngii|fungus|Lily|Tremella|Salad|Orchid|Shepherd's purse|white fungus soup|okra|Konjak silk"

# Fruits pattern
Fruits_char <- "fruit|apple|pear|banana|orange|tangerine|kiwi|lychee|longan|persimmon|dragon fruit|cherry|plum|grape|avocado|
Kumquat|wolfberry|Mangosteen|durian|Cantaloupe|blueberry|litchi|Loquat|peach|pitaya|coconut|kumquat|Strawberry|melon|Dekopon"

# Animal foods pattern
Animal_foods_char <- "meat|pork|beef|chicken|duck|lamb|mutton|pigeon|pig|snakehead|frog|sausage|ham|steak|beaf|Cow tail|
goose|snail|fish|shrimp|seafood|salmon|tuna|crab|hairtail|eel|croaker|croker|abalone|pomfret|bream|carp|perch|basa|clam|
Loach|herring|cod|ribs|egg|sea cucumber|loach soup|bone soup|Cantonese steamed bassa|chicken soup|goose feet|goose liver|grilled filet steak|
Chongqing style boiled blood curd|oyster|scallop|Scallops|Roasted halibut|Salted goose|Roasted goose|Chongqing style boiled blood curd|
Grilled filet steak|Steamed bassa|Stir fired pork rib|Tenderloin|Squid|Stir fired prok rib|sea|Grilled filet steackfood|steack|Scallion grilled chops"

# Dairy products pattern
Dairy_products_char <- "milk|yogurt|yoghurt|latte|Sour cream"

# Legumes and nuts pattern
Legumes_nd_nuts_char <- "bean|chickpea|cowpea|fried cowpea|tofu|cooked pea|Pea|Nut|Pine nut|sesame and walnut|sesame and walnut powder|chia seed|almond|cashew nuts|peanuts with vinegar|walnut|peanut|Seasame paste|chiba tofu|chestnuts|Natto|Sheet jelly|braised tofu"

# Sweets pattern
Sweets_char <- "candy|chocolate|wafer|date|ice cream|glucose|cookie|cake|black sesame and walnut paste|
sweet and sour ginger|Raisin|Sugar|Syrup"

# Other items pattern
Other_items_char <- "Chinese herbal medicine|Hot pot|Hotpot|meal replacement powder|Meal replacement powder|protein powder|gum|Eight treasures in hot sauce|Mustard|Protein|Broth 400 g|Soup 100|Soup 162|Soup 190|Soup 230|Soup 254.4 g|Soup 150 g|Soup 250|Soup 3|Soup 5|Soup 7|Soup 8|beer|ginseng wine|Coke|Lemon tea|Orance juice|Redoxon|Red wine|White wine|oil glutenyam|oil gluten|chili sauce|Snack|Snacks|borscht soup"

# Data not available pattern
data_not_available_char <- "data not available|Data not available"

# =============================================================================
# 5. Classify Foods into Groups
# =============================================================================
# Apply classification for each food group
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Staples", Staples_char)
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Vegetables", Vegetables_char)
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Fruits", Fruits_char)
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Animal_Foods", Animal_foods_char)
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Dairy_Products", Dairy_products_char)
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Legumes_Nuts", Legumes_nd_nuts_char)
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Sweets", Sweets_char)
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Other_Food", Other_items_char)
df_CGM_Foods_long <- Classify_Food_Groups(df_CGM_Foods_long, "Data_not_available", data_not_available_char)

# Remove yellow rice wine from staples
df_CGM_Foods_long$Staples <- ifelse(
  grepl("rice wine", df_CGM_Foods_long$Staples, ignore.case = TRUE), 
  NA, 
  df_CGM_Foods_long$Staples
)

# =============================================================================
# 6. Identify Mixed Food Groups
# =============================================================================
# Get column indices for food groups
First_foodgroup <- which(colnames(df_CGM_Foods_long) == "Staples")
Last_foodgroup <- which(colnames(df_CGM_Foods_long) == "Data_not_available")

# Select food group columns
cols_to_check <- df_CGM_Foods_long %>% 
  dplyr::select(First_foodgroup:Last_foodgroup) %>% 
  names()

# Identify mixed food groups
df_CGM_Foods_long$Mixed_Food_Groups <- apply(
  df_CGM_Foods_long[, cols_to_check], 
  1, 
  function(x) any(!is.na(x) & x != "")
)

# Create food group content column
df_CGM_Foods_long$Food_Group_Content <- apply(
  df_CGM_Foods_long[, cols_to_check], 
  1, 
  function(x) {
    paste(names(x)[which(!is.na(x) & x != "")], collapse = ", ")
  }
)

# Identify mixed foods
df_CGM_Foods_long <- df_CGM_Foods_long %>%
  mutate(
    Mixed_Food = ifelse(str_count(Food_Group_Content, ",") >= 1, 
                       Food_Group_Content, 
                       NA)
  )

# =============================================================================
# 7. Extract Food Amounts and Create Binary Variables
# =============================================================================
# Function to create amount and binary variables for each food group
create_food_variables <- function(df, group_name) {
  df <- df %>%
    mutate(
      !!paste0(group_name, "_g") := ifelse(
        (!is.na(!!sym(group_name)) & is.na(Mixed_Food)), 
        str_extract(!!sym(group_name), "\\d+"), 
        NA
      ),
      !!paste0(group_name, "_bin") := ifelse(!is.na(!!sym(group_name)), 1, 0)
    )
  return(df)
}

# Apply to all food groups
food_groups <- c("Staples", "Vegetables", "Fruits", "Animal_Foods", 
                "Dairy_Products", "Legumes_Nuts", "Sweets", "Other_Food", 
                "Data_not_available")

for(group in food_groups) {
  df_CGM_Foods_long <- create_food_variables(df_CGM_Foods_long, group)
}

# Create mixed food variables
df_CGM_Foods_long <- df_CGM_Foods_long %>%
  mutate(
    Mixed_g = ifelse((!is.na(Food) & !is.na(Mixed_Food)), 
                    str_extract(Food, "\\d+"), 
                    NA),
    Mixed_bin = ifelse(!is.na(Mixed_Food), 1, 0)
  )

# =============================================================================
# 8. Clean and Summarize Data
# =============================================================================
# Remove unnecessary columns
df_CGM_Foods_long <- df_CGM_Foods_long %>% 
  dplyr::select(-c(Food, Staples:Food_Group_Content))

# Convert all numeric columns
df_CGM_Foods_long <- df_CGM_Foods_long %>%
  mutate(across(.cols = Staples_g:Mixed_bin, .fns = as.numeric))

# Summarize food groups by meal
df_CGM_Foods_Grams <- df_CGM_Foods_long %>%
  group_by(ID, Date) %>%
  summarise(across(Staples_g:Mixed_bin, ~sum(.x, na.rm = TRUE)), .groups = 'drop')

# Calculate total grams
df_CGM_Foods_Grams <- df_CGM_Foods_Grams %>%
  rowwise() %>%
  mutate(total_g = sum(c_across(ends_with("_g"))))

  # Only keep Staples_g:Mixed_bin for df_CGM_Foods_Grams and remove it in the df_CGM_Foods_long
  df_CGM_Foods_long <- df_CGM_Foods_long %>%
    dplyr::select(-c(Staples_g:Mixed_bin))

# Remove duplicate rows
df_CGM_Foods_long <- df_CGM_Foods_long %>%
group_by(ID, Date) %>%
ummarise(across(everything(), ~first(na.omit(.)))) %>%
ungroup()

### Merge df_CGM_Foods_long and df_CGM_Foods_Grams
df_Food <- merge(df_CGM_Foods_long, df_CGM_Foods_Grams, by = c("ID", "Date"))

#### Summarize Meals ---------------
### Indicate if the CGM record is a meal time or not 
df_Food <- df_Food %>%
  rowwise() %>%
  mutate(Meal = ifelse(all(c_across(ends_with("_bin")) == 0), 0, 1))

### Indicate if the CGM record is a meal with corresponding food groups  time or not 
df_Food$Meals_Foodgroups <- ifelse(!is.na(df_Food$Meal) & df_Food$Data_not_available_bin == 1, 1, 0)

### Add day 
df_Food$Date <- as.POSIXct(df_Food$Date, format = "%Y-%m-%d %H:%M:%S")

df_Food <- df_Food %>%
  arrange(ID, Date) %>% # Sort by ID and datetime
  group_by(ID) %>%
  mutate(
  date = as.Date(Date), # Extract date part
  Day = match(date, unique(date)) # Assign day number within each ID
  ) %>%
    ungroup()

##### Meals per day ---------------
## Nr of meals per day
df_Food <- df_Food %>% group_by(ID, Day) %>% mutate(Meals_per_day = sum(Meal)) %>% ungroup()

### Make a new column not including those meals with no data available 
df_Food <- df_Food %>% group_by(ID, Day) %>% mutate(Meals_FG_per_day = sum(Meals_Foodgroups)) %>% ungroup()

write.csv(df_Food, "Data_Analysis/Data/Food_Groups_02.csv", row.names = FALSE)

