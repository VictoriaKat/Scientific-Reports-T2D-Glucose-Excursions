# =============================================================================
# TableOne Analysis for Personalized Models
# =============================================================================

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(tableone)
library(dplyr)
library(knitr)
library(kableExtra)

# =============================================================================
# 2. Set Up Environment and Load Data
# =============================================================================
# Set working directory
setwd("/Users/victoriabrugger/Documents/GitHub/Cursor-Repo/T2D-China-Scientific")

# Load initial data
Tableone_df <- read.csv("Data_Analysis/Data/Data_total_06.csv")
IDs <- read.csv("Data_Analysis/Data/List_ID.csv")

# Filter data for selected IDs
Tableone_df <- Tableone_df %>% filter(ID %in% IDs$x)

# =============================================================================
# 3. Data Preparation and Feature Selection
# =============================================================================
# Select relevant columns for analysis
Tableone_df <- Tableone_df %>% dplyr::select(
  "ID", "Staples_g", "Vegetables_g", "Fruits_g", 
                                      "Animal_Foods_g", "Dairy_Products_g", 
                                      "Legumes_Nuts_g", "Sweets_g", 
                                      "total_g", "Day", 
                                      "Meals_per_day", "Meals_FG_per_day", "Gender..Female.1..Male.2.", 
                                      "Age..years.", "Alcohol.Drinking.History..drinker.non.drinker.", 
                                      "Duration.of.diabetes..years.", "Comorbidities", "Fasting.Plasma.Glucose..mg.dl.", 
                                      "X2.hour.Postprandial.Plasma.Glucose..mg.dl.", "Fasting.C.peptide..nmol.L.", 
                                      "X2.hour.Postprandial.C.peptide..nmol.L.", "Fasting.Insulin..pmol.L.", 
                                      "X2.hour.Postprandial.insulin..pmol.L.", "HbA1c..mmol.mol.", 
                                      "Glycated.Albumin....", "Total.Cholesterol..mmol.L.", "Triglyceride..mmol.L.", 
                                      "High.Density.Lipoprotein.Cholesterol..mmol.L.", "Low.Density.Lipoprotein.Cholesterol..mmol.L.", 
                                      "Creatinine..umol.L.", "Estimated.Glomerular.Filtration.Rate...ml.min.1.73m2.", 
                                      "Uric.Acid..mmol.L.", "Blood.Urea.Nitrogen..mmol.L.", "Smoking_history", 
                                      "BMI", "Macrovascular_Complications", "Microvascular_Complications",
  "mean_postprandial_ID", "postprandial_rolling_all", "Hypoglycemia", "Meal_time", "Date"
)

# =============================================================================
# 4. Data Aggregation and Summary Statistics
# =============================================================================
# Calculate maximum day per ID
Tableone_df <- Tableone_df %>% group_by(ID) %>% mutate(max_day = max(Day))

# Calculate counts and percentages
Tableone_df <- Tableone_df %>% group_by(ID, Day) %>% mutate(n_count = n())
Tableone_df <- Tableone_df %>% group_by(ID) %>% mutate(n_count_overall = n())
Tableone_df <- Tableone_df %>% group_by(ID) %>% mutate(n_count_sum_overall = sum(postprandial_rolling_all))
Tableone_df$percentage_balanced_outcome <- 100/ Tableone_df$n_count_overall * Tableone_df$n_count_sum_overall
Tableone_df$Date <- as.POSIXct(Tableone_df$Date, format = "%Y-%m-%d %H:%M:%S")
Tableone_df$Hour <- as.numeric(format(Tableone_df$Date, "%H"))
Tableone_summary_meal_time <- Tableone_df %>%
  group_by(ID, Meal_time) %>%
  summarize(sd_hour = sd(Hour, na.rm = TRUE), n_meals = n(), .groups = 'drop')

 Tableone_df$Date <- NULL 
 Tableone_df$Hour <- NULL 
 Tableone_df$Meal_time <- NULL 


# Calculate daily food intake summaries
Tableone_daily <- Tableone_df %>%
  group_by(ID, Day) %>%
  summarize(across(c("Staples_g", "Vegetables_g", "Fruits_g", "Animal_Foods_g", "Dairy_Products_g", 
                     "Legumes_Nuts_g", "Sweets_g", "total_g"), 
                   sum, na.rm = TRUE), 
            .groups = 'drop')

# Calculate mean daily intake per ID
Tableone_daily_mean <- Tableone_daily %>%
  group_by(ID) %>%
  summarize(across(c("Staples_g", "Vegetables_g", "Fruits_g", "Animal_Foods_g", "Dairy_Products_g", 
                     "Legumes_Nuts_g", "Sweets_g", "total_g"), 
                   mean, na.rm = TRUE), 
            .groups = 'drop')

# Remove Day column as it's no longer needed
Tableone_df$Day <- NULL

# =============================================================================
# 5. Create Final Summary Dataset
# =============================================================================
# Summarize numeric variables and select first for categorical variables
Tableone_summary <- Tableone_df %>%
  group_by(ID) %>%
  summarize(across(everything(), 
                   list(~ if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)),
                   .names = "summary_{col}")) %>%
  rename_with(~ sub("^summary_", "", .), everything())

# Remove food intake columns as they're in daily_mean
Tableone_summary <- Tableone_summary %>% select(-Staples_g:-total_g)

# Combine summary and daily mean data
final_summary <- Tableone_summary %>%
  left_join(Tableone_daily_mean, by = "ID")

# Remove unnecessary columns
final_summary$Meals_FG_per_day <- NULL
final_summary$Meals_per_day <- NULL
sum(final_summary$Hypoglycemia)

# Save final summary
write.csv(final_summary, "Tableone_final_df.csv")

# =============================================================================
# 6. Model Performance Analysis
# =============================================================================
# Load comparison data
comparison_df <- read.csv("Data_Analysis/Data/model_performance_comparison.csv")
final_summary <- read.csv("Tableone_final_df.csv")
final_summary <- final_summary %>% select(-X)

# Load and filter IDs
ids <- read.csv("Data_Analysis/Data/List_ID.csv")
final_summary <- final_summary %>% filter(ID %in% ids$x)
Tableone_summary_meal_time <- Tableone_summary_meal_time %>% filter(ID %in% ids$x)

# Join data with model performance categories
comparison_df <- comparison_df %>% select(ID, Max_Source, f1_high, f1_low, best_f1_score)
final_data <- merge(final_summary, comparison_df, by = "ID")

# Calculate meal time statistics
Names_comparison <- comparison_df %>% select(ID, Max_Source)
Tableone_summary_meal_time <- merge(Tableone_summary_meal_time, Names_comparison, by = "ID")
Tableone_summary_meal_time$sd_min <- Tableone_summary_meal_time$sd_hour*60
round(median(Tableone_summary_meal_time$sd_min, na.rm = TRUE), 2)
round(quantile(Tableone_summary_meal_time$sd_min, probs = c(0.25, 0.75), na.rm = TRUE), 2)

# Calculate meal time statistics by meal type
meal_time_stats <- Tableone_summary_meal_time %>%
  group_by(Meal_time) %>%
  summarize(
    median_sd_min = round(median(sd_min, na.rm = TRUE), 2),
    q25 = round(quantile(sd_min, probs = 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(sd_min, probs = 0.75, na.rm = TRUE), 2),
    n_individuals = n()
  ) %>%
  mutate(
    meal_type = case_when(
      Meal_time == 1 ~ "Morning (5-10h)",
      Meal_time == 2 ~ "Lunch (10-14h)",
      Meal_time == 3 ~ "Afternoon (14-18h)",
      Meal_time == 4 ~ "Dinner (18-21h)",
      Meal_time == 5 ~ "Midnight (21-5h)"
    )
  )

# Print overall meal time statistics
cat("\nOverall meal time standard deviations (in minutes):\n")
print(meal_time_stats %>% select(meal_type, median_sd_min, q25, q75, n_individuals))

# Calculate statistics for high-burden model
high_burden_meal_time <- Tableone_summary_meal_time %>% 
  filter(Max_Source == "High-burden") %>%
  group_by(Meal_time) %>%
  summarize(
    median_sd_min = round(median(sd_min, na.rm = TRUE), 2),
    q25 = round(quantile(sd_min, probs = 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(sd_min, probs = 0.75, na.rm = TRUE), 2),
    n_individuals = n()
  ) %>%
  mutate(
    meal_type = case_when(
      Meal_time == 1 ~ "Morning (5-10h)",
      Meal_time == 2 ~ "Lunch (10-14h)",
      Meal_time == 3 ~ "Afternoon (14-18h)",
      Meal_time == 4 ~ "Dinner (18-21h)",
      Meal_time == 5 ~ "Midnight (21-5h)"
    )
  )

cat("\nHigh-burden model meal time standard deviations (in minutes):\n")
print(high_burden_meal_time %>% select(meal_type, median_sd_min, q25, q75, n_individuals))

# Calculate statistics for low-burden model
low_burden_meal_time <- Tableone_summary_meal_time %>% 
  filter(Max_Source == "Low-burden") %>%
  group_by(Meal_time) %>%
  summarize(
    median_sd_min = round(median(sd_min, na.rm = TRUE), 2),
    q25 = round(quantile(sd_min, probs = 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(sd_min, probs = 0.75, na.rm = TRUE), 2),
    n_individuals = n()
  ) %>%
  mutate(
    meal_type = case_when(
      Meal_time == 1 ~ "Morning (5-10h)",
      Meal_time == 2 ~ "Lunch (10-14h)",
      Meal_time == 3 ~ "Afternoon (14-18h)",
      Meal_time == 4 ~ "Dinner (18-21h)",
      Meal_time == 5 ~ "Midnight (21-5h)"
    )
  )

cat("\nLow-burden model meal time standard deviations (in minutes):\n")
print(low_burden_meal_time %>% select(meal_type, median_sd_min, q25, q75, n_individuals))

# Calculate statistics for equal performance model
equal_performance_meal_time <- Tableone_summary_meal_time %>% 
  filter(Max_Source == "Best performance High-/Low-burden") %>%
  group_by(Meal_time) %>%
  summarize(
    median_sd_min = round(median(sd_min, na.rm = TRUE), 2),
    q25 = round(quantile(sd_min, probs = 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(sd_min, probs = 0.75, na.rm = TRUE), 2),
    n_individuals = n()
  ) %>%
  mutate(
    meal_type = case_when(
      Meal_time == 1 ~ "Morning (5-10h)",
      Meal_time == 2 ~ "Lunch (10-14h)",
      Meal_time == 3 ~ "Afternoon (14-18h)",
      Meal_time == 4 ~ "Dinner (18-21h)",
      Meal_time == 5 ~ "Midnight (21-5h)"
    )
  )

cat("\nEqual performance model meal time standard deviations (in minutes):\n")
print(equal_performance_meal_time %>% select(meal_type, median_sd_min, q25, q75, n_individuals))

# Save results to CSV
write.csv(meal_time_stats, "Data_Analysis/Data/meal_time_stats_overall.csv", row.names = FALSE)
write.csv(high_burden_meal_time, "Data_Analysis/Data/meal_time_stats_high_burden.csv", row.names = FALSE)
write.csv(low_burden_meal_time, "Data_Analysis/Data/meal_time_stats_low_burden.csv", row.names = FALSE)
write.csv(equal_performance_meal_time, "Data_Analysis/Data/meal_time_stats_equal_performance.csv", row.names = FALSE)

# Calculate F1 score statistics by model type
high_burden_f1 <- final_data %>% 
  filter(Max_Source == "High-burden") %>% 
  summarize(
    median_f1 = round(median(f1_high, na.rm = TRUE), 2),
    q1_f1 = round(quantile(f1_high, 0.25, na.rm = TRUE), 2),
    q3_f1 = round(quantile(f1_high, 0.75, na.rm = TRUE), 2)
  )

low_burden_f1 <- final_data %>% 
  filter(Max_Source == "Low-burden") %>% 
  summarize(
    median_f1 = round(median(f1_low, na.rm = TRUE), 2),
    q1_f1 = round(quantile(f1_low, 0.25, na.rm = TRUE), 2),
    q3_f1 = round(quantile(f1_low, 0.75, na.rm = TRUE), 2)
  )

equal_perf_f1 <- final_data %>% 
  filter(Max_Source == "Best performance High-/Low-burden") %>% 
  summarize(
    median_f1 = round(median(best_f1_score, na.rm = TRUE), 2),
    q1_f1 = round(quantile(best_f1_score, 0.25, na.rm = TRUE), 2),
    q3_f1 = round(quantile(best_f1_score, 0.75, na.rm = TRUE), 2)
  )

overall_f1 <- final_data %>%
  summarize(
    median_f1 = round(median(best_f1_score, na.rm = TRUE), 2),
    q1_f1 = round(quantile(best_f1_score, 0.25, na.rm = TRUE), 2),
    q3_f1 = round(quantile(best_f1_score, 0.75, na.rm = TRUE), 2)
  )

# Print F1 score statistics
cat("\nOverall F1 scores:\n")
cat(sprintf("Median [IQR]: %.2f [%.2f, %.2f]\n", 
            overall_f1$median_f1, overall_f1$q1_f1, overall_f1$q3_f1))

cat("\nHigh-burden model F1 scores (using f1_high):\n")
cat(sprintf("Median [IQR]: %.2f [%.2f, %.2f]\n", 
            high_burden_f1$median_f1, high_burden_f1$q1_f1, high_burden_f1$q3_f1))

cat("\nLow-burden model F1 scores (using f1_low):\n")
cat(sprintf("Median [IQR]: %.2f [%.2f, %.2f]\n", 
            low_burden_f1$median_f1, low_burden_f1$q1_f1, low_burden_f1$q3_f1))

cat("\nEqual performance model F1 scores (using best_f1_score):\n")
cat(sprintf("Median [IQR]: %.2f [%.2f, %.2f]\n", 
            equal_perf_f1$median_f1, equal_perf_f1$q1_f1, equal_perf_f1$q3_f1))

# Print group sizes
cat("\nNumber of observations in each group:\n")
cat("High-burden:", sum(final_data$Max_Source == "High-burden", na.rm = TRUE), "\n")
cat("Low-burden:", sum(final_data$Max_Source == "Low-burden", na.rm = TRUE), "\n")
cat("Equal performance:", sum(final_data$Max_Source == "Best performance High-/Low-burden", na.rm = TRUE), "\n")
cat("Total:", nrow(final_data), "\n")

# Check for missing values
cat("\nMissing values in key columns:\n")
cat("f1_high missing:", sum(is.na(final_data$f1_high)), "\n")
cat("f1_low missing:", sum(is.na(final_data$f1_low)), "\n")
cat("best_f1_score missing:", sum(is.na(final_data$best_f1_score)), "\n")
cat("Max_Source missing:", sum(is.na(final_data$Max_Source)), "\n")

# =============================================================================
# 7. Variable Renaming and TableOne Creation
# =============================================================================
# Rename variables for better readability
final_data <- final_data %>%
  rename(
    Gender = `Gender..Female.1..Male.2.`,
    `Age [Years]` = `Age..years.`,
    `Duration of Diabetes [Years]` = `Duration.of.diabetes..years.`,
    `Fasting Plasma Glucose [mg/dL]` = `Fasting.Plasma.Glucose..mg.dl.`,
    `HbA1c [mmol/mol]` = `HbA1c..mmol.mol.`,
    `Mean Postprandial` = X2.hour.Postprandial.Plasma.Glucose..mg.dl.,
    `Duration [day]` = max_day,
    `Observations/Meals [per day]` = n_count,
    `Staples [grams per day]` = Staples_g,
    `Vegetables [grams per day]` = Vegetables_g,
    `Fruits [grams per day]` = Fruits_g,
    `Animal Foods [grams per day]` = Animal_Foods_g,
    `Dairy Products [grams per day]` = Dairy_Products_g,
    `Legumes Nuts [grams per day]` = Legumes_Nuts_g,
    `Sweets [grams per day]` = Sweets_g,
    `Total Food [grams per day]` = total_g,
    `Duration [day]` = max_day,
    `Observations/Meals [per day]` = n_count,
    `Staples [grams per day]` = Staples_g,
    `Vegetables [grams per day]` = Vegetables_g,
    `Fruits [grams per day]` = Fruits_g,
    `Animal Foods [grams per day]` = Animal_Foods_g,
    `Dairy Products [grams per day]` = Dairy_Products_g,
    `Legumes Nuts [grams per day]` = Legumes_Nuts_g,
    `Sweets [grams per day]` = Sweets_g,
    `Total Food [grams per day]` = total_g
  )

# Create binary gender variable
final_data <- final_data %>%
  mutate(Gender_Male = ifelse(Gender == 2, 1, 0))

# Define variables for TableOne
vars <- c("F1 Score", "Gender_Male", "Age [Years]", "Duration of Diabetes [Years]", 
          "Fasting Plasma Glucose [mg/dL]", "HbA1c [mmol/mol]", "BMI", 
          "Mean Postprandial", "Duration [day]", "Observations/Meals [per day]",
          "Staples [grams per day]", "Vegetables [grams per day]", "Fruits [grams per day]", 
          "Animal Foods [grams per day]", "Dairy Products [grams per day]", 
          "Legumes Nuts [grams per day]", "Sweets [grams per day]", "Total Food [grams per day]")

# Specify categorical variables
catVars <- c("Gender_Male")

# Specify non-normal variables
nonNormalVars <- setdiff(vars, catVars) #add hashtag for mean values 

# =============================================================================
# 8. Create and Format TableOne Output
# =============================================================================
# Create TableOne object
tab <- CreateTableOne(vars = vars, 
                     strata = "Max_Source", 
                     data = final_data, 
                     factorVars = catVars,
                     includeNA = TRUE)

# Format TableOne output
tab_output <- print(tab, 
                   nonnormal = nonNormalVars,#add hashtag for mean values 
                   showAllLevels = TRUE, 
                   formatOptions = list(big.mark = ","),
                   exact = catVars,
                   smd = FALSE,
                   printToggle = FALSE)

# Create overall table
tab_overall <- CreateTableOne(vars = vars, 
                             data = final_data, 
                             factorVars = catVars,
                             includeNA = TRUE)

# Get overall statistics
overall_output <- print(tab_overall, 
                       #nonnormal = nonNormalVars,#add hashtag for mean values 
                       showAllLevels = TRUE,
                       formatOptions = list(big.mark = ","),
                       printToggle = FALSE)

# =============================================================================
# 9. Format and Save Results
# =============================================================================
# Format table for output
format_table <- function(tab_output, overall_output) {
  tab_df <- as.data.frame(tab_output)
  overall_df <- as.data.frame(overall_output)
  
  names(tab_df) <- c("Level", "Equal model Performance subset", "High-burden model subset", 
                    "Low-burden Model subset", "p-value", "test")
  
  result_df <- data.frame(
    Variable = rownames(tab_df),
    Level = tab_df$Level,
    "Overall sample" = overall_df$Overall,
    "High-burden model subset" = tab_df$`High-burden model subset`,
    "Low-burden Model subset" = tab_df$`Low-burden Model subset`,
    "Equal model Performance subset" = tab_df$`Equal model Performance subset`,
    "p-value" = tab_df$`p-value`
  )
  
  return(result_df)
}

# Format and save table
formatted_table <- format_table(tab_output, overall_output)
write.csv(formatted_table, "Data_Analysis/Data/model_performance_tableone_mean.csv", row.names = FALSE)

# Create HTML version for publication
pub_table <- final_table %>%
  kable(format = "html", caption = "Table 1: Participant Characteristics by Model Performance") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, 
                font_size = 12) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1, bold = TRUE) %>%
  column_spec(1, bold = TRUE)

# Save HTML table
html_file <- "Data_Analysis/Data/model_performance_tableone.html"
cat(pub_table, file = html_file)
cat("TableOne saved as", html_file)