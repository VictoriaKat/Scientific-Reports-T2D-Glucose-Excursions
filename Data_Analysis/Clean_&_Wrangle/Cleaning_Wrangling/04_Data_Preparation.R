# =============================================================================
# Data Preparation for Insulin and Hypoglycemia Analysis
# =============================================================================
# This script processes insulin and hypoglycemia agent data, including:
# - Cleaning and standardizing insulin doses
# - Calculating effect times for different medications
# - Creating time-based indicators for medication effects
# - Adding lagged dose information

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(dplyr)     # For data manipulation
library(iglu)      # For glucose analysis
library(lubridate) # For date/time handling
library(stringr)
library(tidyr)   # For string manipulation

# =============================================================================
# 2. Load and Initial Data Cleaning
# =============================================================================
# Read the combined dataset
data <- read.csv("Data_Analysis/Data/Data_total_03.csv")

# Remove duplicate rows
data <- unique(data)

# =============================================================================
# 3. Insulin Treatment Processing
# =============================================================================
# Convert insulin treatment indicators to binary variables
data$basal_insulin <- ifelse(
  data$basal_insulin == "temporarily suspend insulin delivery", 
  NA, 
  data$basal_insulin
)

# Extract insulin doses from text fields
data <- data %>%
  mutate(
    basal_insulin_dose = str_extract(
      basal_insulin, 
      "(?<=, )\\d+\\.?\\d*|\\d+\\.?\\d*(?= IU)|^\\d+\\.?\\d*$"
    ),
    bolus_insulin_dose = str_extract(
      bolus_insulin, 
      "(?<=, )\\d+|\\d+(?= IU)|^\\d+$"
    ),
    intermediate_insulin_dose = str_extract(
      Intermediate_insulin, 
      "(?<=, )\\d+|\\d+(?= IU)|^\\d+$"
    )
  )

# Convert doses to numeric and adjust basal insulin for hourly measurements
data$basal_insulin_dose <- as.numeric(data$basal_insulin_dose)
data$basal_insulin_dose <- ifelse(
  data$basal_insulin_dose <= 0.9, 
  data$basal_insulin_dose / 4, 
  data$basal_insulin_dose
)

# Create binary indicators for insulin types
data$basal_insulin <- ifelse(!is.na(data$basal_insulin), 1, 0)
data$bolus_insulin <- ifelse(!is.na(data$bolus_insulin), 1, 0)
data$Intermediate_insulin <- ifelse(!is.na(data$Intermediate_insulin), 1, 0)
data$Non.insulin.hypoglycemic.agents <- ifelse(
  !is.na(data$Non.insulin.hypoglycemic.agents), 
  1, 
  0
)

# =============================================================================
# 4. Insulin Effect Time Calculation
# =============================================================================
# Convert Date to POSIXct format
data <- data %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# =============================================================================
# 4.1 Basal Insulin Effect Time
# =============================================================================
# Basal insulin: effect starts after 2h and lasts for 24h
data <- data %>%
  group_by(ID) %>%
  mutate(
    AdminTime = if_else(basal_insulin == 1, Date, as.POSIXct(NA, tz = "UTC"))
  ) %>%
  fill(AdminTime, .direction = "down") %>%
  mutate(
    EffectStart = if_else(!is.na(AdminTime), AdminTime + hours(2), as.POSIXct(NA, tz = "UTC")),
    EffectEnd = if_else(!is.na(AdminTime), AdminTime + hours(24), as.POSIXct(NA, tz = "UTC")),
    Insulin_Basal_Time = if_else(
      !is.na(EffectStart) & !is.na(EffectEnd) & 
        Date >= EffectStart & Date <= EffectEnd, 
      1, 
      0
    )
  ) %>%
  dplyr::select(-c(AdminTime, EffectStart, EffectEnd))


# =============================================================================
# 4.2 Intermediate Insulin Effect Time
# =============================================================================
# Intermediate insulin: effect starts after 2h and lasts for 24h
data <- data %>%
  group_by(ID) %>%
  mutate(
    AdminTime = if_else(Intermediate_insulin == 1, Date, as.POSIXct(NA, tz = "UTC"))
  ) %>%
  fill(AdminTime, .direction = "down") %>%
  mutate(
    EffectStart = if_else(!is.na(AdminTime), AdminTime + hours(2), as.POSIXct(NA, tz = "UTC")),
    EffectEnd = if_else(!is.na(AdminTime), AdminTime + hours(24), as.POSIXct(NA, tz = "UTC")),
    Intermediate_insulin_Time = if_else(
      !is.na(EffectStart) & !is.na(EffectEnd) & 
        Date >= EffectStart & Date <= EffectEnd, 
      1, 
      0
    )
  ) %>%
  select(-c(AdminTime, EffectStart, EffectEnd))

# =============================================================================
# 4.3 Bolus Insulin Effect Time
# =============================================================================
# Bolus insulin: effect starts after 15 minutes and lasts for 4h
data <- data %>%
  group_by(ID) %>%
  mutate(
    AdminTime = if_else(bolus_insulin == 1, Date, as.POSIXct(NA, tz = "UTC"))
  ) %>%
  fill(AdminTime, .direction = "down") %>%
  mutate(
    EffectStart = if_else(!is.na(AdminTime), AdminTime + minutes(15), as.POSIXct(NA, tz = "UTC")),
    EffectEnd = if_else(!is.na(AdminTime), AdminTime + hours(4), as.POSIXct(NA, tz = "UTC")),
    Bolus_insulin_Time = if_else(
      !is.na(EffectStart) & !is.na(EffectEnd) & 
        Date >= EffectStart & Date <= EffectEnd, 
      1, 
      0
    )
  ) %>%
  dplyr::select(-c(AdminTime, EffectStart, EffectEnd))

# =============================================================================
# 4.4 Non-Insulin Hypoglycemia Agents Effect Time
# =============================================================================
# Non-insulin agents: effect starts immediately and lasts for 6h
data <- data %>%
  group_by(ID) %>%
  mutate(
    AdminTime = if_else(Non.insulin.hypoglycemic.agents == 1, Date, as.POSIXct(NA, tz = "UTC"))
  ) %>%
  fill(AdminTime, .direction = "down") %>%
  mutate(
    EffectStart = if_else(!is.na(AdminTime), AdminTime, as.POSIXct(NA, tz = "UTC")),
    EffectEnd = if_else(!is.na(AdminTime), AdminTime + hours(6), as.POSIXct(NA, tz = "UTC")),
    Non_Insulin_Time = if_else(
      !is.na(EffectStart) & !is.na(EffectEnd) & 
        Date >= EffectStart & Date <= EffectEnd, 
      1, 
      0
    )
  ) %>%
  select(-c(AdminTime, EffectStart, EffectEnd))

# =============================================================================
# 5. Combined Hypoglycemia Agent Effect
# =============================================================================
# Create overall hypoglycemia agent effect indicator
data$Hypoglycemia_agent_effect <- coalesce(
  data$Bolus_insulin_Time, 
  data$Insulin_Basal_Time, 
  data$Intermediate_insulin_Time, 
  data$Non_Insulin_Time
)

# =============================================================================
# 6. Add Lagged Dose Information
# =============================================================================
# =============================================================================
# 6.1 Bolus Insulin Dose Time
# =============================================================================
data$help_bolus_insulin_dose_time <- data$bolus_insulin_dose
data <- data %>% 
  dplyr::group_by(ID) %>% 
  arrange(Date) %>% 
  do(fill(., help_bolus_insulin_dose_time, .direction = 'down')) %>% 
  dplyr::ungroup()
data$bolus_insulin_dose_time <- ifelse(
  data$Bolus_insulin_Time == 1, 
  data$help_bolus_insulin_dose_time, 
  NA
)
data$help_bolus_insulin_dose_time <- NULL

# =============================================================================
# 6.2 Basal Insulin Dose Time
# =============================================================================
data$help_basal_insulin_dose_time <- data$basal_insulin_dose
data <- data %>% 
  dplyr::group_by(ID) %>% 
  arrange(Date) %>% 
  do(fill(., help_basal_insulin_dose_time, .direction = 'down')) %>% 
  dplyr::ungroup()
data$basal_insulin_dose_time <- ifelse(
  data$Insulin_Basal_Time == 1, 
  data$help_basal_insulin_dose_time, 
  NA
)
data$help_basal_insulin_dose_time <- NULL

# =============================================================================
# 6.3 Intermediate Insulin Dose Time
# =============================================================================
data$help_intermediate_insulin_dose_time <- data$intermediate_insulin_dose
data <- data %>% 
  dplyr::group_by(ID) %>% 
  arrange(Date) %>% 
  do(fill(., help_intermediate_insulin_dose_time, .direction = 'down')) %>% 
  dplyr::ungroup()
data$intermediate_insulin_dose_time <- ifelse(
  data$Intermediate_insulin_Time == 1, 
  data$help_intermediate_insulin_dose_time, 
  NA
)
data$help_intermediate_insulin_dose_time <- NULL

# =============================================================================
# 7. Save Processed Data
# =============================================================================
write.csv(data, "Data_Analysis/Data/Data_total_04.csv", row.names = FALSE)





