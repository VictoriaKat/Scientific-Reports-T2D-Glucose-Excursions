# =============================================================================
# Merge Baseline and Longitudinal Data
# =============================================================================

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(readxl)
library(tidyr)

# =============================================================================
# 2. Load Data
# =============================================================================
# Read baseline summary data
Shanghai_T2DM_Summary <- read_excel("diabetes_datasets/Shanghai_T2DM_Summary.xlsx")
summary(Shanghai_T2DM_Summary)

# Read processed food groups data
CGM_Food_df <- read.csv("Data_Analysis/Data/Food_Groups_02.csv")

# =============================================================================
# 3. Clean and Process Baseline Data
# =============================================================================
# Examine data structure
colnames(Shanghai_T2DM_Summary)
str(Shanghai_T2DM_Summary)

# =============================================================================
# 4. Process Smoking History
# =============================================================================
# Convert smoking history to binary (smoker/non-smoker)
Shanghai_T2DM_Summary$`Smoking History (pack year)` <- as.numeric(Shanghai_T2DM_Summary$`Smoking History (pack year)`)
Shanghai_T2DM_Summary$Smoking_history <- ifelse(
  Shanghai_T2DM_Summary$`Smoking History (pack year)` > 0, 
  "smoker", 
  "non-smoker"
)
Shanghai_T2DM_Summary$`Smoking History (pack year)` <- NULL

# =============================================================================
# 5. Process Diabetes Type
# =============================================================================
# Verify all patients are T2D and remove redundant column
summary(as.factor(Shanghai_T2DM_Summary$`Type of Diabetes`)) #all T2D
Shanghai_T2DM_Summary$`Type of Diabetes` <- NULL 

# =============================================================================
# 6. Process BMI Data
# =============================================================================
# Calculate BMI from weight and height
Shanghai_T2DM_Summary$BMI <- Shanghai_T2DM_Summary$`Weight (kg)` / 
                            (Shanghai_T2DM_Summary$`Height (m)`)^2

# Remove redundant columns
Shanghai_T2DM_Summary$`BMI (kg/m2)` <- NULL
Shanghai_T2DM_Summary$`Height (m)` <- NULL
Shanghai_T2DM_Summary$`Weight (kg)` <- NULL

# =============================================================================
# 7. Process Complications Data
# =============================================================================
# Acute Diabetic Complications
summary(as.factor(Shanghai_T2DM_Summary$`Acute Diabetic Complications`)) #all none
Shanghai_T2DM_Summary$`Acute Diabetic Complications` <- NULL

# Macrovascular Complications
summary(as.factor(Shanghai_T2DM_Summary$`Diabetic Macrovascular  Complications`))
Shanghai_T2DM_Summary$Macrovascular_Complications <- ifelse(
  Shanghai_T2DM_Summary$`Diabetic Macrovascular  Complications` == "none", 
  "no", 
  "yes"
)
Shanghai_T2DM_Summary$`Diabetic Macrovascular  Complications` <- NULL

# Microvascular Complications
summary(as.factor(Shanghai_T2DM_Summary$`Diabetic Microvascular Complications`))
Shanghai_T2DM_Summary$Microvascular_Complications <- ifelse(
  Shanghai_T2DM_Summary$`Diabetic Microvascular Complications` == "none", 
  "no", 
  "yes"
)
Shanghai_T2DM_Summary$`Diabetic Microvascular Complications` <- NULL

# Comorbidities
summary(as.factor(Shanghai_T2DM_Summary$Comorbidities))
Shanghai_T2DM_Summary$Comorbidities <- ifelse(
  Shanghai_T2DM_Summary$Comorbidities == "none", 
  "no", 
  "yes"
)

# Create combined complications indicator
Shanghai_T2DM_Summary$Complications <- ifelse(
  Shanghai_T2DM_Summary$Macrovascular_Complications == "yes" |
  Shanghai_T2DM_Summary$Microvascular_Complications == "yes" |
  Shanghai_T2DM_Summary$Comorbidities == "yes", 
  1, 
  0
)

# =============================================================================
# 8. Process Medication Data
# =============================================================================
# Rename and process hypoglycemia agents
names(Shanghai_T2DM_Summary)[7] <- "Hypoglycemia_Agents"
Shanghai_T2DM_Summary <- Shanghai_T2DM_Summary %>%
  mutate(Hypoglycemia_Agents = strsplit(as.character(Hypoglycemia_Agents), ",\\s*")) %>%
  unnest(Hypoglycemia_Agents)

# Create binary indicators for each hypoglycemia agent
Shanghai_T2DM_Summary <- pivot_wider(
  Shanghai_T2DM_Summary, 
  names_from = Hypoglycemia_Agents, 
  values_from = Hypoglycemia_Agents,
  values_fn = list(Hypoglycemia_Agents = function(x) 1), 
  values_fill = list(Hypoglycemia_Agents = 0),
  names_prefix = "Hypoglycemia_Agents_"
)

# Process other agents
summary(as.factor(Shanghai_T2DM_Summary$`Other Agents`))
Shanghai_T2DM_Summary$Other_Agents <- ifelse(
  Shanghai_T2DM_Summary$`Other Agents` == "none", 
  "no", 
  "yes"
)
Shanghai_T2DM_Summary$`Other Agents` <- NULL

# =============================================================================
# 9. Process Clinical Variables
# =============================================================================
# Hypoglycemia status
summary(as.factor(Shanghai_T2DM_Summary$`Hypoglycemia (yes/no)`))
Shanghai_T2DM_Summary$Hypoglycemia <- ifelse(
  Shanghai_T2DM_Summary$`Hypoglycemia (yes/no)` == "no", 
  "no", 
  "yes"
)
Shanghai_T2DM_Summary$`Hypoglycemia (yes/no)` <- NULL

# Duration of diabetes
Shanghai_T2DM_Summary$`Duration of diabetes (years)` <- round(
  Shanghai_T2DM_Summary$`Duration of diabetes (years)`, 
  2
)

# =============================================================================
# 10. Process Blood Biomarkers
# =============================================================================
# Convert blood biomarker columns to numeric and round to 2 decimal places
Shanghai_T2DM_Summary <- Shanghai_T2DM_Summary %>%
  mutate(across(
    .cols = `Fasting Plasma Glucose (mg/dl)`:`Blood Urea Nitrogen (mmol/L)`,
    .fns = as.numeric
  )) 

Shanghai_T2DM_Summary <- Shanghai_T2DM_Summary %>% 
  mutate(across(
    .cols = `Fasting Plasma Glucose (mg/dl)`:`Blood Urea Nitrogen (mmol/L)`,
    .fns = ~ round(., 2)
  ))

# =============================================================================
# 11. Merge and Save Data
# =============================================================================
# Merge baseline and longitudinal data
Data <- merge(
  CGM_Food_df, 
  Shanghai_T2DM_Summary, 
  by.x = "ID", 
  by.y = "Patient Number"
)

# Save merged dataset
write.csv(Data, "Data_Analysis/Data/Data_total_03.csv", row.names = FALSE)

