# =============================================================================
# Data Import and Initial Processing
# =============================================================================

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)

# =============================================================================
# 2. Import Summary Data
# =============================================================================
# Import and examine summary data
Shanghai_T2DM_Summary <- readxl::read_excel("diabetes_datasets/Shanghai_T2DM_Summary.xlsx")
summary(Shanghai_T2DM_Summary)

# =============================================================================
# 3. Import and Process CGM Data
# =============================================================================
# Set path for CGM data files
folder_path <- "/Users/victoriabrugger/Documents/GitHub/Cursor-Repo/Scientific-Reports-T2D-Glucose-Excursions/diabetes_datasets/Shanghai_T2DM"
excel_files <- list.files(path = folder_path, full.names = TRUE)

# Define function to process Excel files
process_excel_file <- function(file) {
  # Read Excel file without specifying col_types for proper date detection
  data <- read_excel(file)
  
  # Define columns to check and process
  columns_to_check <- c(
    "CSII - basal insulin (Novolin R, IU / H)",
    "Non-insulin hypoglycemic agents",
    "Non-insulin.hypoglycemic.agents",
    "Insulin.dose.-.s.c.",
    "Insulin.dose.-.i.v.",
    "CSII.-.basal.insulin.(Novolin.R,.IU./.H)"
  )
  
  # Convert specified columns to character and handle NAs
  for(col in columns_to_check) {
    if(col %in% names(data)) {
      data[[col]] <- as.character(data[[col]])
      data[[col]][is.na(data[[col]])] <- NA
    } else {
      data[[col]] <- NA
    }
  }
  
  #Process Date column with multiple format handling
  if("Date" %in% names(data)) {
    tryCatch({
      if(is.numeric(data$Date)) {
        # Convert Excel serial numbers to dates
        data$Date <- as.POSIXct(data$Date  * 86400, origin = "1899-12-30", tz = "UTC")
      } else if(is.character(data$Date)) {
        # Try multiple date formats
        data$Date <- parse_date_time(data$Date, 
                                   orders = c("ymd HMS", "ymd HM", "ymd H", "ymd", 
                                            "dmy HMS", "dmy HM", "dmy H", "dmy",
                                            "mdy HMS", "mdy HM", "mdy H", "mdy"))
      }
    }, error = function(e) {
      warning(paste("Date conversion issue in file:", basename(file)))
    })
  }
  
  # Add filename as ID column
  file_name <- tools::file_path_sans_ext(basename(file))
  data <- mutate(data, FileName = file_name)
  
  return(data)
}

# Get all Excel files (both .xls and .xlsx)
excel_files <- list.files(
  path = folder_path, 
  pattern = "\\.(xls|xlsx)$", 
  full.names = TRUE
)

# Process all files and combine into single dataframe
combined_data <- map_df(excel_files, process_excel_file)
View(combined_data)

# =============================================================================
# 4. Clean and Reorganize Data
# =============================================================================
# Remove unnecessary columns
combined_data$`胰岛素泵基础量 (Novolin R, IU / H)` <- NULL
combined_data$饮食 <- NULL
combined_data$进食量 <- NULL

# Reorder columns with ID first
combined_data <- combined_data %>% select(FileName, everything())
colnames(combined_data)[colnames(combined_data) == 'FileName'] <- 'ID'

# =============================================================================
# 5. Consolidate Measurement Columns
# =============================================================================
# Consolidate CGM measurements
combined_data$`CGM (mg / dl)` <- coalesce(combined_data$`CGM (mg / dl)`, combined_data$CGM)
combined_data$CGM <- NULL

# Consolidate CBG measurements
combined_data$`CBG (mg / dl)` <- coalesce(combined_data$`CBG (mg / dl)`, combined_data$CBG)
combined_data$CBG <- NULL

# Consolidate Ketone measurements
combined_data$`Blood Ketone (mmol / L)` <- coalesce(combined_data$`Blood Ketone (mmol / L)`, combined_data$`Blood Ketone`)
combined_data$`Blood Ketone` <- NULL

# Consolidate Non-Insulin Hypoglycemic Agent columns
combined_data$`Non-insulin hypoglycemic agents` <- coalesce(
  combined_data$`Non-insulin hypoglycemic agents`, 
  combined_data$`Non-insulin.hypoglycemic.agents`
)
combined_data$`Non-insulin.hypoglycemic.agents` <- NULL

# =============================================================================
# 6. Process Insulin Data
# =============================================================================
# Consolidate IV Insulin columns
combined_data$`Insulin dose - i.v.` <- coalesce(
  combined_data$`Insulin dose - i.v.`, 
  combined_data$`Insulin.dose.-.i.v.`
)
combined_data$`Insulin.dose.-.i.v.` <- NULL

# Consolidate SC Insulin columns
combined_data$`Insulin dose - s.c.` <- coalesce(
  combined_data$`Insulin dose - s.c.`, 
  combined_data$`Insulin.dose.-.s.c.`
)
combined_data$`Insulin.dose.-.s.c.` <- NULL

# Process Basal Insulin
combined_data$`CSII - basal insulin (Novolin R, IU / H)` <- ifelse(
  combined_data$`CSII - basal insulin (Novolin R, IU / H)`==0, 
  NA, 
  combined_data$`CSII - basal insulin (Novolin R, IU / H)`
)

# Consolidate Basal Insulin columns
combined_data$basal_insulin <- coalesce(
  combined_data$`CSII - basal insulin`,
  combined_data$`CSII.-.basal.insulin.(Novolin.R,.IU./.H)`,
  combined_data$`CSII - basal insulin (Novolin R, IU / H)`,
  combined_data$`CSII - basal insulin (Novolin R  IU / H)`
)

# Remove redundant basal insulin columns
combined_data$`CSII - basal insulin` <- NULL
combined_data$`CSII.-.basal.insulin.(Novolin.R,.IU./.H)` <- NULL
combined_data$`CSII - basal insulin (Novolin R, IU / H)` <- NULL
combined_data$`CSII.-.basal.insulin.(Novolin.R.IU./.H)` <- NULL
combined_data$`CSII - basal insulin (Novolin R  IU / H)` <- NULL

# Process Bolus Insulin
combined_data$`CSII - bolus insulin (Novolin R, IU)` <- ifelse(
  combined_data$`CSII - bolus insulin (Novolin R, IU)`==0, 
  NA, 
  combined_data$`CSII - bolus insulin (Novolin R, IU)`
)

# Consolidate Bolus Insulin columns
combined_data$bolus_insulin <- coalesce(
  combined_data$`CSII - bolus insulin`,
  combined_data$`CSII - bolus insulin (Novolin R, IU)`,
  combined_data$`CSII - bolus insulin (Novolin R  IU)`
)

# Remove redundant bolus insulin columns
combined_data$`CSII - bolus insulin` <- NULL
combined_data$`CSII - bolus insulin (Novolin R, IU)` <- NULL
combined_data$`CSII - bolus insulin (Novolin R  IU)` <- NULL

# =============================================================================
# 7. Process Subcutaneous Insulin Types
# =============================================================================
# Identify and process different types of SC insulin
summary(as.factor(combined_data$`Insulin dose - s.c.`)) # insulin degludec,insulin detemir, glargine --> basal; 
# Novolin, Humulin --> intermediate; SciLin, aspart, Gansulin, glulisine --> bolus

# Process Bolus SC Insulin
combined_data$Insulin_bolus <- ifelse(
  grepl("SciLin|aspart|Gansulin|glulisine", combined_data$`Insulin dose - s.c.`), 
  combined_data$`Insulin dose - s.c.`,
  NA
)

# Consolidate Bolus Insulin
combined_data$bolus_insulin <- as.character(combined_data$bolus_insulin)
combined_data$bolus_insulin <- coalesce(
  combined_data$bolus_insulin,
  combined_data$Insulin_bolus
)
combined_data$Insulin_bolus <- NULL

# Process Basal SC Insulin
combined_data$Insulin_basal <- ifelse(
  grepl("degludec|detemir|glargine", combined_data$`Insulin dose - s.c.`), 
  combined_data$`Insulin dose - s.c.`,
  NA
)

# Consolidate Basal Insulin
combined_data$basal_insulin <- as.character(combined_data$basal_insulin)
combined_data$basal_insulin <- coalesce(
  combined_data$basal_insulin,
  combined_data$Insulin_basal
)
combined_data$Insulin_basal <- NULL

# Process Intermediate SC Insulin
combined_data$Insulin_intermediate <- ifelse(
  grepl("Humulin|Novolin", combined_data$`Insulin dose - s.c.`), 
  combined_data$`Insulin dose - s.c.`,
  NA
)
combined_data$`Insulin dose - s.c.` <- NULL

# =============================================================================
# 8. Process IV Insulin Types
# =============================================================================
# Process Basal IV Insulin
combined_data$Insulin_basal <- ifelse(
  grepl("basal", combined_data$`Insulin dose - i.v.`), 
  combined_data$`Insulin dose - i.v.`,
  NA
)

# Consolidate Basal Insulin
combined_data$basal_insulin <- coalesce(
  combined_data$basal_insulin,
  combined_data$Insulin_basal
)
combined_data$Insulin_basal <- NULL

# Process Intermediate IV Insulin
combined_data$Intermediate_insulin <- ifelse(
  grepl("IU Novolin", combined_data$`Insulin dose - i.v.`), 
  combined_data$`Insulin dose - i.v.`,
  NA
)

# Consolidate Intermediate Insulin
combined_data$Intermediate_insulin <- coalesce(
  combined_data$Insulin_intermediate,
  combined_data$Intermediate_insulin
)
combined_data$Insulin_intermediate <- NULL
combined_data$`Insulin dose - i.v.` <- NULL

# =============================================================================
# 9. Process Dietary Data
# =============================================================================
# Rename and clean dietary intake column
combined_data$Dietary_intake <- combined_data$'Dietary intake'
combined_data$`Dietary intake` <- NULL

# =============================================================================
# 10. Save Processed Data
# =============================================================================
write.csv(combined_data, "Data_Analysis/Data/T2D_CGM_Food_01.csv", row.names = FALSE)



