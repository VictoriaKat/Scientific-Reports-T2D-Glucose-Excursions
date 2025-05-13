# =============================================================================
# XGBoost Analysis Script
# =============================================================================

# =============================================================================
# 1. Load Libraries and Set Up Environment
# =============================================================================
library(xgboost)
library(ggplot2)
library(caret)
library(pROC)
library(MLmetrics)
library(parallel)
library(doParallel)
library(dplyr)
library(SHAPforxgboost)

# Set working directory to Shanghai_T2DM-------------------------------------
setwd("/Users/victoriabrugger/Documents/GitHub/Cursor-Repo/Scientific-Reports-T2D-Glucose-Excursions")

# Increase cores -------------------------------------
cores <- detectCores()
registerDoParallel(cores)

# =============================================================================
# 2. Data Loading and Initial Processing
# =============================================================================
# Read Data -------------------------------------
postprandial_df <- read.csv("Data_Analysis/Data/Data_total_06.csv")

#Remove those individuals who only have all 0 or all 1 in the outcome - postprandial_rolling_all group_by ID
postprandial_df <- postprandial_df %>%
  group_by(ID) %>%
  filter(
    sum(postprandial_rolling_all) != 0 & 
    sum(postprandial_rolling_all) != n()
  ) %>%
  ungroup()
  
# =============================================================================
# 3. Data Quality Checks
# =============================================================================
# Check which IDs are not in All_df
# Get more information about these IDs
# Create summary of postprandial rolling binary numbers for each ID
postprandial_summary <- postprandial_df %>%
  group_by(ID) %>%
  summarize(
    total_observations = n(),
    count_0 = sum(postprandial_rolling_all == 0),
    count_1 = sum(postprandial_rolling_all == 1),
    percentage_1 = (count_1 / total_observations) * 100
  ) %>%
  arrange(ID)

# Print the summary
print(postprandial_summary)
hist(postprandial_summary$percentage_1)

# Write to CSV if needed
write.csv(postprandial_summary, "Data_Analysis/Data/postprandial_binary_summary.csv", row.names = FALSE)

# =============================================================================
# 4. Data Preparation and Feature Selection
# =============================================================================
# Variables Set-up--------------------------
# Remove some variables from dataset
### Only have relevant glucose outcomes/values 
#### Manual as I wanted to go through them again 
postprandial_df$premeal_glu <- NULL
postprandial_df$mean_postprandial <- NULL
postprandial_df$mean_postprandial_previous_24h <- NULL
postprandial_df$mean_postprandial_ID <- NULL
postprandial_df$postprandial_constant <- NULL
postprandial_df$iAUC <- NULL
postprandial_df$time <- NULL
postprandial_df$date <- NULL

# Select only important variables 
postprandial_df <- postprandial_df %>% dplyr::select(
  ID, Date, Non.insulin.hypoglycemic.agents,
  basal_insulin, bolus_insulin, Intermediate_insulin,
  Staples_g:Sweets_bin, total_g,
  Day,
  basal_insulin_dose:month,
  Staples_g_8h:Sweets_g_24h, Total_g_8h:Sweets_bin_24h, Meal_bin_8h, 
  Meal_bin_24h, Hour, Meal_time, postprandial_rolling_all, weekday_num, PPG_spike_prev
)

## List of IDs to Match later ID Files numbers 
ID_file_df <- unique(postprandial_df$ID)
ID_file_df <- as.data.frame(ID_file_df)
ID_file_df$List_nr <- c(1:nrow(ID_file_df))

write.csv(ID_file_df, "Data_Analysis/Data/ID_list.csv") 

### Transform data into usable format 
postprandial_df$ID <- as.character(gsub("_", "", postprandial_df$ID))

# Handle missing values in insulin dose columns
postprandial_df$basal_insulin_dose <- ifelse(
  is.na(postprandial_df$basal_insulin_dose), 0, postprandial_df$basal_insulin_dose
)

postprandial_df$bolus_insulin_dose <- ifelse(
  is.na(postprandial_df$bolus_insulin_dose), 0, postprandial_df$bolus_insulin_dose
)

postprandial_df$intermediate_insulin_dose <- ifelse(
  is.na(postprandial_df$intermediate_insulin_dose), 0, postprandial_df$intermediate_insulin_dose
)

postprandial_df$basal_insulin_dose_time <- ifelse(
  is.na(postprandial_df$basal_insulin_dose_time), 0, postprandial_df$basal_insulin_dose_time
)

postprandial_df$bolus_insulin_dose_time <- ifelse(
  is.na(postprandial_df$bolus_insulin_dose_time), 0, postprandial_df$bolus_insulin_dose_time
)

postprandial_df$intermediate_insulin_dose_time <- ifelse(
  is.na(postprandial_df$intermediate_insulin_dose_time), 0, postprandial_df$intermediate_insulin_dose_time
)

ID_file <- unique(postprandial_df$ID)

# Generate summary statistics
summary(postprandial_df)

summary <- postprandial_df %>% group_by(ID) %>% count((n()))
summary2 <- postprandial_df %>% 
  group_by(ID) %>% 
  summarize(total_postprandial = sum(postprandial_rolling_all, na.rm = TRUE))

summary$post <- summary2$total_postprandial
summary3 <- summary %>% mutate(non_post = n-post)
sum(summary3$post)
summary3$post_perc <- (100/summary3$n)*summary3$post
mean(summary3$post_perc)
sd(summary3$post_perc)

# =============================================================================
# 5. Helper Functions
# =============================================================================
## Functions to impute and align training and test data -------------------------
impute_and_add_dummy <- function(data) {
  data_imputed <- data
  columns_to_remove <- c()
  
  for (col in colnames(data)) {
    if (all(is.na(data[[col]]))) { # Check if the entire column is NA
      columns_to_remove <- c(columns_to_remove, col)
    } else if (any(is.na(data[[col]]))) { # Check if there are any NAs
      data_imputed[[paste0(col, "_missing")]] <- as.numeric(is.na(data[[col]]))
      data_imputed[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
    }
  }
  
  # Remove columns that contain only NAs
  data_imputed <- data_imputed[, !colnames(data_imputed) %in% columns_to_remove]
  
  return(data_imputed)
}

align_columns <- function(X_train, X_test) {
  # Find columns that are in X_train but not in X_test
  missing_cols <- setdiff(colnames(X_train), colnames(X_test))
  
  # Add missing columns to X_test with NA values
  for (col in missing_cols) {
    X_test[[col]] <- NA
  }
  
  # Ensure columns are in the same order as in X_train
  X_test <- X_test[, colnames(X_train)]
  
  return(X_test)
}

# =============================================================================
# 6. Hyperparameter Tuning
# =============================================================================
best_list1 <- list()

for(i in seq_along(ID_file)) {
  # Filter data for the current ID
  person_data <- postprandial_df %>% filter(ID == ID_file[i])
  
  # Debug data preparation
  print(paste("ID:", ID_file[i]))
  print(paste("Number of rows in person_data:", nrow(person_data)))
  print(paste("Unique postprandial_rolling_all values:", 
              paste(unique(person_data$postprandial_rolling_all), collapse=", ")))
  
  # Create 70/30 split
  set.seed(123)  # For reproducibility
  person_data <- person_data %>% arrange(Date)
  person_data$Date <- NULL
  
  split_index <- floor(0.7 * nrow(person_data)) # split
  
  X_train <- person_data[1:split_index, ] # select training data 
  X_test <- person_data[(split_index+1):nrow(person_data), ] # select testing data
  
  y_train <- X_train$postprandial_rolling_all 
  y_test <- X_test$postprandial_rolling_all 
  
  # Remove target variable from training and testing data
  X_train$ID <- NULL
  X_train$postprandial_rolling_all <- NULL
  X_test$ID <- NULL
  X_test$postprandial_rolling_all <- NULL
  
  X_train$Date <- NULL
  X_test$Date <- NULL
  
  # Convert X_train and X_test to matrix
  X_train <- as.matrix(X_train)
  X_test <- as.matrix(X_test)
  
  ### Model building
  hyperparam_grid <- expand.grid(
    nrounds = seq(from = 50, to = 150, by = 50), # 50
    eta = c(0, 0.0001, 0.001, 0.01, 0.015), # 0.01
    max_depth = c(1, 2, 3), # 3
    gamma = c(-0.5, 0, 0.5), # 1
    colsample_bytree = c(0.5, 1, 2), # 1
    min_child_weight = c(1, 2), # 0
    subsample = 1
  )
  
  tune_control <- caret::trainControl(
    method = "cv", # cross-validation
    number = 4, # with n folds
    verboseIter = FALSE, # no training log
    allowParallel = FALSE
  )
  
  bst <- caret::train(
    x = X_train,
    y = as.factor(y_train),
    trControl = tune_control,
    tuneGrid = hyperparam_grid,
    method = "xgbTree", # this says we want XGB
    verbose = FALSE,
    verbosity = 0
  )
  
  best_list1[[i]] <- bst$bestTune
  
  print(paste("This is cycle", i, "of", length(ID_file)))
  flush.console()  # Ensure the output is immediately printed to the console
}

best_list1 <- data.frame(matrix(unlist(best_list1), nrow=length(best_list1), byrow=TRUE))

write.csv(best_list1, "Data_Analysis/Data/best_hyperpara_personalized_self_reported.csv") # Un-comment if R crashes
best_list1 <- read.csv("Data_Analysis/Data/best_hyperpara_personalized_self_reported.csv")
best_list1$X <- NULL

# =============================================================================
# 7. Model Training and Evaluation
# =============================================================================
# Initialize metrics storage
all_accuracies <- numeric(length(ID_file))
importance_list2 <- list()
all_recalls <- numeric(length(ID_file))
all_precisions <- numeric(length(ID_file))
all_f1_scores <- numeric(length(ID_file))
all_aucs <- numeric(length(ID_file))
all_shap_values <- list() 

# seeds <- vector(mode = "list", length = length(ID_file))

names(best_list1) <- c(
  "nrounds", "max_depth", "eta", "gamma", 
  "colsample_bytree", "min_child_weight", "subsample"
)

# Loop model 
for (i in seq_along(ID_file)) {
  message("=== Iteration ", i, " for ID: ", ID_file[i], " ===")
  
  # Prepare and split data
  person_data <- postprandial_df %>%
    filter(ID == ID_file[i]) %>%
    arrange(Date)
  
  split_idx <- floor(0.7 * nrow(person_data))
  X_train <- person_data[1:split_idx, ]
  X_test  <- person_data[(split_idx + 1):nrow(person_data), ]
  
  # Convert to numeric
  X_train <- mutate_all(X_train, ~ as.numeric(as.character(.)))
  X_test  <- mutate_all(X_test,  ~ as.numeric(as.character(.)))
  
  # Extract target
  y_train <- X_train$postprandial_rolling_all
  y_test  <- X_test$postprandial_rolling_all
  
  # Drop unnecessary columns
  cols_to_remove <- c("ID", "Date", "postprandial_rolling_all")
  X_train <- X_train[ , !(names(X_train) %in% cols_to_remove)]
  X_test  <- X_test[ , !(names(X_test) %in% cols_to_remove)]
  
  # Preprocessing
  X_train <- impute_and_add_dummy(X_train)
  X_test  <- impute_and_add_dummy(X_test)
  X_test  <- align_columns(X_train, X_test)
  
  X_train <- as.matrix(X_train)
  X_test  <- as.matrix(X_test)
  
  # Cross-validation control
  N <- length(y_train)
  k <- 4
  horizon <- floor(N / (k + 1))
  init_win <- N - horizon * k
  
  set.seed(123)
  my_ctrl <- trainControl(
    method = "timeslice",
    initialWindow = init_win,
    horizon = horizon,
    fixedWindow = FALSE,
    allowParallel = TRUE,
    verboseIter = TRUE
  )
  
  # Hyperparameter grid from best list
  bst <- best_list1[i, ]
  # Ensure that the parameters are numeric or characters as required
  
  grid <- expand.grid(
    nrounds = bst$nrounds,
    eta = bst$eta,
    max_depth = bst$max_depth,
    gamma = bst$gamma,
    colsample_bytree = bst$colsample_bytree,
    min_child_weight = bst$min_child_weight,
    subsample = bst$subsample
  )
  
  # Model training
  model <- caret::train(
    x = X_train,
    y = as.factor(y_train),
    method = "xgbTree",
    trControl = my_ctrl,
    tuneGrid = grid,
    verbose = FALSE
  )
  
  # Prediction
  y_prob <- predict(model, X_test, type = "prob")[, 2]
  y_pred <- ifelse(y_prob > 0.5, 1, 0)
  
  # Metrics
  y_test_factor <- factor(y_test, levels = c(0, 1))
  y_pred_factor <- factor(y_pred, levels = c(0, 1))
  
  acc <- mean(y_pred_factor == y_test_factor)
  rec <- recall(y_pred_factor, y_test_factor)
  prec <- precision(y_pred_factor, y_test_factor)
  f1 <- 2 * prec * rec / (prec + rec)
  acc <- mean(y_pred_factor == y_test_factor)
  auc <- MLmetrics::AUC(y_pred = y_prob, y_true = y_test)
  
  # Save results
  all_accuracies[i]   <- acc
  all_recalls[i]      <- rec
  all_precisions[i]   <- prec
  all_f1_scores[i]    <- f1
  all_aucs[i]         <- auc
  importance_list2[[i]] <- varImp(model)[["importance"]]
  
  # Calculate SHAP values
  xgb_model <- model$finalModel
  shap_values <- shap.values(xgb_model, X_train)
  shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = X_train)
  all_shap_values[[i]] <- shap_long  # Store SHAP values for this ID
  all_shap_values[[i]]$List_Number <- i
  
  message("✓ Completed ID ", ID_file[i], " | Acc: ", round(acc, 3), 
          ", F1: ", round(f1, 3), ", AUC: ", round(auc, 3))
}

# Collate results
metrics_df2 <- data.frame(
  accuracy = all_accuracies,
  recall = all_recalls,
  precision = all_precisions,
  f1_score = all_f1_scores,
  auc = all_aucs
)

summary(metrics_df2)

metrics_df2$ID <- ID_file_df$ID_file_df
write.csv(metrics_df2, "Data_Analysis/Data/Metrics_personalized_self_reported.csv")

importance_list2 <- purrr::imap_dfr(importance_list2, ~ mutate(.x, List_Number = .y))
write.csv(importance_list2, "Data_Analysis/Data/Imporance_personalized_self_reported.csv")

all_shap_values_df_self_reported <- bind_rows(all_shap_values)
write.csv(all_shap_values_df_self_reported, "Data_Analysis/Data/SHAP_personalized_self_reported.csv")

# =============================================================================
# 8. Non Self Reported Features Data Preparation and Feature Selection
# =============================================================================
# Variables Set-up--------------------------
# Remove some variables from dataset
### Only have relevant glucose outcomes/values 
#### Manual as I wanted to go through them again 
postprandial_df <- postprandial_df %>% dplyr::select(
  ID, Date, 
  Day, day, month,
  Hour, postprandial_rolling_all, 
  weekday_num, PPG_spike_prev
)

ID_file <- unique(postprandial_df$ID)

# =============================================================================
# 9. Hyperparameter Tuning
# =============================================================================
best_list2 <- list()

for(i in seq_along(ID_file)) {
  # Filter data for the current ID
  person_data <- postprandial_df %>% filter(ID == ID_file[i])
  
  # Debug data preparation
  print(paste("ID:", ID_file[i]))
  print(paste("Number of rows in person_data:", nrow(person_data)))
  print(paste("Unique postprandial_rolling_all values:", 
              paste(unique(person_data$postprandial_rolling_all), collapse=", ")))
  
  # Create 70/30 split
  set.seed(123)  # For reproducibility
  person_data <- person_data %>% arrange(Date)
  person_data$Date <- NULL
  
  split_index <- floor(0.7 * nrow(person_data)) # split
  
  X_train <- person_data[1:split_index, ] # select training data 
  X_test <- person_data[(split_index+1):nrow(person_data), ] # select testing data
  
  y_train <- X_train$postprandial_rolling_all 
  y_test <- X_test$postprandial_rolling_all 
  
  # Remove target variable from training and testing data
  X_train$ID <- NULL 
  X_train$postprandial_rolling_all <- NULL
  X_test$ID <- NULL
  X_test$postprandial_rolling_all <- NULL
  
  X_train$Date <- NULL
  X_test$Date <- NULL
  
  # Convert X_train and X_test to matrix
  X_train <- as.matrix(X_train)
  X_test <- as.matrix(X_test)
  
  ### Model building
  hyperparam_grid <- expand.grid(
    nrounds = seq(from = 50, to = 150, by = 50), # 50
    eta = c(0, 0.0001, 0.001, 0.01, 0.015), # 0.01
    max_depth = c(1, 2, 3), # 3
    gamma = c(-0.5, 0, 0.5), # 1
    colsample_bytree = c(0.5, 1, 2), # 1
    min_child_weight = c(1, 2), # 0
    subsample = 1
  )
  
  tune_control <- caret::trainControl(
    method = "cv", # cross-validation
    number = 4, # with n folds
    verboseIter = FALSE, # no training log
    allowParallel = FALSE
  )
  
  bst <- caret::train(
    x = X_train,
    y = as.factor(y_train),
    trControl = tune_control,
    tuneGrid = hyperparam_grid,
    method = "xgbTree", # this says we want XGB
    verbose = FALSE,
    verbosity = 0
  )
  
  best_list2[[i]] <- bst$bestTune
  
  print(paste("This is cycle", i, "of", length(ID_file)))
  flush.console()  # Ensure the output is immediately printed to the console
}

best_list2 <- data.frame(matrix(unlist(best_list2), nrow=length(best_list2), byrow=TRUE))

write.csv(best_list2, "Data_Analysis/Data/best_hyperpara_personalized_non_self_reported.csv") # Un-comment if R crashes
best_list2 <- read.csv("Data_Analysis/Data/best_hyperpara_personalized_non_self_reported.csv")
best_list2$X <- NULL

# =============================================================================
# 10. Model Training and Evaluation
# =============================================================================
# Initialize metrics storage
all_accuracies2 <- numeric(length(ID_file))
importance_list3 <- list()
all_recalls2 <- numeric(length(ID_file))
all_precisions2 <- numeric(length(ID_file))
all_f1_scores2 <- numeric(length(ID_file))
all_aucs2 <- numeric(length(ID_file))
all_shap_values2 <- list() 

names(best_list2) <- c(
  "nrounds", "max_depth", "eta", "gamma", 
  "colsample_bytree", "min_child_weight", "subsample"
)

# Loop model 
for (i in seq_along(ID_file)) {
  message("=== Iteration ", i, " for ID: ", ID_file[i], " ===")
  
  # Prepare and split data
  person_data <- postprandial_df %>%
    filter(ID == ID_file[i]) %>%
    arrange(Date)
  
  split_idx <- floor(0.7 * nrow(person_data))
  X_train <- person_data[1:split_idx, ]
  X_test  <- person_data[(split_idx + 1):nrow(person_data), ]
  
  # Convert to numeric
  X_train <- mutate_all(X_train, ~ as.numeric(as.character(.)))
  X_test  <- mutate_all(X_test,  ~ as.numeric(as.character(.)))
  
  # Extract target
  y_train <- X_train$postprandial_rolling_all
  y_test  <- X_test$postprandial_rolling_all
  
  # Drop unnecessary columns
  cols_to_remove <- c("ID", "Date", "postprandial_rolling_all")
  X_train <- X_train[ , !(names(X_train) %in% cols_to_remove)]
  X_test  <- X_test[ , !(names(X_test) %in% cols_to_remove)]
  
  # Preprocessing
  X_train <- impute_and_add_dummy(X_train)
  X_test  <- impute_and_add_dummy(X_test)
  X_test  <- align_columns(X_train, X_test)
  
  X_train <- as.matrix(X_train)
  X_test  <- as.matrix(X_test)
  
  # Cross-validation control
  N <- length(y_train)
  k <- 4
  horizon <- floor(N / (k + 1))
  init_win <- N - horizon * k
  
  set.seed(123)
  my_ctrl <- trainControl(
    method = "timeslice",
    initialWindow = init_win,
    horizon = horizon,
    fixedWindow = FALSE,
    allowParallel = TRUE,
    verboseIter = TRUE
  )
  
  # Hyperparameter grid from best list
  bst <- best_list2[i, ]
  grid <- expand.grid(
    nrounds = bst$nrounds,
    eta = bst$eta,
    max_depth = bst$max_depth,
    gamma = bst$gamma,
    colsample_bytree = bst$colsample_bytree,
    min_child_weight = bst$min_child_weight,
    subsample = bst$subsample
  )
  
  # Model training
  model <- caret::train(
    x = X_train,
    y = as.factor(y_train),
    method = "xgbTree",
    trControl = my_ctrl,
    tuneGrid = grid,
    verbose = FALSE
  )
  
  # Prediction
  y_prob <- predict(model, X_test, type = "prob")[, 2]
  y_pred <- ifelse(y_prob > 0.5, 1, 0)
  
  # Metrics
  y_test_factor <- factor(y_test, levels = c(0, 1))
  y_pred_factor <- factor(y_pred, levels = c(0, 1))
  
  acc <- mean(y_pred_factor == y_test_factor)
  rec <- recall(y_pred_factor, y_test_factor)
  prec <- precision(y_pred_factor, y_test_factor)
  f1 <- 2 * prec * rec / (prec + rec)
  acc <- mean(y_pred_factor == y_test_factor)
  auc <- MLmetrics::AUC(y_pred = y_prob, y_true = y_test)
  
  # Save results
  all_accuracies2[i]   <- acc
  all_recalls2[i]      <- rec
  all_precisions2[i]   <- prec
  all_f1_scores2[i]    <- f1
  all_aucs2[i]         <- auc
  importance_list3[[i]] <- varImp(model)[["importance"]]
  
  # Calculate SHAP values
  xgb_model <- model$finalModel
  shap_values <- shap.values(xgb_model, X_train)
  shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = X_train)
  all_shap_values2[[i]] <- shap_long  # Store SHAP values for this ID
  all_shap_values2[[i]]$List_Number <- i
  
  message("✓ Completed ID ", ID_file[i], " | Acc: ", round(acc, 3), 
          ", F1: ", round(f1, 3), ", AUC: ", round(auc, 3))
}

# Collate results
metrics_df3 <- data.frame(
  accuracy = all_accuracies2,
  recall = all_recalls2,
  precision = all_precisions2,
  f1_score = all_f1_scores2,
  auc = all_aucs2
)

summary(metrics_df3)

metrics_df3$ID <- ID_file_df$ID_file_df
write.csv(metrics_df3, "Data_Analysis/Data/metrics_personalized_non_self_reported.csv")

importance_list3 <- purrr::imap_dfr(importance_list3, ~ mutate(.x, List_Number = .y))
write.csv(importance_list3, "Data_Analysis/Data/Imporance_personalized_self_non_reported.csv")

all_shap_values_df_non_self_reported <- bind_rows(all_shap_values2)
write.csv(all_shap_values_df_non_self_reported, "Data_Analysis/Data/SHAP_personalized_non_self_reported.csv")
