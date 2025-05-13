# =============================================================================
# XGBoost Results Analysis Script
# =============================================================================

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(pROC)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(gridExtra)

# =============================================================================
# 2. Set Up Environment and Load Data
# =============================================================================
# Set working directory
setwd("/Users/victoriabrugger/Documents/GitHub/Cursor-Repo/Scientific-Reports-T2D-Glucose-Excursions")

# Load metrics data for both models
metrics_person <- read.csv("Data_Analysis/Data/Metrics_personalized_self_reported.csv")
metrics_person_no_selfreported <- read.csv("Data_Analysis/Data/metrics_personalized_non_self_reported.csv")

# =============================================================================
# 3. Data Cleaning and Preparation
# =============================================================================
# Identify and remove IDs with no F1 score in either model
No_F1_self_reported <- metrics_person %>% filter(is.na(f1_score))
No_F1_non_self_reported <- metrics_person_no_selfreported %>% filter(is.na(f1_score))

Remove_IDs <- No_F1_self_reported %>%
  filter(ID %in% No_F1_non_self_reported$ID)

# Remove identified IDs from both metrics datasets
metrics_person <- metrics_person %>% filter(!ID %in% Remove_IDs$ID)
metrics_person_no_selfreported <- metrics_person_no_selfreported %>% filter(!ID %in% Remove_IDs$ID)

# Save ID lists for backward compatibility
List_ID <- metrics_person$ID
List_Nr <- metrics_person$X
List_Nr <- as.data.frame(List_Nr)
List_Nr$ID <- metrics_person$ID
write.csv(List_ID, "Data_Analysis/Data/List_ID.csv", row.names = FALSE)
write.csv(List_Nr, "Data_Analysis/Data/List_Nr.csv", row.names = FALSE)

# Clean up dataframes
metrics_person$X <- NULL
metrics_person_no_selfreported$X <- NULL

# =============================================================================
# 4. Calculate Summary Statistics
# =============================================================================
# Calculate summary statistics for self-reported features model
metrics_summary1 <- metrics_person %>%
  summarize(
    accuracy_mean = round(mean(accuracy, na.rm = TRUE), 4),
    accuracy_sd = round(sd(accuracy, na.rm = TRUE), 4),
    accuracy_median = round(median(accuracy, na.rm = TRUE), 4),
    
    recall_mean = round(mean(recall, na.rm = TRUE), 4),
    recall_sd = round(sd(recall, na.rm = TRUE), 4),
    recall_median = round(median(recall, na.rm = TRUE), 4),
    
    precision_mean = round(mean(precision, na.rm = TRUE), 4),
    precision_sd = round(sd(precision, na.rm = TRUE), 4),
    precision_median = round(median(precision, na.rm = TRUE), 4),
    
    f1_score_mean = round(mean(f1_score, na.rm = TRUE), 4),
    f1_score_sd = round(sd(f1_score, na.rm = TRUE), 4),
    f1_score_median = round(median(f1_score, na.rm = TRUE), 4),
    
    auc_mean = round(mean(auc, na.rm = TRUE), 4),
    auc_sd = round(sd(auc, na.rm = TRUE), 4),
    auc_median = round(median(auc, na.rm = TRUE), 4)
  )

print("Summary Statistics for Self-Reported Features Model:")
print(metrics_summary1)

# Calculate summary statistics for non-self-reported features model
metrics_summary2 <- metrics_person_no_selfreported %>%
  summarize(
    accuracy_mean = round(mean(accuracy, na.rm = TRUE), 4),
    accuracy_sd = round(sd(accuracy, na.rm = TRUE), 4),
    accuracy_median = round(median(accuracy, na.rm = TRUE), 4),
    
    recall_mean = round(mean(recall, na.rm = TRUE), 4),
    recall_sd = round(sd(recall, na.rm = TRUE), 4),
    recall_median = round(median(recall, na.rm = TRUE), 4),
    
    precision_mean = round(mean(precision, na.rm = TRUE), 4),
    precision_sd = round(sd(precision, na.rm = TRUE), 4),
    precision_median = round(median(precision, na.rm = TRUE), 4),
    
    f1_score_mean = round(mean(f1_score, na.rm = TRUE), 4),
    f1_score_sd = round(sd(f1_score, na.rm = TRUE), 4),
    f1_score_median = round(median(f1_score, na.rm = TRUE), 4),
    
    auc_mean = round(mean(auc, na.rm = TRUE), 4),
    auc_sd = round(sd(auc, na.rm = TRUE), 4),
    auc_median = round(median(auc, na.rm = TRUE), 4)
  )

print("Summary Statistics for Non-Self-Reported Features Model:")
print(metrics_summary2)

# Write summaries to CSV files
write.csv(metrics_summary1, "Data_Analysis/Data/metrics_summary_self_reported.csv", row.names = FALSE)
write.csv(metrics_summary2, "Data_Analysis/Data/metrics_summary_non_self_reported.csv", row.names = FALSE)

# =============================================================================
# 5. Model Comparison Analysis
# =============================================================================
# Calculate performance difference between models
metrics_person$diff <- metrics_person$f1_score - metrics_person_no_selfreported$f1_score

# Add source labels to each model's metrics
metrics_person$Source <- "High-burden"
metrics_person_no_selfreported$Source <- "Low-burden"

# Define helper functions for model comparison
determine_best_model <- function(f1_high, f1_low) {
  if (is.na(f1_high) && is.na(f1_low)) {
    return(NA)
  } else if (is.na(f1_high)) {
    return("Low-burden")
  } else if (is.na(f1_low)) {
    return("High-burden")
  } else if (f1_high > f1_low) {
    return("High-burden")
  } else if (f1_low > f1_high) {
    return("Low-burden")
  } else {
    return("Best performance High-/Low-burden")
  }
}

get_best_metric <- function(metric_high, metric_low, max_source) {
  if (is.na(max_source)) {
    return(NA)
  } else if (max_source == "High-burden") {
    return(metric_high)
  } else if (max_source == "Low-burden") {
    return(metric_low)
  } else {
    return(metric_high)  # If both are equal, return either one
  }
}

# Create comparison dataframe
metrics_person$List_Number <- rownames(metrics_person)
comparison_df <- data.frame(
  ID = metrics_person$ID,
  f1_high = metrics_person$f1_score,
  f1_low = metrics_person_no_selfreported$f1_score,
  auc_high = metrics_person$auc,
  auc_low = metrics_person_no_selfreported$auc,
  precision_high = metrics_person$precision,
  precision_low = metrics_person_no_selfreported$precision,
  recall_high = metrics_person$recall,
  recall_low = metrics_person_no_selfreported$recall,
  accuracy_high = metrics_person$accuracy,
  accuracy_low = metrics_person_no_selfreported$accuracy,
  List_number = metrics_person$List_Number
)

# Determine best performing model and metrics
comparison_df$Max_Source <- mapply(determine_best_model, 
                                 comparison_df$f1_high, 
                                 comparison_df$f1_low)

# Add best metrics based on Max_Source
comparison_df$best_f1_score <- mapply(get_best_metric,
                                    comparison_df$f1_high,
                                    comparison_df$f1_low,
                                    comparison_df$Max_Source)

comparison_df$best_auc <- mapply(get_best_metric,
                               comparison_df$auc_high,
                               comparison_df$auc_low,
                               comparison_df$Max_Source)

comparison_df$best_precision <- mapply(get_best_metric,
                                    comparison_df$precision_high,
                                    comparison_df$precision_low,
                                    comparison_df$Max_Source)

comparison_df$best_recall <- mapply(get_best_metric,
                                 comparison_df$recall_high,
                                 comparison_df$recall_low,
                                 comparison_df$Max_Source)

comparison_df$best_accuracy <- mapply(get_best_metric,
                                   comparison_df$accuracy_high,
                                   comparison_df$accuracy_low,
                                   comparison_df$Max_Source)

# Print summary of best performing models
print("Summary of Best Performing Models:")
print(table(comparison_df$Max_Source, useNA = "ifany"))

# Write comparison results to CSV
write.csv(comparison_df, "Data_Analysis/Data/model_performance_comparison.csv", row.names = FALSE)

# =============================================================================
# 6. Best Metrics Summary
# =============================================================================
# Calculate summary statistics for best metrics
best_metrics_summary <- comparison_df %>%
  summarize(
    accuracy_mean = round(mean(best_accuracy, na.rm = TRUE), 4),
    accuracy_sd = round(sd(best_accuracy, na.rm = TRUE), 4),
    accuracy_median = round(median(best_accuracy, na.rm = TRUE), 4),
    
    recall_mean = round(mean(best_recall, na.rm = TRUE), 4),
    recall_sd = round(sd(best_recall, na.rm = TRUE), 4),
    recall_median = round(median(best_recall, na.rm = TRUE), 4),
    
    precision_mean = round(mean(best_precision, na.rm = TRUE), 4),
    precision_sd = round(sd(best_precision, na.rm = TRUE), 4),
    precision_median = round(median(best_precision, na.rm = TRUE), 4),
    
    f1_score_mean = round(mean(best_f1_score, na.rm = TRUE), 4),
    f1_score_sd = round(sd(best_f1_score, na.rm = TRUE), 4),
    f1_score_median = round(median(best_f1_score, na.rm = TRUE), 4),
    
    auc_mean = round(mean(best_auc, na.rm = TRUE), 4),
    auc_sd = round(sd(best_auc, na.rm = TRUE), 4),
    auc_median = round(median(best_auc, na.rm = TRUE), 4)
  )

print("Summary Statistics for Best Metrics (based on Max_Source):")
print(best_metrics_summary)

# Write best metrics summary to CSV
write.csv(best_metrics_summary, "Data_Analysis/Data/best_metrics_summary.csv", row.names = FALSE)

# =============================================================================
# 7. Visualization Preparation
# =============================================================================
# Create summary table for F1 scores
summary_table <- data.frame(
  Source = c("Best performance High-/Low-burden", "High-burden", "Low-burden"),
  `N` = c(
    nrow(comparison_df),
    sum(!is.na(comparison_df$f1_high)),
    sum(!is.na(comparison_df$f1_low))
  ),
  `M (%)` = c(
    round(mean(comparison_df$best_f1_score, na.rm = TRUE), 4)*100,
    round(mean(comparison_df$f1_high, na.rm = TRUE), 4)*100,
    round(mean(comparison_df$f1_low, na.rm = TRUE), 4)*100
  ),
  `SD (%)` = c(
    round(sd(comparison_df$best_f1_score, na.rm = TRUE), 4)*100,
    round(sd(comparison_df$f1_high, na.rm = TRUE), 4)*100,
    round(sd(comparison_df$f1_low, na.rm = TRUE), 4)*100
  ),
  `Median (%)` = c(
    round(median(comparison_df$best_f1_score, na.rm = TRUE), 4)*100,
    round(median(comparison_df$f1_high, na.rm = TRUE), 4)*100,
    round(median(comparison_df$f1_low, na.rm = TRUE), 4)*100
  ),
  `Range (%)` = c(
    paste0(round(min(comparison_df$best_f1_score, na.rm = TRUE), 4)*100, "–", round(max(comparison_df$best_f1_score, na.rm = TRUE), 4)*100),
    paste0(round(min(comparison_df$f1_high, na.rm = TRUE), 4)*100, "–", round(max(comparison_df$f1_high, na.rm = TRUE), 4)*100),
    paste0(round(min(comparison_df$f1_low, na.rm = TRUE), 4)*100, "–", round(max(comparison_df$f1_low, na.rm = TRUE), 4)*100)
  )
)

# Custom table theme with larger size and proper header formatting
custom_theme <- ttheme_minimal(
  core = list(
    fg_params = list(col = "black", fontsize = 19.2),
    bg_params = list(fill = NA)
  ),
  colhead = list(
    fg_params = list(
      col = "black",
      fontface = "bold",
      hjust = 0.5,
      x = 0.5,
      fontsize = 19.2
    ),
    bg_params = list(fill = NA)
  ),
  rowhead = list(
    fg_params = list(col = "black", fontsize = 19.2),
    bg_params = list(fill = NA)
  ),
  base_size = 19.2,
  padding = unit(c(9.6, 9.6), "mm")
)

# Create the table grob with custom theme and proper column names
table <- tableGrob(
  summary_table,
  rows = NULL,
  theme = custom_theme,
  cols = c("Source", "N", "M (%)", "SD (%)", "Median (%)", "Range (%)")
)

# Create long format dataframe for plotting
plot_data <- data.frame(
  Source = c(
    rep("Best performance High-/Low-burden", nrow(comparison_df)),
    rep("High-burden", sum(!is.na(comparison_df$f1_high))),
    rep("Low-burden", sum(!is.na(comparison_df$f1_low)))
  ),
  F1_Score = c(
    comparison_df$best_f1_score,
    na.omit(comparison_df$f1_high),
    na.omit(comparison_df$f1_low)
  )
)

# Create the density plot with custom colors
p <- ggplot(plot_data, aes(x = F1_Score, fill = Source)) +
  geom_density(alpha = 0.7, color = NA) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "F1 Score", y = "Density") +
  theme(
    axis.title = element_text(size = 16, color = "black", face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    legend.position = "top", 
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1, "cm")
  ) +
  scale_fill_manual(values = c("Best performance High-/Low-burden" = "blue", "High-burden" = "darkviolet", "Low-burden" = "orange")) +
  scale_y_continuous(limits = c(0, 2.8))  # Adjust y-axis limit

# Combine the plot and the table side by side with adjusted heights and spacing
grid_f1 <- grid.arrange(
  p,
  table,
  ncol = 1,
  heights = c(0.35, 0.65),  # Adjusted heights
  padding = unit(0.1, "line")  # Reduced space between plot and table
)
ggsave("Plots/F1_Plot_Table.png", grid_f1, width = 12, height = 12)  # Increased size

#Participants ID <> Predictive Performance plot F1-Score
# Prepare data for the plot
comparison_df$median_f1 <- rowMeans(comparison_df[, c("f1_high", "f1_low")], na.rm = TRUE)
comparison_df$min_f1 <- pmin(comparison_df$f1_high, comparison_df$f1_low, na.rm = TRUE)
comparison_df$max_f1 <- pmax(comparison_df$f1_high, comparison_df$f1_low, na.rm = TRUE)

# Order by median F1 score (low to high)
comparison_df <- comparison_df[order(comparison_df$median_f1), ]
# Create a numeric index for participant order
comparison_df$participant_index <- 1:nrow(comparison_df)

# Create the plot
variability_plot <- ggplot(comparison_df, aes(y = participant_index)) +
  # Add horizontal line for range
  geom_segment(aes(x = min_f1, xend = max_f1, yend = participant_index), 
               color = "black", linewidth = 0.6) +
  # Add points for median value
  geom_point(aes(x = median_f1), color = "black", size = 2.5) +
  # Customize appearance and labels
  labs(x = "Predictive Performance F1-Score", 
       y = "Participant ID") +
  # Set x-axis breaks to only show 0 and 1
  scale_x_continuous(breaks = c(0, 1), limits = c(0, 1)) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  # Remove y-axis labels (participant IDs)
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.title = element_text(size = 16, color = "black", face = "bold"),
    axis.text.x = element_text(size = 14, color = "black"),
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),  # Remove all grid lines
    text = element_text(color = "black"),
    axis.line = element_line(color = "black"),  # Keep axis lines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)  # Add border
  ) +
  # Add a vertical line for overall median
  geom_vline(xintercept = median(comparison_df$median_f1, na.rm = TRUE), 
             linetype = "dashed", color = "black", linewidth = 0.5)

# Save the plot with 30% further reduced height (from 6 to 4.2 inches)
ggsave("Plots/F1_Score_Variability_by_Participant.png", variability_plot, width = 5, height = 7)


# Create 3 plots comparing high-burden vs low-burden models for different groups
# Prepare data for the three groups
rownames(comparison_df)
low_better <- comparison_df[comparison_df$Max_Source == "Low-burden", ]
high_better <- comparison_df[comparison_df$Max_Source == "High-burden", ]
equal_performance <- comparison_df[comparison_df$Max_Source == "Best performance High-/Low-burden", ]

# Create a function to generate the plots with consistent format
create_comparison_plot <- function(data, letter, position = "right", show_p_value = TRUE) {
  # Convert data to long format for plotting
  plot_data <- data.frame(
    ID = rep(data$ID, 2),
    Model = c(rep("Low-burden", nrow(data)), rep("High-burden", nrow(data))),
    F1_Score = c(data$f1_low, data$f1_high)
  )
  
  # Calculate medians for each model
  median_low <- median(data$f1_low, na.rm = TRUE)
  median_high <- median(data$f1_high, na.rm = TRUE)
  
  # Create statistics text
  n_text <- paste("n =", nrow(data))
  
  # Set position for text
  x_pos <- ifelse(position == "right", 2, 1)
  hjust_val <- ifelse(position == "right", 1, 0)
  
  # Perform Wilcoxon signed-rank test if requested
  if(nrow(data) > 1 && show_p_value) {
    wilcox_test <- wilcox.test(data$f1_high, data$f1_low, paired = TRUE)
    p_value <- wilcox_test$p.value
    
    # Format p-value with expression for italic p
    if(p_value < 0.001) {
      p_text <- expression(italic(p) ~ "< 0.001")
    } else {
      p_text <- substitute(italic(p) ~ "=" ~ x, list(x = format.pval(p_value, digits = 3)))
    }
    
    # Create the plot with both n and p annotations
    p <- ggplot(plot_data, aes(x = Model, y = F1_Score, color = Model)) +
      # Add lines connecting the same participant's scores
      geom_line(aes(group = ID), color = "gray", linewidth = 0.6) +
      # Add points for each score
      geom_point(size = 3) +
      # Add a black line connecting the medians
      geom_segment(aes(x = "Low-burden", xend = "High-burden", y = median_low, yend = median_high), 
                 color = "black", linewidth = 1.5, data = data.frame()) +
      # Add n text
      annotate("text", x = x_pos, y = 0.25, label = n_text, size = 4.5, hjust = hjust_val, vjust = 0) +
      # Add p-value with italic p
      annotate("text", x = x_pos, y = 0.22, label = p_text, size = 4.5, hjust = hjust_val, vjust = 0, parse = TRUE) +
      # Customize colors to match density plot
      scale_color_manual(values = c("Low-burden" = "orange", "High-burden" = "darkviolet"))
  } else {
    # Create the plot with only n annotation
    p <- ggplot(plot_data, aes(x = Model, y = F1_Score, color = Model)) +
      # Add lines connecting the same participant's scores
      geom_line(aes(group = ID), color = "gray", linewidth = 0.6) +
      # Add points for each score
      geom_point(size = 3) +
      # Add a black line connecting the medians
      geom_segment(aes(x = "Low-burden", xend = "High-burden", y = median_low, yend = median_high), 
                 color = "black", linewidth = 1.5, data = data.frame()) +
      # Add only n text
      annotate("text", x = x_pos, y = 0.22, label = n_text, size = 4.5, hjust = hjust_val, vjust = 0) +
      # Customize colors to match density plot
      scale_color_manual(values = c("Low-burden" = "orange", "High-burden" = "darkviolet"))
  }
  
  # Add common elements to all plots
  p <- p +
    # Set y-axis to start at 0.2
    scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.2, 1)) +
    # Remove all titles
    labs(x = "Model Type", y = "F1-Score") +
    # Use classic theme as base and customize from there
    theme_classic() +
    theme(
      axis.title = element_text(size = 18, color = "black", face = "bold"),
      axis.text = element_text(size = 16, color = "black"),
      text = element_text(color = "black"),
      # Remove top and right lines
      axis.line.x.top = element_blank(),
      axis.line.y.right = element_blank(), 
      legend.position = "none",  # Remove legend since colors are self-explanatory
      plot.margin = margin(t = 30, r = 5, b = 5, l = 5, unit = "pt")  # Add top margin for letter
    )
  
  return(p)
}
median(high_better$f1_low, na.rm = TRUE)
median(high_better$f1_high, na.rm = TRUE)

wilcox_test <- wilcox.test(low_better$f1_low, low_better$f1_high, paired = TRUE)
z_value <- qnorm(wilcox_test$p.value/2, lower.tail = FALSE)

wilcox_test <- wilcox.test(high_better$f1_high, high_better$f1_low, paired = TRUE)
z_value <- qnorm(wilcox_test$p.value/2, lower.tail = FALSE)



# Generate the three plots with specific settings for each
plot1 <- create_comparison_plot(low_better, "A", "right", TRUE)
plot2 <- create_comparison_plot(high_better, "B", "left", TRUE)
plot3 <- create_comparison_plot(equal_performance, "C", "right", FALSE)

# Use textGrob to create letter labels
label_A <- textGrob("A", x = unit(0.05, "npc"), y = unit(0.7, "npc"),
                   gp = gpar(fontsize = 24, fontface = "bold"), 
                   just = c("left", "top"))
label_B <- textGrob("B", x = unit(0.05, "npc"), y = unit(0.7, "npc"),
                   gp = gpar(fontsize = 24, fontface = "bold"), 
                   just = c("left", "top"))
label_C <- textGrob("C", x = unit(0.05, "npc"), y = unit(0.7, "npc"),
                   gp = gpar(fontsize = 24, fontface = "bold"), 
                   just = c("left", "top"))

# Arrange plots with labels
plot1_with_label <- arrangeGrob(plot1, top = label_A)
plot2_with_label <- arrangeGrob(plot2, top = label_B)
plot3_with_label <- arrangeGrob(plot3, top = label_C)

# Combine the three plots into a single figure in one row
combined_plot <- grid.arrange(plot1_with_label, plot2_with_label, plot3_with_label, ncol = 3)

# Save the combined figure
ggsave("Plots/Model_Performance_Comparison_by_Group.png", combined_plot, width = 18, height = 6)




##Importance plot-----------------------
importance_person <- read.csv("Data_Analysis/Data/Imporance_personalized_self_reported.csv")
importance_person_no_selfreported <- read.csv("Data_Analysis/Data/Imporance_personalized_self_non_reported.csv")

#---------------------------------------------------------
# 1. HIGH-BURDEN MODEL WITH GRANULAR CATEGORIZATION
#---------------------------------------------------------

# Process importance_person data (High-burden model)
importance_person <- importance_person %>%
  mutate(X = sub("\\.\\.\\..*$", "", X))

names(importance_person)[1] <- "Feature"

all_ids <- unique(importance_person$List_Number)
all_features <- unique(importance_person$Feature)

# Create a data frame with all combinations of IDs and features
all_combinations <- expand.grid(List_Number = all_ids, Feature = all_features)

# Merge with the original data and fill missing gains with 0
df_complete <- all_combinations %>%
  left_join(importance_person, by = c("List_Number", "Feature")) %>%
  mutate(Overall_2 = ifelse(is.na(Overall), 0, Overall))

# Custom categorization function for detailed categories
categorize_feature_detailed <- function(feature) {
  # Food categories
  if (grepl('Staples', feature)) {
    return('Staples')
  } else if (grepl('Animal_Foods', feature)) {
    return('Animal Foods')
  } else if (grepl('Dairy_Products', feature)) {
    return('Dairy')
  } else if (grepl('Fruits', feature)) {
    return('Fruits')
  } else if (grepl('Legumes_Nuts', feature)) {
    return('Legumes and Nuts')
  } else if (grepl('Vegetables', feature)) {
    return('Vegetables')
  } else if (grepl('Sweets', feature)) {
    return('Sweets')
  } else if (grepl('total_g|Total_g', feature)) {
    return('Total Intake')
  } 
  # Time and other features
  else if (feature %in% c('Day')) {
    return('Time of the study')
  } else if (feature %in% c('day')) {
    return('Day of the month')
  } else if (feature %in% c('Hour')) {
    return('Hour of the day')
  } else if (feature %in% c('month')) {
    return('Month of the year')
  } else if (feature %in% c('weekday_num')) {
    return('Day of the week')
  } else if (grepl('Meal_time|Meal_bin', feature)) {
    return('Meal time')
  } else if (grepl('PPG_spike_prev', feature)) {
    return('Previous PPG')
  #} else if (grepl('Time', feature, ignore.case=TRUE)) {
  #  return('Time')
  }
  # Medication features
  else if (grepl('insulin|Non.insulin', feature, ignore.case=TRUE)) {
    return('Medication')
  }
  else {
    return('Other')
  }
}

# Apply the mapping to create the detailed group column
df_complete$Group <- sapply(df_complete$Feature, categorize_feature_detailed)

# Calculate total importance and percentage for each feature
df_complete <- df_complete %>% 
  group_by(List_Number) %>% 
  mutate(Overall_total = sum(Overall_2))

df_complete <- df_complete %>%
  group_by(List_Number) %>%
  mutate(Percentage = Overall_2 / Overall_total * 100)

# Aggregate by groups
result <- df_complete %>%
  group_by(List_Number, Group) %>%
  summarise(Percentage_per_Group = sum(Percentage, na.rm = TRUE), .groups = 'drop')

# Print available groups to debug
print("Available groups in high-burden model:")
print(unique(result$Group))

# Define the order of categories for the factor
category_levels <- c(
  # Food categories
  'Staples', 'Animal Foods', 'Legumes and Nuts', 'Dairy', 
  'Vegetables', 'Fruits', 'Sweets', 'Total Intake',
  # Time and other
  'Medication', 'Day of the month', 'Day of the week', 'Time of the study',
  'Hour of the day', 'Month of the year', 'Meal time', 'Previous PPG'
)

# Filter to only include categories that exist in the data
category_levels <- category_levels[category_levels %in% unique(result$Group)]

# Set factor levels to match desired order
result$Group <- factor(result$Group, levels = category_levels)

summary_table <- result %>%
  group_by(Group) %>%
  summarize(
    n = n(),
    zero_count = sum(Percentage_per_Group == 0, na.rm = TRUE),
    zero_percent = round(zero_count / n * 100, 4),
    mean_percentage = mean(Percentage_per_Group, na.rm = TRUE),
    min_percentage = min(Percentage_per_Group, na.rm = TRUE),
    max_percentage = max(Percentage_per_Group, na.rm = TRUE),
    range = paste0(round(min_percentage, 1), "-", round(max_percentage, 4))
  ) %>%
  mutate(
    mean_percentage = round(mean_percentage, 4),
    formatted_mean = paste0(mean_percentage, " (", range, ")"),
    zero_formatted = paste0(zero_count, " (", zero_percent, "%)")
  )

# Display the table
print(summary_table)

# Order participants by nutrition importance
nutrition_categories <- c('Staples', 'Animal Foods', 'Legumes and Nuts', 'Dairy', 
                          'Vegetables', 'Fruits', 'Sweets', 'Total Intake')
nutrition_categories <- nutrition_categories[nutrition_categories %in% unique(result$Group)]

if(length(nutrition_categories) > 0) {
  nutrition_data <- result %>%
    filter(Group %in% nutrition_categories) %>%
  group_by(List_Number) %>%
    summarise(Total_Nutrition = sum(Percentage_per_Group, na.rm = TRUE))
  
  nutrition_order <- nutrition_data %>%
    arrange(desc(Total_Nutrition)) %>%
    pull(List_Number)
  
  result$List_Number <- factor(result$List_Number, levels = nutrition_order)
  } else {
  cat("Warning: No nutrition categories found for ordering\n")
}

# Define colors for categories
food_colors <- c(
  'Staples' = '#821751',
  'Animal Foods' = '#96326A', 
  'Legumes and Nuts' = '#AF4D87',
  'Dairy' = '#C480AA',
  'Vegetables' = '#D6A6C7',
  'Fruits' = '#E6C6DE',
  'Sweets' = '#F4E2ED',
  'Total Intake' = '#F6EFF4'
)

time_colors <- c(
  'Day of the month' = '#EBF5D6',
  'Day of the week' = '#D2E5AC',
  'Time of the study' = '#B6D285',
  'Hour of the day' = '#96B961',
  'Month of the year' = '#789D4C',
  'Meal time' = '#5C7F3A',
  'Previous PPG' = '#42622D'
  #'Time' = '#00441b'
)

other_colors <- c(
  'Medication' = '#F3F6EC'
  #'Other' = '#cccccc'
)

# Combine all colors
all_colors <- c(food_colors, time_colors, other_colors)
colors_to_use <- all_colors[names(all_colors) %in% levels(result$Group)]

# Create the stacked bar plot for high-burden model with detailed categories
Plot_high_burden_detailed <- ggplot(result, aes(x = factor(List_Number), y = Percentage_per_Group / 100, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Participant ID", y = "Importance", fill = "Importance of") +
  theme_minimal() +
  scale_fill_manual(values = colors_to_use) +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format())  +
  labs(title = "Feature Importance by Category", subtitle = "High-burden model")

# Save the detailed high-burden plot
ggsave("Plots/Feature_Importance_Plot_High_Burden_Detailed.png", Plot_high_burden_detailed, width = 15, height = 10)

#---------------------------------------------------------
# 2. HIGH-BURDEN MODEL FOR PARTICIPANTS WHERE IT PERFORMED BETTER
#---------------------------------------------------------

# Get IDs where high-burden model performed better
# Filter the result data for these IDs
result_high_better <- result[result$List_Number %in% high_better$List_number, ]

result_high_better <- result_high_better %>%
  filter(List_Number %in% high_better$List_number) %>%
  group_by(List_Number) %>%
  # Calculate total percentage across all groups for each List_Number
  mutate(Total_Percentage = sum(Percentage_per_Group)) %>%
  # Keep only List_Numbers where Total_Percentage is greater than 0
  filter(Total_Percentage > 0) %>%
  # Remove the temporary Total_Percentage column
  select(-Total_Percentage) %>%
  ungroup() ##One individual removed due to no importance

#Summary Table 
summary_table_high_better <- result_high_better %>%
  group_by(Group) %>%
  summarize(
    n = n(),
    zero_count = sum(Percentage_per_Group == 0, na.rm = TRUE),
    zero_percent = round(zero_count / n * 100, 4),
    mean_percentage = mean(Percentage_per_Group, na.rm = TRUE),
    min_percentage = min(Percentage_per_Group, na.rm = TRUE),
    max_percentage = max(Percentage_per_Group, na.rm = TRUE),
    range = paste0(round(min_percentage, 4), "-", round(max_percentage, 4))
  ) %>%
  mutate(
    mean_percentage = round(mean_percentage, 4),
    formatted_mean = paste0(mean_percentage, " (", range, ")"),
    zero_formatted = paste0(zero_count, " (", zero_percent, "%)")
  )

# Display the table
print(summary_table_high_better)


# Order participants by nutrition importance within this subset
nutrition_data_high_better <- result_high_better %>%
 filter(Group %in% nutrition_categories) %>%
group_by(List_Number) %>%
  summarise(Total_Nutrition = sum(Percentage_per_Group, na.rm = TRUE))

nutrition_order_high_better <- nutrition_data_high_better %>%
  arrange(desc(Total_Nutrition)) %>%
  pull(List_Number)

# Set factor levels based on Staples order
result_high_better$List_Number <- factor(result_high_better$List_Number, levels = nutrition_order_high_better)

# Create the plot for high-burden model when it performed better
Plot_high_burden_better <- ggplot(result_high_better, aes(x = factor(List_Number), y = Percentage_per_Group / 100, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Participant ID", y = " ", fill = "Importance of") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "right",
    legend.box = "vertical",  # Ensures proper alignment of the legend columns
    legend.key.size = unit(0.8, "cm"),  # Adjusts the size of legend keys
    legend.spacing = unit(0.2, "cm"),    # Adjusts spacing between legend items
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = colors_to_use) +
  guides(fill = guide_legend(ncol = 2)) +  # Split legend into 2 columns
  labs(title = " ", 
       subtitle = " ")

       Plot_high_burden_better <- ggplot(result_high_better, aes(x = factor(List_Number), y = Percentage_per_Group / 100, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Participant ID", y = " ", fill = "Importance of") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    #legend.box = "vertical",  # Ensures proper alignment of the legend columns
    legend.key.size = unit(0.8, "cm"),  # Adjusts the size of legend keys
    legend.spacing = unit(0.2, "cm"),    # Adjusts spacing between legend items
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = colors_to_use) +
  guides(fill = guide_legend(ncol = 2)) +  # Split legend into 2 columns
  labs(title = " ", 
       subtitle = " ")



ggsave("Plots/Feature_Importance_Plot_High_Burden_Better_Performance.png", Plot_high_burden_better, width = 15, height = 5, dpi=500)

#---------------------------------------------------------
# 3. LOW-BURDEN MODEL FOR PARTICIPANTS WHERE IT PERFORMED BETTER
#---------------------------------------------------------

# Process low-burden model data
importance_person_no_selfreported <- importance_person_no_selfreported %>%
  mutate(X = sub("\\.\\.\\..*$", "", X))

names(importance_person_no_selfreported)[1] <- "Feature"

all_ids_low <- unique(importance_person_no_selfreported$List_Number)
all_features_low <- unique(importance_person_no_selfreported$Feature)

# Create a data frame with all combinations of IDs and features
all_combinations_low <- expand.grid(List_Number = all_ids_low, Feature = all_features_low)

# Merge with the original data and fill missing gains with 0
# Low-burden model plot (no nutrition or medication data)
# Read the data
importance_person_no_selfreported <- read.csv("Data_Analysis/Data/Imporance_personalized_self_non_reported.csv")

# Process importance_person_no_selfreported data
importance_person_no_selfreported <- importance_person_no_selfreported %>%
  mutate(X = sub("\\.\\.\\..*$", "", X))

names(importance_person_no_selfreported)[1] <- "Feature"

all_ids_low <- unique(importance_person_no_selfreported$List_Number)
all_features_low <- unique(importance_person_no_selfreported$Feature)

# Create a data frame with all combinations of IDs and features
all_combinations_low <- expand.grid(List_Number = all_ids_low, Feature = all_features_low)

# Merge with the original data and fill missing gains with 0
df_complete_low <- all_combinations_low %>%
  left_join(importance_person_no_selfreported, by = c("List_Number", "Feature")) %>%
  mutate(Overall_2 = ifelse(is.na(Overall), 0, Overall))

# Custom categorization function for time features only
categorize_feature_low_burden <- function(feature) {
  if (feature %in% c('Day')) {
    return('Time of the study')
  } else if (feature %in% c('day')) {
    return('Day of the month')
  } else if (feature %in% c('Hour')) {
    return('Hour of the day')
  } else if (feature %in% c('month')) {
    return('Month of the year')
  } else if (feature %in% c('weekday_num')) {
    return('Day of the week')
  } else if (feature %in% c('Meal_time','Meal_bin_8h','Meal_bin_8h_missing','Meal_bin_24h','Meal_bin_24h_missing')) {
    return('Meal time')
  } else if (feature %in% c('PPG_spike_prev', 'PPG_spike_prev_missing')) {
    return('Previous PPG')
  } else if (feature %in% c('Time')) {
    return('Time')
  } else {
    return('Other')
  }
}

# Apply categorization to create the Group column BEFORE aggregating
df_complete_low$Group <- sapply(df_complete_low$Feature, categorize_feature_low_burden)

# Calculate total importance and percentage for each feature
df_complete_low <- df_complete_low %>% 
  group_by(List_Number) %>% 
  mutate(Overall_total = sum(Overall_2))

df_complete_low <- df_complete_low %>%
  group_by(List_Number) %>%
  mutate(Percentage = Overall_2 / Overall_total * 100)

# Now aggregate by List_Number and Group
result_low <- df_complete_low %>%
  group_by(List_Number, Group) %>%
  summarise(Percentage_per_Group = sum(Percentage, na.rm = TRUE), .groups = 'drop')

# Print available groups to debug
print("Available groups in the data:")
print(unique(result_low$Group))

# Define the order of categories for the factor
result_low$Group <- factor(result_low$Group, levels = c(
  'Day of the month', 'Day of the week', 'Time of the study',
  'Hour of the day', 'Month of the year', 'Meal time', 'Previous PPG',
  'Time', 'Other'
))

# Check if any specified category is missing from the data
all_categories <- c('Day of the month', 'Day of the week', 'Time of the study',
                   'Hour of the day', 'Month of the year', 'Meal time', 'Previous PPG',
                   'Time', 'Other')
missing_categories <- all_categories[!all_categories %in% unique(result_low$Group)]
if(length(missing_categories) > 0) {
  print("Warning: The following categories are missing from the data:")
  print(missing_categories)
}

# Use a category that exists in the data for ordering
existing_categories <- levels(result_low$Group)[levels(result_low$Group) %in% unique(result_low$Group)]
if(length(existing_categories) > 0) {
  ordering_category <- existing_categories[1]
  print(paste("Ordering by category:", ordering_category))
  
  # Order participants by the first available category
  time_order <- result_low %>%
    filter(Group == ordering_category) %>%
    arrange(desc(Percentage_per_Group)) %>%
    pull(List_Number)
  
  result_low$List_Number <- factor(result_low$List_Number, levels = time_order)
} else {
  print("Warning: No valid categories found for ordering. Using original order.")
}

# Calculate summary statistics for available groups
for (group in unique(result_low$Group)) {
  group_data <- result_low %>% filter(Group == group)
  if(nrow(group_data) > 0) {
    cat(sprintf("Group: %s\n", group))
    cat(sprintf("  Mean: %.2f%%\n", mean(group_data$Percentage_per_Group, na.rm = TRUE)))
    cat(sprintf("  Median: %.2f%%\n", median(group_data$Percentage_per_Group, na.rm = TRUE)))
    cat(sprintf("  SD: %.2f%%\n\n", sd(group_data$Percentage_per_Group, na.rm = TRUE)))
  }
}

# Define colors for time categories - only for categories that exist in the data
all_time_colors <- c(
  'Day of the month' = '#F8ECE4',
  'Day of the week' = '#ECC9C3',
  'Time of the study' = '#E2A6B5',
  'Hour of the day' = '#D7779F',
  'Month of the year' = '#A73B84',
  #'Meal time' = '#238b45',
  'Previous PPG' = '#661D6F'
  #'Time' = '#00441b',
  #'Other' = '#cccccc'
)
display.brewer.all(colorblindFriendly = TRUE)
# Filter colors to only include those for existing categories
time_colors <- all_time_colors[names(all_time_colors) %in% unique(result_low$Group)]

# Create the stacked bar plot
Plot_low_burden <- ggplot(result_low, aes(x = factor(List_Number), y = Percentage_per_Group / 100, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Participant ID", y = "Importance", fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = time_colors)+
  theme(
    text = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.direction = "horizontal", 
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 0.1)) +
  
  guides(fill = guide_legend(title = "Importance of")) +
  labs(title = "", subtitle = "Low-burden model")

# Save the plot
ggsave("Plots/Feature_Importance_Plot_Low_Burden.png", Plot_low_burden, width = 15, height = 10)

# Optional: Save the data for further analysis
write.csv(result_low, "Data_Analysis/Data/all_feature_importance_low_burden.csv", row.names = FALSE)


#---------------------------------------------------------
# 2. Low-BURDEN MODEL FOR PARTICIPANTS WHERE IT PERFORMED BETTER
#---------------------------------------------------------
# Filter for low-burden better performance, remove zero-sum List_Numbers, and order by Previous PPG
result_low_better <- result_low %>%
  filter(List_Number %in% low_better$List_number) %>%
  group_by(List_Number) %>%
  # Calculate total percentage across all groups for each List_Number
  mutate(Total_Percentage = sum(Percentage_per_Group)) %>%
  # Keep only List_Numbers where Total_Percentage is greater than 0
  filter(Total_Percentage > 0) %>%
  # Remove the temporary Total_Percentage column
  select(-Total_Percentage) %>%
  ungroup() ##One individual removed due to no importance

  #Summary Table 
summary_table_low_better <- result_low_better %>%
  group_by(Group) %>%
  summarize(
    n = n(),
    zero_count = sum(Percentage_per_Group == 0, na.rm = TRUE),
    zero_percent = round(zero_count / n * 100, 4),
    mean_percentage = mean(Percentage_per_Group, na.rm = TRUE),
    min_percentage = min(Percentage_per_Group, na.rm = TRUE),
    max_percentage = max(Percentage_per_Group, na.rm = TRUE),
    range = paste0(round(min_percentage, 4), "-", round(max_percentage, 4))
  ) %>%
  mutate(
    mean_percentage = round(mean_percentage, 4),
    formatted_mean = paste0(mean_percentage, " (", range, ")"),
    zero_formatted = paste0(zero_count, " (", zero_percent, "%)")
  )

# Display the table
print(summary_table_low_better)


# Get Previous PPG importance for each participant and order by increasing value
ppg_order_low_better <- result_low_better %>%
  filter(Group == 'Previous PPG') %>%
  arrange(Percentage_per_Group) %>%  # Changed from desc() to arrange() for increasing order
  pull(List_Number)

# Set factor levels based on Previous PPG order
result_low_better$List_Number <- factor(result_low_better$List_Number, levels = ppg_order_low_better)

# Create the plot for low-burden model when it performed better
Plot_low_burden_better <- ggplot(result_low_better, aes(x = factor(List_Number), y = Percentage_per_Group / 100, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Participant ID", y = " ", fill = "Importance of") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "right",
    legend.box = "vertical",
    legend.key.size = unit(0.8, "cm"),
    legend.spacing = unit(0.2, "cm"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = time_colors) +
  labs(title = " ", 
       subtitle = " ")
      
# Save the high-burden better plot
ggsave("Plots/Feature_Importance_Plot_Low_Burden_Better_Performance.png", Plot_low_burden_better, width = 15, height = 5, dpi=500)

#---------------------------------------------------------
# HIGH-BURDEN MODEL FOR PARTICIPANTS WHERE MODELS PERFORM EQUALLY
#---------------------------------------------------------
# Filter the result data for participants with equal performance
result_high_equal <- result[result$List_Number %in% equal_performance$List_number, ]

result_high_equal <- result_high_equal %>%
  filter(List_Number %in% equal_performance$List_number) %>%
  group_by(List_Number) %>%
  # Calculate total percentage across all groups for each List_Number
  mutate(Total_Percentage = sum(Percentage_per_Group)) %>%
  # Keep only List_Numbers where Total_Percentage is greater than 0
  filter(Total_Percentage > 0) %>%
  # Remove the temporary Total_Percentage column
  select(-Total_Percentage) %>%
  ungroup()

  # Filter low-burden model results for equal performance participants
result_low_equal <- result_low %>%
  filter(List_Number %in% equal_performance$List_number) %>%
  group_by(List_Number) %>%
  # Calculate total percentage across all groups for each List_Number
  mutate(Total_Percentage = sum(Percentage_per_Group)) %>%
  # Keep only List_Numbers where Total_Percentage is greater than 0
  filter(Total_Percentage > 0) %>%
  # Remove the temporary Total_Percentage column
  select(-Total_Percentage) %>%
  ungroup()

    # Check which List_Numbers are in one dataset but not the other
high_list_numbers <- unique(result_high_equal$List_Number)
low_list_numbers <- unique(result_low_equal$List_Number)

  # List_Numbers in low_burden but not in high_burden
in_low_not_high <- setdiff(low_list_numbers, high_list_numbers)

# List_Numbers in high_burden but not in low_burden
in_high_not_low <- setdiff(high_list_numbers, low_list_numbers) #68

# Print the results
cat("List_Numbers in high_burden but not in low_burden:\n")
print(in_high_not_low)
cat("\nList_Numbers in low_burden but not in high_burden:\n")
print(in_low_not_high)
cat("\nNumber of participants in high_burden:", length(high_list_numbers), "\n")
cat("Number of participants in low_burden:", length(low_list_numbers), "\n")

# Filter to include only List_Numbers present in both datasets
common_ids <- intersect(high_list_numbers, low_list_numbers)
result_high_equal_filtered <- result_high_equal %>% filter(List_Number %in% common_ids)
result_low_equal_filtered <- result_low_equal %>% filter(List_Number %in% common_ids)

cat("\nNumber of participants in both datasets:", length(common_ids), "\n")


# Order by nutrition importance within this subset
nutrition_data_high_equal <- result_high_equal_filtered %>%
  filter(Group %in% nutrition_categories) %>%
  group_by(List_Number) %>%
  summarise(Total_Nutrition = sum(Percentage_per_Group, na.rm = TRUE))

nutrition_order_high_equal <- nutrition_data_high_equal %>%
  arrange(desc(Total_Nutrition)) %>%
  pull(List_Number)

# Set factor levels based on nutrition order
result_high_equal_filtered$List_Number <- factor(result_high_equal_filtered$List_Number, levels = nutrition_order_high_equal)

# Create the plot for high-burden model when performance is equal
Plot_high_burden_equal <- ggplot(result_high_equal_filtered, aes(x = factor(List_Number), y = Percentage_per_Group / 100, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Participant ID", y = " ", fill = "Importance of") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, color = "black", face = "bold"),
    axis.text = element_text(size = 16, color = "black"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = colors_to_use) +
  labs(title = " ")

ggsave("Plots/Feature_Importance_High_Burden_Equal_Performance.png", Plot_high_burden_equal, width = 15, height = 5, dpi=500)

#---------------------------------------------------------
# LOW-BURDEN MODEL FOR PARTICIPANTS WHERE MODELS PERFORM EQUALLY
#---------------------------------------------------------
# If Previous PPG exists as a category, order by it
if("Previous PPG" %in% unique(result_low_equal_filtered$Group)) {
  ppg_order_low_equal <- result_low_equal_filtered %>%
    filter(Group == 'Previous PPG') %>%
    arrange(Percentage_per_Group) %>%
    pull(List_Number)
  
  # Set factor levels based on Previous PPG order
  result_low_equal$List_Number <- factor(result_low_equal$List_Number, levels = ppg_order_low_equal)
} else {
  # If Previous PPG doesn't exist, order by first available time category
  time_categories <- intersect(levels(result_low_equal$Group), unique(result_low_equal$Group))
  if(length(time_categories) > 0) {
    ordering_category <- time_categories[1]
    time_order <- result_low_equal %>%
      filter(Group == ordering_category) %>%
      arrange(desc(Percentage_per_Group)) %>%
      pull(List_Number)
    
    result_low_equal$List_Number <- factor(result_low_equal$List_Number, levels = time_order)
  }
}

# Create the plot for low-burden model when performance is equal
Plot_low_burden_equal <- ggplot(result_low_equal_filtered, aes(x = factor(List_Number), y = Percentage_per_Group / 100, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Participant ID", y = " ", fill = "Importance of") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, color = "black", face = "bold"),
    axis.text = element_text(size = 16, color = "black"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = time_colors) +
  labs(title = " ")

ggsave("Plots/Feature_Importance_Low_Burden_Equal_Performance.png", Plot_low_burden_equal, width = 15, height = 5, dpi=500)

# Combine both plots for equal performance participants
equal_plots <- grid.arrange(
  Plot_high_burden_equal,
  Plot_low_burden_equal,
  ncol = 1,
  heights = c(1, 1)
)

ggsave("Plots/Feature_Importance_Both_Models_Equal_Performance.png", equal_plots, width = 15, height = 10, dpi=500)

#