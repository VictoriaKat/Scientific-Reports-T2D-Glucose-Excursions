# =============================================================================
# SHAP Values Analysis for XGBoost Models
# =============================================================================

# =============================================================================
# 1. Load Required Libraries
# =============================================================================
library(dplyr)
library(caret)
library(xgboost)
library(SHAPforxgboost)
library(ggplot2)
library(data.table)
library(pROC)
library(MLmetrics)
library(parallel)
library(doParallel)

# =============================================================================
# 2. Load and Prepare Data
# =============================================================================
# Load SHAP values data for both models
all_shap_values_df_self_reported <- read.csv("Data_Analysis/Data/SHAP_personalized_self_reported.csv")
all_shap_values_df_non_self_reported <- read.csv("Data_Analysis/Data/SHAP_personalized_non_self_reported.csv")

# Load participant list and filter data
List_Nr <- read.csv("Data_Analysis/Data/List_Nr.csv")
all_shap_values_df_self_reported <- all_shap_values_df_self_reported %>% filter(List_Number %in% List_Nr$List_Nr)
all_shap_values_df_non_self_reported <- all_shap_values_df_non_self_reported %>% filter(List_Number %in% List_Nr$List_Nr)

# =============================================================================
# 3. Define SHAP Plotting Functions
# =============================================================================
# Function to create summary plots of SHAP values
shap.plot.summary <- function(data_long, x_bound = NULL, dilute = FALSE, scientific = FALSE, 
                             my_format = NULL, min_color_bound = "lightblue", max_color_bound = "darkblue", 
                             kind = c("sina", "bar"), exclude_vars = c("month", "PPG")) {
  # Match argument for plot type
  kind <- match.arg(kind)
  
  # Exclude specified variables from analysis
  for (pattern in exclude_vars) {
    data_long <- data_long[!grepl(pattern, data_long$variable, ignore.case = TRUE), ]
  }
  
  # Calculate and filter to top 20 most important features
  imp <- shap.importance(data_long)
  top_features <- head(imp[order(-imp$mean_abs_shap), ]$variable, 20)
  data_long <- data_long[data_long$variable %in% top_features, ]
  
  # Remove any NA values
  data_long <- data_long[!is.na(data_long$value), ]
  
  # Create bar plot if specified
  if (kind == "bar") {
    p <- ggplot(imp, aes(x = variable, y = mean_abs_shap)) + 
      geom_bar(stat = "identity", fill = max_color_bound) + 
      coord_flip() + scale_x_discrete(limits = rev(levels(imp[["variable"]]))) + 
      theme_bw() + theme(axis.title.x = element_text(size = 10)) + 
      labs(x = element_blank(), y = "Avg(|SHAP|)")
    return(p)
  }
  
  # Set label format based on scientific notation preference
  if (scientific) {
    label_format = "%.1e"
  } else {
    label_format = "%.3f"
  }
  if (!is.null(my_format)) 
    label_format <- my_format
  
  # Handle data dilution for large datasets
  N_features <- setDT(data_long)[, uniqueN(variable)]
  if (is.null(dilute)) 
    dilute = FALSE
  nrow_X <- nrow(data_long) / N_features
  if (dilute != 0) {
    dilute <- ceiling(min(nrow_X / 10, abs(as.numeric(dilute))))
    set.seed(1234)
    data_long <- data_long[sample(nrow(data_long), min(nrow(data_long) / dilute, 
                                                      nrow(data_long) / 2))]
  }
  
  # Set x-axis bounds
  x_bound <- if (is.null(x_bound)) 
    max(abs(data_long$value)) * 1.1
  else as.numeric(abs(x_bound))
  
  # Create sina plot
  plot1 <- ggplot(data = data_long) + 
    coord_flip(ylim = c(-x_bound, x_bound)) + 
    geom_hline(yintercept = 0) + 
    ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue), 
                      method = "counts", maxwidth = 0.7, alpha = 0.7) + 
    scale_color_gradient(low = min_color_bound, high = max_color_bound, 
                        breaks = c(0, 1), labels = c(" Low", "High "), 
                        guide = guide_colorbar(barwidth = 12, barheight = 0.3)) + 
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
          legend.position = "bottom", legend.title = element_text(size = 10), 
          legend.text = element_text(size = 8), axis.title.x = element_text(size = 10)) + 
    scale_x_discrete(limits = rev(unique(data_long$variable))) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value  ")
  
  return(plot1)
}

# =============================================================================
# 4. Define SHAP Importance Calculation Function
# =============================================================================
# Function to calculate feature importance based on SHAP values
shap.importance <- function(data_long) {
  # Convert to data.table if not already
  if (!data.table::is.data.table(data_long)) {
    data_long <- data.table::as.data.table(data_long)
  }
  
  # Calculate mean absolute SHAP values
  imp <- data_long[, .(mean_abs_shap = mean(abs(value), na.rm = TRUE)), by = variable]
  
  # Ensure variable is a factor for proper plotting
  imp$variable <- factor(imp$variable)
  
  return(imp)
}

# =============================================================================
# 5. Generate and Save SHAP Plots
# =============================================================================
# Create and save SHAP summary plot for self-reported model
plot_self <- shap.plot.summary(all_shap_values_df_self_reported, exclude_vars = c())
ggsave("Plots/plot_self_reported_blue.png", plot_self, width = 7, height = 5)

# Create and save SHAP summary plot for non-self-reported model
plot_non_self <- shap.plot.summary(all_shap_values_df_non_self_reported, 
                                  exclude_vars = c("month", "PPG")) # Remove features with no impact
ggsave("Plots/plot_non_self_reported_blue.png", plot_non_self, width = 7, height = 5)


