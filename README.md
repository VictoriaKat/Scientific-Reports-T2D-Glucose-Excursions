# Scientific-Reports-T2D-Glucose-Excusrions

## Overview
This repository contains the analysis code and data for the article "Predicting vulnerability to postprandial glucose excursions to personalize dietary interventions for Type-2 diabetes management", focusing on personalized prediction models for postprandial glucose responses. The study compares high-burden (self-reported) with dietary and low-burden (non-self-reported) models to predict glucose responses.

## Repository Structure

### Data_Analysis/
- **Analysis/**
  - `01_Data_Preparation.R`: Initial data preparation and cleaning
  - `02_XGBoost_Results.R`: Analysis of XGBoost model results and performance metrics
  - `03_Tableone_Personalized.R`: Creation of TableOne for participant characteristics
- **Clean_&_Wrangle/**
  - `Cleaning_Wrangling/`: Scripts for data cleaning and preprocessing
  - `Data_Preparation/`: Scripts for feature engineering and data preparation
- **Data/**
  - Contains all processed and raw data files
  - Includes model performance metrics and feature importance data
- **Plots/**
  - Generated visualizations and plots
  - Includes model performance comparisons and feature importance plots

### Key Features
- Personalized prediction models for postprandial glucose responses
- Comparison of high-burden vs. low-burden models
- Analysis of meal timing patterns and their impact on predictions
- Feature importance analysis for different model types
- Comprehensive participant characteristics analysis

### Data Analysis Components
1. **Model Performance Analysis**
   - F1 scores, accuracy, precision, and recall metrics
   - Comparison between high-burden and low-burden models
   - Performance analysis by participant subgroups

2. **Feature Importance Analysis**
   - Detailed analysis of feature importance for each model type
   - Comparison of feature importance across different participant groups
   - Visualization of feature importance patterns

3. **Meal Timing Analysis**
   - Standard deviation of meal times by meal type
   - Analysis of meal timing patterns across different model performance groups
   - Temporal patterns in glucose responses

4. **Participant Characteristics**
   - Comprehensive TableOne analysis
   - Demographic and clinical characteristics
   - Dietary patterns and food intake analysis

## Requirements
- R version 4.0.0 or higher
- Required R packages:
  - dplyr
  - ggplot2
  - tableone
  - xgboost
  - pROC
  - tidyr
  - gridExtra
  - scales
  - RColorBrewer
  - knitr
  - kableExtra

## Usage
1. Clone the repository
2. Install required R packages
3. Set working directory to the repository root
4. Run analysis scripts in order:
   - Start with data preparation scripts
   - Proceed with model analysis
   - Generate visualizations and tables


## Citation
If you use this code or data in your research, please cite:
TBA

## Contact
TBA

## Acknowledgments
TBA
