# This file will be run after "Creating_GWG_var.R" file. 

# Date: October 11, 2024
# Author: Fouzia Farooq

#****************************************************************************
#TODO: THINGS TO DO AS OF 10/11/2024
# Use epitools package - Qing suggested do this first for descriptive.
#****************************************************************************
library(lubridate)
library(gridExtra)
library(grid)
library(dplyr)
library(base)
library(tidyr)
library(ggplot2)
library(readr)
library(wesanderson)
library(writexl)
library(readxl)
library(mlogit)
library(glmnet)

#******************************************************************************
rm(list = ls())
dir.create("data_out")

UploadDate = "2024-06-28"

#****************************************************************************
#0. # READ FILES
#****************************************************************************
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data/Stacked Data/", UploadDate)

gwg_rf_df <- read.csv(paste0("data_out/df_w_GWGvars-n-Outcomes_uploaded_", UploadDate, ".csv"))

#* ***************************************************************************
#* COUNTING NUMBER OF WOMEN IN THE DATASET: 
#* ***************************************************************************
distinct_count <- gwg_rf_df %>% # n=9102 women
  group_by(SITE, MOMID, PREGID) %>%
  summarise(distinct_pregid = n_distinct(PREGID)) %>%
  pull(distinct_pregid)

#* ***************************************************************************
#* LIST RISK FACTORS:
#* AGE_5_CAT, married, marry_age, marry_status, educated, school_yrs,
#* chew_tobacco, chew_betelnut, smoke, drink, 
#* height_index,  bmi_enroll, bmi_index, muac, M05_MUAC_PERES,
#* ga_wks_enroll, folic, nulliparous, num_fetus, num_miscarriage, primigravida, GRAVIDITY_CAT,
#* DIAB_GEST_ANY, DIAB_GEST_DX, HDP_GROUP,
#* HIV_ANY_POINT,
#* MAL_POSITIVE_ENROLL, MAL_POSITIVE_ANY_VISIT, MAL_ANY_POINT,
#* STI_ANY_POINT,
#* SYPH_POSITIVE_ENROLL, SYPH_POSITIVE_ANY_VISIT, SYPH_ANY_POINT,
#* HBV_POSITIVE_ENROLL, HBV_POSITIVE_ANY_VISIT, HBV_ANY_POINT,
#* HCV_POSITIVE_ENROLL, HCV_POSITIVE_ANY_VISIT, HCV_ANY_POINT,
#* HEV_IGM_POSITIVE_ENROLL,HEV_IGG_POSITIVE_ENROLL,
#* TB_SYMP_POSITIVE_ENROLL, TB_SYMP_POSITIVE_ANY_VISIT, TB_SYMP_ANY_POINT,
#* ANEMIA_T1, ANEMIA_T2, ANEMIA_T3,
#* PREG_END_GA, WEALTH_QUINT ,PARITY, 
#* DEPR_ANC20_STND,DEPR_ANC32_STND,
#* FERRITIN70_ANC20, FERRITIN70_ANC32,
#* RBP4_ANC20, RBP4_ANC32,
#* VITB12_COB_ANC20, VITB12_COB_ANC32,
#* FOL_SERUM_ANC20, FOL_SERUM_ANC32,
#* FOL_RBC_ANC20, FOL_RBC_ANC32,
#* FOL_SERUM_ANC20, FOL_SERUM_ANC32

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, IOM_ADEQUACY) 

table(gwg_rf_df$IOM_ADEQUACY, useNA = "always")

#* ************************************************************
#* FACTORIZE IOM ADEQUACY
#* ************************************************************
# 0 = inadequate
# 1 = adequate
# 2 = excessive

# Convert IOM_ADEQUACY to a factor and set 1 as the reference category
gwg_rf_df$IOM_ADEQUACY <- factor(gwg_rf_df$IOM_ADEQUACY, 
                                  levels = c(1, 0, 2),
                                  labels = c("Adequate", "Inadequate", "Excessive"))

table(gwg_rf_df$IOM_ADEQUACY, useNA = "always")

# Remove rows with missing IOM_ADEQUACY and summarize data to ensure one outcome per MOMID
gwg_rf_df <- gwg_rf_df %>%
  filter(!is.na(IOM_ADEQUACY)) # Dropped n=64 rows (not 64 women).

table(gwg_rf_df$educated, useNA = "always")
table(gwg_rf_df$school_yrs, useNA = "always")
table(gwg_rf_df$chew_tobacco, useNA = "always")
table(gwg_rf_df$chew_betelnut, useNA = "always")
table(gwg_rf_df$drink, useNA = "always")
table(gwg_rf_df$muac, useNA = "always")
table(gwg_rf_df$height_index, useNA = "always")

#* ******************************
# FACTORIZE VARIABLES AGAIN: 
#* ******************************
# AGE_5_CAT
gwg_rf_df <- gwg_rf_df %>%
  mutate(AGE_5_CAT = factor(AGE_5_CAT, 
                   levels = c(1, 2, 3, 4, 5), 
                   labels = c("18-19yo", "20-24yo", "25-29yo", 
                              "30-34yo", "35+yo")))
table(gwg_rf_df$AGE_5_CAT, useNA = "always")

# GRAVIDITY_CAT
gwg_rf_df <- gwg_rf_df %>%
  mutate(GRAVIDITY_CAT = factor(GRAVIDITY_CAT, 
                                levels = c(1, 2, 3, 4), 
                                labels = c("Primigravid", "2nd pregnancy", "3rd pregnancy", 
                                           "4+ pregnancy")))
table(gwg_rf_df$GRAVIDITY_CAT, useNA = "always")

gwg_rf_df2 <- gwg_rf_df %>%
  arrange(SITE, MOMID, PREGID, TYPE_VISIT) %>%
  group_by(SITE, MOMID, PREGID) %>% # I don't need TYPE_VISIT here b/c it will already be arranged using visit 1.
  summarise(
    IOM_ADEQUACY = first(IOM_ADEQUACY),  # or use another rule to select the IOM_ADEQUACY value
    # Take the first variable
    AGE_5_CAT = first(AGE_5_CAT),
    GRAVIDITY_CAT = first(GRAVIDITY_CAT))
   #     WEALTH_QUINT = first(WEALTH_QUINT),
    #  educated = first(educated), # 0, 1
    # school_yrs = first(school_years),
    # chew_tobacco = first(chew_tobacco),
    # chew_betelnut = first(chew_betelnut),
    # drink = first(drink),
    # hieight_index = first(height_index))

# n = 5251 after removing missing IOM_ADEQUACY

str(gwg_rf_df2$AGE_5_CAT)

#* *********************************************
#* WIDE DATA FOR MULTINOMIAL REGRESSION
#* *********************************************
# Prepare the data for mlogit (no longer long format since we've aggregated by MOMID)
data_wide <- mlogit.data(gwg_rf_df2, choice = "IOM_ADEQUACY", shape = "wide", id.var = "MOMID")

# Relevel IOM_ADEQUACY to ensure "Adequate" is the reference category
gwg_rf_df2$IOM_ADEQUACY <- relevel(gwg_rf_df2$IOM_ADEQUACY, ref = "Adequate")

#* **************************************************
#* RISK FACTORS ANALYSIS:
#* **************************************************
#* 
# List of new variables to convert to factors and include in the model
riskfactor_vars <- c("AGE_5_CAT", "GRAVIDITY_CAT") # , "PARITY", # WEALTH_QUINT
              # "educated", "chew_tobacco", 
              # "chew_betelnut", "smoke", "drink","height_index", 
              # "DIAB_GEST_ANY", "HIV_ANY_POINT", "MAL_ANY_POINT", "SYPH_ANY_POINT",
              # "HBV_ANY_POINT", "HCV_ANY_POINT", "TB_SYMP_ANY_POINT",
              # )

lapply(gwg_rf_df2[, riskfactor_vars], levels) # Checks how many levels the variables have. 


# Initialize empty data frame to store RR, CI, and counts/percentages. 
RR_CI_df <- data.frame(Variable = character(), Coefficient = character(), 
                       RR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(), 
                       Adequate_N = numeric(), Inadequate_N = numeric(), Excessive_N = numeric(), 
                       Adequate_Perc = numeric(), Inadequate_Perc = numeric(), Excessive_Perc = numeric(),
                       stringsAsFactors = FALSE)

# Loop through the risk factors and calculate RR, CI, and counts for each IOM_ADEQUACY category
for (riskfactor in riskfactor_vars) {
  
  # Filter out rows with NA values in the current risk factor
  temp_df <- gwg_rf_df2 %>%
    filter(!is.na(.data[[riskfactor]]))
  
  # Total number of observations for the current risk factor (non-NA rows)
  total_obs <- nrow(temp_df)
  
  # Recalculate the total number of women in each IOM_ADEQUACY category for the current risk factor
  iom_adequacy_counts <- table(temp_df$IOM_ADEQUACY)
  
  # Extract counts for each category (default to 0 if the category doesn't exist in the filtered data)
  adequate_count <- ifelse("Adequate" %in% names(iom_adequacy_counts), iom_adequacy_counts["Adequate"], 0)
  inadequate_count <- ifelse("Inadequate" %in% names(iom_adequacy_counts), iom_adequacy_counts["Inadequate"], 0)
  excessive_count <- ifelse("Excessive" %in% names(iom_adequacy_counts), iom_adequacy_counts["Excessive"], 0)
  
  # Calculate percentages for each category
  adequate_perc <- (adequate_count / total_obs) * 100
  inadequate_perc <- (inadequate_count / total_obs) * 100
  excessive_perc <- (excessive_count / total_obs) * 100
  
  # Convert the variable to a factor (if not already)
  temp_df[[riskfactor]] <- factor(temp_df[[riskfactor]])
  
  # Update the formula to include the current variable
  formula <- as.formula(paste("IOM_ADEQUACY ~ 1 |", riskfactor))
  
  # Run the multinomial logistic regression model
  mlogit_model <- mlogit(formula, data = mlogit.data(temp_df, choice = "IOM_ADEQUACY", shape = "wide", id.var = "MOMID"))
  
  # Extract coefficients
  coefficients <- coef(mlogit_model)
  
  # Calculate Relative Risks (RR)
  RR <- exp(coefficients)
  
  # Calculate Confidence Intervals (CI)
  CI <- exp(confint(mlogit_model))
  
  # Create a data frame for the current variable with its RR, CI, and category counts and percentages
  var_results <- data.frame(
    Variable = riskfactor,  # The current risk factor being processed
    Coefficient = names(RR),
    RR = RR,
    CI_Lower = CI[, 1],
    CI_Upper = CI[, 2],
    Adequate_N = adequate_count,    # Number of women in the "Adequate" category
    Inadequate_N = inadequate_count,  # Number of women in the "Inadequate" category
    Excessive_N = excessive_count,   # Number of women in the "Excessive" category
    Adequate_Perc = adequate_perc,    # Percentage of "Adequate" category
    Inadequate_Perc = inadequate_perc,  # Percentage of "Inadequate" category
    Excessive_Perc = excessive_perc   # Percentage of "Excessive" category
  )
  
  # Append the results to the main data frame
  RR_CI_df <- rbind(RR_CI_df, var_results)
}

# View the full data frame with RR, CI, and category counts for all variables
print(RR_CI_df)




























#* *******************************************************************
#* THIS WORKS - IGNORE BELOW!!!!!
#* *******************************************************************
# Create an empty data frame to store the RR and CIs for each variable
RR_CI_df <- data.frame(Variable = character(), Coefficient = character(), 
                       RR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(), 
                       stringsAsFactors = FALSE)


# Convert the listed variables to factors and run the model in the loop
for (riskfactor in riskfactor_vars) {
  # Ensure the variable is a factor
  gwg_rf_df2[[riskfactor]] <- factor(gwg_rf_df2[[riskfactor]])
  
  # Update the formula to include the current variable
  formula <- as.formula(paste("IOM_ADEQUACY ~ 1 |", riskfactor))
  
  # Run the multinomial logistic regression model
  mlogit_model <- mlogit(formula, data = data_wide) # , reflevel = 1)
  
  # Extract coefficients
  coefficients <- coef(mlogit_model)
  
  # Calculate Relative Risks (RR)
  RR <- exp(coefficients)
  
  # Calculate Confidence Intervals (CI)
  CI <- exp(confint(mlogit_model))
  
  # Create a data frame for the current variable with its RR and CI
  var_results <- data.frame(
    Variable = riskfactor,
    Coefficient = names(RR),
    RR = RR,
    CI_Lower = CI[, 1],
    CI_Upper = CI[, 2]
  )
  
  # Append the results to the main data frame
  RR_CI_df <- rbind(RR_CI_df, var_results)
}

# View the full data frame with RR and CI for all variables
print(RR_CI_df)

#* *******************************************************************
#* END
#* 
#* ************************************************************
#* *************************************************************

gwg_rf_df2$GRAVIDITY_CAT <- factor(gwg_rf_df2$GRAVIDITY_CAT, levels = c('1','2','3','4+'))

table(gwg_rf_df2$GRAVIDITY_CAT, useNA = "always")
table(gwg_rf_df2$GRAVIDITY_CAT, gwg_rf_df2$IOM_ADEQUACY)

# Prepare the data for mlogit (no longer long format since we've aggregated by MOMID)
data_wide <- mlogit.data(gwg_rf_df2, choice = "IOM_ADEQUACY", shape = "wide", id.var = "MOMID")

# Run the model with GRAVIDITY as a predictor
mlogit_model <- mlogit(IOM_ADEQUACY ~ 1 | GRAVIDITY_CAT, data = data_wide, reflevel = "1")
mlogit_model

# Get the model summary
model_summary <- summary(mlogit_model)
model_summary
# Check if coefficients have standard errors in names
coefficients <- coef(model_summary, subset = "sig")
coef_df <- as.data.frame(coefficients) %>% 
  mutate(RR = exp(Estimate))
coef_df 
exp(confint(mlogit_model))



 