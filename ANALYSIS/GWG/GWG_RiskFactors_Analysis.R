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
#* AGE_5_CAT, married, marry_age, marry_status, educated, school_yrs, WEALTH_QUINT,
#* height_index, muac, bmi_enroll, bmi_index,
#* chew_tobacco, chew_betelnut, smoke, drink, 
#* nulliparous, GRAVIDITY_CAT, PREG_END_GA, ga_wks_enroll, num_miscarriage, 
#* NOTE: PARITY, is NA; num_fetus = all 1.

#* ******** MORBIDITY VARS **************
#* ANEMIA_T1, ANEMIA_T2, ANEMIA_T3,
#* DIAB_GEST_ANY, DIAB_GEST_DX, HDP_GROUP,
#* HIV_ANY_POINT,
#* MAL_POSITIVE_ENROLL, MAL_POSITIVE_ANY_VISIT, MAL_ANY_POINT,
#* STI_ANY_POINT,
#* SYPH_POSITIVE_ENROLL, SYPH_POSITIVE_ANY_VISIT, SYPH_ANY_POINT,
#* HBV_POSITIVE_ENROLL, HBV_POSITIVE_ANY_VISIT, HBV_ANY_POINT,
#* HCV_POSITIVE_ENROLL, HCV_POSITIVE_ANY_VISIT, HCV_ANY_POINT,
#* HEV_IGM_POSITIVE_ENROLL,HEV_IGG_POSITIVE_ENROLL,
#* TB_SYMP_POSITIVE_ENROLL, TB_SYMP_POSITIVE_ANY_VISIT, TB_SYMP_ANY_POINT,
#* DEPR_ANC20_STND,DEPR_ANC32_STND,
#* FERRITIN70_ANC20, FERRITIN70_ANC32,
#* RBP4_ANC20, RBP4_ANC32,
#* VITB12_COB_ANC20, VITB12_COB_ANC32,
#* folic,
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

###############################################
table(gwg_rf_df$AGE_5_CAT, useNA = "always")
table(gwg_rf_df$married, useNA = "always") # 0, 1 
table(gwg_rf_df$marry_age, useNA = "always") # 13 - 43. Categorize this as: <18, 18-29, 30+

table(gwg_rf_df$educated, useNA = "always") # 0, 1
table(gwg_rf_df$school_yrs, useNA = "always") # 0-97
table(gwg_rf_df$WEALTH_QUINT, useNA = "always") # 0-5, 55
table(gwg_rf_df$height_index, useNA = "always") # 1,2,3,4
table(gwg_rf_df$muac, useNA = "always") # Continuous var
table(gwg_rf_df$bmi_enroll, useNA = "always") # Continuous
table(gwg_rf_df$bmi_index, useNA = "always") # 1-4

table(gwg_rf_df$chew_tobacco, useNA = "always") # 0,1.
table(gwg_rf_df$chew_betelnut, useNA = "always") # 0,1
table(gwg_rf_df$drink, useNA = "always") # 0,1,66

table(gwg_rf_df$nulliparous, useNA = "always") # 0,1
table(gwg_rf_df$GRAVIDITY_CAT, useNA = "always")#  1-4
table(gwg_rf_df$num_miscarriage, useNA = "always") # 0-9


#* ******************************
# FACTORIZE VARIABLES AGAIN: 
#* ******************************

############
# AGE_5_CAT
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(AGE_5_CAT = factor(AGE_5_CAT, 
                   levels = c(1, 2, 3, 4, 5), 
                   labels = c("18-19", "20-24", "25-29", 
                              "30-34", "35+")))
table(gwg_rf_df$AGE_5_CAT, useNA = "always")

############
# EDUCATION
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(educated = factor(educated, 
                            levels = c(1, 0), 
                            labels = c("Yes", "No")))
table(gwg_rf_df$educated, useNA = "always")

##################
# WEALTH QUINTILE
##################
# "1=Lowest quintile (lowest wealth)
# 2=Second quintile
# 3=Third quintile
# 4=Fourth quintile
# 5=Top quintile (highest wealth)
# 55=Missing wealth quintile"

gwg_rf_df <- gwg_rf_df %>%
  mutate(WEALTH_QUINT = if_else(WEALTH_QUINT==55, NA, WEALTH_QUINT))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, WEALTH_QUINT)

gwg_rf_df <- gwg_rf_df %>%
  mutate(WEALTH_QUINT = factor(WEALTH_QUINT, 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c(1, 2, 3, 4, 5)))

table(gwg_rf_df$WEALTH_QUINT, useNA = "always")

#########
# HEIGHT
#########
# "1, <145 
# 2, >= 145 & < 150
# 3, >= 150 & < 155
# 4, >= 155"

gwg_rf_df <- gwg_rf_df %>%
  mutate(height_index = factor(height_index, 
                               levels = c(1, 2, 3, 4), 
                               labels = c('<145', '145-<150', '150-<155', '>=155')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, height_index)

table(gwg_rf_df$height_index, useNA = "always")

################
# BMI_INDEX
################
# "1, < 18.5 
# 2, >= 18.5 & < 25 
# 3, >= 25 & < 30 
# 4, >= 30 "

gwg_rf_df <- gwg_rf_df %>%
  mutate(bmi_index = factor(bmi_index, 
                               levels = c(1, 2, 3, 4), 
                               labels = c('<18.5', '18.8-<25', '25-<30', '>=30')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, bmi_index)

table(gwg_rf_df$bmi_index, useNA = "always")

################
# chew_tobacco
################
gwg_rf_df <- gwg_rf_df %>%
  mutate(chew_tobacco = factor(chew_tobacco, 
                            levels = c(0, 1), 
                            labels = c('No', 'Yes')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, chew_tobacco)

table(gwg_rf_df$chew_tobacco, useNA = "always") # 0,1.

################
# chew_betelnut
################
gwg_rf_df <- gwg_rf_df %>%
  mutate(chew_betelnut = factor(chew_betelnut, 
                               levels = c(0, 1), 
                               labels = c('No', 'Yes')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, chew_betelnut)

table(gwg_rf_df$chew_betelnut, useNA = "always") # 0,1

################
# drink
################
gwg_rf_df <- gwg_rf_df %>%
  mutate(drink = if_else(drink==66, NA, drink))

gwg_rf_df <- gwg_rf_df %>%
  mutate(drink = factor(drink, 
                                levels = c(0, 1), 
                                labels = c('No', 'Yes')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, drink)

table(gwg_rf_df$drink, useNA = "always") # 0,1,66 (66: refused to answer)

###################
# Nulliparous (0,1)
###################
gwg_rf_df <- gwg_rf_df %>%
  mutate(nulliparous = factor(nulliparous, 
                        levels = c(0, 1), 
                        labels = c('No', 'Yes')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, nulliparous)

table(gwg_rf_df$nulliparous, useNA = "always") # 0,1

#########################
# GRAVIDITY_CAT (1,2,3,4)
#########################
gwg_rf_df <- gwg_rf_df %>%
  mutate(GRAVIDITY_CAT = factor(GRAVIDITY_CAT, 
                                levels = c(1, 2, 3, 4), 
                                labels = c("1", "2", "3", 
                                           "4+")))
table(gwg_rf_df$GRAVIDITY_CAT, useNA = "always")

#########################
# No. misscarriages (0-9)
#########################
gwg_rf_df <- gwg_rf_df %>%
  mutate(MISCARRIAGE = if_else(num_miscarriage>=3, '3+', as.character(num_miscarriage)))

temp.df2 <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, num_miscarriage, MISCARRIAGE)

gwg_rf_df <- gwg_rf_df %>%
  mutate(MISCARRIAGE = factor(MISCARRIAGE, 
                              levels = c('0', '1', '2', '3+'), 
                              labels = c('0', '1', '2', '3+')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, num_miscarriage, MISCARRIAGE)

table(gwg_rf_df$num_miscarriage, useNA = "always") # 0-9
table(gwg_rf_df$MISCARRIAGE, useNA = "always")

#########################
# ANEMIA #TODO: Need to pull from file again - these are empty.
#########################
# ANEMIA_T1, ANEMIA_T2, ANEMIA_T3,
table(gwg_rf_df$ANEMIA_T1, useNA = "always")
table(gwg_rf_df$ANEMIA_T2, useNA = "always")

#########################
# DIABETES
#########################
gwg_rf_df <- gwg_rf_df %>%
  mutate(DIAB_GEST_ANY = factor(DIAB_GEST_ANY, 
                              levels = c(0, 1), 
                              labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, DIAB_GEST_ANY)

table(gwg_rf_df$DIAB_GEST_ANY, useNA = "always")

# DIAB_GEST_DX
gwg_rf_df <- gwg_rf_df %>%
  mutate(DIAB_GEST_DX = factor(DIAB_GEST_DX, 
                                levels = c(0, 1), 
                                labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, DIAB_GEST_DX)

table(gwg_rf_df$DIAB_GEST_DX, useNA = "always")

#########################
# HDP GROUP
#########################
# "0=No HDP
# 1=Chronic HTN
# 2=Gestational HTN
# 3=Preeclampsia
# 4=Preeclampsia superimposed on chronic HTN 
# 5=Preeclampsia with severe features
# 55=Missing info
# 77=Pregnancy ended at <20 weeks GA"

gwg_rf_df <- gwg_rf_df %>%
  mutate(HDP_GROUP = factor(HDP_GROUP, 
                               levels = c(0, 1, 2, 3, 4, 5), 
                               labels = c("No HDP", "cHTN", "gHTN", "PE", "PE on cHTN", "PE-SF")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, HDP_GROUP)

table(gwg_rf_df$HDP_GROUP, useNA = "always")

#########
# HIV
#########
gwg_rf_df <- gwg_rf_df %>%
  mutate(HIV_ANY_POINT = factor(HIV_ANY_POINT, 
                            levels = c(0, 1), 
                            labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, HIV_ANY_POINT)

table(gwg_rf_df$HIV_ANY_POINT, useNA = "always")

##########
# MALARIA
##########
# MAL_POSITIVE_ENROLL
gwg_rf_df <- gwg_rf_df %>%
  mutate(MAL_POSITIVE_ENROLL = factor(MAL_POSITIVE_ENROLL,
                                levels = c(0, 1),
                                labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, MAL_POSITIVE_ENROLL)

table(gwg_rf_df$MAL_POSITIVE_ENROLL, useNA = "always")
# 
# # MAL_POSITIVE_ANY_VISIT
# gwg_rf_df <- gwg_rf_df %>%
#   mutate(MAL_POSITIVE_ENROLL = factor(MAL_POSITIVE_ENROLL, 
#                                 levels = c(0, 1), 
#                                 labels = c("No", "Yes")))
# 
# temp.df <- gwg_rf_df %>%
#   select(SITE, MOMID, PREGID, MAL_POSITIVE_ENROLL)
# table(gwg_rf_df$MAL_POSITIVE_ANY_VISIT, useNA = "always")
# 
# # MAL_ANY_POINT
# table(gwg_rf_df$MAL_ANY_POINT, useNA = "always")

#* *******************************************************
#* ARRANGE AND GROUP_BY WITH RISK FACTORS:
#* ******************************************************* 
gwg_rf_df2 <- gwg_rf_df %>%
  arrange(SITE, MOMID, PREGID, TYPE_VISIT) %>%
  group_by(SITE, MOMID, PREGID) %>% # I don't need TYPE_VISIT here b/c it will already be arranged using visit 1.
  summarise(
    IOM_ADEQUACY = first(IOM_ADEQUACY),  # or use another rule to select the IOM_ADEQUACY value
    # Take the first variable
    AGE_5_CAT = first(AGE_5_CAT),
    educated = first(educated), # 0,1
    # school_yrs = first(school_years),
    WEALTH_QUINT = first(WEALTH_QUINT),
    height_index = first(height_index),
    chew_tobacco = first(chew_tobacco),
    chew_betelnut = first(chew_betelnut),
    drink = first(drink),
    GRAVIDITY_CAT = first(GRAVIDITY_CAT),
    MISCARRIAGE = first(MISCARRIAGE),
    DIAB_GEST_ANY = first(DIAB_GEST_ANY),
    DIAB_GEST_DX = first(DIAB_GEST_DX),
    HDP_GROUP = first(HDP_GROUP),
    HIV_ANY_POINT = first(HIV_ANY_POINT),
    MAL_POSITIVE_ENROLL = first(MAL_POSITIVE_ENROLL))

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
riskfactor_vars <- c('AGE_5_CAT', 'educated', 'WEALTH_QUINT', 'height_index', 'chew_tobacco', 
                     'chew_betelnut', 'drink', 'GRAVIDITY_CAT', 'MISCARRIAGE','DIAB_GEST_ANY',
                     'DIAB_GEST_DX', 'HDP_GROUP', 'HIV_ANY_POINT', 'MAL_POSITIVE_ENROLL')

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

# Round the estimates:
RR_CI_df$RR <- round(RR_CI_df$RR, 2)
RR_CI_df$CI_Lower <- round(RR_CI_df$CI_Lower, 2)
RR_CI_df$CI_Upper <- round(RR_CI_df$CI_Upper, 2)
RR_CI_df$Adequate_Perc <- round(RR_CI_df$Adequate_Perc, 1)
RR_CI_df$Inadequate_Perc <- round(RR_CI_df$Inadequate_Perc, 1)
RR_CI_df$Excessive_Perc <- round(RR_CI_df$Excessive_Perc, 1)

# View the full data frame with RR, CI, and category counts for all variables
print(RR_CI_df)

#* **************************************************
#* FOREST PLOT
#* **************************************************
RR_CI_plot_df <- RR_CI_df %>%
  filter(!startsWith(x = Coefficient, prefix = "(Intercept)")) %>%
  mutate(Coefficient = str_replace(Coefficient, "AGE_5_CAT", "Age ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "educated", "Education ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "GRAVIDITY_CAT", "Gravidity ")) %>%
  mutate(Coefficient = str_replace(Coefficient, 'WEALTH_QUINT', "Wealth Quintile ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "height_index", "Height ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "chew_tobacco", "Tobacco ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "chew_betelnut", "Betelnut ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "drink", "Alcohol ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "MISCARRIAGE", "Miscarriage ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "DIAB_GEST_ANY", "GDM any method ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "DIAB_GEST_DX", "GDM diagnosed ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "HDP_GROUP", "HDP ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "HIV_ANY_POINT", "HIV at any point ")) %>%
  mutate(Coefficient = str_replace(Coefficient, "MAL_POSITIVE_ENROLL", "Malaria at enroll ")) 

# Add a new column to group coefficients (for faceting):
RR_CI_plot_df <- RR_CI_plot_df %>%
  mutate(Group = case_when(grepl("Age", Coefficient) ~ "Age",
                           grepl("Education", Coefficient) ~ "Education",
                           grepl("Gravidity", Coefficient) ~ "Gravidity",
                           grepl("Wealth Quintile", Coefficient) ~ "Wealth Quintile",
                           grepl("Height", Coefficient) ~ "Height",
                           grepl("Tobacco", Coefficient) ~ "Tobacco",
                           grepl("Betelnut", Coefficient) ~ "Betelnut",
                           grepl("Alcohol", Coefficient) ~ "Alcohol",
                           grepl("Miscarriage", Coefficient) ~ "Miscarriages",
                           grepl("GDM any method", Coefficient) ~ "GDM any",
                           grepl("GDM diagnosed", Coefficient) ~ "GDM dx",
                           grepl("HDP", Coefficient) ~ "HDP",
                           grepl("HIV at any point", Coefficient) ~ "HIV",
                           grepl("Malaria at enroll", Coefficient) ~ "Malaria",
                           TRUE ~ "Other"))  # Optional: in case a coefficient doesn't match any group


# Convert Coefficient to factor
RR_CI_plot_df$Coefficient <- factor(RR_CI_plot_df$Coefficient, 
                                    levels = rev(unique(RR_CI_plot_df$Coefficient)))

# Create the forest plot with conditional arrows for truncated lines
forest_plot <- RR_CI_plot_df %>%
  ggplot(aes(y = Coefficient, x = RR)) +
  # Plot the points
  geom_point(size = 2) +
  # Draw lines only for non-truncated values
  geom_segment(
    data = RR_CI_plot_df %>%
      filter(CI_Lower >= 0 & CI_Upper <= 6),  # Only keep lines within limits
    aes(x = CI_Lower, xend = CI_Upper, y = Coefficient, yend = Coefficient),
    size = 0.5
  ) +
  # Draw lines with arrows for truncated values
  geom_segment(
    data = RR_CI_plot_df %>%
      filter(CI_Lower < 0 | CI_Upper > 6),  # Only keep out-of-bound lines
    aes(
      x = pmax(CI_Lower, 0),  # Start at 0 if CI_Lower < 0
      xend = pmin(CI_Upper, 6),  # End at 5 if CI_Upper > 5
      y = Coefficient, yend = Coefficient
    ),
    size = 0.5,
    arrow = arrow(type = "closed", length = unit(0.15, "cm"))
  ) +
  # Add a vertical reference line at x = 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  # Add RR and CI values as text
  geom_text(aes(
    label = sprintf("%.2f (%.2f, %.2f)", RR, CI_Lower, CI_Upper)
  ), size = 2.5, nudge_y = 0.4) + 
  # Use faceting to group coefficients
  facet_grid(Group ~ ., scales = "free_y", space = "free", switch = "y") +
  # Set x-axis limits
  scale_x_continuous(limits = c(0, 6)) +
  # Add themes
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1))  # Adjust y-axis text

# Print the plot
forest_plot
today_date <- Sys.Date()
ggsave(filename = paste0('data_out/GWG_RiskFactors_',today_date, '.pdf'), width = 8, height = 20, units = "in")




