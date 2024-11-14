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
library(cowplot)
library(stringr)
library(meta)
library(ggpubr)
library(cowplot) # for use in ggplot arrangement of tiles.
library(patchwork)

#******************************************************************************
rm(list = ls())
dir.create("data_out")

UploadDate = "2024-06-28"
dir.create("plots_forest")
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
#LIST OUT ALL THE VARIABLES TO CHECK THE CATEGORY

#TABLE 1 DEMOGRAPHIC: 
# AGE_5_CAT, MARRY_AGE_3_CAT, married, educated, school_yrs, OCCUPATION, WEALTH_QUINT, smoke, chew_tobacco, chew_betelnut, drink, BMI4CAT, muac,
# height_index, PARITY, GRAVIDITY_CAT, num_miscarriage

table(gwg_rf_df$AGE_5_CAT, useNA = "always")# 1,2,3,4,5
table(gwg_rf_df$MARRY_AGE_3_CAT, useNA = "always") # 0,1,2 #FF ask Lili: Are these: Categorize this as: <18, 18-29, 30+
table(gwg_rf_df$married, useNA = "always") # 0, 1 
table(gwg_rf_df$educated, useNA = "always") # 0, 1
table(gwg_rf_df$school_yrs, useNA = "always") # 0-97 - not sure how to categorize it
table(gwg_rf_df$OCCUPATION,useNA = "always")# Farming, Housewife, No work, Other, Skilled labor Unskilled labor 
table(gwg_rf_df$WEALTH_QUINT, useNA = "always") # 0-5, 55
table(gwg_rf_df$smoke,useNA = "always")# 0, 1
table(gwg_rf_df$chew_tobacco, useNA = "always") # 0,1
table(gwg_rf_df$chew_betelnut, useNA = "always") # 0,1
table(gwg_rf_df$drink, useNA = "always") # 0,1,66
table(gwg_rf_df$BMI4CAT,useNA = "always")# 0, 1, 2, 3
table(gwg_rf_df$muac, useNA = "always") # Continuous var
table(gwg_rf_df$height_index, useNA = "always") # 1,2,3,4
table(gwg_rf_df$PARITY, useNA = "always")# 0, 1, 2
table(gwg_rf_df$GRAVIDITY_CAT, useNA = "always")# 1-4
## The following variables are both BMI but not sure why we look at them
# table(gwg_rf_df$bmi_enroll, useNA = "always") # Continuous
# table(gwg_rf_df$bmi_index, useNA = "always") # 1-4
# table(gwg_rf_df$nulliparous, useNA = "always") # 0,1 # don't need this since part of parity.
table(gwg_rf_df$num_miscarriage, useNA = "always") # 0-9

#TABLE 2 MATERNAL CLINICAL RISK FACTORS

# HIV_ANY_POINT, MAL_POSITIVE_ENROLL, STI_ANY_POINT, SYPH_ANY_POINT, HBV_ANY_POINT, HCV_ANY_POINT, HEV_IGM_POSITIVE_ENROLL, HEV_IGG_POSITIVE_ENROLL,
# TB_SYMP_ANY_POINT, DIAB_GEST_DX, HDP_GROUP, ANEMIA_T1, ANEMIA_T2, ANEMIA_T3, DEPR_ANY_POINT
table(gwg_rf_df$HIV_ANY_POINT, useNA = "always")# 0, 1
table(gwg_rf_df$MAL_POSITIVE_ENROLL, useNA = "always")# 0, 1
table(gwg_rf_df$STI_ANY_POINT, useNA = "always")# 0, 1
table(gwg_rf_df$SYPH_ANY_POINT, useNA = "always")# 0, 1
table(gwg_rf_df$HBV_ANY_POINT, useNA = "always")# 0, 1
table(gwg_rf_df$HCV_ANY_POINT, useNA = "always")# 0, 1
table(gwg_rf_df$HEV_IGM_POSITIVE_ENROLL, useNA = "always")# 0, 1
table(gwg_rf_df$HEV_IGG_POSITIVE_ENROLL, useNA = "always")# 0, 1
table(gwg_rf_df$TB_SYMP_ANY_POINT, useNA = "always")# 0, 1
table(gwg_rf_df$DIAB_GEST_ANY, useNA = "always")# 0, 1 # Erin said during biweekly GW Thurs meeting not to look at this one.
table(gwg_rf_df$DIAB_GEST_DX, useNA = "always")# 0, 1
table(gwg_rf_df$HDP_GROUP, useNA = "always")#0-5
table(gwg_rf_df$ANEMIA_T1, useNA = "always")#0,1,2,3
table(gwg_rf_df$ANEMIA_T2, useNA = "always")#0,1,2,3
table(gwg_rf_df$ANEMIA_T3, useNA = "always")#0,1,2,3
table(gwg_rf_df$DEPR_ANY_POINT, useNA = "always")# 0, 1

# NUTRITION RISK FACTORS: 
# FERRITIN70_ANY_POINT, RBP4_ANY_POINT, VITB12_COB_ANY_POINT, FOL_RBC_ANY_POINT, FOL_SERUM_ANY_POINT, M04_MICRONUTRIENT_CMOCCUR
table(gwg_rf_df$FERRITIN70_ANY_POINT, useNA = "always")# 1,2 
table(gwg_rf_df$RBP4_ANY_POINT, useNA = "always")# 1-4
table(gwg_rf_df$VITB12_COB_ANY_POINT, useNA = "always")# 1-3
table(gwg_rf_df$FOL_RBC_ANY_POINT, useNA = "always")# all NAs for 06/28 data
table(gwg_rf_df$FOL_SERUM_ANY_POINT, useNA = "always")# 1-4 
table(gwg_rf_df$M04_MICRONUTRIENT_CMOCCUR, useNA = "always")#0,1

#* ******************************
# FACTORIZE VARIABLES AGAIN: 
#* ******************************

############
# AGE_5_CAT
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(AGE_5_CAT = factor(AGE_5_CAT, 
                   levels = c(2, 1, 3, 4, 5), 
                   labels = c("20-24", "<20", "25-29", 
                              "30-34", "35+")))
table(gwg_rf_df$AGE_5_CAT, useNA = "always")


############
# marry age 
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(MARRY_AGE_3_CAT = factor(MARRY_AGE_3_CAT, 
                                  levels = c(1, 0, 2), 
                                  labels = c("18-29", "<18", "30+")))
temp.df <-gwg_rf_df%>%
  select(SITE, MOMID, PREGID,MARRY_AGE_3_CAT)

table(gwg_rf_df$MARRY_AGE_3_CAT, useNA = "always")

############
# married
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(married = factor(married, 
                          levels = c(0,1), 
                          labels = c("Yes", "No"))) # Yes: Married/cohabitating", No: "Divorced/separated/widow/single"
table(gwg_rf_df$married, useNA = "always")


############
# EDUCATION
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(educated = factor(educated, 
                            levels = c(1, 0), 
                            labels = c("Yes", "No")))
table(gwg_rf_df$educated, useNA = "always")

############
# OCCUPATION - # No work is the ref. category.
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(OCCUPATION = factor(OCCUPATION, 
                             levels = c("No work", "Skilled labor", "Unskilled labor","Farming", "Housewife", "Other"), 
                             labels = c("No work", "Skilled labor", "Unskilled labor","Farming", "Housewife", "Other")))
table(gwg_rf_df$OCCUPATION, useNA = "always")
str(gwg_rf_df$OCCUPATION)

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
                                levels = c(5, 1, 2, 3, 4), # Highest quintile is the ref.
                                labels = c(5, 1, 2, 3, 4)))

table(gwg_rf_df$WEALTH_QUINT, useNA = "always")

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

######################################################################
# BMI4CAT - @Lili - we can use this and remove BMI_index (BMI_index 
# comes from Xiaoyan).
######################################################################
# "1, < 18.5 
# 2, >= 18.5 & < 25 
# 3, >= 25 & < 30 
# 4, >= 30 "

gwg_rf_df <- gwg_rf_df %>%
  mutate(BMI4CAT = factor(BMI4CAT, 
                          levels = c(2, 1, 3, 4), 
                          labels = c('18.5-<25', '<18.5', '25-<30', '>=30')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, BMI4CAT)

table(gwg_rf_df$BMI4CAT, useNA = "always") # Don't have anyone above 30 BMI atm.

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

#########
# PARITY
#########
gwg_rf_df <- gwg_rf_df %>%
  mutate(PARITY = factor(PARITY, 
                         levels = c(0,1,2), 
                         labels = c('0', '1', '>=2')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, PARITY)

table(gwg_rf_df$PARITY, useNA = "always")

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
# GRAVIDITY_CAT (1,2,3)
#########################
gwg_rf_df <- gwg_rf_df %>%
  mutate(GRAVIDITY_CAT = factor(GRAVIDITY_CAT, 
                                levels = c(1, 2, 3), 
                                labels = c("1", "2", "3+")))
table(gwg_rf_df$GRAVIDITY_CAT, useNA = "always")

#########################
# No. misscarriages (0-9)
#########################
# gwg_rf_df <- gwg_rf_df %>%
#   mutate(MISCARRIAGE = if_else(num_miscarriage>=3, '3+', as.character(num_miscarriage)))

gwg_rf_df <- gwg_rf_df %>%
  mutate(MISCARRIAGE = if_else(num_miscarriage>=1, 1, 0))

table(gwg_rf_df$MISCARRIAGE, useNA = "always")

temp.df2 <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, num_miscarriage, MISCARRIAGE)

# gwg_rf_df <- gwg_rf_df %>%
#   mutate(MISCARRIAGE = factor(MISCARRIAGE, 
#                               levels = c('0', '1', '2', '3+'), 
#                               labels = c('0', '1', '2', '3+')))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, num_miscarriage, MISCARRIAGE)

table(gwg_rf_df$num_miscarriage, useNA = "always") # 0-9
table(gwg_rf_df$MISCARRIAGE, useNA = "always")

# CONVERT TO FACTOR VARIABLE WITH LABELS: 
gwg_rf_df <- gwg_rf_df %>%
  mutate(MISCARRIAGE = factor(MISCARRIAGE, 
                             levels = c(0, 1), 
                             labels = c("No", "Yes")))
str(gwg_rf_df$MISCARRIAGE)   

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

######
# STI
######
gwg_rf_df <- gwg_rf_df %>%
  mutate(STI_ANY_POINT = factor(STI_ANY_POINT,
                                levels = c(0, 1),
                                labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, STI_ANY_POINT)

table(gwg_rf_df$STI_ANY_POINT, useNA = "always")

##########
# Syphilis
##########
gwg_rf_df <- gwg_rf_df %>%
  mutate(SYPH_ANY_POINT = factor(SYPH_ANY_POINT,
                                 levels = c(0, 1),
                                 labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, SYPH_ANY_POINT)

table(gwg_rf_df$SYPH_ANY_POINT, useNA = "always")

##########
# HBV
##########
gwg_rf_df <- gwg_rf_df %>%
  mutate(HBV_ANY_POINT = factor(HBV_ANY_POINT,
                                levels = c(0, 1),
                                labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, HBV_ANY_POINT)

table(gwg_rf_df$HBV_ANY_POINT, useNA = "always")

##########
# HCV
##########
gwg_rf_df <- gwg_rf_df %>%
  mutate(HCV_ANY_POINT = factor(HCV_ANY_POINT,
                                levels = c(0, 1),
                                labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, HCV_ANY_POINT)

table(gwg_rf_df$HCV_ANY_POINT, useNA = "always")

############
# HEV - IGM
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(HEV_IGM_POSITIVE_ENROLL = factor(HEV_IGM_POSITIVE_ENROLL,
                                          levels = c(0, 1),
                                          labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, HEV_IGM_POSITIVE_ENROLL)

table(gwg_rf_df$HEV_IGM_POSITIVE_ENROLL, useNA = "always")


############
# HEV - IGG
############
gwg_rf_df <- gwg_rf_df %>%
  mutate(HEV_IGG_POSITIVE_ENROLL = factor(HEV_IGG_POSITIVE_ENROLL,
                                          levels = c(0, 1),
                                          labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, HEV_IGG_POSITIVE_ENROLL)

table(gwg_rf_df$HEV_IGG_POSITIVE_ENROLL, useNA = "always")

#####
# TB
#####
gwg_rf_df <- gwg_rf_df %>%
  mutate(TB_SYMP_ANY_POINT = factor(TB_SYMP_ANY_POINT,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, TB_SYMP_ANY_POINT)

table(gwg_rf_df$TB_SYMP_ANY_POINT, useNA = "always")

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

#############
# DEPRESSION
#############
gwg_rf_df <- gwg_rf_df %>%
  mutate(DEPR_ANY_POINT = factor(DEPR_ANY_POINT,
                                 levels = c(0, 1),
                                 labels = c("No", "Yes")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, DEPR_ANY_POINT)

table(gwg_rf_df$DEPR_ANY_POINT, useNA = "always")

###########
# FERRITIN
###########
gwg_rf_df <- gwg_rf_df %>%
  mutate(FERRITIN70_ANY_POINT= factor(FERRITIN70_ANY_POINT,
                                      levels = c(2, 1),
                                      labels = c("Above threshold", "Below threshold")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, FERRITIN70_ANY_POINT)

table(gwg_rf_df$FERRITIN70_ANY_POINT, useNA = "always")

###########
# RBP4
###########
gwg_rf_df <- gwg_rf_df %>%
  mutate(RBP4_ANY_POINT= factor(RBP4_ANY_POINT,
                                levels = c(4, 1, 2, 3),
                                labels = c("No deficiency", "Severe deficiency", "Moderate deficiency","Mild deficiency")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, RBP4_ANY_POINT)

table(gwg_rf_df$RBP4_ANY_POINT, useNA = "always")


###########
# Serum B12
###########
gwg_rf_df <- gwg_rf_df %>%
  mutate(VITB12_COB_ANY_POINT= factor(VITB12_COB_ANY_POINT,
                                      levels = c(3, 1, 2),
                                      labels = c("Sufficient", "Deficient", "Insufficient")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, VITB12_COB_ANY_POINT)

table(gwg_rf_df$VITB12_COB_ANY_POINT, useNA = "always")

##########################
# RBC folate deficiency
##########################
gwg_rf_df <- gwg_rf_df %>%
  mutate(FOL_RBC_ANC20= factor(FOL_RBC_ANC20,
                               levels = c(2, 1),
                               labels = c("Normal", "Low")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, FOL_RBC_ANC20)

table(gwg_rf_df$FOL_RBC_ANC20, useNA = "always")

##########################
# Serum folate deficiency
##########################
gwg_rf_df <- gwg_rf_df %>%
  mutate(FOL_SERUM_ANY_POINT= factor(FOL_SERUM_ANY_POINT,
                                     levels = c(2, 1),
                                     labels = c("Normal", "Low")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, FOL_SERUM_ANY_POINT)

table(gwg_rf_df$FOL_SERUM_ANY_POINT, useNA = "always")

####################
# ANEMIA T1, T2, T3
####################
# ANEMIA_T1
####################
gwg_rf_df <- gwg_rf_df %>%
  mutate(ANEMIA_T1 = factor(ANEMIA_T1, 
                            levels = c(0,1,2,3), 
                            labels = c("No anemia", "Mild anemia","Moderate anemia","Severe anemia")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, ANEMIA_T1)

table(gwg_rf_df$ANEMIA_T1, useNA = "always")

####################
# ANEMIA Yes/No
####################
# gwg_rf_df <- gwg_rf_df %>%
#   mutate(ANEMIA_T1_ANY = if_else(ANEMIA_T1 %in% c('1','2','3'), 1, 0))
# 
# table(gwg_rf_df$ANEMIA_T1_ANY, useNA = "always")

# ANEMIA_T2
gwg_rf_df <- gwg_rf_df %>%
  mutate(ANEMIA_T2 = factor(ANEMIA_T2, 
                            levels = c(0,1,2,3), 
                            labels = c("No anemia", "Mild anemia","Moderate anemia","Severe anemia")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, ANEMIA_T2)

table(gwg_rf_df$ANEMIA_T2, useNA = "always")

# ANEMIA_T3
gwg_rf_df <- gwg_rf_df %>%
  mutate(ANEMIA_T3 = factor(ANEMIA_T3, 
                            levels = c(0,1,2,3), 
                            labels = c("No anemia", "Mild anemia","Moderate anemia","Severe anemia")))

temp.df <- gwg_rf_df %>%
  select(SITE, MOMID, PREGID, ANEMIA_T3)

table(gwg_rf_df$ANEMIA_T3, useNA = "always")

####################
# ANEMIA Yes/No
####################
gwg_rf_df <- gwg_rf_df %>%
  mutate(ANEMIA_T1_ANY = if_else(ANEMIA_T1 %in% c('1','2','3'), 1, 0))

table(gwg_rf_df$ANEMIA_T1_ANY, useNA = "always")
#* *******************************************************
#* ARRANGE AND GROUP_BY WITH RISK FACTORS:
#* ******************************************************* 
#* AGE_5_CAT, MARRY_AGE_3_CAT, married, educated, school_yrs, OCCUPATION, WEALTH_QUINT, smoke, chew_tobacco, chew_betelnut, drink, BMI4CAT, muac,
# height_index, PARITY, GRAVIDITY_CAT, num_miscarriage
# HIV_ANY_POINT, MAL_POSITIVE_ENROLL, STI_ANY_POINT, SYPH_ANY_POINT, HBV_ANY_POINT, HCV_ANY_POINT, HEV_IGM_POSITIVE_ENROLL, HEV_IGG_POSITIVE_ENROLL,
# TB_SYMP_ANY_POINT, DIAB_GEST_DX, HDP_GROUP, ANEMIA_T1, ANEMIA_T2, ANEMIA_T3, DEPR_ANY_POINT
# FERRITIN70_ANY_POINT, RBP4_ANY_POINT, VITB12_COB_ANY_POINT, FOL_RBC_ANY_POINT, FOL_SERUM_ANY_POINT, M04_MICRONUTRIENT_CMOCCUR
gwg_rf_df2 <- gwg_rf_df %>%
  arrange(SITE, MOMID, PREGID, TYPE_VISIT) %>%
  group_by(SITE, MOMID, PREGID) %>% # I don't need TYPE_VISIT here b/c it will already be arranged using visit 1.
  summarise(
    IOM_ADEQUACY = first(IOM_ADEQUACY),  # or use another rule to select the IOM_ADEQUACY value
    # Take the first variable
    AGE_5_CAT = first(AGE_5_CAT),
    MARRY_AGE_3_CAT = first(MARRY_AGE_3_CAT),
    married = first(married),
    educated = first(educated), # 0,1
    school_yrs = first(school_yrs),
    OCCUPATION = first(OCCUPATION),
    WEALTH_QUINT = first(WEALTH_QUINT),
    smoke = first(smoke),
    chew_tobacco = first(chew_tobacco),
    chew_betelnut = first(chew_betelnut),
    drink = first(drink),
    BMI4CAT = first(BMI4CAT),
   # muac = first(muac),
    height_index = first(height_index),
    PARITY = first(PARITY),
    GRAVIDITY_CAT = first(GRAVIDITY_CAT),
    MISCARRIAGE = first(MISCARRIAGE),
   # num_miscarriage = first(num_miscarriage), # don't need to use this.
    HIV_ANY_POINT = first(HIV_ANY_POINT),
    MAL_POSITIVE_ENROLL = first(MAL_POSITIVE_ENROLL),
    STI_ANY_POINT = first(STI_ANY_POINT),
    SYPH_ANY_POINT = first(SYPH_ANY_POINT),
    HBV_ANY_POINT = first(HBV_ANY_POINT),
    HCV_ANY_POINT = first(HCV_ANY_POINT),
    HEV_IGM_POSITIVE_ENROLL = first(HEV_IGM_POSITIVE_ENROLL),
    HEV_IGG_POSITIVE_ENROLL = first(HEV_IGG_POSITIVE_ENROLL),
    TB_SYMP_ANY_POINT = first(TB_SYMP_ANY_POINT),
    DIAB_GEST_DX = first(DIAB_GEST_DX),
    HDP_GROUP = first(HDP_GROUP),
    ANEMIA_T1 = first(ANEMIA_T1),
    ANEMIA_T2 = first(ANEMIA_T2),
    ANEMIA_T3 = first(ANEMIA_T3),
    DEPR_ANY_POINT = first(DEPR_ANY_POINT),
    FERRITIN70_ANY_POINT = first(FERRITIN70_ANY_POINT),
    RBP4_ANY_POINT = first(RBP4_ANY_POINT),
    VITB12_COB_ANY_POINT = first(VITB12_COB_ANY_POINT),
    FOL_RBC_ANY_POINT = first(FOL_RBC_ANY_POINT),
    FOL_SERUM_ANY_POINT = first(FOL_SERUM_ANY_POINT),
    M04_MICRONUTRIENT_CMOCCUR = first(M04_MICRONUTRIENT_CMOCCUR))
    
# n = 5251 after removing missing IOM_ADEQUACY - 11/12/2024

str(gwg_rf_df2$AGE_5_CAT)

#* *********************************************
#* WIDE DATA FOR MULTINOMIAL REGRESSION
#* *********************************************
# Prepare the data for mlogit (no longer long format since we've aggregated by MOMID)
data_wide <- mlogit.data(gwg_rf_df2, choice = "IOM_ADEQUACY", shape = "wide", id.var = "MOMID")

# Relevel IOM_ADEQUACY to ensure "Adequate" is the reference category
gwg_rf_df2$IOM_ADEQUACY <- relevel(gwg_rf_df2$IOM_ADEQUACY, ref = "Adequate")

# Figure out why some variables are not working like betelnut for SAS.
temp.df <- gwg_rf_df2 %>% 
  select(SITE, MOMID, chew_tobacco, AGE_5_CAT)

temp.df <- gwg_rf_df2 %>% 
  filter(SITE == "India-SAS")

#* **************************************************
#* RISK FACTORS ANALYSIS:
#* **************************************************
# List of new variables to convert to factors and include in the model
riskfactor_vars <- c('AGE_5_CAT', 'MARRY_AGE_3_CAT','married', 'educated', 
                     'OCCUPATION', 'WEALTH_QUINT', 'smoke', # 'school_yrs',
                     'chew_tobacco', 'chew_betelnut','drink', 'BMI4CAT', #'muac',
                     'height_index', 'PARITY', 'GRAVIDITY_CAT', 'MISCARRIAGE',
                     'HIV_ANY_POINT', 'MAL_POSITIVE_ENROLL', 'STI_ANY_POINT', 
                     'SYPH_ANY_POINT', 'HBV_ANY_POINT', 'HCV_ANY_POINT', 
                     'HEV_IGM_POSITIVE_ENROLL', 'HEV_IGG_POSITIVE_ENROLL',
                     'TB_SYMP_ANY_POINT', 'DIAB_GEST_DX', 'HDP_GROUP', 
                     'ANEMIA_T1', 'ANEMIA_T2', 'ANEMIA_T3', 'DEPR_ANY_POINT',
                     'FERRITIN70_ANY_POINT', 'RBP4_ANY_POINT', 'VITB12_COB_ANY_POINT',
                     'FOL_RBC_ANY_POINT', 'FOL_SERUM_ANY_POINT', 
                     'M04_MICRONUTRIENT_CMOCCUR')



lapply(gwg_rf_df2[, riskfactor_vars], levels) # Checks how many levels the variables have. 

# Figure out why some variables are not working like betelnut for SAS.
temp.df <- gwg_rf_df2 %>% 
  select(SITE, MOMID, chew_tobacco, AGE_5_CAT)

temp.df <- gwg_rf_df2 %>% 
  filter(SITE == "India-SAS")

# Initialize empty data frame to store RR, CI, and counts/percentages. 
RR_CI_df <- data.frame(Site = character(), 
                       Variable = character(), Coefficient = character() , 
                       logRR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(), 
                       logSE = numeric(),
                       n_subjects = numeric(),
                       # Adequate_N = numeric(), Inadequate_N = numeric(), Excessive_N = numeric(), 
                       # Adequate_Perc = numeric(), Inadequate_Perc = numeric(), Excessive_Perc = numeric(),
                       stringsAsFactors = FALSE)

sites <- unique(gwg_rf_df$SITE)

for (site in sites) {
  gwg_rf_site_df <- gwg_rf_df2 %>%
    filter(SITE == site) %>%
    select(-SITE)
  
  # Loop through the risk factors and calculate RR, CI, and counts for each IOM_ADEQUACY category
  for (riskfactor in riskfactor_vars) {
    # Checking to see how many levels are in a risk factor
    riskfactor_tab <- table(gwg_rf_site_df[[riskfactor]]) 
    print(riskfactor_tab)
    n_levels <- sum(riskfactor_tab > 0)
    if(n_levels <=1) {
      print(paste("Skipping", riskfactor, "in", site))
      next
    }
    
    # Filter out rows with NA values in the current risk factor
    gwg_df_rem_NA <- gwg_rf_site_df %>%
      filter(!is.na(.data[[riskfactor]]))
    
    # Total number of observations for the current risk factor (non-NA rows)
    total_obs <- nrow(gwg_df_rem_NA)
    
    # Recalculate the total number of women in each IOM_ADEQUACY category for the current risk factor
    iom_adequacy_counts <- table(gwg_df_rem_NA$IOM_ADEQUACY)
    
    # Extract counts for each category (default to 0 if the category doesn't exist in the filtered data)
    adequate_count <- ifelse("Adequate" %in% names(iom_adequacy_counts), iom_adequacy_counts["Adequate"], 0)
    inadequate_count <- ifelse("Inadequate" %in% names(iom_adequacy_counts), iom_adequacy_counts["Inadequate"], 0)
    excessive_count <- ifelse("Excessive" %in% names(iom_adequacy_counts), iom_adequacy_counts["Excessive"], 0)
    
    # Calculate percentages for each category
    adequate_perc <- (adequate_count / total_obs) * 100
    inadequate_perc <- (inadequate_count / total_obs) * 100
    excessive_perc <- (excessive_count / total_obs) * 100
    
    # Convert the variable to a factor (if not already)
    gwg_df_rem_NA[[riskfactor]] <- factor(gwg_df_rem_NA[[riskfactor]])
    
    # Update the formula to include the current variable
    formula <- as.formula(paste("IOM_ADEQUACY ~ 1 |", riskfactor))
    
    # Run the multinomial logistic regression model
    mlogit_model <- mlogit(formula, data = mlogit.data(gwg_df_rem_NA, choice = "IOM_ADEQUACY", shape = "wide", id.var = "MOMID"))
    
    # Extract coefficients
    coefficients <- coef(mlogit_model)
    
    # Calculate Relative Risks (RR)
    logRR <- coefficients
    # RR <- exp(coefficients)
    
    # Calculate Confidence Intervals (CI)
    CI <- exp(confint(mlogit_model))
    
    logSE <- coef(summary(mlogit_model))[, "Std. Error"]
    # SE <- exp(coef(summary(mlogit_model))[, "Std. Error"])
    
    # Create a data frame for the current variable with its RR, CI, and category counts and percentages
    var_results <- data.frame(
      Site = site,
      Variable = riskfactor,  # The current risk factor being processed
      n_subjects = total_obs,
      Coefficient = names(logRR),
      logRR = logRR,
      CI_Lower = CI[, 1],
      CI_Upper = CI[, 2],
      logSE = logSE
      # Adequate_N = adequate_count,    # Number of women in the "Adequate" category
      # Inadequate_N = inadequate_count,  # Number of women in the "Inadequate" category
      # Excessive_N = excessive_count,   # Number of women in the "Excessive" category
      # Adequate_Perc = adequate_perc,    # Percentage of "Adequate" category
      # Inadequate_Perc = inadequate_perc,  # Percentage of "Inadequate" category
      # Excessive_Perc = excessive_perc   # Percentage of "Excessive" category
    )
    
    # Append the results to the main data frame
    RR_CI_df <- rbind(RR_CI_df, var_results)
  }
}

# Round the estimates:
RR_CI_df$logRR <- round(RR_CI_df$logRR, 3)
RR_CI_df$CI_Lower <- round(RR_CI_df$CI_Lower, 3)
RR_CI_df$CI_Upper <- round(RR_CI_df$CI_Upper, 3)
# RR_CI_df$Adequate_Perc <- round(RR_CI_df$Adequate_Perc, 1)
# RR_CI_df$Inadequate_Perc <- round(RR_CI_df$Inadequate_Perc, 1)
# RR_CI_df$Excessive_Perc <- round(RR_CI_df$Excessive_Perc, 1)

RR_CI_df <- RR_CI_df %>%
  filter(logSE<Inf) %>%
  filter(!startsWith(Coefficient, "(Intercept)"))

RR_CI_df <- RR_CI_df %>%
  rowwise() %>%
  mutate(IOM_adequacy_label = str_split(Coefficient, ":", simplify = TRUE)[2], # comes from stringr
         Riskfactor_level = str_split(Coefficient, ":", simplify = TRUE)[1]) %>%
  select(-Coefficient)

#* ***************************************************
#* RENAME RISK FACTORS
#* ***************************************************
RR_CI_df <- RR_CI_df %>%
  mutate(Riskfactor_level = recode_factor(Riskfactor_level,
                                          # DEMOGRAPHICS
                                          'AGE_5_CAT<20' = 'Age <20',
                                          'AGE_5_CAT20-24' = 'Age 20-24',
                                          'AGE_5_CAT25-29' = 'Age 25-29',
                                          'AGE_5_CAT30-34' = 'Age 30-34',
                                          'AGE_5_CAT35+' = 'Age 35+',
                                          
                                          'BMI4CAT<18.5' = 'BMI underweight',
                                          'BMI4CAT18.5-<25' = 'BMI normal',
                                          'BMI4CAT25-<30' = 'BMI overweight',
                                          'BMI4CAT>=30' = 'BMI obese',
                                          
                                          'MarriedNo' = 'Not married',
                                          
                                          'MARRY_AGE_3_CAT<18' = 'Age married <18',
                                          'MARRY_AGE_3_CAT18-29' = 'Age married 18-29',
                                          'MARRY_AGE_3_CAT30+' = 'Age married 30+',
                                          
                                          'chew_betelnutYes' = 'Betelnut use',
                                          'chew_tobaccoYes' = 'Tobacco use',
                                          'drinkYes' = 'Alcohol use',
                                          'smoke1' = 'Smoking Yes',
                                          
                                          'height_index<145' = 'Height <145',
                                          'height_index145-<150' = 'Height 145-<150',
                                          'height_index150-<155' = 'Height 150-<155',
                                          'height_index>=155' = 'Height 155+',
                                          
                                          'GRAVIDITY_CAT1' = 'Gravidity 1',
                                          'GRAVIDITY_CAT2' = 'Gravidity 2',
                                          'GRAVIDITY_CAT3+' = 'Gravidity 3+',
                                          
                                          'MISCARRIAGEYes' = 'Miscarriage Yes',
                                          
                                          'OCCUPATIONNo work' = 'Occupation No work', # Ref.
                                          'OCCUPATIONSkilled labor' = 'Occupation Skilled labor',
                                          'OCCUPATIONUnskilled labor' = 'Occupation Unskilled labor',
                                          'OCCUPATIONOther' = 'Occupation Other',
                                          'OCCUPATIONHousewife' = 'Occupation Housewife',
                                          'OCCUPATIONFarming' = 'Occupation Farming',
                                          
                                          'PARITY1' = 'Parity 1', # 0 is ref.
                                          'PARITY>=2' = 'Parity 2+',
                                          
                                          'WEALTH_QUINT1' = "WealthQ 1st",
                                          'WEALTH_QUINT2' = "WealthQ 2nd",
                                          'WEALTH_QUINT3' = "WealthQ 3rd",
                                          'WEALTH_QUINT4' = "WealthQ 4th",
                                          'WEALTH_QUINT5' = "WealthQ 5th",
                                          
                                          # NUTRITION
                                          # Cobalamin
                                          'VITB12_COB_ANY_POINTDeficient' = 'Serum B12 at any point - Deficient',
                                          'VITB12_COB_ANY_POINTInsufficient' = 'Serum B12 at any point - Insufficient',
                                          'VITB12_COB_ANY_POINTSufficient' = 'Serum B12 at any point - Sufficient',
                                          
                                          # Vitamin A
                                          'RBP4_ANY_POINTNo deficiency' = 'RBP4 at any point - No deficiency',
                                          'RBP4_ANY_POINTMild deficiency' = 'RBP4 at any point - Mild deficiency',
                                          'RBP4_ANY_POINTModerate deficiency' = 'RBP4 at any point - Moderate deficiency',
                                          'RBP4_ANY_POINTSevere deficiency' = 'RBP4 at any point - Severe deficiency',
                                          
                                          'M04_MICRONUTRIENT_CMOCCUR1' = 'Received MMS supp.',
                                          
                                          'FOL_SERUM_ANY_POINTDeficient' = 'Serum folic acid level - Deficient',
                                          'FOL_SERUM_ANY_POINTPossibly deficient' = 'Serum folic acid level - Possibly deficient',
                                          'FOL_SERUM_ANY_POINTNormal' = 'Serum folic acid level - Normal',
                                          'FOL_SERUM_ANY_POINTElevated' = 'Serum folic acid level - Elevated',
                                          
                                          'FERRITIN70_ANY_POINTAbove threshold' = 'Ferritin above 70ug/L threshold',
                                          'FERRITIN70_ANY_POINTBelow threshold' = 'Ferritin below 70ug/L threshold'))

#*******************************************************************************
# META ANALYSIS - test 
#*******************************************************************************
#TODO (11/11/2024 - I do need to do the continuity correction if # events is 0 for some)
temp.df <- RR_CI_df %>%
  filter(Variable == "AGE_5_CAT") %>%
  filter(IOM_adequacy_label == "Inadequate")

temp.metagen <- metagen(logRR, logSE,
                        data=temp.df,
                        studlab = Site,
                        fixed= FALSE, random=TRUE,
                        subset=NULL, sm="RR",
                        method.tau="ML") # Default Restricted maximum likelihood estimator (REML) did not always converge so using ML. 

print(temp.metagen)
forest(temp.metagen)

#* **********************************************************
#* WRITE OUT THE RISK FACTOR LABELS AND CATEGORIES TO A FILE
#* **********************************************************
# Create a list of risk factors as a data frame and label it as Demo, Clinical and Micronutrients.
riskfactor_vars_df <- data.frame(Variable = riskfactor_vars)
write.csv(riskfactor_vars_df, file = 'data_out/gwg_riskfactor_vars.csv', row.names = FALSE)
riskfactor_cat_df <- read.csv(file = 'data_out/gwg_riskfactor_vars_category.csv')

#*******************************************************************************
meta_results <- data.frame()

for(riskfactor in riskfactor_vars) {
  riskfactor_cat <- riskfactor_cat_df %>%
    filter(riskfactor == Variable) %>%
    pull(riskfactor_category)
  
  print(paste0(riskfactor, " (", riskfactor_cat, ")"))
  
  for(iom_adequacy in c("Excessive", "Inadequate")) {
    print(paste("      ", iom_adequacy))
    atomic_rr_df_all_levels <- RR_CI_df %>%
      filter(Variable == riskfactor) %>%
      filter(IOM_adequacy_label == iom_adequacy)
    
    for(riskfactor_level in unique(atomic_rr_df_all_levels$Riskfactor_level)) {
      atomic_rr_df <- atomic_rr_df_all_levels %>%
        filter(Riskfactor_level == riskfactor_level)
      print(paste("        ", riskfactor_level))
      
      if(nrow(atomic_rr_df)==0){
        meta_results <- rbind(meta_results, 
                              data.frame(riskfactor_cat, riskfactor, riskfactor_level, iom_adequacy,
                                         n_studies=0, n_subjects=0, pooled_rr=NA, pooled_lower_ci=NA, pooled_upper_ci=NA))
        next
      }
      
      atomic_metagen <- metagen(logRR, logSE,
                                data=atomic_rr_df,
                                studlab = Site,
                                fixed= FALSE, random=TRUE,
                                subset=NULL, sm="RR",
                                method.tau="ML") # Default Restricted maximum likelihood estimator (REML) did not always converge so using ML. 
      
      plot_filename <- make.names(paste0(riskfactor, "_", iom_adequacy, "_", riskfactor_level, ".pdf"))
      pdf(file= paste0("plots_forest/", plot_filename), height = 7, width = 10)
      
      forest(atomic_metagen, layout = "RevMan5")
      dev.off() # turns off the pdf writing.
      
      # Pulling out the RR, 95%CI.
      pooled_rr <- exp(atomic_metagen$TE.random)
      pooled_lower_ci <- exp(atomic_metagen$lower.random)
      pooled_upper_ci <- exp(atomic_metagen$upper.random)
      n_studies <- nrow(atomic_rr_df)
      n_subjects <- sum(atomic_rr_df$n_subjects)
      
      # Add in to the meta_results
      meta_results <- rbind(meta_results, 
                            data.frame(riskfactor_cat, riskfactor, riskfactor_level, iom_adequacy,
                                       n_studies, n_subjects, pooled_rr, pooled_lower_ci, pooled_upper_ci))
    }
  }
}

write.csv(x = meta_results, file = 'data_out/GWG_Metaanalysis_RiskFactors_20240628.csv', row.names = FALSE)
#* **************************************************
# TILE MAP OF RISK FACTORS
#* **************************************************
tilemap_data <- meta_results %>%
  mutate(RR_sig = case_when((pooled_rr>1.00 & pooled_lower_ci>1 & pooled_upper_ci>1) ~ "RR>1.00, CI-sig",
                            (pooled_rr<1.00 & pooled_lower_ci <1 & pooled_upper_ci<1) ~ "RR<1.00, CI-sig",
                            TRUE~ "NS"))

tilemap_data$RR_sig <- factor(tilemap_data$RR_sig, 
                              levels=c("RR>1.00, CI-sig", "RR<1.00, CI-sig", 
                                       "NS"),
                              ordered = TRUE)

colScale <- c("RR>1.00, CI-sig" = "orangered", 
              "RR<1.00, CI-sig" = "dodgerblue", 
              "NS" = "gray") 

tileplot_list <- list()
for (rf_cat in unique(riskfactor_cat_df$riskfactor_category)) {
  tilemap_cat_data <- tilemap_data %>%
    filter(riskfactor_cat == rf_cat)
  tileplot <- tilemap_cat_data %>%
    ggplot() +
    aes(x = iom_adequacy, y = riskfactor_level, fill = RR_sig) + 
    geom_tile(color = "white", alpha = 0.8) + 
    scale_fill_manual(values = colScale) 
  
  tileplot_list[[length(tileplot_list) + 1]] <- tileplot  
}
plot_grid(plotlist = tileplot_list, 
          byrow = FALSE)

ggsave(plot = tileplot_list[[1]],
       filename = 'data_out/gwg_riskfactors_demographics.pdf', height = 6, width = 6)


ggsave(plot = tileplot_list[[2]],
       filename = 'data_out/gwg_riskfactors_clinical.pdf', height = 6, width = 6)

ggsave(plot = tileplot_list[[3]],
       filename = 'data_out/gwg_riskfactors_nutrients.pdf', height = 3, width = 7)


# free(tileplot_list[[1]])/free(tileplot_list[[2]])/free(tileplot_list[[3]])

#* **************************************************
#* STOP HERE. BELOW IS EXTRA
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

##################################
# Create formatted labels with the Coefficient, counts, and rounded percentages
RR_CI_plot_df <- RR_CI_plot_df %>%
  mutate(
    Label = sprintf(
      "%-30s | A: %d (%.0f%%) I: %d (%.0f%%) E: %d (%.0f%%)", 
      Coefficient, 
      Adequate_N, round(Adequate_Perc), 
      Inadequate_N, round(Inadequate_Perc), 
      Excessive_N, round(Excessive_Perc)
    )
  )

# Convert the label to a factor with reversed levels for proper ordering.
RR_CI_plot_df$Label <- factor(RR_CI_plot_df$Label, levels = rev(unique(RR_CI_plot_df$Label)))

# Add a new col to check if the CI crosses 1.0
RR_CI_plot_df <- RR_CI_plot_df %>%
  mutate(
    Significant = if_else(CI_Lower > 1 | CI_Upper < 1, "blue", "black")  # Blue if CI does not cross 1.0
  )

# Ensure the order of the labels is preserved as in the original data #TODO this is currently not working.
  RR_CI_plot_df <- RR_CI_plot_df %>%
  mutate(
    Label = fct_inorder(Label),  # Preserve the original order of Labels
    Coefficient = fct_inorder(Coefficient)  # Preserve the original order of Coefficients
  )
  # Create forest plot
  forest_plot <- RR_CI_plot_df %>%
    ggplot(aes(y = Label, x = RR, color = Significant)) +
    geom_point(size = 2) +  # Plot RR points
    geom_segment(
      data = RR_CI_plot_df %>% filter(CI_Lower >= 0 & CI_Upper <= 6),
      aes(x = CI_Lower, xend = CI_Upper, y = Label, yend = Label),
      size = 0.5
    ) +
    geom_segment(
      data = RR_CI_plot_df %>% filter(CI_Lower < 0 | CI_Upper > 6),
      aes(
        x = pmax(CI_Lower, 0), 
        xend = pmin(CI_Upper, 6), 
        y = Label, yend = Label
      ),
      size = 0.5,
      arrow = arrow(type = "closed", length = unit(0.15, "cm"))
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Reference line at 1.0
    geom_text(
      aes(label = sprintf("%.2f (%.2f, %.2f)", RR, CI_Lower, CI_Upper)),
      size = 2.5, nudge_y = 0.4
    ) +
    facet_grid(Group ~ ., scales = "free_y", space = "free", switch = "y") +
    scale_x_continuous(limits = c(0, 6)) +
    scale_color_identity() +  # Use predefined colors
    ggtitle("Risk Factors for GWG") +
    xlab("Unadjusted RR (95% CI)") +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),  # No y-axis title
      axis.text.y = element_text(hjust = 1, size = 8),  # Align y-axis text
      strip.placement = "outside",  # Place facet strips outside
      strip.text.y.left = element_text(angle = 0),  # Align facet labels horizontally
      panel.grid.major.y = element_blank(),  # Remove grid lines
      plot.title = element_text(hjust = 0.5, face = "bold")  # Center title
    )
  
  # Print the plot
  print(forest_plot)
  
  # Save the plot
  today_date <- Sys.Date()
  ggsave(filename = paste0('data_out/GWG_RiskFactors_', today_date, '.pdf'), 
         plot = forest_plot, width = 10, height = 20, units = "in")

#########################







