# Date: June 25, 2024
# Author: Fouzia Farooq

# THIS FILE CREATES BOE VARIABLES THAT I CAN RUN EVERYTIME WITH NEW DATASET: 

#****************************************************************************
#TODO: THINGS TO DO AS OF 07/01/2024
#* Need to change all 55, 77 etc to NA before giving dataset to Lili.
#****************************************************************************

library(tidyverse)
library(lubridate)
library(naniar)
library(writexl) # only for writing excel files
library(haven) # STATA files

rm(list = ls())
dir.create("data_out")

UploadDate = "2024-06-28"

#****************************************************************************
#0. # READ FILES
#****************************************************************************
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data/Stacked Data/", UploadDate)

merged_df <- read.csv('ANALYSIS/GWG/data_out/merged_df_BOE-calc_uploaded_2024-06-28.csv')

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


#****************************************************************************
#0. # DON'T NEED TO READ THESE FILES EVERYTIME - THEY'R SAVED AS .RDA
# Just read in the 'mat_inf_demogph_dataframes_2024-06-28.rda'.
# When data is updated, these have to be read in again. 
#****************************************************************************

# # INFANT OUTCOMES:
# infant_outcomes <- read.csv('Z:/Outcome Data/2024-06-28/INF_OUTCOMES.csv')
# 
# # MATERNAL OUTCOMES:
# mat_preg_endpoints <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_ENDPOINTS.dta') 
# mat_infection <- read.csv('Z:/Outcome Data/2024-06-28/MAT_INFECTION.csv')
# mat_hdp <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_HDP.dta')
# mat_anemia <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_ANEMIA.dta')
# mat_gdm <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_GDM.dta')
# mat_nearmiss <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_NEAR_MISS.dta')
# mat_nearmiss_interim <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_NEAR_MISS_INTERIM.dta')
#  
# 
# # DEMOGRAPHICS: 
# mat_demographics <- read.csv('Z:/Outcome Data/2024-06-28/MAT_DEMOGRAPHIC.csv')
# 
#  save(mat_preg_endpoints, mat_infection, mat_hdp, mat_anemia, 
#       mat_gdm,  mat_nearmiss, mat_nearmiss_interim,
#       infant_outcomes,
#       mat_demographics, 
#       file = "ANALYSIS/GWG/data_out/mat_inf_demogph_dataframes_2024-06-28.rda")

#****************************************************************************
# Load in the Rda file:
#****************************************************************************
load("ANALYSIS/GWG/data_out/mat_inf_demogph_dataframes_2024-06-28.rda")


# READ IN ADDITIONAL FILES: 
# mnh06 <- read.csv('data/Stacked Data/2024-06-14/mnh06_merged.csv')
# mnh06 <- mnh06 %>% select(SITE, MOMID, PREGID, M06_SINGLETON_PERES, M06_TYPE_VISIT)

#****************************************************************************
# INFANT OUTCOMES:
#****************************************************************************
# USE THESE VARIABLES FOR GWG:
# Preterm birth <34 weeks = PRETERMBIRTH_LT34
# Preterm birth <37 weeks = PRETERMBIRTH_LT37
#LBW <2500g = LBW2500_ANY
# Birth weight of an infant per INTERGROWTH standards (Per infant, including livebirths, including multiples), cateogorical	"11, SGA <3rd
#      12, SGA <10th; 13, AGA 10 to < 90th; 14, LGA >=90; 55, Missing information = SGA_CAT
# Death prior to delivery of a fetus at ≥22 weeks of gestation = STILLBIRTH_22WK
# Death of a neonate or an infant up to 1 year of life and has passed the risk period (risk period = >365 days).=INF_DTH
# Fetal loss <20 weeks (miscarriage). = INF_ABOR_SPN

infant_outcomes_subset <- infant_outcomes %>%
  select(SITE, MOMID, PREGID, INFANTID, PRETERMBIRTH_LT34, PRETERMBIRTH_LT37, 
         LBW2500_ANY, SGA_CAT, STILLBIRTH_22WK, INF_DTH, INF_ABOR_SPN)

#****************************************************************************
# MATERNAL OUTCOMES:
#****************************************************************************
mat_gdm_subset <-mat_gdm %>%
  select(SITE, MOMID, PREGID, DIAB_GEST_ANY, DIAB_GEST_DX, COMPLETE_ANC28)

mat_hdp_subset <- mat_hdp %>%
  select(SITE, MOMID, PREGID, HDP_GROUP)

mat_infect_subset <- mat_infection %>%
  select(SITE, MOMID, PREGID, 
         HIV_POSITIVE_ENROLL, HIV_POSITIVE_ANY_VISIT,
         MAL_POSITIVE_ENROLL, MAL_POSITIVE_ANY_VISIT,
         HBV_POSITIVE_ENROLL, HBV_POSITIVE_ANY_VISIT,
         HCV_POSITIVE_ENROLL, HCV_POSITIVE_ANY_VISIT)

mat_nearmiss_subset <- mat_nearmiss_interim %>%
  select(SITE, MOMID, PREGID,
         ANEMIA_SEV_ANC)

#****************************************************************************
#. Singleton pregnancy - i already have these from the BOE_calc.R file
#****************************************************************************
# NA here.

#****************************************************************************
#. Subset Infant outcome and pregnancy outcome file to women who only had a singleton pregnancy
#****************************************************************************
# Step 1: Extract MOMIDs from MERGED_DF
momids_vector <- merged_df %>% 
  pull(MOMID) %>% 
  unique()

# Step 2: Filter mnh05 based on MOMIDs vector
infant_outcomes_subset2 <- infant_outcomes_subset %>%
  filter(MOMID %in% momids_vector) %>%
  relocate(SITE, MOMID, PREGID)

##### PREG ENDPOINT:
mat_preg_endpoints2 <- mat_preg_endpoints %>%
  filter(MOMID %in% momids_vector) %>% 
  relocate(SITE, MOMID, PREGID)
#****************************************************************************
#. Merge on the infant outcomes.
#****************************************************************************
merged_df2 <- left_join(merged_df, infant_outcomes_subset2,
                        by = c("SITE", "MOMID", "PREGID"))

# Count unique MOMIDs for each ENROLL group
unique_counts <- merged_df2 %>%
  distinct(MOMID, PREGID, SITE, ENROLL, .keep_all = TRUE) %>%
  group_by(ENROLL) %>%
  summarize(unique_MOMIDs = n_distinct(MOMID))


temp.df <- merged_df2 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL, PRETERMBIRTH_LT37, LBW2500_ANY)

#****************************************************************************
#. Merge on pregnancy outcome data.
### I will only use women who had a pregnancy outcome with a visit 5 to calc.
### GWG from VISIT 1 to VISIT 5.
#****************************************************************************
mat_preg_endpoints2_subset <- mat_preg_endpoints2 %>%
  select(SITE, MOMID, PREGID, PREG_END)

merged_df3 <- left_join(merged_df2, mat_preg_endpoints2_subset,
                        by = c("SITE", "MOMID", "PREGID"))


#****************************************************************************
#. Merge on the maternal outcomes.
#****************************************************************************
merged_df4 <- merged_df3 %>% 
  left_join(mat_gdm_subset, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mat_hdp_subset,  by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mat_infect_subset,  by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mat_nearmiss_subset,  by = c("SITE", "MOMID", "PREGID"))

merged_df4 <- merged_df4 %>%
  mutate(DIAB_GEST_ANY = if_else((DIAB_GEST_ANY==55 | DIAB_GEST_ANY==77), NA, DIAB_GEST_ANY),
         DIAB_GEST_DX = if_else((DIAB_GEST_DX==55 | DIAB_GEST_DX==77), NA, DIAB_GEST_DX),
         COMPLETE_ANC28 = if_else((COMPLETE_ANC28==55 | COMPLETE_ANC28==77), NA, COMPLETE_ANC28))

merged_df4 <- merged_df4 %>%
  mutate(HDP_GROUP = if_else((HDP_GROUP==55 | HDP_GROUP==77), NA, HDP_GROUP))

temp.df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, DIAB_GEST_ANY, DIAB_GEST_DX, COMPLETE_ANC28)


#****************************************************************************
#. Merge on demographics
#****************************************************************************
mat_demo_subset <- mat_demographics %>%
  select(SITE, MOMID, PREGID, age18, married, marry_age, marry_status, chew_tobacco, chew_betelnut, smoke, drink, 
         height_index, educated, school_yrs, bmi_enroll, bmi_index, muac,
         ga_wks_enroll, folic, nulliparous, num_fetus, num_miscarriage, primigravida)


merged_df5 <- merged_df4 %>% 
  left_join(mat_demo_subset, by = c("SITE", "MOMID", "PREGID"))

temp.df <- merged_df4 %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


#****************************************************************************
#. Tables: 
#****************************************************************************

#######
# PTB # 
#######
merged_df3 %>% distinct (MOMID, PREGID, SITE, PRETERMBIRTH_LT34, .keep_all = TRUE) %>%
  count(PRETERMBIRTH_LT34)

merged_df3 %>% distinct (MOMID, PREGID, SITE, PRETERMBIRTH_LT37, .keep_all = TRUE) %>%
  count(PRETERMBIRTH_LT37)

temp.df <- merged_df3 %>% 
  select(SITE, MOMID, PREGID, INFANTID, TYPE_VISIT, PRETERMBIRTH_LT34, PRETERMBIRTH_LT37)

#######
# LBW # 
#######
merged_df3 %>% distinct (MOMID, PREGID, SITE, LBW2500_ANY, .keep_all = TRUE) %>%
  count(LBW2500_ANY)

#######
# SGA # 
#######
merged_df3 %>% distinct (MOMID, PREGID, SITE, SGA_CAT, .keep_all = TRUE) %>%
  count(SGA_CAT)

##########################
# Stillbirth >=22 weeks # 
#########################
merged_df3 %>% distinct (MOMID, PREGID, SITE, STILLBIRTH_22WK, .keep_all = TRUE) %>%
  count(STILLBIRTH_22WK)

#################
# Infant Death # 
################
merged_df3 %>% distinct (MOMID, PREGID, SITE, INF_DTH, .keep_all = TRUE) %>%
  count(INF_DTH)

#######################
# Abortion <20 weeks # 
######################
merged_df3 %>% distinct (MOMID, PREGID, SITE, INF_ABOR_SPN, .keep_all = TRUE) %>%
  count(INF_ABOR_SPN)



#****************************************************************************
# WRITE OUT THE FILE
#****************************************************************************
write.csv(merged_df5, paste0("ANALYSIS/GWG/data_out/merged_df_w_Outcomes_uploaded_", UploadDate, ".csv"))


#****************************************************************************
#0. # READ IN THE FILE I JUST CREATED - in case I need this to check:
#****************************************************************************
UploadDate = "2024-06-28"
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data/Stacked Data/", UploadDate)
getwd()
merged_df <- read.csv(paste0('ANALYSIS/GWG/data_out/merged_df_w_Outcomes_uploaded_', UploadDate, '.csv'))
                      


