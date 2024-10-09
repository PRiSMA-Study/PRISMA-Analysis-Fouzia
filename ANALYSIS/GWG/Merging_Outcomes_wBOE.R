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

merged_df <- read.csv('D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data_out/merged_df_BOE-calc_uploaded_2024-06-28.csv')

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
#  infant_outcomes <- read.csv('Z:/Outcome Data/2024-06-28/INF_OUTCOMES.csv')
#  
# # MATERNAL OUTCOMES:
#  mat_preg_endpoints <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_ENDPOINTS.dta') 
#  mat_infection <- read.csv('Z:/Outcome Data/2024-06-28/MAT_INFECTION.csv')
#  mat_hdp <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_HDP.dta')
#  mat_anemia <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_ANEMIA.dta')
#  mat_gdm <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_GDM.dta')
#  mat_nearmiss <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_NEAR_MISS.dta')
#  mat_nearmiss_interim <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_NEAR_MISS_INTERIM.dta')
#  mat_risks <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_RISKS.dta')
#  mat_depr <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_DEPR.dta')
#  mat_nutr <- haven::read_dta('Z:/Outcome Data/2024-06-28/MAT_NUTR.dta')
#   
#  
# ## DEMOGRAPHICS: 
#  mat_demo <- read.csv('Z:/Outcome Data/2024-06-28/MAT_DEMOGRAPHIC.csv')
 
 # save(mat_preg_endpoints, mat_infection, mat_hdp, mat_anemia,
 #      mat_gdm,  mat_nearmiss, mat_nearmiss_interim, mat_risks, mat_depr, mat_nutr,
 #      mat_demo,
 #      infant_outcomes,
 #     # file = "D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data_out/mat_inf_demogph_dataframes_2024-06-28.rda")
 #      file = "data_out/mat_inf_demogph_dataframes_2024-06-28.rda")
 
#****************************************************************************
# Load in the Rda file:
#****************************************************************************
load("data_out/mat_inf_demogph_dataframes_2024-06-28.rda")

#****************************************************************************
# Bring in other MNH forms - STACKED:
#****************************************************************************
mnh03<- read.csv(paste0(folder_path, "/mnh03_merged.csv"))
mnh04<- read.csv(paste0(folder_path, "/mnh04_merged.csv"))
mnh05 <- read.csv(paste0(folder_path, "/mnh05_merged.csv"))
mnh19 <- read.csv(paste0(folder_path, "/mnh19_merged.csv"))

# mnh04 <- mnh04 %>%
#   distinct (MOMID, PREGID, SITE, .keep_all = TRUE)

table(mnh04$M04_TYPE_VISIT, useNA = "always") # MNH04 at multipe visits.

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
         # HELM_LBORRES,#this variable is under develop
         GON_POSITIVE_ENROLL,GON_POSITIVE_ANY_VISIT,
         CHL_POSITIVE_ENROLL, CHL_POSITIVE_ANY_VISIT,
         SYPH_POSITIVE_ENROLL, SYPH_POSITIVE_ANY_VISIT,
         HBV_POSITIVE_ENROLL, HBV_POSITIVE_ANY_VISIT,
         HCV_POSITIVE_ENROLL, HCV_POSITIVE_ANY_VISIT,
         HEV_IGM_POSITIVE_ENROLL,HEV_IGG_POSITIVE_ENROLL,
         TB_SYMP_POSITIVE_ENROLL, TB_SYMP_POSITIVE_ANY_VISIT
         )

mat_nearmiss_subset <- mat_nearmiss_interim %>%
  select(SITE, MOMID, PREGID,
         ANEMIA_SEV_ANC)

mat_ga_subset <-mat_preg_endpoints %>%
  select(SITE,MOMID,PREGID,PREG_END_GA)

mat_risks_subset <-mat_risks %>%
  select(SITE,MOMID,PREGID,WEALTH_QUINT,PARITY)

mat_depr_subset <-mat_depr %>%
  select(SITE,MOMID,PREGID,DEPR_ANC20_STND,DEPR_ANC32_STND)

mat_nutr_subset <- mat_nutr %>%
  select(SITE,MOMID,PREGID,
         FERRITIN70_ANC20,FERRITIN70_ANC32,
         RBP4_ANC20,RBP4_ANC32,
         VITB12_COB_ANC20, VITB12_COB_ANC32,
         FOL_SERUM_ANC20, FOL_SERUM_ANC32,
         FOL_RBC_ANC20, FOL_RBC_ANC32,
         FOL_SERUM_ANC20, FOL_SERUM_ANC32)

# # Micronutrient supplementation:
# nutrition_subset <-mnh04 %>% select("MOMID", "PREGID", "SITE", "M04_TYPE_VISIT",
#                                       #Iron supplement
#                                       M04_IRON_CMOCCUR,
#                                       M04_IRON_ORAL_CMOCCUR,
#                                       M04_IRON_IV_CMOCCUR,
#                                       #Folic acid supplement
#                                       M04_IFA_CMOCCUR,
#                                       #Calcium supplement
#                                       M04_CALCIUM_CMOCCUR,
#                                       #Vitamin A supplement
#                                       M04_VITAMIN_A_CMOCCUR,
#                                       #Multiple micronutrient supplement
#                                       M04_MICRONUTRIENT_CMOCCUR,
#                                       #Anthelmintic treatment
#                                       M04_ANTHELMINTHIC_CMOCCUR
# )  %>% filter(M04_TYPE_VISIT==1) # only need this info at visit 1

mat_demo_subset <- mat_demo %>%
  select(SITE, MOMID, PREGID, age18, married, marry_age, educated, marry_status, chew_tobacco, chew_betelnut, smoke, drink, 
         height_index, educated, school_yrs, bmi_enroll, bmi_index, muac,
         ga_wks_enroll, folic, nulliparous, num_fetus, num_miscarriage, primigravida)

#****************************************************************************
#. OUTCOMES FROM MNH FORMS
#****************************************************************************
mnh03_subset <- mnh03 %>%
  select(SITE, MOMID, PREGID,M03_JOB_SCORRES)

mnh04_subset <-mnh04 %>%
  select(SITE, MOMID, PREGID,M04_MICRONUTRIENT_CMOCCUR)

mnh05_subset <- mnh05 %>%
  select(SITE, MOMID, PREGID,M05_MUAC_PERES)

mnh19_subset <- mnh19 %>%
  select(SITE, MOMID, PREGID,M19_INFECTION_MHTERM_6)

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
# 9037 unique counts - FF

temp.df <- merged_df2 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL, PRETERMBIRTH_LT37, LBW2500_ANY)

#****************************************************************************
#. Merge on pregnancy outcome data.
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
  left_join(mat_nearmiss_subset,  by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mat_ga_subset,  by = c("SITE", "MOMID", "PREGID"))%>%
  left_join(mat_risks_subset,  by = c("SITE", "MOMID", "PREGID"))%>%
  left_join(mat_depr_subset,  by = c("SITE", "MOMID", "PREGID"))%>%
  left_join(mat_nutr_subset, by = c("SITE", "MOMID", "PREGID"))

#*********************************************************
#TODO Review these if else statements before moving on. 
#*********************************************************
# merged_df4 <- merged_df4 %>%
#   # IRON
#   mutate(M04_IRON_CMOCCUR = if_else((M04_IRON_CMOCCUR==55 |
#                                        M04_IRON_CMOCCUR==77 |
#                                        M04_IRON_CMOCCUR==99), 
#                                     NA, M04_IRON_CMOCCUR),
#          
#          M04_IRON_ORAL_CMOCCUR = if_else((M04_IRON_ORAL_CMOCCUR==55 |
#                                             M04_IRON_ORAL_CMOCCUR==77 |
#                                             M04_IRON_ORAL_CMOCCUR==99), 
#                                     NA, M04_IRON_ORAL_CMOCCUR),
#          
#          M04_IRON_IV_CMOCCUR = if_else((M04_IRON_IV_CMOCCUR==55 |
#                                           M04_IRON_IV_CMOCCUR==77 |
#                                           M04_IRON_IV_CMOCCUR==99), 
#                                     NA, M04_IRON_IV_CMOCCUR),
#          # IFA
#          M04_IFA_CMOCCUR = if_else((M04_IFA_CMOCCUR==55 |
#                                        M04_IFA_CMOCCUR==77 |
#                                        M04_IFA_CMOCCUR==99), 
#                                     NA, M04_IFA_CMOCCUR),
#          
#          M04_CALCIUM_CMOCCUR = if_else((M04_CALCIUM_CMOCCUR==55 |
#                                        M04_CALCIUM_CMOCCUR==77 |
#                                        M04_CALCIUM_CMOCCUR==99), 
#                                     NA, M04_CALCIUM_CMOCCUR),
#          
#          M04_VITAMIN_A_CMOCCUR = if_else((M04_VITAMIN_A_CMOCCUR==55 |
#                                        M04_VITAMIN_A_CMOCCUR==77 |
#                                        M04_VITAMIN_A_CMOCCUR==99), 
#                                     NA, M04_VITAMIN_A_CMOCCUR),
#          
#          M04_MICRONUTRIENT_CMOCCUR = if_else((M04_MICRONUTRIENT_CMOCCUR==55 |
#                                        M04_MICRONUTRIENT_CMOCCUR==77 |
#                                        M04_MICRONUTRIENT_CMOCCUR==99), 
#                                     NA, M04_MICRONUTRIENT_CMOCCUR),
#          
#          M04_ANTHELMINTHIC_CMOCCUR = if_else((M04_ANTHELMINTHIC_CMOCCUR==55 |
#                                                 M04_ANTHELMINTHIC_CMOCCUR==77 |
#                                                 M04_ANTHELMINTHIC_CMOCCUR==99), 
#                                              NA, M04_ANTHELMINTHIC_CMOCCUR))
         
merged_df4 <- merged_df4 %>%
           mutate(DIAB_GEST_ANY = if_else((DIAB_GEST_ANY==55 | DIAB_GEST_ANY==77), NA, DIAB_GEST_ANY),
                  DIAB_GEST_DX = if_else((DIAB_GEST_DX==55 | DIAB_GEST_DX==77), NA, DIAB_GEST_DX),
                  COMPLETE_ANC28 = if_else((COMPLETE_ANC28==55 | COMPLETE_ANC28==77), NA, COMPLETE_ANC28))

temp.df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, DIAB_GEST_ANY, DIAB_GEST_DX, COMPLETE_ANC28)

merged_df4 <- merged_df4 %>%
  mutate(HDP_GROUP = if_else((HDP_GROUP==55 | HDP_GROUP==77), NA, HDP_GROUP))

# Parity
merged_df4 <-merged_df4 %>%
  mutate(PARITY = if_else(PARITY==55,NA,PARITY))

# HIV infection at any point
merged_df4 <-merged_df4 %>%
  mutate(HIV_ANY_POINT = case_when(HIV_POSITIVE_ENROLL==1|HIV_POSITIVE_ANY_VISIT==1~1,
                                   HIV_POSITIVE_ENROLL==0|HIV_POSITIVE_ANY_VISIT==0~0,
                                   TRUE~NA))
# Malaria at any point
merged_df4 <-merged_df4 %>%
  mutate(MAL_ANY_POINT = case_when(MAL_POSITIVE_ENROLL==1|MAL_POSITIVE_ANY_VISIT==1~1,
                                   MAL_POSITIVE_ENROLL==0|MAL_POSITIVE_ANY_VISIT==0~0,
                                   TRUE~NA))
# STI at any point
merged_df4 <-merged_df4 %>%
  mutate(STI_ANY_POINT = case_when(GON_POSITIVE_ENROLL==1|CHL_POSITIVE_ENROLL==1|GON_POSITIVE_ANY_VISIT==1|CHL_POSITIVE_ANY_VISIT==1~1,
                                   GON_POSITIVE_ENROLL==0|CHL_POSITIVE_ENROLL==0|GON_POSITIVE_ANY_VISIT==0|CHL_POSITIVE_ANY_VISIT==0~0,
                                   TRUE~NA))

# Syphilis at any point
merged_df4 <-merged_df4 %>%
  mutate(SYPH_ANY_POINT = case_when(SYPH_POSITIVE_ENROLL==1|SYPH_POSITIVE_ANY_VISIT==1~1,
                                    SYPH_POSITIVE_ENROLL==0|SYPH_POSITIVE_ANY_VISIT==0~0,
                                    TRUE~NA))

# Hep B at any point
merged_df4 <-merged_df4 %>%
  mutate(HBV_ANY_POINT = case_when(HBV_POSITIVE_ENROLL==1|HBV_POSITIVE_ANY_VISIT==1~1,
                                   HBV_POSITIVE_ENROLL==0|HBV_POSITIVE_ANY_VISIT==0~0,
                                   TRUE~NA))

# Hep C at any point
merged_df4 <-merged_df4 %>%
  mutate(HCV_ANY_POINT = case_when(HCV_POSITIVE_ENROLL==1|HCV_POSITIVE_ANY_VISIT==1~1,
                                   HCV_POSITIVE_ENROLL==0|HCV_POSITIVE_ANY_VISIT==0~0,
                                   TRUE~NA))

# TB at any point
merged_df4 <-merged_df4 %>%
  mutate(TB_SYMP_ANY_POINT = case_when(TB_SYMP_POSITIVE_ENROLL==1|TB_SYMP_POSITIVE_ANY_VISIT==1~1,
                                       TB_SYMP_POSITIVE_ENROLL==0|TB_SYMP_POSITIVE_ANY_VISIT==0~0,
                                       TRUE~NA))

# Depression
merged_df4 <-merged_df4 %>%
  mutate(DEPR_ANC20_STND=if_else(DEPR_ANC20_STND==55,NA,DEPR_ANC20_STND),
         DEPR_ANC32_STND=if_else(DEPR_ANC32_STND==55,NA,DEPR_ANC32_STND),
         DEPR_ANY_POINT = case_when(DEPR_ANC20_STND==1|DEPR_ANC32_STND==1~1,
                                    DEPR_ANC20_STND==0|DEPR_ANC32_STND==0~0,
                                    TRUE~NA))

# Ferritin
merged_df4 <-merged_df4 %>%
  mutate(FERRITIN70_ANC20=if_else(FERRITIN70_ANC20==55,NA,FERRITIN70_ANC20),
         FERRITIN70_ANC32=if_else(FERRITIN70_ANC32==55,NA,FERRITIN70_ANC32),
         FERRITIN70_ANY_POINT = case_when(FERRITIN70_ANC20==1|FERRITIN70_ANC32==1~1,
                                          FERRITIN70_ANC20==2|FERRITIN70_ANC32==2~2,
                                          TRUE~NA))

# RBP4
# 1, Severe deficiency
# 2, Moderate deficiency
# 3, Mild deficiency
# 4, No deficiency

temp.df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, RBP4_ANC20, RBP4_ANC32)

merged_df4 <-merged_df4 %>%
  mutate(RBP4_ANC20 = if_else(RBP4_ANC20== 55, NA, RBP4_ANC20),
         RBP4_ANC32 = if_else(RBP4_ANC32== 55, NA, RBP4_ANC32),
         RBP4_ANY_POINT = case_when(RBP4_ANC20== 1 | RBP4_ANC32==1 ~ 1,
                                    RBP4_ANC20== 2 | RBP4_ANC32==2 ~ 2,
                                    RBP4_ANC20== 3 | RBP4_ANC32==3 ~ 3,
                                    RBP4_ANC20== 4 | RBP4_ANC32==4 ~ 4,
                                          TRUE ~ NA))
temp.df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, RBP4_ANC20, RBP4_ANC32, RBP4_ANY_POINT)

table(merged_df4$RBP4_ANC20, useNA = "always")
table(merged_df4$RBP4_ANC32, useNA = "always")
table(merged_df4$RBP4_ANY_POINT, useNA = "always")

# Serum B12
# 1, Deficient
# 2, Insufficient
# 3, Sufficient

temp.df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, VITB12_COB_ANC20, VITB12_COB_ANC32)

table(merged_df4$VITB12_COB_ANC20, useNA = "always")
table(merged_df4$VITB12_COB_ANC32, useNA = "always")

merged_df4 <- merged_df4 %>%
  mutate(VITB12_COB_ANC20 = if_else(VITB12_COB_ANC20 == 55, NA, VITB12_COB_ANC20),
         VITB12_COB_ANC32 = if_else(VITB12_COB_ANC32 == 55, NA, VITB12_COB_ANC32),
         
         VITB12_COB_ANY_POINT = case_when(VITB12_COB_ANC20 ==1 | VITB12_COB_ANC32 ==1 ~ 1,
                                          VITB12_COB_ANC20 ==2 | VITB12_COB_ANC32 ==2 ~ 2,
                                          VITB12_COB_ANC20 ==3 | VITB12_COB_ANC32 ==3 ~ 3,
                                          TRUE ~ NA))
temp.df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, VITB12_COB_ANC20, VITB12_COB_ANC32, VITB12_COB_ANY_POINT)

table(merged_df4$VITB12_COB_ANC20, useNA = "always")
table(merged_df4$VITB12_COB_ANC32, useNA = "always")
table(merged_df4$VITB12_COB_ANC20, useNA = "always")


# Folate deficiency
# 1, Deficient
# 2, Possibly deficient
# 3, Normal
# 4, Elevated

table(merged_df4$FOL_SERUM_ANC20, useNA = "always")
table(merged_df4$FOL_SERUM_ANC32, useNA = "always")

merged_df4_temp <-merged_df4 %>%
  mutate(FOL_SERUM_ANC20 = if_else(FOL_SERUM_ANC20 == 55, NA, FOL_SERUM_ANC20),
         FOL_SERUM_ANC32 = if_else(FOL_SERUM_ANC32 == 55, NA, FOL_SERUM_ANC32),
         
         FOL_SERUM_ANY_POINT = case_when(FOL_SERUM_ANC20==1 | FOL_SERUM_ANC32==1 ~ 1,
                                         FOL_SERUM_ANC20==2 | FOL_SERUM_ANC32== 2 ~ 2,
                                         FOL_SERUM_ANC20==3 | FOL_SERUM_ANC32== 3 ~ 3,
                                         FOL_SERUM_ANC20==4 | FOL_SERUM_ANC32== 4 ~ 4,
                                         TRUE ~ NA))

temp.df <- merged_df4_temp %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, FOL_SERUM_ANC20, FOL_SERUM_ANC32, FOL_SERUM_ANY_POINT)

table(merged_df4_temp$FOL_SERUM_ANC20, useNA = "always")
table(merged_df4_temp$FOL_SERUM_ANC32, useNA = "always")
table(merged_df4_temp$FOL_SERUM_ANY_POINT, useNA = "always")

# RBC folate deficiency
# 1, Low
# 2, Normal

table(merged_df4$FOL_RBC_ANC20, useNA = "always")
table(merged_df4$FOL_RBC_ANC32, useNA = "always")

merged_df4 <-merged_df4 %>%
  mutate(FOL_RBC_ANC20 = if_else(FOL_RBC_ANC20 == 55, NA, FOL_RBC_ANC20),
         FOL_RBC_ANC32 = if_else(FOL_RBC_ANC32 == 55, NA, FOL_RBC_ANC32),
         
         FOL_RBC_ANY_POINT = case_when(FOL_RBC_ANC20 == 1 | FOL_RBC_ANC32 == 1~ 1,
                                         FOL_RBC_ANC20==2 | FOL_RBC_ANC32== 2 ~ 2,
                                         TRUE ~ NA))

temp.df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, FOL_RBC_ANC20, FOL_RBC_ANC32, FOL_RBC_ANY_POINT)

table(merged_df4$FOL_RBC_ANC20, useNA = "always")
table(merged_df4$FOL_RBC_ANC32, useNA = "always")
table(merged_df4$FOL_RBC_ANY_POINT, useNA = "always")


# Serum folate deficiency
# 1, Deficient
# 2, Possibly deficient
# 3, Normal
# 4, Elevated

table(merged_df4$FOL_SERUM_ANC20, useNA = "always")
table(merged_df4$FOL_SERUM_ANC32, useNA = "always")

merged_df4 <-merged_df4 %>%
  mutate(FOL_SERUM_ANC20 = if_else(FOL_SERUM_ANC20 == 55, NA, FOL_SERUM_ANC20),
         FOL_SERUM_ANC32 = if_else(FOL_SERUM_ANC32 == 55, NA, FOL_SERUM_ANC32),
         
         FOL_SERUM_ANY_POINT = case_when(FOL_SERUM_ANC20 == 1 | FOL_SERUM_ANC32 == 1 ~ 1,
                                         FOL_SERUM_ANC20== 2 | FOL_SERUM_ANC32 == 2 ~ 2,
                                         FOL_SERUM_ANC20== 3 | FOL_SERUM_ANC32 == 3 ~ 3,
                                         FOL_SERUM_ANC20== 4 | FOL_SERUM_ANC32 == 4 ~ 4,
                                         TRUE ~ NA))

temp.df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, FOL_SERUM_ANC20, FOL_SERUM_ANC32, FOL_SERUM_ANY_POINT)

table(merged_df4$FOL_SERUM_ANC20, useNA = "always")
table(merged_df4$FOL_SERUM_ANC32, useNA = "always")
table(merged_df4$FOL_SERUM_ANY_POINT, useNA = "always")


#****************************************************************************
#. Merge on demographics
#****************************************************************************
#TODO STOPPED HERE - FF 

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
                      


