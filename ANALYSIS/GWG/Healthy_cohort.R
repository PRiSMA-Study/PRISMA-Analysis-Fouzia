#****************************************************************************
#*Aim 1: Healthy cohort criterias
#*Author: Fouzia Farooq (Adapted from Xiaoyan's code)
#*# Xiaoyan's github: https://github.com/PRiSMA-Study/REMAPP-Analysis-Xiaoyan/blob/main/ReMAPP-Aim1/Data-prep.R
#****************************************************************************
# Healthy Cohort Criteria
# https://docs.google.com/document/d/1skBWk_Ry7to9V34h4NO8uZFt_ucj7tJK/edit
#****************************************************************************
#*Clinically healthy pregnant women, defined as:
# Aged 18 to 34 years
# Gestational age <14 weeks [44]
# Pre-pregnancy or early pregnancy body mass index (BMI) of >18.5 and <30 kg/m2 AND mid-upper arm circumference (MUAC) > 23cm [45]
# Height ≥150 cm [46] - NOTE: We will not include this as part of the criteria.  We want to include all BBMI and MUAC.
# Singleton pregnancy 
# Not iron deficient (serum ferritin >15 mcg/L– adjusted for inflammation) [47]
# No subclinical inflammation (CRP≤5 and/or AGP≤1) [48] 
# At low risk of pregnancy complications, determined by the following criteria:
#   Systolic blood pressure <140 mmHg and diastolic blood pressure <90 mmHg [21]
# No previous reported low birth weight delivery
# No previous reported stillbirth
# No previous reported unplanned cesarean delivery
# Normal glucose-6-phosphate dehydrogenase (≥6.1 U/g Hb)
# No hemoglobinopathies: SS, SC, SE, EE, CC, SD-Punjab, Sβthal, Eβthal, Cβthal, CD-Punjab, ED-Punjab, D-D-Punjab, D-Punjabβthal, Thalassemia major, Thalassemia intermedia, or Alpha thalassemia
# No reported cigarette smoking, tobacco chewing, or betel nut use during pregnancy
# No reported alcohol consumption during pregnancy
# No known history or current chronic disease including cancer, kidney disease, and cardiac conditions
# No known history or current HIV
# No current malaria infection (per rapid diagnostic test)
# No current Hepatitis B virus infection (per rapid diagnostic test)
# No current Hepatitis C virus infection (per rapid diagnostic test)


# After enrollment, participants will be excluded from the final analysis if any of the following occur: 
#   Multiple pregnancies not identified at recruitment
# Severe conditions not evident at recruitment including cancer, HIV, TB, or Malaria
# Severe pregnancy-related conditions requiring hospital admission including eclampsia or severe pre-eclampsia
#    May have to include other adverse outcomes as well such as GDM - look at SAP - ERS comment.
#****************************************************************************


rm(list = ls())

library(tidyverse)
library(lubridate)
library(naniar)

UploadDate = "2024-06-14"

#****************************************************************************
#0. READ IN MERGED_DF - has MNH00, MNH01, MNH02. 
#****************************************************************************
# READ IN MY merged I CREATED IN BOE_FF.R File. 
merged_df <- read.csv('data_out/merged_df_BOE-calc_uploaded_2024-06-14.csv')


#****************************************************************************
#0. READ IN OTHER FILES.
#****************************************************************************

mnh03 <- read.csv(paste0("D:/Users/fouziafarooq/Documents/PRISMA_ANALYSIS/GWG/data/Stacked Data/", UploadDate, "/mnh03_merged.csv"))

# mnh03 - keep necessary variables
mnh03_subset <- mnh03 %>% 
  select(SITE, MOMID, PREGID, M03_MARITAL_SCORRES,
         M03_SMOKE_OECOCCUR, M03_CHEW_BNUT_OECOCCUR, M03_CHEW_OECOCCUR, M03_DRINK_OECOCCUR)

mnh04 <- read.csv(paste0("D:/Users/fouziafarooq/Documents/PRISMA_ANALYSIS/GWG/data/Stacked Data/", UploadDate, "/mnh04_merged.csv"))

# mnh04 - keep necessary variables
mnh04_subset <- mnh04 %>%
  filter(M04_TYPE_VISIT == 1) %>%
  select(SITE, MOMID, PREGID, M04_PRETERM_RPORRES, M04_PH_PREV_RPORRES, M04_PH_PREVN_RPORRES, M04_PH_LIVE_RPORRES, 
         M04_MISCARRIAGE_RPORRES, M04_MISCARRIAGE_CT_RPORRES, M04_PH_OTH_RPORRES,M04_STILLBIRTH_RPORRES,
         M04_LOWBIRTHWT_RPORRES, M04_MALARIA_EVER_MHOCCUR, 
         M04_CANCER_EVER_MHOCCUR, M04_KIDNEY_EVER_MHOCCUR, M04_CARDIAC_EVER_MHOCCUR,
         M04_HIV_MHOCCUR, M04_HIV_EVER_MHOCCUR, M04_UNPL_CESARIAN_PROCCUR, M04_PREECLAMPSIA_RPORRES,
         M04_GEST_DIAB_RPORRES, M04_PREMATURE_RUPTURE_RPORRES,
         M04_MACROSOMIA_RPORRES, M04_OLIGOHYDRAMNIOS_RPORRES,
         M04_APH_RPORRES, M04_PPH_RPORRES)

  
mnh05 <- read.csv(paste0("D:/Users/fouziafarooq/Documents/PRISMA_ANALYSIS/GWG/data/Stacked Data/", UploadDate, "/mnh05_merged.csv")) 

#mnh05 - keep necessary variables
mnh05_subset <- mnh05 %>% 
  filter(M05_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, M05_ANT_PEDAT, M05_WEIGHT_PERES, M05_HEIGHT_PERES, M05_MUAC_PERES)

mnh06 <- read.csv(paste0("D:/Users/fouziafarooq/Documents/PRISMA_ANALYSIS/GWG/data/Stacked Data/", UploadDate, "/mnh06_merged.csv"))

#MNH06 - keep necessary variables
mnh06_subset <- mnh06 %>%
  filter(M06_TYPE_VISIT == 1) %>% 
  select(SITE, MOMID, PREGID, M06_SINGLETON_PERES, 
         M06_BP_SYS_VSORRES_1, M06_BP_SYS_VSORRES_2, M06_BP_SYS_VSORRES_3,
         M06_BP_DIA_VSORRES_1, M06_BP_DIA_VSORRES_2, M06_BP_DIA_VSORRES_3,
         M06_MALARIA_POC_LBORRES, M06_MALARIA_POC_LBPERF, 
         M06_HBV_POC_LBORRES, M06_HBV_POC_LBPERF, M06_HCV_POC_LBORRES, M06_HCV_POC_LBPERF,
         M06_HIV_POC_LBORRES, M06_HIV_POC_LBPERF,
         num_range("M06_HB_POC_LBORRES_",1:12))


mnh08 <- read.csv(paste0("D:/Users/fouziafarooq/Documents/PRISMA_ANALYSIS/GWG/data/Stacked Data/", UploadDate, "/mnh08_merged.csv"))

# MNH08 - keep necessary variables

mnh08_subset <- mnh08 %>%
  filter(M08_TYPE_VISIT == 1) %>%
  select(SITE, MOMID, PREGID,
         M08_MN_LBPERF_8, M08_FERRITIN_LBORRES,
         M08_RBC_LBPERF_2, M08_RBC_THALA_LBORRES, M08_RBC_LBPERF_3, M08_RBC_GLUC6_LBORRES,
         M08_MN_LBPERF_12, M08_CRP_LBORRES, M08_MN_LBPERF_13, M08_AGP_LBORRES)


mnh09 <- read.csv(paste0("D:/Users/fouziafarooq/Documents/PRISMA_ANALYSIS/GWG/data/Stacked Data/", UploadDate, "/mnh09_merged.csv"))

# MNH09 - keep necessary variables
mnh09_subset <- mnh09 %>% 
  select(SITE, MOMID, PREGID, num_range("M09_INFANTID_INF",1:4),
         num_range("M09_INFANTID_INF",1:4),
         num_range("M09_BIRTH_DSTERM_INF",1:4), 
         num_range("M09_DELIV_DSSTDAT_INF",1:4))


#****************************************************************************
# Merge maternal data after ReMAPP luanches - FF: I dont think i need to do this.
#****************************************************************************
# df_maternal <- df_mat %>%
#   mutate(REMAPP_LAUNCH = ifelse((SITE == "Ghana" & M02_SCRN_OBSSTDAT >= "2022-12-28") |
#                                   (SITE == "Kenya" & M02_SCRN_OBSSTDAT >= "2023-04-14") |
#                                   (SITE == "Zambia" & M02_SCRN_OBSSTDAT >= "2022-12-15") |
#                                   (SITE == "Pakistan" & M02_SCRN_OBSSTDAT >= "2022-09-22") |
#                                   (SITE == "India-CMC" & M02_SCRN_OBSSTDAT >= "2023-06-20") |
#                                   (SITE == "India-SAS" & M02_SCRN_OBSSTDAT >= "2023-08-15"), 1, 0)) %>% 
#   filter(REMAPP_LAUNCH == 1) %>%
#   left_join(mnh00, by = c("SITE", "SCRNID")) %>% 
#   left_join(mnh03, by = c("SITE", "MOMID", "PREGID")) %>% 
#   left_join(mnh04, by = c("SITE", "MOMID", "PREGID")) %>% 
#   left_join(mnh05, by = c("SITE", "MOMID", "PREGID")) %>% 
#   left_join(mnh06, by = c("SITE", "MOMID", "PREGID")) %>% 
#   left_join(mnh08, by = c("SITE", "MOMID", "PREGID")) %>% 
#   left_join(mnh09, by = c("SITE", "MOMID", "PREGID"))
# 
# #save data
# save(df_maternal, file = "derived_data/df_maternal.rda")

#****************************************************************************
#. Merge my merged_df and the subsets together
#****************************************************************************
merged_df <- left_join(merged_df, 
                       mnh03_subset, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh04_subset, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh05_subset, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh06_subset, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh08_subset, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(mnh09_subset, by = c("SITE", "MOMID", "PREGID"))

duplicate_df <- merged_df %>% group_by(MOMID, PREGID) %>%  filter(n()>1)
duplicate_df2 <- duplicate_df %>% distinct(MOMID, PREGID)

merged_df <- merged_df %>%
  distinct (MOMID, PREGID, SITE, .keep_all = TRUE)

#****************************************************************************
#1. Define criteria
#****************************************************************************
# Criteria suggested by Emily: 
# Age
# GA<14 weeks.
#derive criteria
df_criteria <- merged_df %>%
  mutate(
    # A. age at enrollment
    # Aged 18 to 34 years
    AGE_ENROLL = ifelse(M00_KNOWN_DOBYN_SCORRES == 1 &  M00_BRTHDAT != "1907-07-07", 
                        as.numeric(ymd(M02_SCRN_OBSSTDAT) - ymd(M00_BRTHDAT))/365,
                        ifelse(M00_KNOWN_DOBYN_SCORRES == 0 & M00_ESTIMATED_AGE != -7, M00_ESTIMATED_AGE, 99)),
    CRIT_AGE = ifelse((AGE_ENROLL > 0 & AGE_ENROLL < 18) | AGE_ENROLL > 34, 0,
                      ifelse(AGE_ENROLL >= 18 & AGE_ENROLL <= 34, 1, 55)
    ),
    
    # B. GA at enrollment
    # gestational age at enrollment - Gestational age <14 weeks 
    # Xiaoyan is calling it BOE_GA_DAYS_ENROLL and I am calling it: GA_ENROLL_DAYS 
    BASELINE_GA_WKS = floor(GA_ENROLL_DAYS/7),
    CRIT_GA = ifelse(BASELINE_GA_WKS > 0 & BASELINE_GA_WKS < 14, 1,
                     ifelse(BASELINE_GA_WKS >= 14 & BASELINE_GA_WKS <=26, 0,
                            ifelse(BASELINE_GA_WKS == -7 | is.na(BASELINE_GA_WKS), NA, 77))),
    
    # FF NOTE: we are not using BMI or MUAC as a healty cohort criteria b/c for GWG we want to keep all of these women.
    # C. Pre-pregnancy or early pregnancy body mass index (BMI) of >18.5 and <30 kg/m2 AND mid-upper arm circumference (MUAC) > 23cm [45]
    # BMI
    # BMI = M05_WEIGHT_PERES / M05_HEIGHT_PERES / M05_HEIGHT_PERES * 10000,
    # 
    # TEMP_BMI = ifelse(BMI <= 18.5 | BMI >= 30, 0, 
    #                   ifelse(BMI > 18.5 & BMI < 30, 1, 55)
    # ),
    # # MUAC mid-upper arm circumference - MUAC
    # TEMP_MUAC = ifelse(M05_MUAC_PERES <= 23, 0, 
    #                    ifelse(M05_MUAC_PERES > 23, 1, 55)
    # ),
    # CRIT_BMI_MUAC = case_when(
    #   TEMP_BMI == 1 & TEMP_MUAC == 1 ~ 1, 
    #   TEMP_BMI == 0 | TEMP_MUAC == 0 ~ 0, 
    #   TRUE ~ 55
    # ),
    # D. Height ≥150 cm
    CRIT_HEIGHT = ifelse(M05_HEIGHT_PERES < 150, 0,
                         ifelse(M05_HEIGHT_PERES >= 150, 1, 55)
    ),
    # E. Singleton pregnancy
    CRIT_SINGLEPREG = ifelse(M06_SINGLETON_PERES == 0, 0,
                             ifelse(M06_SINGLETON_PERES == 1, 1, 55)
    ),
    # F. no iron deficiency (not iron deficient: serum ferritin > 15 mcg/L) data unit is ??g/dL couble check before use
    #convert unit from ug/dL to mcg/L
    FERRITIN_LBORRES = case_when(
      SITE %in% c("Ghana", "Kenya") ~ 10*M08_FERRITIN_LBORRES, #need add Pakistan and India conversion rate when we have data
      SITE == "Zambia" ~ M08_FERRITIN_LBORRES 
    ),
    CRIT_IRON = ifelse(FERRITIN_LBORRES > 15, 1,
                       ifelse(FERRITIN_LBORRES >0 & FERRITIN_LBORRES <= 15, 0,
                              ifelse(FERRITIN_LBORRES == 0, 0, 55))
    ),
    
   # G. no subclinical inflammation (CRP???5 and/or AGP???1) ??? check unit (mg/L for CRP and g/L for AGP in dd) double check the calculation before use
    CRIT_INFLAM = case_when(
      M08_CRP_LBORRES > 0 & M08_CRP_LBORRES <= 5 & M08_AGP_LBORRES >0 & M08_AGP_LBORRES <= 1 ~ 1,
      M08_CRP_LBORRES > 5 | M08_AGP_LBORRES > 1 ~ 0,
      M08_MN_LBPERF_12 == 0 | M08_MN_LBPERF_13 == 0 ~ 55,
      TRUE ~ 55
    )
  ) %>% 
  rowwise() %>% 
  mutate(
    # H.a. blood pressure
    M06_BP_SYS_1 = mean(c(M06_BP_SYS_VSORRES_1, M06_BP_SYS_VSORRES_2, M06_BP_SYS_VSORRES_3), na.rm = TRUE),
    M06_BP_DIA_1 = mean(c(M06_BP_DIA_VSORRES_1, M06_BP_DIA_VSORRES_2, M06_BP_DIA_VSORRES_3), na.rm = TRUE),
    
    CRIT_BP = ifelse(M06_BP_SYS_1 > 0 & M06_BP_SYS_1 < 140 & M06_BP_DIA_1 > 0 & M06_BP_DIA_1 < 90, 1,
                     ifelse(M06_BP_SYS_1 >= 140 | M06_BP_DIA_1 >= 90, 0, 55)
    )) %>% 
  ungroup() %>% 
  mutate(
    # H.b. no previous low birth weight delivery
    CRIT_LBW = ifelse(M04_LOWBIRTHWT_RPORRES == 1, 0,
                      ifelse(M04_PH_PREV_RPORRES == 0 | M04_LOWBIRTHWT_RPORRES == 0, 1,
                             ifelse(M04_LOWBIRTHWT_RPORRES == 99, 0, 55))
    ),
    # H.c. No previous reported stillbirth
    CRIT_STILLBIRTH = ifelse(M04_STILLBIRTH_RPORRES == 1, 0, #stillbirth,
                             ifelse(M04_PH_PREV_RPORRES == 0 | 
                                      M04_PH_OTH_RPORRES == 0 | #no fetal loss
                                      M04_STILLBIRTH_RPORRES == 0, 1,
                                    ifelse(M04_STILLBIRTH_RPORRES == 99, 0, 55))  
    ),
    # H.d. No previous reported unplanned cesarean delivery
    CRIT_UNPL_CESARIAN = case_when(
      M04_UNPL_CESARIAN_PROCCUR == 1 ~ 0, 
      M04_PH_PREV_RPORRES == 0 | M04_UNPL_CESARIAN_PROCCUR == 0 ~ 1,
      M04_UNPL_CESARIAN_PROCCUR == 99 ~ 0,
      TRUE ~ 55 
    ),
    #************************************************************************************************
    #* #TODO FF: Need to add Normal glucose-6-phosphate dehydrogenase (≥6.1 U/g Hb) in the future. 
    #************************************************************************************************
    # I. No hemoglobinopathies: SS, SC, SE, EE, CC, SD-Punjab, Sβthal, Eβthal, Cβthal, CD-Punjab, ED-Punjab, D-D-Punjab, 
    # D-Punjabβthal, Thalassemia major, Thalassemia intermedia, glucose-6-phosphate dehydrogenase deficiency, or Alpha thalassemia
    CRIT_HEMOGLOBINOPATHIES = ifelse(M08_RBC_THALA_LBORRES == 0 & M08_RBC_GLUC6_LBORRES == 0, 1,
                                     ifelse(M08_RBC_THALA_LBORRES == 1 | M08_RBC_GLUC6_LBORRES == 1, 0,
                                            ifelse(M08_RBC_LBPERF_2 == 0 | M08_RBC_LBPERF_3 == 0, 55, 55))
    ),
    #J. No reported cigarette smoking, tobacco chewing, or betel nut use during pregnancy
    CRIT_SMOKE = case_when(
      SITE == "Zambia" & (M03_SMOKE_OECOCCUR == 1 | M03_CHEW_OECOCCUR == 1) ~ 0,
      SITE == "Zambia" & (M03_SMOKE_OECOCCUR == 0 & M03_CHEW_OECOCCUR == 0) ~ 1,
      M03_SMOKE_OECOCCUR == 1 | M03_CHEW_BNUT_OECOCCUR == 1 | M03_CHEW_OECOCCUR == 1 ~ 0,
      M03_SMOKE_OECOCCUR == 0 & M03_CHEW_BNUT_OECOCCUR == 0 & M03_CHEW_OECOCCUR == 0 ~ 1,
      TRUE ~ 55
    ),
    #K. No reported alcohol consumption during pregnancy
    CRIT_DRINK = ifelse(SITE == "Pakistan", 666,
                        ifelse(M03_DRINK_OECOCCUR == 1, 0,
                               ifelse(M03_DRINK_OECOCCUR == 0, 1,
                                      ifelse(M03_DRINK_OECOCCUR == 66, 0,
                                             ifelse(M03_DRINK_OECOCCUR == 77, 0, 55)))) #temporary code for Kenya, check for other country
    ), 
    #L. No known history or current chronic disease including cancer, kidney disease, and cardiac conditions
    CRIT_CHRONIC = ifelse(M04_CANCER_EVER_MHOCCUR == 1 | M04_KIDNEY_EVER_MHOCCUR == 1 | 
                            M04_CARDIAC_EVER_MHOCCUR == 1, 0,
                          ifelse(M04_CANCER_EVER_MHOCCUR == 0 & M04_KIDNEY_EVER_MHOCCUR == 0 & 
                                   M04_CARDIAC_EVER_MHOCCUR == 0, 1,
                                 ifelse(M04_CANCER_EVER_MHOCCUR == 99 | M04_KIDNEY_EVER_MHOCCUR == 99 | 
                                          M04_CARDIAC_EVER_MHOCCUR == 99, 0, 55))
    ),
    #M. No known history or current HIV
    # if "Record HIV results" = positive, then CRIT_HIV=0 (ineligible) [M06_HIV_POC_LBORRES]
    CRIT_HIV = ifelse(M06_HIV_POC_LBORRES == 1, 0, 
                      # if "Record HIV results" = negative, then CRIT_HIV=1 (eligible) [M06_HIV_POC_LBORRES]
                      ifelse(M06_HIV_POC_LBORRES == 0, 1,
                             # if "Record HIV results" = 55, then CRIT_HIV=55 (pending) [M06_HIV_POC_LBORRES]
                             ifelse(M06_HIV_POC_LBORRES == 55, 55,  
                                    # if "Have you ever been diagnosed with HIV?" = yes OR 
                                    # if "Have you had any of the following issues since becoming pregnant with the current pregnancy, HIV" = yes, then CRIT_HIV=0 (ineligible)
                                    ifelse(M04_HIV_EVER_MHOCCUR == 1 |  M04_HIV_MHOCCUR == 1, 0,
                                           # if "Have you ever been diagnosed with HIV?" = no AND 
                                           # if "Have you had any of the following issues since becoming pregnant with the current pregnancy, HIV" = no, then CRIT_HIV=1 (eligible)
                                           ifelse(M04_HIV_EVER_MHOCCUR == 0 & M04_HIV_MHOCCUR == 0, 1,
                                                  # if "Have you ever been diagnosed with HIV?" = 55, then CRIT_HIV=55 (pending) OR
                                                  # if "Have you had any of the following issues since becoming pregnant with the current pregnancy, HIV" = 55, then CRIT_HIV=55 (pending)
                                                  ifelse(M04_HIV_EVER_MHOCCUR == 55 | M04_HIV_MHOCCUR == 55, 55,  
                                                         # if "Have you ever been diagnosed with HIV?" = 77/99 AND 
                                                         # if "Have you had any of the following issues since becoming pregnant with the current pregnancy, HIV" = 77/99, then CRIT_HIV=55 (pending)
                                                         ifelse(M04_HIV_EVER_MHOCCUR %in% c(0,99) & M04_HIV_MHOCCUR %in% c(0,99), 0, 55)))))) 
    ),
    #N. No current malaria infection (per rapid diagnostic test)
    CRIT_MALARIA = case_when(
      M06_MALARIA_POC_LBORRES == 1 ~ 0,
      M06_MALARIA_POC_LBORRES == 0 ~ 1,
      M06_MALARIA_POC_LBPERF == 0 ~ 0,
      TRUE ~ 55
    ),
    #O. No current Hepatitis B virus infection (per rapid diagnostic test)
    CRIT_HEPATITISB = ifelse(M06_HBV_POC_LBORRES == 1, 0,
                             ifelse(M06_HBV_POC_LBORRES == 0, 1,
                                    ifelse(M06_HBV_POC_LBPERF == 0, 55, 55))
    ),
    #P. No current Hepatitis C virus infection (per rapid diagnostic test)
    CRIT_HEPATITISC = ifelse(M06_HCV_POC_LBORRES == 1, 0,
                             ifelse(M06_HCV_POC_LBORRES == 0, 1,
                                    ifelse(M06_HCV_POC_LBPERF == 0, 55, 55)))
  ) 
#After enrollment, participants will be excluded from the final analysis if any of the following occur: 
#Multiple pregnancies not identified at recruitment
#Severe conditions not evident at recruitment including cancer, HIV, TB, or Malaria
#Severe pregnancy-related conditions requiring hospital admission including eclampsia or severe pre-eclampsia
save(df_criteria, file = "data_out/df_criteria.rda")





#**************************************************************************************
#*2. check eligibility and save df_healthy.rda
#**************************************************************************************
#code 666 for any not applicable by site
healthyOutcome <- df_criteria %>% 
  rowwise() %>%
  mutate(HEALTHY_CHECK = sum(across(starts_with("CRIT_"), ~ .x %in% c(1, 0, 666)), na.rm = TRUE)) %>% 
  mutate(
    HEALTHY_ELIGIBLE = case_when(
      if_all(starts_with("CRIT_"), ~.x %in% c(1, 666)) ~ 1, #eligible
      if_any(starts_with("CRIT_"), ~.x == 0) ~ 0, #Not eligible
      HEALTHY_CHECK < 19 ~ 3), #19 criterias
    #!!!!!! temp code for healthy_eligible due to small eligible sample
    HEALTHY_ELIGIBLE = case_when(
      CRIT_AGE == 1 &
       CRIT_GA == 1 & #TODO 
       # CRIT_BMI_MUAC == 1 & #TODO FF: REMOVE MUAC AND BMI FOR GWG ANALYSIS
        CRIT_HEIGHT == 1 &
        CRIT_SINGLEPREG == 1 &
        # CRIT_IRON == 1 #TODO FF: Xiaoyan has this #ed out.  I see that this is not a criteria in healthy cohort either. 
        # CRIT_INFLAM == 1 & #TODO FF: Xiaoyan had this #ed out, why? I see that if I remove this criteria, then total n goes from n=300 to n=1500+
        CRIT_BP == 1 &
       #  CRIT_LBW == 1 & #TODO: FF removed b/c don't need for GWG
       #  CRIT_STILLBIRTH == 1 & #TODO: FF removed b/c don't need for GWG
      #   CRIT_UNPL_CESARIAN == 1 & #TODO: FF removed b/c don't need for GWG
        # Hemoglobinopathies: #TODO: FF removed b/c don't need for GWG
        # CRIT_HEMOGLOBINOPATHIES == 1 & #TODO FF: Why does Xiaoyan has this #ed out. Is it b/c the nubmers are low? 
        CRIT_SMOKE == 1 & 
        CRIT_DRINK %in% c(1,666) &
        CRIT_CHRONIC == 1 &
        CRIT_HIV == 1 &
        CRIT_MALARIA == 1 & 
        CRIT_HEPATITISB == 1 & 
        CRIT_HEPATITISC == 1
      #TODO: FF NEED TO ADD:
      # No current Hepatitis C infection
      # No gestational diabetes diagnosis
      # No diagnosis of severe anemia in pregnancy
      # No preterm birth <34 weeks
      # No SGA <3rd percentile
      # No fetal death of current pregnancy
      # No chronic or gestational hypertension.
      
      ~ 1, 
      TRUE ~ 0
    ) ) %>%
  ungroup() 

df_healthy <- healthyOutcome %>% 
  filter(HEALTHY_ELIGIBLE == 1)
table(df_healthy$SITE)

# TABLES:
table(df_healthy$BASELINE_GA_WKS, useNA = "always")
table(df$healthy$CRIT_STILLBIRTH, useNA = "always")
#save data
save(healthyOutcome, file = "data_out/healthyOutcome.rda")
save(df_healthy, file = "data_out/df_healthy.rda")

#################
# BRAIN DUMP

# Next, I can take these 2051 women and merge just the IDs with the dataset that Lili has and do her analysis.