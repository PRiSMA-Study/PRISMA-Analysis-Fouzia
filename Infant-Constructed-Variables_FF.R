#*****************************************************************************
#* PRISMA Infant Outcomes
#* Drafted: 21 September 2023, Stacie Loisate
#* Last updated: 17 April 2023
#* Updated by FF 09/25/2023
#* 
# 1. Low birth-weight 
# 2. Pre-term birth
# 3. SGA 
#*****************************************************************************
#*****************************************************************************
#* Data Setup 
#*****************************************************************************
#rm(list = ls())
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(gmodels)
library(kableExtra)
library(lubridate)
#library(growthstandards) ## INTERGROWTH PACKAGE


# UPDATE EACH RUN # 
# set upload date 
#UploadDate = "2023-09-01"

# set path to data
#path_to_data <- paste("Z:/Stacked Data/",UploadDate, sep = "")

# set path to save 
#path_to_save <- "D:/Users/stacie.loisate/Box/PRISMA-Analysis/Infant-Constructed-Variables/"
path_to_data <-  'D:/Users/fouziafarooq/Documents/PRISMA_constructed-outcomes/data/2023-09-15/'# - for AWS data
path_to_save <- "D:/Users/fouziafarooq/Documents/PRISMA_constructed-outcomes/data_out/" # for AWS on my D:
# import forms 
mnh01 <- read_csv(paste0(path_to_data,"/", "mnh01_merged.csv")) 
mnh02 <- read_csv(paste0(path_to_data,"/", "mnh02_merged.csv")) 
mnh09 <- read_csv(paste0(path_to_data,"/", "mnh09_merged.csv")) 
mnh11 <- read_csv(paste0(path_to_data,"/", "mnh11_merged.csv")) 

## zambia does not have momid/pregid in mnh01 
mnh02_zam_ids <- mnh02 %>% filter(SITE == "Zambia") %>% select(SCRNID, MOMID, PREGID)
mnh01_zam <- mnh01 %>% filter(SITE == "Zambia", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% 
  left_join(mnh02_zam_ids, by = c("SCRNID"))
mnh01_all <- mnh01 %>% filter(SITE != "Zambia") 
mnh01 <- bind_rows(mnh01_zam, mnh01_all)

## kenya ids
mnh02_ke_ids <- mnh02 %>% filter(SITE == "Kenya") %>% select(SCRNID, MOMID, PREGID)
mnh01_ke <- mnh01 %>% filter(SITE == "Kenya", M01_TYPE_VISIT == 1)  %>% select(-MOMID, -PREGID) %>% 
  left_join(mnh02_ke_ids, by = c("SCRNID"))
mnh01_all <- mnh01 %>% filter(SITE != "Kenya") 
mnh01 <- bind_rows(mnh01_ke, mnh01_all)


#*****************************************************************************
#* PULL IDS OF PARTICIPANTS WHO ARE ENROLLED 
#*****************************************************************************

enrolled_ids <- mnh02 %>% 
  mutate(ENROLL = ifelse(M02_AGE_IEORRES == 1 & 
                           M02_PC_IEORRES == 1 & 
                           M02_CATCHMENT_IEORRES == 1 & 
                           M02_CATCH_REMAIN_IEORRES == 1 & 
                           M02_CONSENT_IEORRES == 1, 1, 0)) %>% 
  select(SITE, SCRNID, MOMID, PREGID,ENROLL, M02_AGE_IEORRES, M02_PC_IEORRES, M02_CATCHMENT_IEORRES,M02_CATCH_REMAIN_IEORRES, M02_CONSENT_IEORRES) %>% 
  filter(ENROLL == 1) %>% 
  select(SITE, MOMID, PREGID,ENROLL)

enrolled_ids_vec <- as.vector(enrolled_ids$PREGID)
## if a participant is missing an enrollment form then they will be EXCLULDED from the following analyses
#*****************************************************************************
#* Add constructed vars to forms that will be used across outcomes (09/01)
#*****************************************************************************
### MNH01 ###
## add constructed vars for: 
  # BOE_EDD, [varname: EDD_BOE]
  # BOE_GA, [varnames: BOE_GA_WKS, BOE_GA_DAYS]
  # estimate conception date [varname: EST_CONCEP_DATE]

mnh01_constructed <- mnh01 %>% 
  # only want participatns who are enrolled
  filter(PREGID %in% enrolled_ids_vec) %>% 
  ## only want the first ultrasound visit -- take the earliest date for each participant
  # group_by(SITE, MOMID, PREGID) %>%
  # arrange(M01_US_OHOSTDAT) %>%
  # slice(1) %>%
  filter(M01_TYPE_VISIT == 1) %>% 
  # select a subset of variables
  select(SITE, MOMID,PREGID,M01_TYPE_VISIT,M01_US_OHOSTDAT,contains("M09_BIRTH_DSTERM_INF"), contains("M01_US_GA_WKS_AGE_FTS"),
         contains("M01_US_GA_DAYS_AGE_FTS"), M01_GA_LMP_WEEKS_SCORRES, M01_GA_LMP_DAYS_SCORRES) %>% 
  # filter out any ultrasound visit dates that are 07-07-1907
  filter(M01_US_OHOSTDAT != ymd("1907-07-07")) %>%   
  # calculate us ga in days with reported ga in wks + days. if ga is -7 or -5, replace with NA
  mutate(GA_US_DAYS_FTS1 =  ifelse(M01_US_GA_WKS_AGE_FTS1!= -7 & M01_US_GA_DAYS_AGE_FTS1 != -7 & M01_US_GA_WKS_AGE_FTS1 != -5 & M01_US_GA_DAYS_AGE_FTS1 != -5,  (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(M01_US_GA_WKS_AGE_FTS2!= -7 & M01_US_GA_DAYS_AGE_FTS2 != -7 & M01_US_GA_WKS_AGE_FTS2 != -5 & M01_US_GA_WKS_AGE_FTS2 != -5,  (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(M01_US_GA_WKS_AGE_FTS3!= -7 & M01_US_GA_DAYS_AGE_FTS3 != -7 & M01_US_GA_WKS_AGE_FTS3 != -5 & M01_US_GA_WKS_AGE_FTS3 != -5,  (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(M01_US_GA_WKS_AGE_FTS4!= -7 & M01_US_GA_DAYS_AGE_FTS4 != -7 & M01_US_GA_WKS_AGE_FTS4 != -5 & M01_US_GA_WKS_AGE_FTS4 != -5,  (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4), NA)) %>% 
  #  pull the largest GA for multiple fetuses + convert to weeks
  mutate(US_GA_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% ## where GA_US_DAYS_FTSx is the reported GA by ultrasound (added together M01_US_GA_WKS_AGE_FTSx and M01_US_GA_DAYS_AGE_FTSx to get a single estimate in days)
  mutate(US_GA_WKS = floor(US_GA_DAYS/7)) %>% 
  #  convert ga by LMP to days and wks
  mutate(LMP_GA_DAYS =  ifelse(M01_GA_LMP_WEEKS_SCORRES != -7 & M01_GA_LMP_DAYS_SCORRES != -7,  (M01_GA_LMP_WEEKS_SCORRES * 7 + M01_GA_LMP_DAYS_SCORRES), NA)) %>% 
  mutate(LMP_GA_WKS = floor(LMP_GA_DAYS/7)) %>%
  ## generate indicator variable for missing US 
  mutate(MISSING_BOTH_US_LMP = ifelse((US_GA_WKS < 0 & LMP_GA_WKS < 0) | 
                                        (is.na(US_GA_WKS) & is.na(LMP_GA_WKS)), 1, 0)) %>% 
  #  calculate the difference in days between reported LMP and reported US
  mutate(GA_DIFF_DAYS = LMP_GA_DAYS-US_GA_DAYS) %>%
  #  obtain best obstetric estimate in weeks
  mutate(BOE_GA_WKS = case_when(LMP_GA_WKS %/% 7 < 9 ~
                                  if_else(abs(GA_DIFF_DAYS) <= 5,
                                          LMP_GA_WKS,
                                          US_GA_WKS),
                                LMP_GA_WKS %/% 7 < 16 ~
                                  if_else(abs(GA_DIFF_DAYS) <=7,
                                          LMP_GA_WKS, US_GA_WKS),
                                LMP_GA_WKS %/% 7 >= 16 ~
                                  if_else(abs(GA_DIFF_DAYS) <=10,
                                          LMP_GA_WKS, US_GA_WKS),
                                TRUE ~ US_GA_WKS)) %>%
  mutate(BOE_GA_DAYS = BOE_GA_WKS*7) %>%
  #filter(!is.na(BOE_GA_WKS)) %>% 
  # generate indicator variable if LMP or US was used (where 1 = US and 2 = LMP)
  mutate(BOE_METHOD = ifelse(BOE_GA_WKS == US_GA_WKS, 1, 
                             ifelse(BOE_GA_WKS == LMP_GA_WKS, 2, 55))) %>% 
  # generate EDD based on BOE 
  # "zero out" GA and obtain the estimated "date of conception" 
  mutate(EST_CONCEP_DATE = M01_US_OHOSTDAT - BOE_GA_DAYS) %>% 
  # add 280 days to EST_CONCEP_DATE to generate EDD based on BOE 
  mutate(EDD_BOE = EST_CONCEP_DATE + 280) 

write.csv(mnh01_constructed, paste0(path_to_save, "mnh01_constructed" ,".csv"), row.names=FALSE)

### MNH09 ###
## add constructed vars to mnh09 for:
  # GA at Birth [varname: GA_AT_BIRTH_DAYS, GA_AT_BIRTH_WKS]
  # earliest DOB for multiple fetuses [varname: DOB] 
mnh09_constructed <- mnh09 %>%
  # only want participatns who are enrolled
  filter(PREGID %in% enrolled_ids_vec) %>% 
 # select(SITE, MOMID, PREGID, contains("M09_DELIV_DSSTDAT_INF"), contains("M09_BIRTH_DSTERM_INF")) %>% 
  ## 1. Calculating GA at birth ## 
  # merge in MNH01 info
  left_join(mnh01_constructed, by = c("SITE", "MOMID", "PREGID")) %>% 
  # pull earliest date of birth 
  # first replace default value date with NA 
  mutate(M09_DELIV_DSSTDAT_INF1 = replace(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF2 = replace(M09_DELIV_DSSTDAT_INF2, M09_DELIV_DSSTDAT_INF2==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF3 = replace(M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF3==ymd("1907-07-07"), NA),
         M09_DELIV_DSSTDAT_INF4 = replace(M09_DELIV_DSSTDAT_INF4, M09_DELIV_DSSTDAT_INF4==ymd("1907-07-07"), NA)) %>% 
  mutate(DOB = 
           pmin(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF2, 
                M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF4, na.rm = TRUE)) %>% 
  # generate indicator variable for having a birth outcome
  mutate(BIRTH_OUTCOME = ifelse(M09_BIRTH_DSTERM_INF1 == 1 | M09_BIRTH_DSTERM_INF1 == 2 | 
                                  M09_BIRTH_DSTERM_INF2 == 1 | M09_BIRTH_DSTERM_INF2 == 2 | 
                                  M09_BIRTH_DSTERM_INF3 == 1 | M09_BIRTH_DSTERM_INF3 == 3 |
                                  M09_BIRTH_DSTERM_INF4 == 1 | M09_BIRTH_DSTERM_INF4 == 2, 1, 0)) %>% 
  #e only want those who have had a birth outcome 
  filter(BIRTH_OUTCOME == 1) %>% 
  # calculate the number of days between DOB and estimated conception date
  mutate(GA_AT_BIRTH_DAYS = as.numeric(DOB - EST_CONCEP_DATE), 
         GA_AT_BIRTH_WKS = floor(GA_AT_BIRTH_DAYS/7)) %>% 
  #filter(!is.na(DOB)) %>% 
  select(-names(mnh01_constructed[,-c(1:3)]))

### MNH09 - long ###
  # make data long for infant required outcomes -- pull out each infant's data and merge back together in long format
m09_INF1 <- mnh09_constructed %>% 
  rename("INFANTID" = "M09_INFANTID_INF1") %>% 
  filter(INFANTID != "n/a") %>% 
  select(-contains("_INF2"), -contains("_INF3"), -contains("_INF4")) %>% 
  rename_with(~str_remove(., '_INF1')) 

m09_INF2 <- mnh09_constructed %>% rename("INFANTID" = "M09_INFANTID_INF2") %>% 
  filter(INFANTID != "n/a") %>% 
  select(-contains("_INF1"), -contains("_INF3"), -contains("_INF4")) %>% 
  rename_with(~str_remove(., '_INF2')) 

m09_INF3 <- mnh09_constructed %>% rename("INFANTID" = "M09_INFANTID_INF3") %>% 
  filter(INFANTID != "n/a") %>% 
  select(-contains("_INF1"), -contains("_INF2"), -contains("_INF4")) %>% 
  rename_with(~str_remove(., '_INF3')) 

m09_INF4 <- mnh09_constructed %>% rename("INFANTID" = "M09_INFANTID_INF4") %>%
  filter(INFANTID != "n/a") %>% 
  select(-contains("_INF1"), -contains("_INF2"), -contains("_INF3")) %>% 
  rename_with(~str_remove(., '_INF4'))

## bind all infants together 
mnh09_long <- bind_rows(m09_INF1, m09_INF2, m09_INF3, m09_INF4) 

### MNH11 ###
## add constructed vars to mnh11 for:
# birthweight: PRISMA (BWEIGHT_PRISMA) and PRISMA + Facility (BWEIGHT_ANY)
mnh11_constructed <- mnh11 %>% 
  # only want participatns who are enrolled
  filter(PREGID %in% enrolled_ids_vec) %>% 
  mutate(BWEIGHT_PRISMA = ifelse(M11_BW_EST_FAORRES < 72 & M11_BW_EST_FAORRES >=0 & M11_BW_FAORRES > 0 & !is.na(M11_BW_FAORRES), M11_BW_FAORRES, -5),
         BWEIGHT_ANY = ifelse(BWEIGHT_PRISMA <= 0  & ## if PRISMA is missing 
                              M11_BW_FAORRES_REPORT > 0, M11_BW_FAORRES_REPORT, ## if facility is NOT missing
                       ifelse(BWEIGHT_PRISMA <= 0  & (M11_BW_EST_FAORRES > 72 | is.na(M11_BW_EST_FAORRES)), -5, M11_BW_FAORRES)))  
 # select(M11_BW_EST_FAORRES, M11_BW_FAORRES, M11_BW_FAORRES_REPORT, BWEIGHT_PRISMA, BWEIGHT_ANY)
  
#*****************************************************************************
#* 1. Low birth-weight 
  # a. PRISMA staff weight (missing if no weight taken): [varname: LBW2500_PRISMA, LBW1500_PRISMA]
  # b. PRISMA (+facility weight if PRISMA is missing): [varname:LBW2500_ANY, LBW1500_ANY]
  # c. HOLD: PRISMA staff weight adjusted for time at weighing (+facility weight if PRISMA is missing) 
#*****************************************************************************
## load in MNH11 Newborn Birth Outcome

lowbirthweight <- mnh11_constructed %>%
  ## pull key variables 
  select(SITE, M11_INF_DSTERM, BWEIGHT_PRISMA, BWEIGHT_ANY, M11_BW_FAORRES, M11_BW_FAORRES_REPORT, M11_BW_EST_FAORRES) %>% 
  ## filter livebirths 
  filter(M11_INF_DSTERM == 1) %>% 
  ## LBW PRISMA measured (bw <2500g)
  mutate(LBW2500_PRISMA = ifelse(BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 2500, 1,
                                 ifelse(BWEIGHT_PRISMA <= 0 , 55, 0))) %>% 
  ## LBW PRISMA measured (bw <1500g)
  mutate(LBW1500_PRISMA = ifelse(BWEIGHT_PRISMA >= 0 & BWEIGHT_PRISMA < 1500, 1,
                                   ifelse(BWEIGHT_PRISMA <= 0 , 55, 0))) %>% 
  ## LBW PRISMA measured (bw <2500g); varname: LBW2500_ANY
  mutate(LBW2500_ANY = ifelse(BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 2500, 1,
                              ifelse(BWEIGHT_ANY <= 0 | is.na(BWEIGHT_ANY), 55, 0))) %>% 
  ## LBW PRISMA measured (bw <1500g); varname: LBW1500_ANY 
  mutate(LBW1500_ANY = ifelse(BWEIGHT_ANY >= 0 & BWEIGHT_ANY < 1500, 1,
                              ifelse(BWEIGHT_ANY <= 0 | is.na(BWEIGHT_ANY), 55, 0))) %>% 
  ## generate indicator for missing weights 
  mutate(MISSING_PRISMA = ifelse(M11_BW_FAORRES < 0 & M11_BW_FAORRES_REPORT > 0, 1, 0), #  
         MISSING_FACILITY = ifelse(M11_BW_FAORRES_REPORT < 0 & M11_BW_FAORRES > 0, 1, 0), #  
         MISSING_BOTH = ifelse(M11_BW_FAORRES < 0  & M11_BW_FAORRES_REPORT < 0, 1, 0)) %>% # 
  
  mutate(BW_TIME = ifelse(M11_BW_EST_FAORRES < 0 | M11_BW_EST_FAORRES >= 73, NA, M11_BW_EST_FAORRES))

# lowbirthweight <- mnh11_constructed %>%
#   ## pull key variables 
#   select(SITE, M11_INF_DSTERM, M11_BW_FAORRES, M11_BW_FAORRES_REPORT, M11_BW_EST_FAORRES) %>% 
#   ## filter livebirths 
#   filter(M11_INF_DSTERM == 1) %>% 
#   ## LBW PRISMA measured (bw <2500g)
#   mutate(LBW2500_PRISMA = ifelse(M11_BW_EST_FAORRES < 72 & M11_BW_FAORRES > 0 & M11_BW_FAORRES < 2500, 1,
#                                  ifelse(M11_BW_FAORRES == -5 | M11_BW_FAORRES == -7, 55, 0))) %>% 
#   ## LBW PRISMA measured (bw <1500g)
#   mutate(LBW1500_PRISMA = ifelse(M11_BW_EST_FAORRES < 72 & M11_BW_FAORRES > 0 & M11_BW_FAORRES < 1500, 1,
#                                  ifelse(M11_BW_FAORRES == -5 | M11_BW_FAORRES == -7, 55, 0))) %>% 
#   ## LBW PRISMA measured (bw <2500g); varname: LBW2500_ANY
#     # first generate variable that shows the reported weight: if PRISMA available, this weight will be reported, if not, the facility will work
#   mutate(BW_REPORT_ANY = ifelse(M11_BW_FAORRES <= 0 & ## if PRISMA is missing 
#                                 M11_BW_FAORRES_REPORT > 0 | ## if facility is NOT missing
#                                  M11_BW_EST_FAORRES > 72 | M11_BW_EST_FAORRES == -5 | M11_BW_EST_FAORRES == -7, ## if time is > 72 hours OR is missing
#                                  M11_BW_FAORRES_REPORT, M11_BW_FAORRES)) %>% 
#   mutate(LBW2500_ANY = ifelse(M11_BW_EST_FAORRES < 72 & BW_REPORT_ANY > 0 & BW_REPORT_ANY < 2500, 1,
#                                  ifelse(BW_REPORT_ANY == -5 | BW_REPORT_ANY == -7, 55, 0))) %>% 
#   ## LBW PRISMA measured (bw <1500g); varname: LBW1500_ANY 
#   mutate(LBW1500_ANY = ifelse(M11_BW_EST_FAORRES < 72 & BW_REPORT_ANY > 0 & BW_REPORT_ANY < 1500, 1,
#                               ifelse(BW_REPORT_ANY == -5 | BW_REPORT_ANY == -7, 55, 0))) %>%
#   ## generate indicator for missing weights 
#   mutate(MISSING_PRISMA = ifelse(M11_BW_FAORRES < 0 & M11_BW_FAORRES_REPORT > 0, 1, 0), #  
#          MISSING_FACILITY = ifelse(M11_BW_FAORRES_REPORT < 0 & M11_BW_FAORRES > 0, 1, 0), #  
#          MISSING_BOTH = ifelse(M11_BW_FAORRES < 0  & M11_BW_FAORRES_REPORT < 0, 1, 0)) %>% # 
#   
#   mutate(BW_TIME = ifelse(M11_BW_EST_FAORRES < 0 | M11_BW_EST_FAORRES >= 73, NA, M11_BW_EST_FAORRES))
  



# % missing PRISMA value, missing facility value, missing both.
# + histogram of birthweight and (BW_REPORT_ANY)
# histogram of time weight is measured.

write.csv(lowbirthweight, paste0(path_to_save, "lowbirthweight" ,".csv"), row.names=FALSE)
#*****************************************************************************
#* 2. Pre-term birth 
# a. Preterm birth (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT37]
# b. Preterm birth (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT34]
# c. Preterm birth (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT32]
# d. Preterm birth (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT28]
# e. Preterm birth severity (categorical): Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMBIRTH_CAT]
#*****************************************************************************
## Forms required: MNH01 constructed, MNH09_constructed

# N(%) without both US and LMP; 
# Distribution of GA_DIFF_DAYS; 
# N% where US is used vs. where LMP is used. 
# Histograms of GA at birth

preterm_birth <- mnh01_constructed %>% 
## 1. Generate indicator variable for those who have had a birth outcome ## 
  # merge in MNH09 labor and delivery 
  left_join(mnh09_constructed, by = c("SITE", "MOMID", "PREGID")) %>% 
  # generate indicator variable for having a birth outcome
  mutate(BIRTH_OUTCOME = ifelse(M09_BIRTH_DSTERM_INF1 == 1 | M09_BIRTH_DSTERM_INF1 == 2 | 
                                  M09_BIRTH_DSTERM_INF2 == 1 | M09_BIRTH_DSTERM_INF2 == 2 | 
                                  M09_BIRTH_DSTERM_INF3 == 1 | M09_BIRTH_DSTERM_INF3 == 2 |
                                  M09_BIRTH_DSTERM_INF4 == 1 | M09_BIRTH_DSTERM_INF4 == 2, 1, 0)) %>% 
  #e only want those who have had a birth outcome 
  filter(BIRTH_OUTCOME == 1) %>% 
## 2. Generate Outcomes ## 
  # a. Preterm birth (<37 weeks): Delivery prior to 37 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT37]
  mutate(PRETERMBIRTH_LT37 = ifelse(GA_AT_BIRTH_WKS >= 20 & GA_AT_BIRTH_WKS < 37, 1, 0)) %>% 
 
  # b. Preterm birth (<34 weeks): Delivery prior to 34 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT34]
  mutate(PRETERMBIRTH_LT34 = ifelse(GA_AT_BIRTH_WKS >= 20 & GA_AT_BIRTH_WKS < 34, 1, 0)) %>% 
  
  # c. Preterm birth (<32 weeks): Delivery prior to 32 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT32]
  mutate(PRETERMBIRTH_LT32 = ifelse(GA_AT_BIRTH_WKS >= 20 & GA_AT_BIRTH_WKS < 32, 1, 0)) %>% 
  
  # d. Preterm birth (<28 weeks): Delivery prior to 28 completed weeks of gestation (live or stillbirth). [varname: PRETERMBIRTH_LT28]
  mutate(PRETERMBIRTH_LT28 = ifelse(GA_AT_BIRTH_WKS >= 20 & GA_AT_BIRTH_WKS < 28, 1, 0)) %>% 
  
  # e. Preterm birth severity (categorical): term (>37 wks), Late preterm (34 to <37 wks), early preterm (32 to <34 wks), very preterm (28 to <32 wks), extermely preterm (<28 weeks) [varname: PRETERMBIRTH_CAT]
  mutate(PRETERMBIRTH_CAT = ifelse(GA_AT_BIRTH_WKS >= 37, 11,  
                                   ifelse(GA_AT_BIRTH_WKS >= 34 & GA_AT_BIRTH_WKS < 37, 12, 
                                        ifelse(GA_AT_BIRTH_WKS >= 32 & GA_AT_BIRTH_WKS < 34, 13,
                                            ifelse(GA_AT_BIRTH_WKS >= 28 & GA_AT_BIRTH_WKS < 32, 14,
                                                 ifelse(GA_AT_BIRTH_WKS >= 20 & GA_AT_BIRTH_WKS <28, 15, 55)))))) %>% 
  # only need a subset of vars
  select(SITE, MOMID, PREGID, M01_US_OHOSTDAT,GA_DIFF_DAYS, BIRTH_OUTCOME,LMP_GA_WKS,US_GA_WKS, BOE_GA_WKS, contains("PRETERMBIRTH_"), GA_AT_BIRTH_WKS)


write.csv(preterm_birth, paste0(path_to_save, "preterm_birth" ,".csv"), row.names=FALSE)

## QUESTIONS: 
  # 1. what to do with weird data? Example: M01_visit date is default value -- would this be missing or would this be excluded 
  # 2. what about those with no birth outcome -- they are techincally not eligible so they are excluded?
  # 3. if GA is <20 wks are they missing? for the cat variable should these be 55 or excluded?
  # 4. what to do with people who have an l/d but have incorrect type visit for ultrasound 


#*****************************************************************************
#* 3. SGA
# a. Size for gestational age - categorical. [varname: SGA_CAT]
# b. Preterm small for gestational age: Preterm < 37 weeks AND SGA (<10th). [varname: INF_SGA_PRETERM]
# c. Preterm appropriate for gestational age: Preterm < 37 weeks AND not SGA (<10th). [varname: INF_AGA_PRETERM]
# d. Term small for gestational age: Term >=37 weeks AND SGA (<10th). [varname: INF_SGA_TERM]
# e. Term appropriate for gestational age: Term >=37 weeks AND not SGA (<10th). [varname: INF_AGA_TERM]
#*****************************************************************************
## Need MNH09_long and MNH11 and preterm_birth (generated above) 
## varnames: EDD_BOE (MNH09), GESTAGEBIRTH_BOE (MNH09), BIRTH_DSTERM_INF1 (MNH09), SEX_INF1-SEX_INF4 (MNH09), 
## BWEIGHT_PRISMA (MNH11), BWEIGHT_ANY (MNH11), 
## PRETERMBIRTH_LT37 (preterm_birth), PRETERMBIRTH_CAT (preterm_birth)

sga <- mnh09_long %>% 
  #select(SITE, MOMID, PREGID, GA_AT_BIRTH_DAYS, M09_INFANTS_FAORRES, INFANTID, M09_BIRTH_DSTERM, M09_SEX)  %>% 
  ## only want live births 
  filter(M09_BIRTH_DSTERM == 1) %>% 
  ## merge with mnh11 
  left_join(mnh11_constructed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>% 
  ## convert weight from grams to kg 
  mutate(BWEIGHT_PRISMA_KG = ifelse(BWEIGHT_PRISMA > 0, BWEIGHT_PRISMA/1000, -5), 
         BWEIGHT_ANY_KG =ifelse(BWEIGHT_ANY < 0 | is.na(BWEIGHT_ANY), -5, BWEIGHT_ANY/1000)) %>% 
  #select(SITE, MOMID, PREGID, INFANTID, GA_AT_BIRTH_DAYS, BWEIGHT_PRISMA_KG, BWEIGHT_ANY_KG, M09_SEX) %>% 
  ## calculate percentile - GA at birth must be between 232 and 300 days, bweight must be greater than 0 (remove default value)
  mutate(SGA = ifelse(is.na(GA_AT_BIRTH_DAYS), -5,  
                    ifelse(M09_SEX == 1  & BWEIGHT_ANY_KG > 0 & (GA_AT_BIRTH_DAYS >= 232 & GA_AT_BIRTH_DAYS <=300), floor(igb_wtkg2centile(GA_AT_BIRTH_DAYS, BWEIGHT_ANY_KG, sex = "Male")),
                        ifelse(M09_SEX == 2 & BWEIGHT_ANY_KG > 0 & (GA_AT_BIRTH_DAYS >= 232 & GA_AT_BIRTH_DAYS <=300), floor(igb_wtkg2centile(GA_AT_BIRTH_DAYS, BWEIGHT_ANY_KG, sex = "Female")),
                             ifelse(GA_AT_BIRTH_DAYS > 0 & GA_AT_BIRTH_DAYS <232 | GA_AT_BIRTH_DAYS > 300, -5,
                                           ifelse(BWEIGHT_ANY_KG <= 0, -5, -5)))))) %>% 

  # a. Size for gestational age - categorical. [varname: SGA_CAT]
  mutate(SGA_CAT = ifelse(SGA >= 0 & SGA < 3, 11, # SGA < 3rd
                          ifelse(SGA >= 0 & SGA < 10, 12, # SGA < 10rd
                                 ifelse(SGA >= 10 & SGA < 90, 13, # AGA 10to <90th 
                                        ifelse(SGA >= 90, 14, 55)))))  %>%  # LGA >= 90; 55 for missing
  select(-GA_AT_BIRTH_WKS) %>% 
  ## merge with preterm births dataset to get preterm vars 
  left_join(preterm_birth, by = c("SITE", "MOMID", "PREGID")) %>%
  # b. Preterm small for gestational age: Preterm < 37 weeks AND SGA (<10th). [varname: INF_SGA_PRETERM]
  mutate(INF_SGA_PRETERM = ifelse(PRETERMBIRTH_LT37 == 1 & SGA_CAT == 12, 1, 
                                  ifelse(SGA_CAT == 55, 55, 0))) %>% 
  # c. Preterm appropriate for gestational age: Preterm < 37 weeks AND not SGA (<10th). [varname: INF_AGA_PRETERM]
  mutate(INF_AGA_PRETERM = ifelse(PRETERMBIRTH_LT37 == 1 & (SGA_CAT == 13 | SGA_CAT == 14), 1, 0)) %>% 
  # d. Term small for gestational age: Term >=37 weeks AND SGA (<10th). [varname: INF_SGA_TERM]
  mutate(INF_SGA_TERM = ifelse(PRETERMBIRTH_CAT == 11 & SGA_CAT == 12, 1, 0)) %>% 
  # e. Term appropriate for gestational age: Term >=37 weeks AND not SGA (<10th). [varname: INF_AGA_TERM]
  mutate(INF_AGA_TERM = ifelse(PRETERMBIRTH_CAT == 11 & (SGA_CAT == 13 | SGA_CAT == 14), 1, 0))  %>% 
  #select(SITE, MOMID, PREGID, INFANTID, SGA_CAT, starts_with("INF_"))
  select(SITE, MOMID, PREGID, INFANTID, LMP_GA_WKS,US_GA_WKS, BOE_GA_WKS,  M09_BIRTH_DSTERM, M11_BW_EST_FAORRES, M11_BW_FAORRES, M11_BW_FAORRES_REPORT, BWEIGHT_PRISMA, BWEIGHT_ANY, BWEIGHT_ANY_KG,
         GA_AT_BIRTH_DAYS, GA_AT_BIRTH_WKS,
         SGA, SGA_CAT, INF_SGA_PRETERM, INF_AGA_PRETERM, INF_SGA_TERM, INF_AGA_TERM)

write.csv(sga, paste0(path_to_save, "sga" ,".csv"), row.names=FALSE)

## question: 
  # for people who are missing for the categorical -- do we also want to count them as missing for the binary?

## NAs 
# ids <- sga %>% filter(SGA_CAT==55) %>% select(SITE, MOMID, US_GA_WKS) %>% 
#   left_join(mnh02, by = c("SITE", "MOMID")) %>% 
#   select(SITE, MOMID,US_GA_WKS, M02_AGE_IEORRES, M02_PC_IEORRES, M02_CATCHMENT_IEORRES,M02_CATCH_REMAIN_IEORRES, M02_CONSENT_IEORRES) %>% 
#   left_join(mnh01, by = c("SITE", "MOMID")) %>% 
#   filter(M01_TYPE_VISIT == 1) %>% 
#   left_join(mnh11, by = c("SITE", "MOMID")) %>% 
#   select(SITE, MOMID,US_GA_WKS,M01_TYPE_VISIT, 
#          M02_AGE_IEORRES, M02_PC_IEORRES, M02_CATCHMENT_IEORRES,M02_CATCH_REMAIN_IEORRES, M02_CONSENT_IEORRES,
#          M11_BW_EST_FAORRES, M11_BW_FAORRES, M11_BW_FAORRES_REPORT,
#          M01_US_OHOSTDAT, contains("M01_US_GA_WKS_AGE_FTS"), contains("M01_US_GA_DAYS_AGE_FTS"), M01_GA_LMP_WEEKS_SCORRES)
# 
# 
# sga_out <- sga %>% filter(SGA_CAT == 55)

## reasons they are NA or excluded: 
  # 1. participant does not have US GA or lmp ga -- issue for ghana data 
  # 2. participant does not have ultrasound enrollment (type_visit = 1) -- issue for pakistan data
  # weights -- Zambia has weird things going on with the time of reporting 
  # Pakistan -- missing MOMIDs from mnh11 forms 
  # ga at birth <232 and >300

# ## EXPORT MNH11 AS SAS FILE
#  library(haven) ## export as sas files
#  mnh11_merged <- MNH11
#  mnh11_merged %>% write_sas("mnh11_merged.sas7bdat")
#  test <- read_sas("mnh11_merged.sas7bdat")



