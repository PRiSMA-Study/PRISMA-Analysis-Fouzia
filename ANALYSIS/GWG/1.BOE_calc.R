# Date: June 25, 2024
# Author: Fouzia Farooq

# THIS FILE CREATES BOE VARIABLES THAT I CAN RUN EVERYTIME WITH NEW DATASET: 


library(tidyverse)
library(lubridate)
library(naniar)
library(writexl) # only for writing excel files

rm(list = ls())
dir.create("data_out")

UploadDate = "2024-06-28"

#****************************************************************************
#0. # READ FILES
#****************************************************************************
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data/Stacked Data/", UploadDate)


merged_df <- read.csv(paste0('ANALYSIS/GWG/data_out/cleaned_merged_from_uploaded_', UploadDate, ".csv"))

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


#****************************************************************************
# PROCESSING OTHER MNH FILES.
#****************************************************************************
mnh09 <- read.csv('ANALYSIS/GWG/data/Stacked Data/2024-06-28/mnh09_merged.csv')
# First deduplicate MNH09
mnh09 <- mnh09 %>%
  distinct (MOMID, PREGID, SITE, .keep_all = TRUE)


# NOTE: Although MNH09 is VISIT 6, I don't need this information for the creation of the DOB variable.  
# B/c I don't want extra rows, I am going to remove this information for now.

#****************************************************************************
# CREATE BOE
#****************************************************************************
# CREATE BOE AND EDD_BOE (BOE = just at enrollment)

# LOGIC:  
  ## only want the first ultrasound visit -- take the earliest date for each participant
  # group_by(SITE, MOMID, PREGID) %>%
  # arrange(M01_US_OHOSTDAT) %>%
  # slice(1) %>%
  # filter(M01_TYPE_VISIT == 1) %>% 
  # select a subset of variables
temp.df <- merged_df %>%
  select(MOMID, PREGID, SITE, TYPE_VISIT, M05_WEIGHT_PERES, M05_HEIGHT_PERES)

# Reset M02_SCRN_OBSSTDAT for other TYPE_VISITS. 
merged_df <- merged_df %>% 
  mutate(M02_SCRN_OBSSTDAT = if_else(TYPE_VISIT==1, M02_SCRN_OBSSTDAT, NA))

merged_df <- merged_df %>%
  mutate(M01_US_OHOSTDAT = ymd(M01_US_OHOSTDAT))

temp.df <- merged_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M01_US_OHOSTDAT, M01_US_GA_WKS_AGE_FTS1) %>%
  filter(SITE == "Zambia") %>% 
  filter(TYPE_VISIT %in% c(1,2,3,4,5))

merged_df <- merged_df %>%
mutate(M01_US_OHOSTDAT = ymd(M01_US_OHOSTDAT)) %>%
  # filter out any ultrasound visit dates that are 07-07-1907 - don't need to do this b/c it throws 
  # filter(M01_US_OHOSTDAT != ymd("1907-07-07")) %>%   
  # calculate us ga in days with reported ga in wks + days. if ga is -7 or -5, replace with NA
  mutate(GA_US_DAYS_FTS1 =  ifelse(M01_US_GA_WKS_AGE_FTS1!= -7 & M01_US_GA_DAYS_AGE_FTS1 != -7 & M01_US_GA_WKS_AGE_FTS1 != -5 & M01_US_GA_DAYS_AGE_FTS1 != -5,  (M01_US_GA_WKS_AGE_FTS1 * 7 + M01_US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(M01_US_GA_WKS_AGE_FTS2!= -7 & M01_US_GA_DAYS_AGE_FTS2 != -7 & M01_US_GA_WKS_AGE_FTS2 != -5 & M01_US_GA_WKS_AGE_FTS2 != -5,  (M01_US_GA_WKS_AGE_FTS2 * 7 + M01_US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(M01_US_GA_WKS_AGE_FTS3!= -7 & M01_US_GA_DAYS_AGE_FTS3 != -7 & M01_US_GA_WKS_AGE_FTS3 != -5 & M01_US_GA_WKS_AGE_FTS3 != -5,  (M01_US_GA_WKS_AGE_FTS3 * 7 + M01_US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(M01_US_GA_WKS_AGE_FTS4!= -7 & M01_US_GA_DAYS_AGE_FTS4 != -7 & M01_US_GA_WKS_AGE_FTS4 != -5 & M01_US_GA_WKS_AGE_FTS4 != -5,  (M01_US_GA_WKS_AGE_FTS4 * 7 + M01_US_GA_DAYS_AGE_FTS4), NA)) %>% 
  #  pull the largest GA for multiple fetuses + convert to weeks
  mutate(US_GA_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% ## where GA_US_DAYS_FTSx is the reported GA by ultrasound (added together M01_US_GA_WKS_AGE_FTSx and M01_US_GA_DAYS_AGE_FTSx to get a single estimate in days)
  mutate(US_GA_WKS = floor(US_GA_DAYS/7)) %>% 
  #  convert ga by LMP to days and wks
  # on US meaturement for all 4 babies. 
  # For LMP we just have info on one baby.
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
  
  # M01_US_OHOSTDAT, M01_US_GA_DAYS_AGE_FTS1, M01_US_GA_DAYS_AGE_FTS2, M01_US_GA_DAYS_AGE_FTS3, M01_US_GA_DAYS_AGE_FTS4, M01_US_GA_WKS_AGE_FTS1, M01_US_GA_WKS_AGE_FTS2, M01_US_GA_WKS_AGE_FTS3, M01_US_GA_WKS_AGE_FTS4, M01_GA_LMP_WEEKS_SCORRES,
# M01_GA_LMP_DAYS_SCORRES

  # generate indicator variable if LMP or US was used (where 1 = US and 2 = LMP)
  mutate(BOE_METHOD = ifelse(BOE_GA_WKS == US_GA_WKS, 1, 
                             ifelse(BOE_GA_WKS == LMP_GA_WKS, 2, 55))) %>%
  
  # generate EDD_BOE based on BOE 
  # "zero out" GA and obtain the estimated "date of conception" 
  mutate(EST_CONCEP_DATE = M01_US_OHOSTDAT - BOE_GA_DAYS) %>% 
  # add 280 days to EST_CONCEP_DATE to generate EDD based on BOE 
  mutate(EDD_BOE = EST_CONCEP_DATE + 280)


temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.

# RESET ESTIMATED CONCEPTION DATE AND EDD_BOE FOR ALL TYPE_VISITS TO NA EXCEPT VISIT 1.
merged_df <- merged_df %>%
  mutate(EST_CONCEP_DATE = case_when(
    TYPE_VISIT == 1 ~ as.Date(EST_CONCEP_DATE),
    TRUE ~ as.Date(NA)),
    
    # EDD_BOE: 
    EDD_BOE = case_when(
      TYPE_VISIT == 1 ~ as.Date(EDD_BOE),
      TRUE ~ as.Date(NA)))

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


temp.df <- merged_df %>%
  select(MOMID, PREGID, SITE, TYPE_VISIT,
         GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4,
         US_GA_DAYS, US_GA_WKS, LMP_GA_DAYS, LMP_GA_WKS, MISSING_BOTH_US_LMP, GA_DIFF_DAYS, 
         BOE_GA_WKS, BOE_GA_DAYS, BOE_METHOD, US_GA_DAYS,
         EST_CONCEP_DATE, EDD_BOE)

#****************************************************************************
# EXTRACT GA AT ENROLLMENT:
#****************************************************************************
visit1 <- merged_df %>% filter(TYPE_VISIT==1)

merged_df <- merged_df %>%
  rowwise() %>%
  mutate(DIFFDAYS = as.numeric(difftime(M02_SCRN_OBSSTDAT, M01_US_OHOSTDAT, units = "days"))) %>% # EXTRACT THE DIFFERENCE IN NUM OF DAYS B/W SCREENING US AND ENROLLMENT VISIT
  # MNH02 is being used as definitive enrollment. 
  # MNH01 visit 1 is when US is done. 
  # MNH02 is only done at VISIT=1.  M02_SCRN_OBSSTDAT is populated at other TYPE_VISIT also and that is not correct. 
  mutate(DIFFDAYS = ifelse(DIFFDAYS < 0,0,DIFFDAYS)) %>% #IF THE DIFF IS NEG, REPLACE WITH 0 (JUST USE DATE OF ULTRASOUND SCREENING)
  mutate(GA_ENROLL_WKS = floor(as.numeric((DIFFDAYS + US_GA_DAYS)/7)), #TODO Stacie had this as GA_US_DAYS
         GA_ENROLL_DAYS = floor(as.numeric(DIFFDAYS + US_GA_DAYS))) #TODO Stacie had this as GA_US_DAYS

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


temp.df <- merged_df %>% select(SITE, MOMID, PREGID, TYPE_VISIT, M01_US_OHOSTDAT, M02_SCRN_OBSSTDAT, DIFFDAYS, US_GA_DAYS, GA_ENROLL_DAYS, GA_ENROLL_WKS)
#TODO STACIE SAID SHE USED GA_US_DAYS IN THE MONITORING REPORT AND IN MATERNAL_WIDE FILE HAS US_GA_DAYS - SAME THING

# mutate(GA_ENROLL_WKS = floor(as.numeric((DIFFDAYS + GA_US_DAYS)/7)), #TODO Stacie had this as GA_US_DAYS
#        GA_ENROLL_DAYS = floor(as.numeric(DIFFDAYS + GA_US_DAYS))) #TODO Stacie had this as GA_US_DAYS

temp.df <- merged_df %>% select(SITE, MOMID, PREGID, TYPE_VISIT, M01_US_OHOSTDAT, M02_SCRN_OBSSTDAT, DIFFDAYS, US_GA_DAYS, GA_ENROLL_DAYS, GA_ENROLL_WKS)


merged_df %>% distinct (MOMID, PREGID, SITE, ENROLL, .keep_all = TRUE) %>%
  count(ENROLL)

temp.df <- merged_df %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL)

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.

#****************************************************************************
# WRITE OUT THE FILE
#****************************************************************************
write.csv(merged_df, paste0("ANALYSIS/GWG/data_out/merged_df_BOE-calc_uploaded_", UploadDate, ".csv"))

################################## END! ######################################


# I AM NOT SURE IF I WANT TO DO THIS PROCESSING AT THE MOMENT TO CREATE GA AT EACH VISIT AND DELIVERY. 
#****************************************************************************
# MNH09 to create var: DOB
#****************************************************************************

## For each participant, extract the minimum delivery time and minimum delivery date 

mnh09_subset_df <- mnh09 %>% select(SITE, MOMID, PREGID, M09_INFANTS_FAORRES, 
                                       M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTDAT_INF2, 
                                       M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTDAT_INF4,
                                       M09_DELIV_DSSTTIM_INF1, M09_DELIV_DSSTTIM_INF2, 
                                       M09_DELIV_DSSTTIM_INF3, M09_DELIV_DSSTTIM_INF4)


# replace default value date with NA 
mnh09_subset_df <- mnh09_subset_df %>% #b/c I am passing in the entire dataframe, I don't need to specify the first parameter.
  mutate_all(function(d) {
    if_else(d=="1907-07-07", NA, d)
  })

# replace default value time with NA 
mnh09_subset_df <- mnh09_subset_df %>% 
  mutate_all(function(d) {
    if_else(d=="77:77", NA, d)
  })

mnh09_subset_df <- mnh09_subset_df %>%
  # concatenate date and time of birth 
  mutate(DELIVERY_DATETIME_INF1 = paste(M09_DELIV_DSSTDAT_INF1, M09_DELIV_DSSTTIM_INF1),
         DELIVERY_DATETIME_INF2 = paste(M09_DELIV_DSSTDAT_INF2, M09_DELIV_DSSTTIM_INF2),
         DELIVERY_DATETIME_INF3 = paste(M09_DELIV_DSSTDAT_INF3, M09_DELIV_DSSTTIM_INF3),
         DELIVERY_DATETIME_INF4 = paste(M09_DELIV_DSSTDAT_INF4, M09_DELIV_DSSTTIM_INF4)) %>% 
  # assign time field type for time of birth
  mutate(DELIVERY_DATETIME_INF1 = as.POSIXct(DELIVERY_DATETIME_INF1, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF2 = as.POSIXct(DELIVERY_DATETIME_INF2, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF3 = as.POSIXct(DELIVERY_DATETIME_INF3, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF4 = as.POSIXct(DELIVERY_DATETIME_INF4, format= "%Y-%m-%d %H:%M")) %>%
  # assign minimum dob and time of birth
  mutate(DOB = # fist baby gives us DOB and time. 
           pmin(DELIVERY_DATETIME_INF1, DELIVERY_DATETIME_INF2, 
                DELIVERY_DATETIME_INF3, DELIVERY_DATETIME_INF4, na.rm = TRUE)) %>% 
  select(SITE, MOMID, PREGID, DOB)

#****************************************************************************
# MERGE MERGED_DF WITH MNH09.
#****************************************************************************
temp.df <- left_join(merged_df, mnh09_subset_df,
                       by = c("SITE", "MOMID", "PREGID"))

#****************************************************************************
# GA AT DELIVERY VISIT:
#****************************************************************************
# GA AT DELIVERY VISITs
# NEED JUST DATE FROM DOB DATETIME VARIABLE FOR CREATING GA AT DELIVERY:
temp.df <- temp.df %>% 
  mutate(DOB_DATEONLY = as.Date(DOB))

#****************************************************************************
# GA AT EACH VISIT VISIT:
#****************************************************************************
# VISIT date from the form, estimated conception date (visit date - estimated conception date = this is estimated GA at that visit in days or weeks)
# CALC. GA AT VISIT.

# XX_GA_AT_VISIT_X can have NAs
temp.df <- temp.df %>%
  mutate(M04_GA_AT_VISIT_1 = as.Date(M04_ANC_OBSSTDAT_1) - EST_CONCEP_DATE, #DATE OF VISIT - CONCEPTION DATE = GA AT VISIT. 
         M04_GA_AT_VISIT_2 = as.Date(M04_ANC_OBSSTDAT_2) - EST_CONCEP_DATE,
         M04_GA_AT_VISIT_3 = as.Date(M04_ANC_OBSSTDAT_3) - EST_CONCEP_DATE,
         M04_GA_AT_VISIT_4 = as.Date(M04_ANC_OBSSTDAT_4) - EST_CONCEP_DATE,
         M04_GA_AT_VISIT_5 = as.Date(M04_ANC_OBSSTDAT_5) - EST_CONCEP_DATE) 

temp.df <- temp.df %>%
  mutate(M06_GA_AT_VISIT_1 = as.Date(M06_DIAG_VSDAT_1) - EST_CONCEP_DATE, #DATE OF VISIT - CONCEPTION DATE = GA AT VISIT. 
         M06_GA_AT_VISIT_2 = as.Date(M06_DIAG_VSDAT_2) - EST_CONCEP_DATE,
         M06_GA_AT_VISIT_3 = as.Date(M06_DIAG_VSDAT_3) - EST_CONCEP_DATE,
         M06_GA_AT_VISIT_4 = as.Date(M06_DIAG_VSDAT_4) - EST_CONCEP_DATE,
         M06_GA_AT_VISIT_5 = as.Date(M06_DIAG_VSDAT_5) - EST_CONCEP_DATE)


temp.df <- temp.df %>%
  mutate(M08_GA_AT_VISIT_1 = as.Date(M08_LBSTDAT_1) - EST_CONCEP_DATE,
         M08_GA_AT_VISIT_2 = as.Date(M08_LBSTDAT_2) - EST_CONCEP_DATE,
         M08_GA_AT_VISIT_3 = as.Date(M08_LBSTDAT_3) - EST_CONCEP_DATE,
         M08_GA_AT_VISIT_4 = as.Date(M08_LBSTDAT_4) - EST_CONCEP_DATE,
         M08_GA_AT_VISIT_5 = as.Date(M08_LBSTDAT_5) - EST_CONCEP_DATE)

temp.df <- temp.df %>%
  select(MOMID, SITE, M04_GA_AT_VISIT_1, M04_ANC_OBSSTDAT_1, 
         EST_CONCEP_DATE, M01_US_OHOSTDAT_1, BOE_GA_DAYS)  %>% filter(SITE=="Pakistan")
