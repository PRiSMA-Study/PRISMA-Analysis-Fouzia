# Date: June 25, 2024
# Author: Fouzia Farooq

# THIS FILE CREATES BOE VARIABLES THAT I CAN RUN EVERYTIME WITH NEW DATASET: 

#****************************************************************************
#* NOTES/UPDATES:
#* Updated Singleton fetus variables 09/03/2024
#****************************************************************************

library(tidyverse)
library(lubridate)
library(naniar)
library(writexl) # only for writing excel files
library(haven)
rm(list = ls())
dir.create("data_out")

UploadDate = "2024-10-18"

#****************************************************************************
#0. # READ FILES
#****************************************************************************
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data/Stacked Data/", UploadDate)

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# NOTE: Don't need to read the files in over and over again now that I've created an RDA file.
# # Loop through each CSV file
# for (file in csv_files) {
#   # Extract the base name of the file (without the path)
#   base_name <- basename(file)
#   
#   # Extract the part of the name before the underscore
#   name_prefix <- str_split(base_name, "_")[[1]][1]
#   
#   # Read the CSV file into a data frame
#   data_frame <- read.csv(file)
#   
#   # Assign the data frame to a variable with the extracted name
#   assign(name_prefix, data_frame)
# }
# 
# # Optional: List all variables in the environment to verify
# ls()
# 
# save(mnh00, mnh01, mnh02, mnh03, mnh04, mnh05, mnh06, mnh07, mnh08, mnh09,
#             file = "ANALYSIS/GWG/data_out/mnh_dataframes_2024-06-28.rda")
     
#****************************************************************************
#*# Load in the Rda file
#****************************************************************************
# load("ANALYSIS/GWG/data_out/mnh_dataframes_2024-06-28.rda")
load("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data_out/mnh_dataframes_2024-06-28.rda")
     
#****************************************************************************
# CREATE A DATASET OF ENROLLED WOMEN
#****************************************************************************
mnh02 <- mnh02 %>% 
  mutate(ELIGIBLE = case_when(M02_AGE_IEORRES == 1 & M02_PC_IEORRES == 1 & M02_CATCHMENT_IEORRES == 1 &
                       M02_CATCH_REMAIN_IEORRES == 1  & (M02_SCRN_RETURN != 0 | is.na(M02_SCRN_RETURN)) ~ 1,
                     M02_AGE_IEORRES == 0 | M02_PC_IEORRES == 0 | M02_CATCHMENT_IEORRES == 0 |
                       M02_CATCH_REMAIN_IEORRES == 0  ~ 0,
                     TRUE ~ 99),
CONSENT = case_when(!is.na(M02_CONSENT_IEORRES) ~ M02_CONSENT_IEORRES,
                    TRUE ~ 99),
ENROLL = case_when(M02_CONSENT_IEORRES == 1 & !is.na(M02_FORMCOMPLDAT_MNH02) & ELIGIBLE == 1 ~ 1,## MAY7 UPDATES: (M02_SCRN_RETURN == 1 | is.na(M02_SCRN_RETURN) to account for particpants who returned for screening and/or sites not using the variable
                   ELIGIBLE == 0 | M02_CONSENT_IEORRES == 0 | M02_CONSENT_IEORRES == 77 ~ 0,
                   TRUE ~ 77)) %>%
  ## Assign denominators
  mutate(ENROLL_DENOM = case_when(ENROLL == 1 ~ 1,
                                  TRUE ~ 0)) 
temp.df <- mnh02 %>% 
  select(SITE, MOMID, PREGID, ENROLL)

table(mnh02$ENROLL, useNA = "always")

mnh02 %>% distinct (MOMID, PREGID, SITE, ENROLL, .keep_all = TRUE) %>%
  count(ENROLL)

mnh02_enrolled <- mnh02 %>% 
  filter(ENROLL ==1)

mnh02_enrolled %>% distinct (MOMID, PREGID, SITE, ENROLL, .keep_all = TRUE) %>%
  count(ENROLL)

mnh02_enrolled <- mnh02_enrolled %>% 
  mutate(TYPE_VISIT=1)

#****************************************************************************
# SUBSET MNH02
#****************************************************************************
mnh02_enrolled <- mnh02_enrolled %>%
  select(SITE, SCRNID, MOMID, PREGID, TYPE_VISIT, ENROLL, ENROLL_DENOM, M02_SCRN_OBSSTDAT)


#****************************************************************************
# CREATING A VECTOR OF ENROLLED WOMEN:
#****************************************************************************
# Step 1: Extract MOMIDs from MERGED_DF
SCRNID_vector <- mnh02_enrolled %>% 
  pull(SCRNID) %>% 
  unique()

# Step 2: Filter mnh05 based on MOMIDs vector
mnh01_filtered <- mnh01 %>%
  filter(SCRNID %in% SCRNID_vector)


#****************************************************************************
# SUBSET MNH01
#****************************************************************************
mnh01_filtered2 <- mnh01_filtered %>%
  select(SITE, SCRNID, MOMID, PREGID, M01_TYPE_VISIT,
         M01_US_OHOSTDAT, 
         M01_US_GA_DAYS_AGE_FTS1, M01_US_GA_DAYS_AGE_FTS2, 
         M01_US_GA_DAYS_AGE_FTS3, M01_US_GA_DAYS_AGE_FTS4,
         M01_US_GA_WKS_AGE_FTS1, M01_US_GA_WKS_AGE_FTS2, 
         M01_US_GA_WKS_AGE_FTS3, M01_US_GA_WKS_AGE_FTS4, 
         M01_GA_LMP_WEEKS_SCORRES,M01_GA_LMP_DAYS_SCORRES)

#****************************************************************************
# MERGE MNH01 ONTO MNH02 ENROLLED WOMEN:
#****************************************************************************
# MNH00 and MNH01 don't have MOMID and PREGID in Kenya and Zambia. Merge it in from MNH02. 
# THERE ARE NA'S AND THEN THERE ARE "" IN ZAMBIA MOMID AND PREGID
# Keep an eye on the SITE variable.

mnh01_filtered2 <- mnh01_filtered2 %>%
  rename(TYPE_VISIT = M01_TYPE_VISIT)

# MNH01 PREGID and MOMID have to be removed b/c Zambia MNH01 doesn't have these. 
mnh01_filtered2 <- mnh01_filtered2 %>% select(-PREGID, -MOMID)

mnh01_02 <- full_join(mnh01_filtered2, mnh02_enrolled,
                      by = c("SCRNID", "SITE", "TYPE_VISIT")) %>% 
  relocate(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL)

# Propagate MOMID to rows with matching SCRNID
mnh01_02 <- mnh01_02 %>%
  group_by(SCRNID) %>%
  arrange(TYPE_VISIT) %>% 
  mutate(MOMID = first(na.omit(MOMID)),
         PREGID = first(na.omit(PREGID))) %>%
  ungroup()


 mnh01_02 %>% distinct (MOMID, PREGID, SITE, ENROLL, .keep_all = TRUE) %>%
  count(ENROLL)

mnh01_02_distinct <- mnh01_02 %>% distinct (MOMID, PREGID, SITE, ENROLL, .keep_all = TRUE) 

temp.df <- mnh01_02_distinct %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL)

# mnh01_02 <- mnh01_02 %>%
#   mutate(ENROLL = 1)

mnh01_02 %>% distinct (MOMID, PREGID, SITE, ENROLL, .keep_all = TRUE) %>%
  count(ENROLL)

#****************************************************************************
# MERGE in MNH05:
#****************************************************************************
# CLEAN UP MNH05 FIRST AND SUBSET TO THE RIGHT MOMID BEFORE MERGING ON. 
# Step 1: Extract MOMIDs from MERGED_DF
momids_vector <- mnh02_enrolled %>% 
  pull(MOMID) %>% 
  unique()

save(momids_vector, file = 'D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data_out/enrolled_momid_vec.rda')
# save(momids_vector, file = 'ANALYSIS/GWG/data_out/enrolled_momid_vec.rda')

# Step 2: Filter mnh05 based on MOMIDs vector
mnh05_filtered <- mnh05 %>%
  filter(MOMID %in% momids_vector) %>%
  relocate(SITE, MOMID, PREGID, M05_TYPE_VISIT)

temp.df <- mnh05_filtered %>% 
  filter(SITE=="Zambia") %>% 
  filter(M05_TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, M05_TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.

mnh01_02_05 <- full_join(mnh01_02, mnh05_filtered %>% 
                          select(MOMID, PREGID, SITE, M05_TYPE_VISIT, M05_ANT_PEDAT, M05_WEIGHT_PERES, M05_WEIGHT_PEPERF, M05_HEIGHT_PERES),
                        by = c("MOMID", "PREGID", "SITE", "TYPE_VISIT" = "M05_TYPE_VISIT"))

# Zambia's data also exists!
temp.df <- mnh01_02_05 %>%
  select(MOMID, PREGID, SITE, TYPE_VISIT, M05_WEIGHT_PERES, M05_HEIGHT_PERES)

temp.df <- mnh01_02_05 %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1228 obs at visit 1.


#****************************************************************************
# CLEAN in MNH06:
#****************************************************************************
# CLEAN UP MNH06 FIRST AND SUBSET TO THE RIGHT MOMID BEFORE MERGING ON. 
# Filter mnh06 based on MOMIDs vector for enrolled women:

mnh06_filtered <- mnh06 %>%
  filter(MOMID %in% momids_vector) %>%
  relocate(SITE, MOMID, PREGID, M06_TYPE_VISIT)


#****************************************************************************
# create singleton dataframe:
#****************************************************************************
## READ IN MAT_PLACENTA_PREVIA FOR SINGLETON
singleton <- read_dta('Z:/Outcome Data/2024-06-28/MAT_PLACENTA_PREVIA.dta') %>%
  select("SITE","MOMID", "PREGID","FETUS_CT_PERES_US")

singleton_filtered <- singleton %>%
  filter(MOMID %in% momids_vector) %>%
  relocate(SITE,MOMID, PREGID, FETUS_CT_PERES_US)
#****************************************************************************
# create singleton dataframe:
#****************************************************************************
singleton_momids <- singleton_filtered %>%
  filter(FETUS_CT_PERES_US==1) %>%
  select(SITE, MOMID, PREGID)

# This was old code before I started using FETUS_CT_PERES_US
# singleton_momids <- mnh06_filtered %>%
#   filter(M06_TYPE_VISIT==1) %>%
#   filter(M06_SINGLETON_PERES==1) %>%
#   select(SITE, MOMID, PREGID)

#****************************************************************************
# MERGE the MNH01_02_05 dataset onto this singleton dataset.
#****************************************************************************
mnh01_02_05_singleton <- left_join(singleton_momids, mnh01_02_05,
                             by=c("SITE", "MOMID", "PREGID"))

mnh01_02_05_singleton %>% distinct (MOMID, PREGID, SITE, ENROLL, .keep_all = TRUE) %>%
  count(ENROLL)

temp.df <- mnh01_02_05_singleton %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


#*nrow()#****************************************************************************
# REMOVE NA ROWS ON MISSING MOMID:
#****************************************************************************
mnh01_02_05_singleton <- mnh01_02_05_singleton %>%
  filter(!is.na(MOMID))


merged_df <- mnh01_02_05_singleton %>%
  relocate(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL)

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


table(merged_df$ENROLL) # 2024-06-28 data has n=9105 women enrolled and singleton.
#****************************************************************************
# WRITE OUT THE FILE
#****************************************************************************
write.csv(merged_df, paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data_out/cleaned_merged_from_uploaded_", UploadDate, ".csv"))



######################################################
# IGNORE CODE BELOW!!!!

# #****************************************************************************
# # IDENTIFY DUPLICATES:
# #****************************************************************************
# duplicate_df <- merged_df %>% group_by(MOMID, PREGID, M01_TYPE_VISIT) %>%  filter(n()>1)
# duplicate_df2 <- duplicate_df %>% distinct(MOMID, PREGID, M01_TYPE_VISIT) # There are n=15 women who have duplicate rows based on MOMID PREGID and TYPE_VISIT.
# 
# temp.df <- merged_df %>% filter(MOMID=="ZQf432b49d-d37e-40c9-bc0c-6e2cf79bafae")
# write_xlsx(duplicate_df, paste0("data_out/duplicate momid_pregid_uploaded", UploadDate, '.xlsx'))
# # NOTE: Stacie said: Not surprising unfortunately, although 13k+ duplicates seems worrisome
# 
# #****************************************************************************
# # REMOVING DUPLICATES: TTHIS HAS MNH01, MNH02, MNH03.
# #****************************************************************************
# merged_df <- merged_df %>%
#   distinct (MOMID, PREGID, SITE, M01_TYPE_VISIT, .keep_all = TRUE)

