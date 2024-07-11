# This file will be run after "Merging_Outcomes_wBOE.R" file. 

# Date: July 02, 2024
# Author: Fouzia Farooq

#****************************************************************************
#TODO: THINGS TO DO AS OF 07/01/2024
#* Need to change all 55, 77 etc to NA before giving dataset to Lili.
#* #FF: Subset to enrolled women who have visit 1 and visit 5 weight data.
#*     So those who have total weight gain and have singleton preg.

#*Ask Lili: 
#*How is she accounting for the updated Type Visit? 
#*Didn't arrange when calculating lag: GWG_BW_ANC.
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


rm(list = ls())
dir.create("data_out")

UploadDate = "2024-06-28"

#****************************************************************************
#0. # READ FILES
#****************************************************************************
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data/Stacked Data/", UploadDate)

merged_df <- read.csv(paste0("ANALYSIS/GWG/data_out/merged_df_w_Outcomes_uploaded_", UploadDate, ".csv"))

mnh01 <- read.csv(paste0(folder_path, "/mnh01_merged.csv"))
mnh05 <- read.csv(paste0(folder_path, "/mnh05_merged.csv"))
mnh06 <- read.csv(paste0(folder_path, "/mnh06_merged.csv"))

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.



#* *******************************************************
#* Deduplicate data sets that need to be merged on:
#* *******************************************************
#* NOTE: MNH05 WILL HAVE LOTS OF VISIT TYPES! SO DON'T DE-DUPLICATE!!!
duplicate_df <- mnh05 %>% group_by(MOMID, PREGID, M05_TYPE_VISIT) %>%  filter(n()>1) %>% 
  filter(!(M05_TYPE_VISIT %in% c(13, 14))) # SHOWS THAT THERE ARE NO DUPLICATES!

temp.df <- merged_df %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, EST_CONCEP_DATE)

temp.df <- merged_df %>%
  select(MOMID, PREGID, SITE, TYPE_VISIT, GA_ENROLL_WKS, GA_ENROLL_DAYS, BOE_GA_WKS, EST_CONCEP_DATE, EDD_BOE)

# Trying to figure out how much data is in Zambia at visit ==1 and visit ==5 
temp.df <- mnh05 %>% 
  filter(SITE=="Zambia") %>% 
  filter(M05_TYPE_VISIT==1) %>%
  select(SITE, MOMID, PREGID, M05_TYPE_VISIT, M05_WEIGHT_PERES)

temp.df <- merged_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES)


#* *******************************************************
#* Checking things:
#* *******************************************************
# temp.df1 <- merged_df %>% 
#   filter(MOMID=='Z3-202-1740') %>%
#   select(SITE, MOMID, PREGID, TYPE_VISIT, ENROLL)
# 
# temp.df2 <- mnh05_filtered %>% 
#   filter(MOMID=='Z3-202-1740') %>%
#   select(SITE, MOMID, PREGID, M05_TYPE_VISIT)
# 
# temp.df3 <- full_join(temp.df1, temp.df2, 
#                         by = c("MOMID", "PREGID", "SITE", "TYPE_VISIT" = "M05_TYPE_VISIT"))



# Use dplyr to propagate EST_CONCEP_DATE within the same MOMID
merged_df2 <- merged_df %>% 
  group_by(SITE, MOMID, PREGID) %>%
  mutate(EST_CONCEP_DATE = ifelse(is.na(EST_CONCEP_DATE), 
                                  first(EST_CONCEP_DATE[TYPE_VISIT == 1]), 
                                  EST_CONCEP_DATE))

temp.df <- merged_df2 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, EST_CONCEP_DATE, M05_WEIGHT_PERES)


#* *******************************************************
#* REPLACE -7 VALUES TO NA:
#* *******************************************************
merged_df2 <- merged_df2 %>%
  mutate(M05_WEIGHT_PERES = ifelse(M05_WEIGHT_PERES==-7, NA, M05_WEIGHT_PERES),
         M05_HEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-7, NA, M05_HEIGHT_PERES),
         M05_WEIGHT_PERES = ifelse(M05_WEIGHT_PERES==-5, NA, M05_WEIGHT_PERES),
         M05_HEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-5, NA, M05_HEIGHT_PERES))

temp.df <- merged_df2 %>% 
  filter(SITE=="Zambia") %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.



merged_df2 <- merged_df2 %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5,13))

temp.df <- merged_df2 %>% 
  filter(SITE=="Zambia") %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


temp.df <- merged_df2 %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


merged_df3 <- merged_df2 # %>% 
  # select(MOMID, PREGID, SITE, TYPE_VISIT, PREG_END, INFANTID, 
  #        PRETERMBIRTH_LT34, PRETERMBIRTH_LT37, 
  #        LBW2500_ANY, SGA_CAT, STILLBIRTH_22WK, 
  #        INF_DTH, INF_ABOR_SPN,
  #        GA_ENROLL_WKS, GA_ENROLL_DAYS, BOE_GA_WKS, EST_CONCEP_DATE, EDD_BOE,
  #         M05_ANT_PEDAT, M05_WEIGHT_PERES, M05_WEIGHT_PEPERF, M05_HEIGHT_PERES)


merged_df3 <- merged_df3 %>%
  #DATE OF VISIT - CONCEPTION DATE = GA AT VISIT. 
  mutate(M05_GA_AT_VISIT = as.integer(floor(as.numeric(as.Date(M05_ANT_PEDAT) - as.Date(EST_CONCEP_DATE)))/7))

merged_df4 <- merged_df3
# merged_df4 <- merged_df3 %>%
#   filter(TYPE_VISIT!=14)

# CREATING AN UPDATED TYPE VISIT WHERE TYPE_VISIT ==13 GETS REPLACED WITH NA FIRST.
# THEN WE FILL IT WITH THE UPDATED TYPE VISIT FOR THAT WINDOW.
weight_df <- merged_df4 %>%
  # select(SITE, MOMID, PREGID, TYPE_VISIT, PREG_END, INFANTID, 
  #        PRETERMBIRTH_LT34, PRETERMBIRTH_LT37, 
  #        LBW2500_ANY, SGA_CAT, STILLBIRTH_22WK, 
  #        INF_DTH, INF_ABOR_SPN,
  #        EST_CONCEP_DATE, M05_ANT_PEDAT, M05_GA_AT_VISIT, M05_WEIGHT_PERES, M05_HEIGHT_PERES) %>% 
  mutate(TYPE_VISIT_UPDATED = ifelse(TYPE_VISIT==13, NA, TYPE_VISIT)) %>% 
  group_by(MOMID, PREGID, SITE) %>%
  arrange(MOMID, PREGID, SITE, as.Date(M05_ANT_PEDAT), TYPE_VISIT) %>%
  fill(TYPE_VISIT_UPDATED, .direction = "down") %>% 
  # Drop NA rows based on WEIGHT
  drop_na(M05_WEIGHT_PERES) %>% 
  # NOW doing the numbering:
  group_by(MOMID, PREGID, SITE, TYPE_VISIT_UPDATED) %>%
  filter(row_number() == 1)

temp.df <- weight_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT_UPDATED==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.



#* ***************************TYPE_VISIT_UPDATED#* *******************************************************
#* CREATE INITIAL WEIGHT, INITIAL HEIGHT VARS
#* *******************************************************
# Create INITIAL_WEIGHT variable from TYPE_VISIT 1
weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(WEIGHT_ENROLL = M05_WEIGHT_PERES[TYPE_VISIT_UPDATED == 1][1],
         HEIGHT_ENROLL = M05_HEIGHT_PERES[TYPE_VISIT_UPDATED == 1][1]) %>%
  ungroup()

#* *******************************************************
#* CREATE INITIAL BMI AND BMI CATEGORIES:
#* *******************************************************
#TODO Stopped here.  Next, need to calc. last visit total_gwg (BASED ON VISIT=5)
weight_df <- weight_df %>%
  mutate(BMI=(WEIGHT_ENROLL/(HEIGHT_ENROLL^2))*10000,
         
         BMI4CAT=ifelse(BMI<=18.5,0, #underweight
                          ifelse(BMI>18.5 & BMI<25,1, # normal weight
                                 ifelse(BMI>=25 & BMI<30,2, # overweight
                                        ifelse(BMI >= 30 & BMI < 50, 3, NA)))),# obese
         bmi4cat=factor(BMI4CAT,levels=c(0,1,2,3),
                        labels= c("Underweight", "Normal Weight", "Overweight", "Obesity")))


#* *******************************************************
#* BMI FLAG:
#* *******************************************************
weight_df <- weight_df %>%
  mutate(BMI_FLAG = case_when(BMI <= 5 | BMI >=50 | BMI =='NA' ~ 1,
                       BMI > 5 & BMI < 50 ~ 0,
                       TRUE ~ NA_real_))


#* *******************************************************
#* LABELING GA WEEKS - CREATING VARIABLE FOR THIS:
#* *******************************************************
weight_df <- weight_df %>%
  mutate(TYPE_VISIT_GA  = case_when(TYPE_VISIT_UPDATED == 1 ~ '<20',
                                    TYPE_VISIT_UPDATED == 2 ~ '20',
                                    TYPE_VISIT_UPDATED ==3 ~ '28',
                                    TYPE_VISIT_UPDATED ==4 ~ '32',
                                    TYPE_VISIT_UPDATED ==5 ~ '36',
                                    TRUE ~ as.character(NA)))


temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_GA, TYPE_VISIT_UPDATED)
#* *******************************************************
#* GA FLAGS:
#* *******************************************************
weight_df <- weight_df %>% 
  group_by(SITE, MOMID, PREGID) %>%
  mutate(GA_DAYS = as.numeric(difftime(as.Date(M05_ANT_PEDAT), as.Date(EST_CONCEP_DATE), units = "days")),
         GA_WKS = as.numeric(difftime(as.Date(M05_ANT_PEDAT), as.Date(EST_CONCEP_DATE), units = "weeks")),
         
         GA_WKS_FLAG = case_when(GA_WKS>=0 & GA_WKS <= 42 ~ 0,
                                 GA_WKS < 0 | GA_WKS > 42 ~ 1,
                                 TRUE ~ NA_real_),
         
         GA_TRIM=case_when(GA_DAYS>=0 & GA_DAYS<=97 ~ 1,
                           GA_DAYS>=98 & GA_DAYS<=195 ~ 2,
                           GA_DAYS>=196 & GA_DAYS<=294 ~ 3,
                           TRUE ~ NA_real_)) %>%
  ungroup()


#* *******************************************************
#* GWG AT VISIT 5 FROM ENROLLMENT:
#* *******************************************************
#* #TODO NOTE: FF updated from >=30 for GWG_FROM_ENROLL to >50. IT is possible for a woman to gain 50kg. 
weight_df <- weight_df %>% 
  group_by(SITE, MOMID, PREGID) %>%
  arrange(as.Date(M05_ANT_PEDAT)) %>% # Added this line to make sure we arrange first by date within each mom - since ther are multiple rows per woman.
  mutate(GWG_FROM_ENROLL = M05_WEIGHT_PERES - WEIGHT_ENROLL,
         GWG_FROM_ENROLL_FLAG = case_when(is.na(GWG_FROM_ENROLL) ~ NA,
                                 GWG_FROM_ENROLL <= -10 | GWG_FROM_ENROLL >50 ~ 1,
                                 TRUE~ 0),
         # This current visit weight minus the previous visit. If there is no previous visit, it uses the first value (so value at enrollment will be 0)
         GWG_BW_ANC = M05_WEIGHT_PERES - lag(M05_WEIGHT_PERES, default = first(M05_WEIGHT_PERES)),
         
         GWG_FROM_ENROLL = M05_WEIGHT_PERES - WEIGHT_ENROLL,
         
         GWG_FROM_ENROLL_FLAG = case_when(is.na(GWG_FROM_ENROLL) ~ as.numeric(NA),
                                          GWG_FROM_ENROLL <= -10 | GWG_FROM_ENROLL > 50 ~ 1,
                                          TRUE~ 0),
         
         GWG_TOTAL = ifelse(any(PREG_END==1 & TYPE_VISIT_UPDATED == 5), 
                            M05_WEIGHT_PERES[TYPE_VISIT_UPDATED == 5][1] - WEIGHT_ENROLL, NA),
         
         GWG_TOTAL_FLAG = case_when(is.na(GWG_TOTAL)~NA,
                           GWG_TOTAL<= -10 | GWG_TOTAL > 50 ~ 1,
                           TRUE~ 0) )%>%
  ungroup()

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES, WEIGHT_ENROLL, GWG_FROM_ENROLL, GWG_FROM_ENROLL_FLAG, GWG_TOTAL, GWG_TOTAL_FLAG)

#* *******************************************************
#* GWG RATE:
#* (Weight at each GA - weight at enrollment)/GA in weeks.
#* *******************************************************
weight_df <- weight_df %>% 
  group_by(SITE, MOMID, PREGID) %>%
  mutate(GWG_RATE = GWG_FROM_ENROLL/GA_WKS) %>%
  ungroup()

#* *******************************************************
#* IOM ADEQUACY:
#* *******************************************************
weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(IOM_ADEQUACY = case_when(
    (BMI4CAT == 0 & GWG_TOTAL < 12.5) |
      (BMI4CAT == 1 & GWG_TOTAL < 11.5) |
      (BMI4CAT == 2 & GWG_TOTAL < 7) |
      (BMI4CAT == 3 & GWG_TOTAL < 5) ~ 0,
    (BMI4CAT == 0 & GWG_TOTAL >= 12.5 & GWG_TOTAL <= 18) |
      (BMI4CAT == 1 & GWG_TOTAL >= 11.5 & GWG_TOTAL <= 16) |
      (BMI4CAT == 2 & GWG_TOTAL >= 7 & GWG_TOTAL <= 11.5) |
      (BMI4CAT == 3 & GWG_TOTAL >= 5 & GWG_TOTAL <= 9) ~ 1,
    (BMI4CAT == 0 & GWG_TOTAL > 18) |
      (BMI4CAT == 1 & GWG_TOTAL > 16) |
      (BMI4CAT == 2 & GWG_TOTAL > 11.5) |
      (BMI4CAT == 3 & GWG_TOTAL > 9) ~ 2,
    TRUE ~ NA_real_)) %>%
  ungroup()

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, BMI4CAT, TYPE_VISIT, M05_WEIGHT_PERES, GWG_TOTAL, IOM_ADEQUACY)

temp.df <- weight_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES, GWG_TOTAL, GWG_BW_ANC, GWG_FROM_ENROLL)

#* ***************************************************************************
#* CREATING VARIABLES SO I CAN COUNT UP NUMBER OF WOMEN FOR DENOMINATOR CALC.
#* ***************************************************************************
# Total number of who have visit 5 data and are singleton and are enrolled.

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES)

weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(DENOM_VISIT_5 = if_else(!is.na(M05_WEIGHT_PERES[TYPE_VISIT_UPDATED==5][1]), 1, 0)) %>%
  ungroup()

temp.df <- weight_df %>%
  filter(TYPE_VISIT==1) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES, DENOM_VISIT_5, PRETERMBIRTH_LT34, PRETERMBIRTH_LT37)

#* *******************************************************
#* WRITE OUT A FILE TO START BUILDING OUT THE REPORT:
#* *******************************************************
write.csv(weight_df, paste0("ANALYSIS/GWG/data_out/df_w_GWGvars-n-Outcomes_uploaded_", UploadDate, ".csv"))


#****************************************************************************
#0. # READ IN THE FILE I JUST CREATED - in case I need this to check:
#****************************************************************************
UploadDate = "2024-06-28"
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data/Stacked Data/", UploadDate)
getwd()
weight_df <- read.csv(paste0('ANALYSIS/GWG/data_out/df_w_GWGvars-n-Outcomes_uploaded_', UploadDate, '.csv')
