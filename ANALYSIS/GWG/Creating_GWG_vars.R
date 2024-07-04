# This file will be run after "Merging_Outcomes_wBOE.R" file. 

# Date: July 02, 2024
# Author: Fouzia Farooq

#****************************************************************************
#TODO: THINGS TO DO AS OF 07/01/2024
#* Need to change all 55, 77 etc to NA before giving dataset to Lili.
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

UploadDate = "2024-06-14"

#****************************************************************************
#0. # READ FILES
#****************************************************************************
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA_ANALYSIS/GWG/data/Stacked Data/", UploadDate)

merged_df <- read.csv(paste0("data_out/merged_df_w_Outcomes_uploaded_", UploadDate, ".csv"))

mnh01 <- read.csv(paste0(folder_path, "/mnh01_merged.csv"))
mnh05 <- read.csv(paste0(folder_path, "/mnh05_merged.csv"))
mnh06 <- read.csv(paste0(folder_path, "/mnh06_merged.csv"))


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
         M05_HEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-7, NA, M05_HEIGHT_PERES))

merged_df3 <- merged_df2 %>% 
  select(MOMID, PREGID, SITE, TYPE_VISIT, PREG_END, INFANTID, 
         PRETERMBIRTH_LT34, PRETERMBIRTH_LT37, 
         LBW2500_ANY, SGA_CAT, STILLBIRTH_22WK, 
         INF_DTH_CAT, INF_ABOR_SPN,
         GA_ENROLL_WKS, GA_ENROLL_DAYS, BOE_GA_WKS, EST_CONCEP_DATE, EDD_BOE,
          M05_ANT_PEDAT, M05_WEIGHT_PERES, M05_WEIGHT_PEPERF, M05_HEIGHT_PERES)


merged_df3 <- merged_df3 %>%
  #DATE OF VISIT - CONCEPTION DATE = GA AT VISIT. 
  mutate(M05_GA_AT_VISIT = as.integer(floor(as.numeric(as.Date(M05_ANT_PEDAT) - as.Date(EST_CONCEP_DATE)))/7))

merged_df4 <- merged_df3 %>%
  filter(TYPE_VISIT!=14)

# CREATING AN UPDATED TYPE VISIT WHERE TYPE_VISIT ==13 GETS REPLACED WITH NA FIRST.
# THEN WE FILL IT WITH THE UPDATED TYPE VISIT FOR THAT WINDOW.
weight_df <- merged_df4 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, PREG_END, INFANTID, 
         PRETERMBIRTH_LT34, PRETERMBIRTH_LT37, 
         LBW2500_ANY, SGA_CAT, STILLBIRTH_22WK, 
         INF_DTH_CAT, INF_ABOR_SPN,
         EST_CONCEP_DATE, M05_ANT_PEDAT, M05_GA_AT_VISIT, M05_WEIGHT_PERES, M05_HEIGHT_PERES) %>% 
  mutate(TYPE_VISIT_UPDATED = ifelse(TYPE_VISIT==13, NA, TYPE_VISIT)) %>% 
  group_by(MOMID, PREGID, SITE) %>%
  arrange(MOMID, PREGID, SITE, as.Date(M05_ANT_PEDAT), TYPE_VISIT) %>%
  fill(TYPE_VISIT_UPDATED, .direction = "down") %>% 
  # Drop NA rows based on WEIGHT
  drop_na(M05_WEIGHT_PERES) %>% 
  # NOW doing the numbering:
  group_by(MOMID, PREGID, SITE, TYPE_VISIT_UPDATED) %>%
  filter(row_number() == 1)


#* *******************************************************
#* CREATE INITIAL WEIGHT, INITIAL HEIGHT VARS
#* *******************************************************
# Create INITIAL_WEIGHT variable from TYPE_VISIT 1
weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(INITIAL_WEIGHT = M05_WEIGHT_PERES[TYPE_VISIT == 1][1],
         HEIGHT = M05_HEIGHT_PERES[TYPE_VISIT == 1][1]) %>%
  ungroup()

#* *******************************************************
#* CREATE BMI
#* *******************************************************
#TODO Stopped here.  Next, need to calc. last visit total_gwg (BASED ON VISIT=5)
weight_df <- weight_df %>%
  mutate(INITIAL_BMI=(INITIAL_WEIGHT/(HEIGHT^2))*10000,
         BMI_4_CAT=ifelse(INITIAL_BMI<=18.5,0, #underweight
                          ifelse(INITIAL_BMI>18.5 & INITIAL_BMI<25,1, # normal weight
                                 ifelse(INITIAL_BMI>=25 & INITIAL_BMI<30,2, # overweight
                                        ifelse(INITIAL_BMI>=30,3,NA)))),# obese
         bmi4cat=factor(BMI_4_CAT,levels=c(0,1,2,3),
                        labels= c("Underweight", "Normal Weight", "Overweight", "Obesity")))


#* *******************************************************
#* LABELING GA WEEKS - CREATING VARIABLE FOR THIS:
#* *******************************************************
weight_df <- weight_df %>%
  mutate(TYPE_VISIT_GA  = case_when(TYPE_VISIT==2 ~ '20',
                                    TYPE_VISIT==3 ~ '28',
                                    TYPE_VISIT==4 ~ '32',
                                    TYPE_VISIT==5 ~ '36',
                                    TRUE ~ as.character(NA)))



#* *******************************************************
#* WRITE OUT A TEMP FILE TO START BUILDING OUT THE REPORT:
#* *******************************************************
write.csv(weight_df, paste0("data_out/df_w_GWGvars-n-Outcomes_uploaded_", UploadDate, ".csv"))


 
#TODO: STOPPED HERE - NEED TO CREATE TOTAL GWG VARIABLE. 
# This calc. will be weight at TYPE_VISIT5 - INITIAL_WEIGHT

#* *******************************************************
#* IOM ADEQUACY:
#* *******************************************************
weight_df <- weight_df %>%
  mutate(IOM_ADEQUACY = case_when(
           (bmi4cat == "Underweight" & total_gwg < 12.5) | 
             (bmi4cat == "Normal Weight" & total_gwg < 11.5) | 
             (bmi4cat == "Overweight" & total_gwg < 7) | 
             (bmi4cat == "Obesity" & total_gwg < 5) ~ 0,
           
           (bmi4cat == "Underweight" & total_gwg >= 12.5 & total_gwg <= 18) | 
             (bmi4cat == "Normal Weight" & total_gwg >= 11.5 & total_gwg <= 16) | 
             (bmi4cat == "Overweight" & total_gwg >= 7 & total_gwg <= 11.5) | 
             (bmi4cat == "Obesity" & total_gwg >= 5 & total_gwg <= 9) ~ 1,
           
           (bmi4cat == "Underweight" & total_gwg > 18) | 
             (bmi4cat == "Normal Weight" & total_gwg > 16) | 
             (bmi4cat == "Overweight" & total_gwg > 11.5) | 
             (bmi4cat == "Obesity" & total_gwg > 9) ~ 2,
           
           TRUE ~ NA_real_),
         IOM_adequacy=factor(IOM_adequacy,levels=c(0,1,2),
                             labels=c("Inadequate","Adequate","Excessive")),
         M05_ANT_PEDAT = parse_date_time (M05_ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")),
         M05_ANT_PEDAT = ymd(M05_ANT_PEDAT), # change date format to calculate GA weeks
         EST_CONCEP_DATE = as.Date(EST_CONCEP_DATE),
         GA_WEEK=as.numeric((M05_ANT_PEDAT-EST_CONCEP_DATE)/7),
         TRIMESTER=case_when(
           GA_WEEK > 0 & GA_WEEK < 14 ~ 1,
           GA_WEEK >= 14 & GA_WEEK < 28 ~ 2,
           GA_WEEK >= 28 & GA_WEEK < 43 ~ 3,
           TRUE ~ NA_real_),#trimester
         GWG_RATE=(gwg/GA_WEEK) #simply gwg rate
  ) 
