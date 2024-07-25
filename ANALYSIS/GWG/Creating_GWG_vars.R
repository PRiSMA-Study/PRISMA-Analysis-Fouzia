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

# n = 5130 singleton (from Lynda's presentation)
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

#******************************************************************************
#* UPDATES:
#* 1. Meeting with SAS team week of July 14, 2024
#***** Keep all women for GWG analysis even with miscarriages. 
#*****   No adverse outcome should be excluded. 

#* 2. GWG is calculated for all women - even those who may not have a 
#*****  TYPE_VISIT==5 weight
#*****  Its a calculation for delivery at any GA, even those that are PTB etc.

#*3. Remove outliers: Lii said:  removed GA outside of range 0-42 (GA_WKS_FLAG==0) and 
#*****  GWG from enrollment outside -10~50kg (GWG_FROM_ENROLL_FLAG!=1).

#*4. Fix anemia variable 
#*#****************************************************************************

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

########
# Read in Imputed Data:
########
imputed_df <- read.csv(paste0(folder_path, '/GWG_ImputeObs_wk9_wk14.csv')) # This has many rows per woman

imputed_df <- imputed_df %>% 
  rename(MOMID = momid,
         WEIGHT_IMPUTED_9 = weight_imputed_9,
         WEIGHT_IMPUTED_14 = weight_imputed_14)

# Trying to remove first 2 characters from the PREGID. 
# imputed_df <- imputed_df %>%
#   mutate(PREGID = substring(PREGID, 3))

imputed_pregids <- imputed_df$PREGID
merged_pregids <- merged_df$PREGID
length(intersect(imputed_pregids, merged_pregids))


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


#* ****************************************************************************
#* Checking things:
#* ****************************************************************************
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


imputed_pregids <- imputed_df$MOMID # only n=1227 matching MOMIDs
merged_pregids <- merged_df$MOMID
length(intersect(imputed_pregids, merged_pregids))

imputed_pregids <- imputed_df$PREGID # only n=9090 matching PREGIDs
merged_pregids <- merged_df$PREGID
length(intersect(imputed_pregids, merged_pregids))

#* ****************************************************************************
# LEFT_JOIN() IMPUTED DATA FIRST: 
#* ****************************************************************************
imputed_df2 <- imputed_df  %>%
  group_by(SITE, PREGID, MOMID) %>%
  slice(1) %>%
  ungroup()

imputed_df2 <- imputed_df2 %>%
  select(-SITE, -MOMID, -SCRNID)

merged_df2 <- left_join(merged_df, imputed_df2, by ='PREGID')

temp.df <- merged_df2 %>% 
  select( MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES, GA_ENROLL_WKS, WEIGHT_IMPUTED_9, WEIGHT_IMPUTED_14)

#* ****************************************************************************
# Use dplyr to propagate EST_CONCEP_DATE within the same MOMID
#* ****************************************************************************
merged_df3 <- merged_df2 %>% 
  group_by(SITE, MOMID, PREGID) %>%
  mutate(EST_CONCEP_DATE = ifelse(is.na(EST_CONCEP_DATE), 
                                  first(EST_CONCEP_DATE[TYPE_VISIT == 1]), 
                                  EST_CONCEP_DATE))

temp.df <- merged_df3 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, EST_CONCEP_DATE, M05_WEIGHT_PERES, WEIGHT_IMPUTED_9)

#* ****************************************************************************
# Use dplyr to propagate GA_ENROLL_WKS within the same MOMID
#* ****************************************************************************
merged_df3 <- merged_df3 %>% 
  group_by(SITE, MOMID, PREGID) %>%
  mutate(GA_ENROLL_WKS = ifelse(is.na(GA_ENROLL_WKS), 
                                      first(GA_ENROLL_WKS[TYPE_VISIT == 1]), 
                                GA_ENROLL_WKS))

temp.df <- merged_df3 %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, GA_ENROLL_WKS)

#* ****************************************************************************
#* REPLACE -7 VALUES TO NA:
#* ****************************************************************************
merged_df3<- merged_df3 %>%
  mutate(M05_WEIGHT_PERES = ifelse(M05_WEIGHT_PERES==-7, NA, M05_WEIGHT_PERES),
         M05_HEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-7, NA, M05_HEIGHT_PERES),
        # M05_WEIGHT_PERES = ifelse(M05_WEIGHT_PERES==-7.00, NA, M05_WEIGHT_PERES),
         # M05_WEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-7.00, NA, M05_HEIGHT_PERES),
         M05_WEIGHT_PERES = ifelse(M05_WEIGHT_PERES==-5, NA, M05_WEIGHT_PERES),
         M05_HEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-5, NA, M05_HEIGHT_PERES))
         # M05_WEIGHT_PERES = ifelse(M05_WEIGHT_PERES==-5.00, NA, M05_WEIGHT_PERES),
         # M05_WEIGHT_PERES = ifelse(M05_HEIGHT_PERES==-5.00, NA, M05_HEIGHT_PERES))

temp.df <- merged_df3 %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.



temp.df <- merged_df3 %>% 
  filter(SITE=="Zambia") %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


merged_df3 <- merged_df3 %>%
  filter(TYPE_VISIT %in% c(1,2,3,4,5,13))

temp.df <- merged_df3 %>% 
  filter(SITE=="Zambia") %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.


temp.df <- merged_df3 %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.



merged_df3 <- merged_df3 %>%
  #DATE OF VISIT - CONCEPTION DATE = GA AT VISIT. 
  mutate(M05_GA_AT_VISIT = as.integer(floor(as.numeric(as.Date(M05_ANT_PEDAT) - as.Date(EST_CONCEP_DATE)))/7))

merged_df4 <- merged_df3
# merged_df4 <- merged_df3 %>%
#   filter(TYPE_VISIT!=14)

#* COUNTING NUMBER OF WOMEN IN THE DATASET:         
temp.df %>% distinct (MOMID, PREGID, SITE, .keep_all = TRUE) %>%
  count(MOMID) # n=9042 total women.

#* ****************************************************************************
# CREATING AN UPDATED TYPE VISIT WHERE TYPE_VISIT ==13 GETS REPLACED WITH NA FIRST.
# THEN WE FILL IT WITH THE UPDATED TYPE VISIT FOR THAT WINDOW.
#* ****************************************************************************
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
 # filter(TYPE_VISIT_UPDATED==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES, WEIGHT_IMPUTED_9) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.

#* ******************************************
#* COUNTING NUMBER OF WOMEN IN THE DATASET: 
#* ****************************************** 
distinct_count <- weight_df %>% # n=9102 women
  group_by(SITE, MOMID, PREGID) %>%
  summarise(distinct_pregid = n_distinct(PREGID)) %>%
  pull(distinct_pregid)

#* ***************************TYPE_VISIT_UPDATED#* *******************************************************


#* *******************************************************
#* CREATE INITIAL WEIGHT, INITIAL HEIGHT VARS
#* *******************************************************
# Create INITIAL_WEIGHT variable from TYPE_VISIT 1
weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(WEIGHT_ENROLL = M05_WEIGHT_PERES[TYPE_VISIT_UPDATED == 1][1],
         HEIGHT_ENROLL = M05_HEIGHT_PERES[TYPE_VISIT_UPDATED == 1][1]) %>%
  ungroup()


#* ******************************************************* 
#* # CREATE A NEW VARIABLE USING IMPUTED WEIGHT AT WEEK 9 FOR EARLY PREGNANCY 
#* *******************************************************

# Step 1: Figure out a conditional logic
weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(GA_Enroll_Condition = GA_ENROLL_WKS[TYPE_VISIT_UPDATED == 1][1]) %>%
  ungroup()

# Step 2: Apply the conditional logic
# If GA_enroll is >9 weeks, replace weight at enrollment visit with the imputed weight.
# If GA_enroll is <=9 weeks, keep the weight at enrollment visit (no need to use imputed.)
weight_df <- weight_df %>%
  mutate(
    WEIGHT_ENROLL_IMPUTED = if_else(
      GA_Enroll_Condition > 9,
      WEIGHT_IMPUTED_9,
      WEIGHT_ENROLL
    )
  ) %>%
  select(-GA_Enroll_Condition)  # Remove the temporary column

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES, GA_ENROLL_WKS, WEIGHT_ENROLL, WEIGHT_IMPUTED_9, WEIGHT_ENROLL_IMPUTED)

#############################################
# NOW LET'S JUST REPLACE THE WEIGHT_ENROLL WITH WEIGHT_ENROLL_IMPUTED SO I DON'T HAVE TO UPDATE THE CODE BELOW.
#############################################

weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(WEIGHT_ENROLL_TEMP = WEIGHT_ENROLL,
    WEIGHT_ENROLL = WEIGHT_ENROLL_IMPUTED) %>%
  ungroup

temp.df <- weight_df %>%
  select(SITE, MOMID, TYPE_VISIT_UPDATED,  GA_ENROLL_WKS, WEIGHT_ENROLL,WEIGHT_ENROLL_TEMP, WEIGHT_IMPUTED_9, WEIGHT_ENROLL_IMPUTED)


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

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_UPDATED, GA_ENROLL_WKS, WEIGHT_ENROLL, WEIGHT_ENROLL_TEMP, HEIGHT_ENROLL, BMI4CAT)

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
#* CALCULATING GA_WKS AND GA FLAGS:
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

# Floor GA weeks. 
weight_df$GA_WKS <- floor(weight_df$GA_WKS)

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_GA, TYPE_VISIT_UPDATED, GA_WKS)
#* *******************************************************
#* GWG FROM ENROLLMENT:
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
                                          TRUE ~ 0)) %>% 
  ungroup()

temp.df <- weight_df %>% select(SITE, MOMID, PREGID, TYPE_VISIT, WEIGHT_ENROLL, M05_WEIGHT_PERES, GWG_FROM_ENROLL, GWG_BW_ANC)

#* *******************************************************
#* GWG FROM ENROLLMENT AT THE END OF A PREGNANCY:
#* *******************************************************
weight_df <- weight_df %>% 
  group_by(SITE, MOMID, PREGID) %>%
  arrange(as.Date(M05_ANT_PEDAT)) %>% # Added this line to make sure we arrange first by date within each mom - since there are multiple rows per woman.
  mutate(GWG_TOTAL = ifelse(PREG_END==1,
                                        GWG_FROM_ENROLL[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)], NA), # Taking the last weight measurement at end of pregnancy.
         
         GWG_TOTAL_FLAG = case_when(is.na(GWG_TOTAL) ~ NA,
                                    GWG_TOTAL <= -10 | GWG_TOTAL > 50 ~ 1,
                                    TRUE~ 0)) %>%
  ungroup()
                                
temp.df <- weight_df %>% select(SITE, MOMID, PREGID, TYPE_VISIT, WEIGHT_ENROLL, M05_WEIGHT_PERES, GWG_FROM_ENROLL, GWG_BW_ANC, GWG_TOTAL, PREG_END)                               
                                

#* *******************************************************
#* LET'S NOW SUBSET TO WOMEN REMOVING OUTLIERS
#* *******************************************************
# From Lili: 
# filter(GA_WKS_FLAG==0 & GWG_FROM_ENROLL_FLAG != 1)

temp.df <- weight_df %>% filter(GA_WKS_FLAG==1 | GWG_FROM_ENROLL_FLAG == 1) %>%
  select(SITE, MOMID, PREGID, GWG_TOTAL, GA_WKS, GA_WKS_FLAG, GWG_FROM_ENROLL_FLAG)

weight_df <- weight_df %>% filter(GA_WKS_FLAG==0 | GWG_FROM_ENROLL_FLAG!=1)
  
#* COUNTING NUMBER OF WOMEN IN THE DATASET:         
temp.df %>% distinct (MOMID, PREGID, SITE, .keep_all = TRUE) %>%
  count(MOMID) # n=9042 total women.

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES, WEIGHT_ENROLL, GWG_FROM_ENROLL, GWG_FROM_ENROLL_FLAG, GWG_TOTAL, GWG_TOTAL_FLAG)

#* ******************************************
#* COUNTING NUMBER OF WOMEN IN THE DATASET: 
#* ****************************************** 
distinct_count <- weight_df %>% # n=9090 women
  group_by(SITE, MOMID, PREGID) %>%
  summarise(distinct_pregid = n_distinct(PREGID)) %>%
  pull(distinct_pregid)

#* ************************************************************
#* CREATE A FLAG FOR WOMEN WHO HAVE A GA<=16 WEEKS AT ENROLLMENT
#* ************************************************************
# I don't need to subset on this.  This was needed for imputing weight for Qing.
weight_df <- weight_df %>% 
  mutate(GA_LTE_16_AT_ENROLL = if_else(GA_ENROLL_WKS<=16, 1, 0))

temp.df <- weight_df %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT, GA_ENROLL_WKS, GA_LTE_16_AT_ENROLL)


#* *******************************************************
#* GWG RATE:
#* (Weight at each GA - weight at enrollment)/GA in weeks.
#* *******************************************************
temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT_UPDATED, GA_WKS, M05_WEIGHT_PERES, WEIGHT_ENROLL, GWG_FROM_ENROLL, GWG_FROM_ENROLL_FLAG, GWG_TOTAL, GWG_TOTAL_FLAG)

weight_df <- weight_df %>% 
  group_by(SITE, MOMID, PREGID) %>%
  mutate(GWG_RATE = GWG_FROM_ENROLL/GA_WKS) %>%
  ungroup()

#* *******************************************************
#* CREATING GWG AT THE LAST VISIT (PER QING)
#* *******************************************************
#* Fouzia, can you work with Lily to produce a dataset with pregnancy weights measured no later than GA week 16? 
#* Update the recommended GWG definition  and the inadequate/normal/excess variables as the following - 
#*      Recommended GWG at the last observed weight measure = ((BMI specific expected first trimester weight ÷ 13.86 weeks) × 
#*      (13.86 weeks − GA at first observed or imputed weight measure)) + 
#*      ((GA at last weight measure − 13.86 weeks) × 
#*      BMI specific recommended mean rate of GWG in the second and third trimesters)  

#* 2009 IOM RECS FOR TOTAL WEIGHT GAIN:
#* UNDERWEIGHT: 28-40 lbs = 12.5-18 kg = avg: 15.25
#* NORMAL: 25-35 lbs = 11.5 - 16 kg = avg: 13.75
#* OVERWEIGHT: 15-25 lbs = 7 - 11.5 kg = avg: 9.25
#* OBESE: 11-20 lbs = 5 - 9 kg = avg: 7
#* 
#* RECS OF RATE OF WEIGHT GAIN IN 2ND/3RD TRI.
#* UNDERWEIGHT: 0.0.44 - 0.58 kg
#* NORMAL:  0.35 - 0.50 kg
#* OVERWEIGHT: o.23 - 0.33  kg
#* OBESE: 0.17 - 0.27 kg
#* From Qing: The set of four low rate values are 0.44, 0.35, 0.23 and 0.17; the set of four high rate values are 0.58, 0.50, 0.33 and 0.27
 
# condition: Last week has to be >13 weeks do this: 
# if her outcome happened before 13.86 weeks then this formula probably doesn't apply.  but use for now and we can discuss. 
# Create a flag for negative weigth gain b/c pregnancy ended early. 
# After Qing Imputes: 
# So if first weight we have at enrollment is at <9 weeks, use it for BMI.  Otherwise use the imputed weight at 9 weeks. Qing will impute for everyone so i will have to properly join.
#SAS code impute both at week 13 and at week 9. Lili mentioned she is using Wk 9 imputed weight to calc. BMI.

# weight_df <- weight_df %>%
#   mutate(GWG_REC_LO = case_when(
#     # First piece is weight gain in the first timester.  2nd piece is weight gain in the 2nd and 3rd tri (after wk 13)
#     BMI4CAT == 0 ~ ((12.5/13.86)*(13.86-GA_ENROLL_WKS) + This is extra weight gain after 13 weeks: ((GA_WKS AT last visit(25 say in T2) - 13.86)* rec rate in T2=0.51))

weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(GWG_REC_LO = case_when(
    # First piece is weight gain in the first trimester.  2nd piece is weight gain in the 2nd and 3rd tri (after wk 13)
    # (((12.5+18)/2)/13.86) = is calc. to average weight gain.
   # The set of four low rate values are 0.44, 0.35, 0.23 and 0.17; the set of four high rate values are 0.58, 0.50, 0.33 and 0.27
   # BMI4CAT == 0 ~ ((((12.5+18)/2)/13.86)*(13.86-GA_ENROLL_WKS) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.44)), # This
    # is wrong.  12.5+18/2 is the average total weight gain.  We need to have expected first trimester weight (not weight gain total)
    BMI4CAT == 0 ~ ((2/13.86)*(13.86-GA_ENROLL_WKS)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.44),
    BMI4CAT == 1 ~ ((2/13.86)*(13.86-GA_ENROLL_WKS)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.35),
    BMI4CAT == 2 ~ ((1/13.86)*(13.86-GA_ENROLL_WKS)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.23),
    BMI4CAT == 3 ~ ((0.5/13.86)*(13.86-GA_ENROLL_WKS)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.17))) %>% 
  
  mutate(GWG_REC_LO = if_else(PREG_END==1, GWG_REC_LO, NA)) %>% 
  ungroup() 

weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(GWG_REC_HI = case_when(
    # First piece is weight gain in the first trimester.  2nd piece is weight gain in the 2nd and 3rd tri (after wk 13)
    BMI4CAT == 0 ~ ((2/13.86)*(13.86-GA_ENROLL_WKS)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.58),
    BMI4CAT == 1 ~ ((2/13.86)*(13.86-GA_ENROLL_WKS)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.50),
    BMI4CAT == 2 ~ ((1/13.86)*(13.86-GA_ENROLL_WKS)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.33),
    BMI4CAT == 3 ~ ((0.5/13.86)*(13.86-GA_ENROLL_WKS)) + ((GA_WKS[TYPE_VISIT_UPDATED==max(TYPE_VISIT_UPDATED)] - 13.86)*0.27))) %>% 
  
  mutate(GWG_REC_HI = if_else(PREG_END==1, GWG_REC_HI, NA)) %>% 
  ungroup()

weight_df$GWG_REC_LO <- round(weight_df$GWG_REC_LO, digits = 2)
weight_df$GWG_REC_HI <- round(weight_df$GWG_REC_HI, digits = 2)

temp.df <- weight_df %>% 
  select(SITE, MOMID, PREGID, TYPE_VISIT_UPDATED, BMI4CAT, GWG_REC_LO, GWG_TOTAL, GWG_REC_HI, GA_ENROLL_WKS, GA_WKS, PREG_END)


#* *******************************************************
#* IOM ADEQUACY:
#* *******************************************************

weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(IOM_ADEQUACY = case_when(
    (BMI4CAT %in% c(0,1,2,3) & GWG_TOTAL < GWG_REC_LO)  ~ 0,
    (BMI4CAT %in% c(0,1,2,3) & (GWG_TOTAL >= GWG_REC_LO & GWG_TOTAL<= GWG_REC_HI))  ~ 1,
    (BMI4CAT %in% c(0,1,2,3) & GWG_TOTAL > GWG_REC_LO)  ~ 2,
    TRUE ~ NA_real_)) %>%
  ungroup()

# B/c there are some negative _LO and _HI, and GWG_TOTAL ==0, those I will just asign as inadequate GWG based on IOM. 

weight_df <- weight_df %>%
  group_by(SITE, MOMID, PREGID) %>%
  mutate(IOM_ADEQUACY = if_else(GWG_TOTAL==0, 0, IOM_ADEQUACY))
         
temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, BMI4CAT, TYPE_VISIT, M05_WEIGHT_PERES, GA_ENROLL_WKS, GA_WKS, GWG_REC_LO, GWG_TOTAL, GWG_REC_HI, IOM_ADEQUACY, PREG_END)


# weight_df <- weight_df %>%
#   group_by(SITE, MOMID, PREGID) %>%
#   mutate(IOM_ADEQUACY = case_when(
#     (BMI4CAT == 0 & GWG_TOTAL < 12.5) |
#       (BMI4CAT == 1 & GWG_TOTAL < 11.5) |
#       (BMI4CAT == 2 & GWG_TOTAL < 7) |
#       (BMI4CAT == 3 & GWG_TOTAL < 5) ~ 0,
#     (BMI4CAT == 0 & GWG_TOTAL >= 12.5 & GWG_TOTAL <= 18) |
#       (BMI4CAT == 1 & GWG_TOTAL >= 11.5 & GWG_TOTAL <= 16) |
#       (BMI4CAT == 2 & GWG_TOTAL >= 7 & GWG_TOTAL <= 11.5) |
#       (BMI4CAT == 3 & GWG_TOTAL >= 5 & GWG_TOTAL <= 9) ~ 1,
#     (BMI4CAT == 0 & GWG_TOTAL > 18) |
#       (BMI4CAT == 1 & GWG_TOTAL > 16) |
#       (BMI4CAT == 2 & GWG_TOTAL > 11.5) |
#       (BMI4CAT == 3 & GWG_TOTAL > 9) ~ 2,
#     TRUE ~ NA_real_)) %>%
#   ungroup()


temp.df <- weight_df %>% 
  filter(SITE=="Zambia") %>% 
  filter(TYPE_VISIT==5) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, M05_WEIGHT_PERES) # Zambia has n=668 obs at visit 5 and n=1272 obs at visit 1.

temp.df <- weight_df %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES, GWG_TOTAL, GWG_BW_ANC, GWG_FROM_ENROLL, BMI4CAT, IOM_ADEQUACY, PREG_END)

weight_df_single_row <- weight_df  %>% # has n=9097 women
  group_by(SITE, PREGID, MOMID) %>%
  slice(1) %>%
  ungroup()

table(weight_df_single_row$IOM_ADEQUACY, useNA = "always") 


#* *******************************************************
#* SAVE FILE - VARIABLES THAT SAVANNAH NEEDS
#* *******************************************************
# SCRNID	MOMID	PREGID	SITE	TYPE_VISIT	M05_WEIGHT_PERES	WEIGHT_ENROLL	GWG_FROM_ENROLL	GWG_TOTAL	BMI	BMI4CAT	IOM_ADEQUACY
savannah_subset <- weight_df %>%
  select(SITE, SCRNID, MOMID, PREGID, TYPE_VISIT_UPDATED,	M05_WEIGHT_PERES,	WEIGHT_ENROLL,	GWG_FROM_ENROLL,	GWG_TOTAL,	BMI, BMI4CAT,	IOM_ADEQUACY, PREG_END)

write.csv(savannah_subset, paste0("ANALYSIS/GWG/data_out/",'GWG_OUTCOME_LONG_', UploadDate, ".csv"))

#* *******************************************************
#* NEED TO SUBSET TO WOMEN WHO HAVE HAD A PREGNANCY END.
#* *******************************************************
 weight_df2 <- weight_df %>%
   filter(PREG_END==1)

 weight_df_single_row <- weight_df2  %>% # has n=9097 women
   group_by(SITE, PREGID, MOMID) %>%
   slice(1) %>%
   ungroup()
 
 table(weight_df_single_row$IOM_ADEQUACY, useNA = "always") 
 
#* ******************************************
#* COUNTING NUMBER OF WOMEN IN THE DATASET: 
#* ****************************************** 
 distinct_count <- weight_df2 %>% # n=9102 women
   group_by(SITE, MOMID, PREGID) %>%
   summarise(distinct_pregid = n_distinct(PREGID)) %>%
   pull(distinct_pregid)
# Erin: in the 06-268-2024 dataset, n=5402 women have had a PREG_END==1 (including deaths)
 # I have n= 5294 women with in this dataset with PREG_END==1.  I know that n=74 women are dropped when I do the GWG_TOTAL and GA_WKS filter. 
 # The rest are n=34 women that I can't account for yet. 
 # Erin said there are n=104? non-singleton pregnancies.
 
 


#* ***************************************************************************
#* CREATING VARIABLES SO I CAN COUNT UP NUMBER OF WOMEN FOR DENOMINATOR CALC.
#* ***************************************************************************
# Total number of who have visit 5 data and are singleton and are enrolled.

temp.df <- weight_df2 %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES)

weight_df2 <- weight_df2 %>%
  group_by(SITE, MOMID, PREGID) %>% 
  mutate(DENOM_VISIT_5 = if_else(!is.na(M05_WEIGHT_PERES[TYPE_VISIT_UPDATED==5][1]), 1, 0)) %>%
  ungroup()

temp.df <- weight_df2 %>%
  filter(TYPE_VISIT==1) %>%
  select(SITE, MOMID, PREGID, TYPE_VISIT, TYPE_VISIT_UPDATED, M05_WEIGHT_PERES, DENOM_VISIT_5, PRETERMBIRTH_LT34, PRETERMBIRTH_LT37)

#* *******************************************************
#* WRITE OUT A FILE TO START BUILDING OUT THE REPORT:
#* *******************************************************
write.csv(weight_df2, paste0("ANALYSIS/GWG/data_out/df_w_GWGvars-n-Outcomes_uploaded_", UploadDate, ".csv"))


#****************************************************************************
#0. # READ IN THE FILE I JUST CREATED - in case I need this to check:
#****************************************************************************
UploadDate = "2024-06-28"
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA-Analysis-Fouzia/ANALYSIS/GWG/data/Stacked Data/", UploadDate)
getwd()
weight_df <- read.csv(paste0('ANALYSIS/GWG/data_out/df_w_GWGvars-n-Outcomes_uploaded_', UploadDate, '.csv'))
