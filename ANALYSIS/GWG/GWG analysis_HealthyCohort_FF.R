# Date: June 20 2024
# Author: Fouzia Farooq - adapted from Lili's GWG.
# Updated: June 26, 2024

#NOTE: This is the file where I have some of the code from Lili trying to merge in with the healthy cohort dataset.
#NOTE: This is the file i was working on after BOE_Calc.R file and Healthy Cohort.R and was trying to merge in healthy cohort info with what Lili had given me for GWG.
#TODO:  I need to calculate GA at each visit based on visit date. When weight is missing for a TYPE_VISIT!=14, i want it to take that info from TYPE_VISIT==13.
rm(list = ls())

library(lubridate)
library(gridExtra)
library(grid)
library(dplyr)
library(base)
library(tidyr)
library(ggplot2)
library(readr)
library(wesanderson)

# Upload date
UploadDate <- "2024-06-14"
# Define the path to the folder containing the CSV files
folder_path <- paste0("D:/Users/fouziafarooq/Documents/PRISMA_ANALYSIS/GWG/data/Stacked Data/", UploadDate)

mnh01 <- read.csv(paste0(folder_path, "/mnh01_merged.csv"))
mnh05 <- read.csv(paste0(folder_path, "/mnh05_merged.csv"))
mnh06 <- read.csv(paste0(folder_path, "/mnh06_merged.csv"))

# READ IN HEALTHY COHORT DATASET:
load(file = 'data_out/df_healthy.rda')
load(file = "data_out/healthyOutcome.rda")
#* *******************************************************
#* Subsetting dataframes:
#* *******************************************************
mnh06_subset <- mnh06 %>% select(SITE, MOMID, PREGID, M06_TYPE_VISIT, M06_SINGLETON_PERES)

#* *******************************************************
#* Deduplicate data sets that need to be merged on:
#* *******************************************************
duplicate_df <- mnh05 %>% group_by(MOMID, PREGID) %>%  filter(n()>1)
duplicate_df2 <- duplicate_df %>% distinct(MOMID, PREGID)
mnh05 <- mnh05 %>% 
  distinct(MOMID, PREGID, M05_TYPE_VISIT, .keep_all = TRUE)


#* *******************************************************
#* Merge data frames onto healthy cohort dataset.
#* *******************************************************
## Merge MNH05 on to the enrolled women: 
enrolled_mnh05_df <- left_join(healthyOutcome %>% 
                                  select(SITE, MOMID, PREGID, HEALTHY_ELIGIBLE, EST_CONCEP_DATE, EDD_BOE, ENROLLED_FF),
                                mnh05 %>% 
                                 select(SITE, MOMID, PREGID,  M05_TYPE_VISIT, M05_WEIGHT_PERES, M05_WEIGHT_PEPERF, M05_HEIGHT_PERES),
                               by=c("SITE", 'MOMID', 'PREGID'))
temp.df <- enrolled_mnh05_df %>% select(SITE, MOMID, PREGID, M05_TYPE_VISIT, M05_WEIGHT_PERES, M05_WEIGHT_PEPERF)


dups <- mnh05 %>% group_by(MOMID, PREGID, M05_TYPE_VISIT) %>%  filter(n()>1)

## Identify missing initial mom weight on MNH05 and save ID to test2
miss_weight <- enrolled_mnh05_df %>%
  group_by(MOMID, PREGID) %>%
  filter(!M05_WEIGHT_PEPERF==1 & M05_TYPE_VISIT==1)

## To add a 'dup' column and 'missing initial weight' column so that can filter out non-dup and non-missing later
enrolled_mnh05_df <- enrolled_mnh05_df %>%
  mutate(miss01w=ifelse(MOMID %in% miss_weight$MOMID,1,0)) 


## Only include ANC visit 20-36 or unplanned ANC visit and exclude the following: duplicates and missing initial weights;
enrolled_mnh05_df <- enrolled_mnh05_df %>%
  filter((M05_TYPE_VISIT %in% c(1:5,13)) & miss01w!=1) %>% #Visit 1:5 or 13 included.
  mutate(M05_WEIGHT_PERES=ifelse(M05_WEIGHT_PERES==-7, NA, M05_WEIGHT_PERES))

# Clcualting initial weight and taking height at enrollment
initial_stats_df <- enrolled_mnh05_df %>% filter(M05_TYPE_VISIT == 1) %>%
  select(SITE, MOMID, PREGID,  M05_WEIGHT_PERES, M05_HEIGHT_PERES) %>% rename(initial_weight =  M05_WEIGHT_PERES,
                                                                              height = M05_HEIGHT_PERES)

temp.df <- enrolled_mnh05_df %>% select(SITE, MOMID, PREGID, M05_TYPE_VISIT, M05_WEIGHT_PERES, miss01w)

# Join it back on: 
enrolled_mnh05_df <- left_join(enrolled_mnh05_df, initial_stats_df,
                     by = c("SITE", "MOMID", "PREGID"))

# Calculate Weight gain: 
enrolled_mnh05_df <- enrolled_mnh05_df %>% 
  mutate(gwg = M05_WEIGHT_PERES - initial_weight)
  
  
temp.df <- enrolled_mnh05_df %>% select(SITE, MOMID, PREGID, M05_TYPE_VISIT, M05_WEIGHT_PERES, initial_weight, height)

#TODO Stopped here.  Next, need to calc. last visit total_gwg (it's not necessarily at TYPE_VISIT==5)
## Calculate BMI for each participant using MNH05(data) and create a new variable for ANC visit
enrolled_mnh05_df <- enrolled_mnh05_df %>%
  mutate(bmi=(initial_weight/(height^2))*10000,
         bmi4cat=ifelse(bmi<=18.5,0, #underweight
                        ifelse(bmi>18.5 & bmi<25,1, # normal weight
                               ifelse(bmi>=25 & bmi<30,2, # overweight
                                      ifelse(bmi>=30,3,NA)))),# obese
         bmi4cat=factor(bmi4cat,levels=c(0,1,2,3),
                        labels= c("Underweight", "Normal Weight", "Overweight", "Obesity")),
         type_visit_GA  = case_when(M05_TYPE_VISIT==2 ~ '20',
                                    M05_TYPE_VISIT==3 ~ '28',
                                    M05_TYPE_VISIT==4 ~ '32',
                                    M05_TYPE_VISIT==5 ~ '36',
                                    TRUE ~ as.character(NA)),
         IOM_adequacy = case_when(
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
  ) # calculate GA weeks

temp.df <- enrolled_mnh05_df %>% select(SITE, MOMID, PREGID, )
#enrolled_mnh05_df$GA_WEEK <- as.numeric(enrolled_mnh05_df$GA_WEEK) # removes 'days' from the values in GA_WEEKS. 

# Step 5
## Generate graphs
ggp<-ggplot(enrolled_mnh05_df, aes(x = GA_WEEK, y = gwg) ) + 
  geom_point()  +
 # geom_smooth(method = "loess", 
  #            formula = y ~ x) +
  facet_wrap(~SITE,scales="free")
#  xlim(15,50)#one person has GA_week 58 but no gwg
ggp

ggp_lim<-ggplot(enrolled_mnh05_df, aes(GA_WEEK, gwg) ) + 
  geom_point()  +
  geom_smooth(method = "loess", 
              formula = y ~ x) +
  facet_wrap(~SITE,scales="free") +
  xlim(0,42) +
  ylim(-5,20)
ggp_lim


ggp2<-ggplot(enrolled_mnh05_df, aes(GA_WEEK, gwg) ) + 
  geom_point(color = "red3",alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", 
              formula = y ~ x) +
  facet_grid(SITE ~ bmi4cat, scales = "free_y") +
  labs(x = "Gestational week",
       y = "Gestational Weight Gain")+
  theme(strip.background=element_rect(fill="white"),
        strip.text.y = element_blank()) 
ggp2

ggp3<-ggplot(enrolled_mnh05_df, aes(GA_WEEK, gwg) ) + 
  geom_point(color = "red3",alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", 
              formula = y ~ x) +
  facet_grid(SITE ~ bmi4cat, scales = "free_y") +
  labs(x = "Gestational week",
       y = "Gestational Weight Gain")+
  theme(strip.background=element_rect(fill="white"),
        strip.text.y = element_text()) +
  xlim(0,42) +
  ylim(-5,20)
ggp3

plot <- ggplot(enrolled_mnh05_df, aes(x=GA_WEEK, y=gwg)) + 
  geom_point(aes(color=bmi4cat), alpha=0.6) +  # Points colored by BMI Group
  geom_smooth(method="lm", aes(color=bmi4cat), se=FALSE) +  # Linear regression line, no confidence band
  facet_grid(SITE ~ bmi4cat) +  # Creating facets for each combination of Site and BMI Group
  labs(title="GWG across GA by BMI Group and Site",
       x="Gestational Age (weeks)",
       y="Gestational Weight Gain (kg)",
       color="BMI Group") +
  theme_minimal()
plot


# STEP 6: Check distribution
## total GWG (kg)
gwg_total <-enrolled_mnh05_df %>%
  filter(!M05_TYPE_VISIT==13) %>%
  ggplot(aes(x = gwg)) +
  geom_histogram(aes(y=..density..),
                 fill = "white", color = "black",lwd=0.5,
                 alpha=0.5, position="identity", binwidth = 0.5) +
  geom_density(lwd=0.6, colour="turquoise3", fill="turquoise1", alpha=0.16) +
  scale_x_continuous(breaks=seq(4,16,1))+
  xlab("Total gestational weight gain (kg)") +
  ylab("Density")+
  facet_grid(rows = vars(SITE), scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) 
gwg_total

gwg_total_lim <-enrolled_mnh05_df %>%
  filter(!M05_TYPE_VISIT==13) %>%
  ggplot(aes(x = gwg)) +
  geom_histogram(aes(y=..density..),
                 fill = "white", color = "black",lwd=0.5,
                 alpha=0.5, position="identity", binwidth = 0.5) +
  geom_density(lwd=0.6, colour="turquoise3", fill="turquoise1", alpha=0.16) +
  scale_x_continuous(breaks=seq(4,16,1))+
  xlab("Total gestational weight gain (kg)") +
  ylab("Density")+
  facet_grid(rows = vars(SITE), scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) +
  xlim(-5,20)
gwg_total_lim

## GWG rate (kg/rate)
gwg_rate <-enrolled_mnh05_df %>%
  filter(!M05_TYPE_VISIT==13) %>%
  ggplot(aes(x = GWG_RATE)) +
  geom_histogram(aes(y=..density..),
                 fill = "white", color = "black",lwd=0.5,
                 alpha=0.5, position="identity", binwidth = 0.5) +
  geom_density(lwd=0.6, colour="turquoise3", fill="turquoise1", alpha=0.16) +
  scale_x_continuous(breaks=seq(4,16,1))+
  xlab("Gestational weight gain rate (kg/week)") +
  ylab("Density")+
  facet_grid(rows = vars(SITE), scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) 
gwg_rate

gwg_rate_lim <-enrolled_mnh05_df %>%
  filter(!M05_TYPE_VISIT==13) %>%
  ggplot(aes(x = gwg)) +
  geom_histogram(aes(y=..density..),
                 fill = "white", color = "black",lwd=0.5,
                 alpha=0.5, position="identity", binwidth = 0.5) +
  geom_density(lwd=0.6, colour="turquoise3", fill="turquoise1", alpha=0.16) +
  scale_x_continuous(breaks=seq(4,16,1))+
  xlab("Total gestational weight gain (kg)") +
  ylab("Density")+
  facet_grid(rows = vars(SITE), scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) +
  xlim(-6,5)
gwg_rate_lim


## gwg by trimester
gwg_tri <-enrolled_mnh05_df %>% 
  filter(!M05_TYPE_VISIT==13) %>%
  mutate(
    TRIMES = case_when(
      TRIMESTER == 1 ~ "Trimester 1",
      TRIMESTER == 2 ~ "Trimester 2",
      TRIMESTER == 3 ~ "Trimester 3",
    )) %>% 
  filter(!is.na(TRIMES)) %>% 
  ggplot(aes(x = gwg, fill = ..x..)) +
  geom_histogram(color = "white", size = 0, 
                 alpha=1, position="identity", binwidth = 0.5) +
  scale_x_continuous(breaks=seq(5,18,1))+
  xlab("Gestational weight gain (kg)") +
  ylab("Frequency")+
  facet_grid(SITE ~ TRIMES, scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill="white"),
        strip.text = element_text(size = 7), 
        panel.spacing = unit(0.3, "lines"), 
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.position="right", 
        legend.title = element_text(size = 7),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 6)) +
  scale_fill_gradientn(colours = wes_palette("FantasticFox1", 10, type = "continuous"),
                       name = "GWG value")
gwg_tri

gwg_tri_lim <-enrolled_mnh05_df %>% 
  filter(!M05_TYPE_VISIT==13) %>%
  mutate(
    TRIMES = case_when(
      TRIMESTER == 1 ~ "Trimester 1",
      TRIMESTER == 2 ~ "Trimester 2",
      TRIMESTER == 3 ~ "Trimester 3",
    )) %>% 
  filter(!is.na(TRIMES)) %>% 
  ggplot(aes(x = gwg, fill = ..x..)) +
  geom_histogram(color = "white", size = 0, 
                 alpha=1, position="identity", binwidth = 0.5) +
  scale_x_continuous(breaks=seq(5,18,1))+
  xlab("Gestational weight gain (kg)") +
  ylab("Frequency")+
  facet_grid(SITE ~ TRIMES, scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill="white"),
        strip.text = element_text(size = 7), 
        panel.spacing = unit(0.3, "lines"), 
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.position="right", 
        legend.title = element_text(size = 7),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 6)) +
  scale_fill_gradientn(colours = wes_palette("FantasticFox1", 10, type = "continuous"),
                       name = "GWG value") +
  xlim(-5,20)
gwg_tri_lim


# ## gwg rate by trimester
# gwg_rate_tri <-enrolled_mnh05_df %>% 
#   filter(!M05_TYPE_VISIT==13) %>%
#   mutate(
#     TRIMES = case_when(
#       TRIMESTER == 1 ~ "Trimester 1",
#       TRIMESTER == 2 ~ "Trimester 2",
#       TRIMESTER == 3 ~ "Trimester 3",
#     )) %>% 
#   filter(!is.na(TRIMES)) %>% 
#   ggplot(aes(x = GWG_RATE, fill = ..x..)) +
#   geom_histogram(color = "white", size = 0, 
#                  alpha=1, position="identity", binwidth = 0.1) +
#   scale_x_continuous(breaks=seq(5,18,1))+
#   xlab("Gestational weight gain rate (kg/week)") +
#   ylab("Frequency")+
#   facet_grid(SITE ~ TRIMES, scales="free_y") +
#   theme_bw() +
#   theme(strip.background=element_rect(fill="white"),
#         strip.text = element_text(size = 7), 
#         panel.spacing = unit(0.3, "lines"), 
#         axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
#         legend.position="right", 
#         legend.title = element_text(size = 7),
#         panel.grid.minor = element_blank(),
#         axis.text = element_text(size = 6), 
#         axis.title = element_text(size = 8),
#         legend.key.size = unit(0.3, "cm"),
#         legend.text = element_text(size = 6)) +
#   scale_fill_gradientn(colours = wes_palette("FantasticFox1", 10, type = "continuous"),
#                        name = "GWG rate value")
# gwg_rate_tri
# 
# gwg_rate_tri_lim <-enrolled_mnh05_df %>% 
#   filter(!M05_TYPE_VISIT==13) %>%
#   mutate(
#     TRIMES = case_when(
#       TRIMESTER == 1 ~ "Trimester 1",
#       TRIMESTER == 2 ~ "Trimester 2",
#       TRIMESTER == 3 ~ "Trimester 3",
#     )) %>% 
#   filter(!is.na(TRIMES)) %>% 
#   ggplot(aes(x = GWG_RATE, fill = ..x..)) +
#   geom_histogram(color = "white", size = 0, 
#                  alpha=1, position="identity", binwidth = 0.1) +
#   scale_x_continuous(breaks=seq(5,18,1))+
#   xlab("Gestational weight gain rate (kg/week)") +
#   ylab("Frequency")+
#   facet_grid(SITE ~ TRIMES, scales="free_y") +
#   theme_bw() +
#   theme(strip.background=element_rect(fill="white"),
#         strip.text = element_text(size = 7), 
#         panel.spacing = unit(0.3, "lines"), 
#         axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
#         legend.position="right", 
#         legend.title = element_text(size = 7),
#         panel.grid.minor = element_blank(),
#         axis.text = element_text(size = 6), 
#         axis.title = element_text(size = 8),
#         legend.key.size = unit(0.3, "cm"),
#         legend.text = element_text(size = 6)) +
#   scale_fill_gradientn(colours = wes_palette("FantasticFox1", 10, type = "continuous"),
#                        name = "GWG ratae value") +
#   xlim(-1,5)
# gwg_rate_tri_lim


## gwg by ANC visit
gwg_anc <-enrolled_mnh05_df %>% 
  filter(!M05_TYPE_VISIT==13) %>%
  filter(!is.na(M05_TYPE_VISIT)) %>% 
  ggplot(aes(x = gwg, fill = ..x..)) +
  geom_histogram(color = "white", size = 0, 
                 alpha=1, position="identity", binwidth = 0.5) +
  scale_x_continuous(breaks=seq(5,18,1))+
  xlab("Gestational weight gain (kg) by ANC visit") +
  ylab("Frequency")+
  facet_grid(SITE ~  type_visit_GA, scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill="white"),
        strip.text = element_text(size = 7), 
        panel.spacing = unit(0.3, "lines"), 
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.position="right", 
        legend.title = element_text(size = 7),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 6)) +
  scale_fill_gradientn(colours = wes_palette("FantasticFox1", 10, type = "continuous"),
                       name = "GWG value")
gwg_anc

gwg_anc_lim <-enrolled_mnh05_df %>% 
  filter(!M05_TYPE_VISIT==13) %>% 
  filter(!is.na(M05_TYPE_VISIT)) %>% 
  ggplot(aes(x = gwg, fill = ..x..)) +
  geom_histogram(color = "white", size = 0, 
                 alpha=1, position="identity", binwidth = 0.5) +
  scale_x_continuous(breaks=seq(5,18,1))+
  xlab("Gestational weight gain by (kg) ANC visit") +
  ylab("Frequency")+
  facet_grid(SITE ~ type_visit_GA, scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill="white"),
        strip.text = element_text(size = 7), 
        panel.spacing = unit(0.3, "lines"), 
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        legend.position="right", 
        legend.title = element_text(size = 7),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 6)) +
  scale_fill_gradientn(colours = wes_palette("FantasticFox1", 10, type = "continuous"),
                       name = "GWG value") +
  xlim(-5,20)
gwg_anc_lim


## 06/19 - generate a list with data errors
errors_GA <- enrolled_mnh05_df %>%
  subset(M05_WEIGHT_PEPERF==1)%>%
  filter (GA_WEEK<14 | GA_WEEK>40) %>%
  select(SCRNID,MOMID,PREGID,EST_CONCEP_DATE,M05_ANT_PEDAT,EDD_BOE,SITE,
         M05_TYPE_VISIT,M05_WEIGHT_PERES,GA_WEEK)

write.csv(errors_GA, "D:/Users/xinyili/Documents/PRISMA-Analysis-Lili/GWG analysis/errors_GA.csv", row.names = FALSE)

errors_gwg <-enrolled_mnh05_df %>%
  subset(M05_WEIGHT_PEPERF==1)%>%
  filter(gwg < -5 | gwg > 20)%>%
  select(SCRNID,MOMID,PREGID,SITE,M05_TYPE_VISIT,initial_weight,M05_WEIGHT_PERES,type_visit_GA,gwg)

write.csv(errors_gwg, "D:/Users/xinyili/Documents/PRISMA-Analysis-Lili/GWG analysis/errors_gwg.csv", row.names = FALSE)