##Rapid test sensitivity analyseis
##12/18/2021
##Analyst: Emily Stein

rm(list = ls())
library(tidyverse)
library(Hmisc)
library(Gmisc)
library(knitr)
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(ggplot2)
library(xts)
library(tidyverse)
library(gtsummary)

###Pulling in and binding lab data into one datatable
map_df_read_csv <- function(path, pattern = "*.csv") {
  list.files(path, pattern, full.names = TRUE) %>% 
    map_df(~read_csv(., col_types = cols(.default = "c")))
}


lab_data<-map_df_read_csv("D:/12-15-2021/Lab Data", pattern=".CSV")

#pull in merged clean data
setwd("D:/12-15-2021")

lab_data$collection_date<-as.Date(lab_data$`Collection Date`, format="%Y/%m/%d")

main<-readRDS("final.dataset.RDS")

###Vaccination analysis###
#Recode as no vacc, partially vacc, full vacc
main<-main%>%
  mutate(Vax_clean=case_when(Vax_rec=="Yes" & (Fully_vax!="Yes") ~ "Partially vaccinated",
                             Fully_vax=="Yes" ~ "Fully vaccinated",
                             Vax_rec=="No"|Vax_rec=="No response"~ "Unvaccinated"))

#Filtering on 4/1/2021
vaccination_analysis<-main%>%
  filter(Test_date>="2021-04-01")

#Vaccination table
vaccination_table<-vaccination_analysis%>%
  select(Symptomatic_new, Vax_clean, antigen_result)

vaccination_table<-tbl_summary(vaccination_table,
                               by="Vax_clean")%>%
  add_n()

###Overall table###
look1<-look%>%
  filter(antigen_result=="FP")
                      

#Table 1
table1<-merged.data3%>%
  select(PatientID, Age, Gender, racecat, new_racecat, Ethnicity)%>%
  distinct()%>%
  select(-PatientID)
table1<-tbl_summary(table1,
              label=list(
                Age ~ "Age",
                racecat ~"Race categorizations: Madhura's coding"))
table1

#Table 2, disease characteristics by testing result
table2<-merged.data3%>%
  select(Age, Gender, racecat, Symptomatic, Symptomatic_new, Vax_rec, Fully_vax, antigen_result, Variant, Updated_Date)

table2<-tbl_summary(table2,
                    by="antigen_result",
                    label=list(
                      Updated_Date ~ "Test date",
                      racecat ~ "Madhura's coding",
                      Symptomatic_new ~ "Symptomatic, assuming blank entries were not symptomatic",
                      Vax_rec ~ "At least one dose received",
                      Fully_vax ~ "Fully vaccinated"
                    ))%>%
  add_p()

table2

#table 3, disease characteristics by symptomatic status ASSUMING all those who don't have anythign entered were asymptomatic
table3<-merged.data3%>%
  select(Symptomatic_new, Vax_rec, Fully_vax, antigen_result, Updated_Date)%>%
  ungroup()

table3<-tbl_summary(table3,
                    by="Symptomatic_new",
                    label=list(
                      Updated_Date ~ "Test date",
                      Vax_rec ~ "At least one dose received",
                      Fully_vax ~ "Fully vaccinated",
                      antigen_result ~ "Test result"
                    ))%>%
  add_p()
table3

#table4, disease characteristics and test results by variant
table4<-merged.data3%>%
  select(Symptomatic_new, Vax_rec, Fully_vax, antigen_result, Updated_Date, Variant)%>%
  ungroup()

table4<-tbl_summary(table4,
                    by="Variant",
                    label=list(
                      Updated_Date ~ "Test date",
                      Vax_rec ~ "At least one dose received",
                      Fully_vax ~ "Fully vaccinated",
                      antigen_result ~ "Test result"
                    ))%>%
  add_p()
table4
  
ggplot(data=merged.data2, aes(x=Updated_Date))+
         geom_bar() +
  facet_wrap("antigen_result")


#####Lab data####
lab_data<-lab_data%>%
  rename(LabPatientID=`Patient ID`)%>%
  mutate(CMD_ID= grepl("CMD|CMDA", LabPatientID))%>%
  distinct() #27889

CMD_ID<-lab_data%>%
  filter(CMD_ID==TRUE) #14573

###Not using numerical IDs since they do not match###
#NUM_ID<-lab_data%>%
#  filter(CMD_ID==FALSE)%>%
#  mutate(PatientID=as.numeric(LabPatientID))%>%
#  mutate(PatientID=PatientID+10)#4575


#Removing CMD in front of CMD IDs and adding 10, renaming as Patient ID to merge with COVID Results
CMD_ID<-CMD_ID%>%
  mutate(PatientID=str_replace(LabPatientID, "CMDA|CMD", ""))%>%
  mutate(PatientID=as.numeric(PatientID))%>%
  mutate(PatientID=PatientID+10)%>%
  distinct() #14573

#Merged data 3 includes Covid results and lab data
merged.data3<-inner_join(CMD_ID, COVIDResults_confPCR, by="PatientID") #10243 matches

merged.data3<-merged.data3%>%
  mutate(antigen_result=case_when(Lab.Result.Interpretation=="NEGATIVE" & Result.PCR=="NEGATIVE" ~ "TN",
                                  Lab.Result.Interpretation=="POSITIVE" & Result.PCR=="POSITIVE" ~ "TP",
                                  Lab.Result.Interpretation=="POSITIVE" & Result.PCR=="NEGATIVE" ~ "FP",
                                  Lab.Result.Interpretation=="NEGATIVE" & Result.PCR=="POSITIVE" ~ "FN"))


#Filling in symptomatic visits if yes or no was entered for a visit
PCR_results<-merged.data3%>%
  filter(Test=="SARS CoV 2 E Gene"|Test=="SARS CoV 2 ORF 1 Gene")%>%
  mutate_at(vars(Symptomatic), funs('Symptomatic_old' = na_if(., ''))) %>% 
  # set grouping for following operations
  group_by(PatientID, Test_date) %>% 
  # for added columns, fill values downwards and upwards within each group
  fill(Symptomatic) %>% 
  fill(Symptomatic, .direction = 'up') %>%
  # reinsert empty strings for NAs
  mutate_at(vars(Symptomatic), funs(coalesce(., factor(NA))))%>%
  ungroup()%>%
  distinct()

Antigen_results<-merged.data3%>%
  filter(Test=="COVID-19 Spike Total Ab")%>%
  mutate_at(vars(Symptomatic), funs('Symptomatic_old' = na_if(., ''))) %>% 
  # set grouping for following operations
  group_by(PatientID, Test_date) %>% 
  # for added columns, fill values downwards and upwards within each group
  fill(Symptomatic) %>% 
  fill(Symptomatic, .direction = 'up') %>%
  # reinsert empty strings for NAs
  mutate_at(vars(Symptomatic), funs(coalesce(., factor(NA))))%>%
  ungroup()%>%
  distinct()

PCR_IDs<-PCR_results%>%
  select(PatientID)%>%
  distinct()

Antigen_IDs<-Antigen_results%>%
  select(PatientID)%>%
  distinct()

merge<-inner_join(PCR_IDs, Antigen_IDs, by="PatientID")

look<-lab_data%>%
  filter(LabPatientID=="CMD6420116")

###Only 20 IDs have CT values for both PCR and antigen results?!

PCR_results<-lab_data%>%
  filter(Test=="SARS CoV 2 E Gene"|Test=="SARS CoV 2 ORF 1 Gene")

Antigen_results<-lab_data%>%
  filter(Test=="COVID-19 Spike Total Ab")

merge<-inner_join(Antigen_results, PCR_results, by="LabPatientID")

merge2<-inner_join(merge, CUNY_Labs, by="LabPatientID")


merge2<-merge2%>%
  select(PatientID)

merge3<-inner_join(COVIDResults_confPCR, merge2, by="PatientID")


#Adding in vaccination and demographic data
ChiefComplaint <- as.data.table(ChiefComplaint) 

ChiefComplaint %>% 
  rename(Complaint = `Chief Complaint`,
         Vax_date = `Adjusted Visit Date`,
         Vax_rec = `COVID-19 Vaccine?`,
         Vax_manu = `Which vaccine did you receive?`,
         Fully_vax = `> 2 weeks since final dose?`) -> ChiefComplaint

Visit <- as.data.table(Visit)

Visit %>%
  rename(Visit_date = `Adjusted Visit Date`,
         Facility_Address = `Facility Address`,
         Facility_Name = `Facility Name`,
         Facility_City =  `Facility City`,
         Facility_State =  `Facility State`) -> Visit

save.image()

getwd()


