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
library(lubridate)

#set wd
setwd("D:/12-15-2021")
###Pulling in and binding lab data into one datatable
map_df_read_csv <- function(path, pattern = "*.csv") {
  list.files(path, pattern, full.names = TRUE) %>% 
    map_df(~read_csv(., col_types = cols(.default = "c")))
}


lab_data<-map_df_read_csv("D:/12-15-2021/Lab Data", pattern=".CSV")

#pull in data
Visit <- read_delim("D:/12-15-2021/CUNY_Visit.csv","|", escape_double = FALSE, trim_ws = TRUE) #10273173 
COVIDResults <- read_delim("D:/12-15-2021/CUNY_COVIDResults.csv", "|",escape_double = FALSE, 
                           col_types = list(col_character(),col_character(),col_character(),
                                            col_character(),col_character(),col_character(),col_character(),
                                            col_character(),col_double(),col_double()),
                           trim_ws = TRUE) #8109375
ChiefComplaint  <- read_delim("D:/12-15-2021/CUNY_ChiefComplaint.csv","|", escape_double = FALSE, trim_ws = TRUE) #4236024

CUNY_Labs <- read_delim("D:/12-15-2021/CUNY_Labs.csv", delim="|",escape_double = FALSE, trim_ws = TRUE)


COVIDResults$Test_date <- as.Date(COVIDResults$Test_date, format="%m/%d/%Y")

ChiefComplaint$Vax_date <- as.Date(ChiefComplaint$Vax_date, format="%m/%d/%Y")

Visit.demo <- readRDS("Visit_demo_updatedrace.RDS")

lab_data$collection_date<-as.Date(lab_data$`Collection Date`, format="%Y/%m/%d")

main <-readRDS("final.dataset.RDS")

####Cleaning chief complaints####
ChiefComplaint <- as.data.table(ChiefComplaint) 

#Renaming chief complaints variables and changing the character string to all upper case
ChiefComplaint %>% 
  rename(Chief.Complaint = `Chief Complaint`,
         Vax_date = `Adjusted Visit Date`,
         Vax_rec = `COVID-19 Vaccine?`,
         Vax_manu = `Which vaccine did you receive?`,
         Fully_vax = `> 2 weeks since final dose?`,
         D1=`Diagnosis 1`,
         D2=`Diagnosis 2`,
         D3=`Diagnosis 3`,
         D4=`Diagnosis 4`)%>%
  mutate_if(is.character, str_to_upper)-> ChiefComplaint


#Generating lists of chosen symptoms
chills_terms=c("CHILLS", "CHILLS (WITHOUT FEVER)", "CHILLS WITH FEVER", 
               "CHILLS WITHOUT FEVER", "FEVER AND CHILLS", "FEVER CHILLS", 
               "FEVER WITH CHILLS")
cough_terms=c("COUGH", "COUGHING BLOOD", "COUGH HEADACHE", "COUGH IN ADULT", "COUGH IN ADULT PATIENT",
              "COUGH WITH HEMOPTYSIS", "COUGH WITH SPUTUM", "COUGH, PERSISTENT", "COUGHING", "DRY COUGH", "PERSISTENT COUGH",
              "PRODUCTIVE COUGH", "UPPER AIRWAY COUGH SYNDROME", "UPPER RESPIRATORY INFECTION WITH COUGH AND CONGESTION", 
              "URI WITH COUGH AND CONGESTION", "VIRAL URI WITH COUGH", "COUGH", "VIRAL UPPER RESPIRATORY TRACT INFECTION WITH COUGH", 
              "COUGH IN ADULT", "COUGH IN PEDIATRIC PATIENT", "COUGHING", "PRODUCTIVE COUGH",
              "UPPER AIRWAY COUGH SYNDROME", "URI WITH COUGH AND CONGESTION")
fever_terms=c("FEVER", 
              "FEVER CHILLS", "FEVER IN PEDIATRIC PATIENT", "CHILLS WITH FEVER",
              "FEVER IN ADULT", "FEVER, UNSPECIFIED FEVER CAUSE", "FEVER IN CHILD", 
              "FEVER OF UNKOWN ORIGIN (FUO)", "CHILLS WITH FEVER", "FEVER AND CHILLS", 
              "FEVER IN PEDIATRIC PATIENT", "FEVER, UNSPECIFIED",
              "FEELS FEVERISH", "FEVER IN ADULT", "FEVER OF UNKNOWN ORIGIN",
              "LOW GRADE FEVER", "HIGH FEVER", "INTERMITTENT FEVER", "RECENT UNEXPLAINED FEVER",
              "PERSISTENT FEVER")
fatigue_terms=c("FATIGUE", "TIREDNESS","MALAISE AND FATIGUE", 
                "FATIGUE, UNSPECIFIED TYPE", "OTHER FATIGUE", "TIRED")
body.ache_terms=c("MYALGIAS", "MYALGIA", "BODY ACHES","MUSCLE ACHE", "GENERALIZED BODY ACHES", 
                  "MUSCLE ACHE", "MUSCLE SORENESS", "MYALGIA, UNSPECIFIED SITE", "MUSCLE ACHE OF EXTREMITY")
headache_terms=c("ACUTE HEADACHE", "HEADACHE", "ACUTE INTRACTABLE HEADACHE, UNSPECIFIED HEADACHE TYPE", 
                 "ACUTE NON INTRACTABLE TENSION-TYPE HEADACHE", "COUGH HEADACHE",
                 "NEW PERSISTENT DAILY HEADACHE", "OTHER HEADACHE SYNDROME", "HEADACHE ABOVE THE EYE REGION")
taste.smell_terms=c("ABNORMAL SENSE OF TASTE", "ABNORMAL SMELL", "ALTERED TASTE", 
                    "DECREASED SENSE OF SMELL", "DECREASED SENSE OF TASTE", "DISTURBANCE OF SMELL", 
                    "DISTURBANCE OF SMELL AND TASTE", "BITTER TASTE", "LOSS OF SMELL", "LOSS OF TASTE", 
                    "SENSE OF SMELL ALTERED", "SMELL DISTURBANCE", "SMELL OR TASTE SENSATION DISTURBANCE", 
                    "TASTE ABSENT", "TASTE IMPAIRMENT", "UNSPECIFIED DISTURBANCES OF SMELL AND TASTE", "SMELL, IMPAIRED")
sore.throat_terms=c("ACUTE INFECTIVE PHARYNGITIS", "ACUTE NASOPHARYNGITIS", "ACUTE NASOPHARYNGITIS (COMMON COLD)", 
                    "ACUTE NASOPHARYNGITIS [COMMON COLD]", "ACUTE PHARYNGITIS", "ACUTE PHARYNGITIS, UNSPECIFIED", 
                    "ACUTE PHARYNGITIS, UNSPECIFIED ETIOLOGY",  "ACUTE SORE THROAT", "ACUTE VIRAL PHARYNGITIS",
                    "EXUDATIVE PHARYNGITIS", "NASOPHARYNGITIS", "NASOPHARYNGITIS ACUTE", "PHARYNGITIS", "PHARYNGITIS, ACUTE",
                    "RHINOPHARYNGITIS", "SORE THROAT", "SORE THROAT (VIRAL)", "SORE THROAT AND LARYNGITIS", 
                    "VIRAL PHARYNGITIS", "VIRAL SORE THROAT")
congestion.nose_terms=c("CHEST CONGESTION", "CONGESTION OF NASAL SINUS", "CONGESTION OF PARANASAL SINUS", 
                        "CONGESTION OF RESPIRATORY TRACT", "CONGESTION OF UPPER AIRWAY", 
                        "MILD NASAL CONGESTION", "NASAL CONGESTION", "NASAL CONGESTION WITH RHINORRHEA", 
                        "NASAL SINUS CONGESTION", "NOSE CONGESTION", "PULMONARY CONGESTION", "RUNNY NOSE", 
                        "SINUS CONGESTION", "STUFFY AND RUNNY NOSE", "UPPER RESPIRATORY INFECTION WITH COUGH AND CONGESTION", 
                        "URI WITH COUGH AND CONGESTION")
nausea.vomit_terms=c("ABDOMINAL PAIN, VOMITING, AND DIARRHEA", "ACUTE VOMITING", "CYCLICAL VOMITING", 
                     "HEMATEMESIS WITH NAUSEA", 
                     "INTRACTABLE VOMITING", "INTRACTABLE VOMITING WITH NAUSEA",  
                     "NAUSEA", "NAUSEA & VOMITING", "NAUSEA AND VOMITING",
                     "NAUSEA ALONE", "NAUSEA AND VOMITING IN CHILD", "PROJECTILE VOMITING WITH NAUSEA", 
                     "PERSISTENT VOMITING", "VOMITING", "NAUSEA (WITHOUT VOMITING)", "NAUSEA WITH VOMITING", 
                     "NAUSEA WITHOUT VOMITING", "POST-TUSSIVE VOMITING", "VOMITING (WITHOUT DIARRHEA)", 
                     "VOMITING AND DIARRHEA", "VOMITINGS", "VOMITING IN PEDIATRIC PATIENT", "VOMITING ALONE")
diarrhea_terms=c("ABDOMINAL PAIN, VOMITING, AND DIARRHEA", "ACUTE DIARRHEA", "DIARRHEA", "DIARRHEA (WITHOUT VOMITING)", 
                 "DIARRHEA IN ADULT PATIENT", "DIARRHEA, UNSPECIFIED", "NAUSEA VOMITING AND DIARRHEA", "INFECTIOUS DIARRHEA", 
                 "VOMITING AND DIARRHEA")
sob_terms=c("ABNORMALITY OF BREATHING", "BREATHING DIFFICULTY", "BREATHING PROBLEM", "CHEST PAIN MADE WORSE BY BREATHING", 
            "CHEST PAIN ON BREATHING", "DIFFICULTY BREATHING", "EXERTIONAL SHORTNESS OF BREATH", "MILD SHORTNESS OF BREATH", 
            "HEAVY BREATHING", "MILD SHORTNESS OF BREATH", "PAIN AGGRAVATED BY COUGHING AND DEEP BREATHING", "PAINFUL BREATHING", 
            "FAST BREATHING", "SENSATION OF BREATHLESSNESS", "SHORTNESS OF BREATH", "SHORTNESS OF BREATH AT REST", "SOB (SHORTNESS OF BREATH)", 
            "SOB (SHORTNESS OF BREATH) ON EXERTION", "SHORTNESS OF BREATH ON EXERTION", "SOBOE (SHORTNESS OF BREATH ON EXERTION)")
chest.pain_terms=c("ACUTE CHEST PAIN", "ATYPICAL CHEST PAIN", "CHEST PAIN", "CHEST PAIN AT REST", "CHEST PAIN IN ADULT",
                   "CHEST PAIN IN PATIENT YOUNGER THAN 17 YEARS", "CHEST PAIN MADE WORSE BY BREATHING", 
                   "CHEST PAIN OF UNCERTAIN ETIOLOGY", "CHEST PAIN ON BREATHING", "CHEST PAIN ON EXERTION", 
                   "CHEST PAIN ON RESPIRATION", "CHEST PAIN SYNDROME", "ATYPICAL CHEST PAIN, EXERTIONAL",  
                   "CHEST PAIN, MID STERNAL", "CHEST PAIN, NON-CARDIAC", "CHEST PAIN, UNSPECIFIED", "CHEST PAIN, UNSPECIFIED TYPE", 
                   "CHEST PRESSURE", "CHEST TIGHTNESS", "CHEST TIGHTNESS OR PRESSURE", "FEELING OF CHEST TIGHTNESS",
                   "INTERMITTENT CHEST PAIN", "NONSPECIFIC CHEST PAIN", "OTHER CHEST PAIN", "PLEURITIC CHEST PAIN", 
                   "SENSATION OF CHEST TIGHTNESS")
confusion_terms=c("ALTERED MENTAL STATE", "ALTERED MENTAL STATUS", "ALTERED MENTAL STATUS, UNSPECIFIED", "CONFUSION")
#Creating symptom categories out of the chief complaints dataset using the symptoms lists
ChiefComplaint<-ChiefComplaint%>%
  mutate(chills = ifelse(Chief.Complaint %in% chills_terms|
                           D1 %in% chills_terms|
                           D2 %in% chills_terms|
                           D3 %in% chills_terms|
                           D4 %in% chills_terms, "Yes", NA),
         cough = ifelse(Chief.Complaint %in% cough_terms|
                          D1 %in% cough_terms|
                          D2 %in% cough_terms|
                          D3 %in% cough_terms|
                          D4 %in% cough_terms, "Yes", NA),
         fever= ifelse(Chief.Complaint %in% fever_terms|
                         D1 %in% fever_terms|
                         D2 %in% fever_terms|
                         D3 %in% fever_terms|
                         D4 %in% fever_terms, "Yes", NA),
         fatigue = ifelse(Chief.Complaint %in% fatigue_terms|
                            D1 %in% fatigue_terms|
                            D2 %in% fatigue_terms|
                            D3 %in% fatigue_terms|
                            D4 %in% fatigue_terms, "Yes", NA),
         body.ache = ifelse(Chief.Complaint %in% body.ache_terms|
                              D1 %in% body.ache_terms|
                              D2 %in% body.ache_terms|
                              D3 %in% body.ache_terms|
                              D4 %in% body.ache_terms, "Yes", NA),
         headache = ifelse(Chief.Complaint %in% headache_terms|
                             D1 %in% headache_terms|
                             D2 %in% headache_terms|
                             D3 %in% headache_terms|
                             D4 %in% headache_terms, "Yes", NA),
         taste.smell = ifelse(Chief.Complaint %in% taste.smell_terms|
                                D1 %in% taste.smell_terms|
                                D2 %in% taste.smell_terms|
                                D3 %in% taste.smell_terms|
                                D4 %in% taste.smell_terms, "Yes", NA),
         sore.throat = ifelse(Chief.Complaint %in% sore.throat_terms|
                                D1 %in% sore.throat_terms|
                                D2 %in% sore.throat_terms|
                                D3 %in% sore.throat_terms|
                                D4 %in% sore.throat_terms, "Yes", NA),
         congestion.nose = ifelse(Chief.Complaint %in% congestion.nose_terms|
                                    D1 %in% congestion.nose_terms|
                                    D2 %in% congestion.nose_terms|
                                    D3 %in% congestion.nose_terms|
                                    D4 %in% congestion.nose_terms, "Yes", NA),
         nausea.vomit = ifelse(Chief.Complaint %in% nausea.vomit_terms|
                                 D1 %in% nausea.vomit_terms|
                                 D2 %in% nausea.vomit_terms|
                                 D3 %in% nausea.vomit_terms|
                                 D4 %in% nausea.vomit_terms, "Yes", NA),
         diarrhea = ifelse(Chief.Complaint %in% diarrhea_terms|
                             D1 %in% diarrhea_terms|
                             D2 %in% diarrhea_terms|
                             D3 %in% diarrhea_terms|
                             D4 %in% diarrhea_terms, "Yes", NA),
         sob = ifelse(Chief.Complaint %in% sob_terms|
                        D1 %in% sob_terms|
                        D2 %in% sob_terms|
                        D3 %in% sob_terms|
                        D4 %in% sob_terms, "Yes", NA),
         chest.pain = ifelse(Chief.Complaint %in% chest.pain_terms|
                               D1 %in% chest.pain_terms|
                               D2 %in% chest.pain_terms|
                               D3 %in% chest.pain_terms|
                               D4 %in% chest.pain_terms, "Yes", NA),
         confusion = ifelse(Chief.Complaint %in% confusion_terms|
                              D1 %in% confusion_terms|
                              D2 %in% confusion_terms|
                              D3 %in% confusion_terms|
                              D4 %in% confusion_terms, "Yes", NA))

ChiefComplaint_Vax<-ChiefComplaint%>%
  select(VisitID, Vax_date, Vax_rec, Vax_manu, Fully_vax)%>%
  distinct()

ChiefComplaint_Vax<-ChiefComplaint_Vax%>%
  mutate_at(vars(Vax_rec), funs('Vax_rec_old' = na_if(., ''))) %>% 
  # set grouping for following operations
  group_by(VisitID) %>% 
  # for added columns, fill values downwards and upwards within each group
  fill(Vax_rec) %>% 
  fill(Vax_rec, .direction = 'up') %>%
  # reinsert empty strings for NAs
  mutate_at(vars(Vax_rec), funs(coalesce(., factor(NA))))%>%
  ungroup()

ChiefComplaint_Vax<-ChiefComplaint_Vax%>%
  mutate_at(vars(Fully_vax), funs('Vax_rec_old' = na_if(., ''))) %>% 
  # set grouping for following operations
  group_by(VisitID) %>% 
  # for added columns, fill values downwards and upwards within each group
  fill(Fully_vax) %>% 
  fill(Fully_vax, .direction = 'up') %>%
  # reinsert empty strings for NAs
  mutate_at(vars(Fully_vax), funs(coalesce(., factor(NA))))%>%
  ungroup()

ChiefComplaint_Vax_Clean<-ChiefComplaint_Vax%>%
  select(VisitID, Fully_vax, Vax_rec)%>%
  distinct()

#Saving COVID Results file
saveRDS(ChiefComplaint_Vax_Clean, file = "ChiefComplaint_Vax_Clean.RDS")

####CLEANING VISIT DATA. DONE AND SAVED UPDATED DATASET FOR 12/2021 DATA####
Visit <- as.data.table(Visit)

Visit %>%
  rename(Visit_date = `Adjusted Visit Date`,
         Facility_Address = `Facility Address`,
         Facility_Name = `Facility Name`,
         Facility_City =  `Facility City`,
         Facility_State =  `Facility State`) -> Visit

Visit %>%
  filter(Facility_State=="NY") %>%
  filter(Visit_date >= "2020-03-01")%>%
  distinct()-> Visit 

#changing dates to formatting where they can be manipulated
Visit$Visit_date <- as.Date(Visit$Visit_date, format="%m/%d/%Y")

#Create separate wide datasets for each variable and combine at the end; unwieldy otherwise
Visit <- Visit %>%
  group_by(PatientID) %>%
  arrange(Visit_date, .by_group=TRUE)%>%
  mutate(Visit = 1:n())

Visit<- as.data.table(Visit)
Visit.w <- dcast(Visit, PatientID  ~ Visit, value.var = "Visit_date")

Visit.w.age <- dcast(Visit, PatientID  ~ Visit, value.var = "PatientAge")
Visit.w.age <- Visit.w.age[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.age, by="PatientID")

Visit.w.gender <- dcast(Visit, PatientID  ~ Visit, value.var = "PatientGender")
Visit.w.gender <- Visit.w.gender[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.gender, by="PatientID")

Visit.w.re <- dcast(Visit, PatientID  ~ Visit, value.var = "Race")
Visit.w.race <- Visit.w.re[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.race, by="PatientID")

Visit.w.ethnicity <-  dcast(Visit, PatientID  ~ Visit, value.var = "Ethnicity")
Visit.w.ethnicity <- Visit.w.ethnicity[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.ethnicity, by="PatientID")

Visit.w.PIG <-  dcast(Visit, PatientID  ~ Visit, value.var = "PrimaryInsuranceGroup")
Visit.w.PIG <- Visit.w.PIG[,1:6]
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI1 = 2)
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI2 = 3)
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI3 = 4)
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI4 = 5)
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI5 = 6)
Visit.w <- left_join(Visit.w, Visit.w.PIG, by="PatientID")

Visit.w.UHF <- dcast(Visit, PatientID  ~ Visit, value.var = "UHF_Neighborhood")
Visit.w.UHF <- Visit.w.UHF[,1:2]

Visit.w <- left_join(Visit.w, Visit.w.UHF, by="PatientID")

Visit.w.facility <- dcast(Visit, PatientID  ~ Visit, value.var = "Facility_City")
Visit.w.facility <- Visit.w.facility[,1:6]
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac1 = 2)
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac2 = 3)
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac3 = 4)
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac4 = 5)
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac5 = 6)
Visit.w <- left_join(Visit.w, Visit.w.facility, by="PatientID")

Visit.w.region <- dcast(Visit, PatientID  ~ Visit, value.var = "Geograpic Region")
Visit.w.region <- Visit.w.region[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.region, by="PatientID")

#rename vars
Visit.w <- setnames(Visit.w, old = c("1.x","1.y","1.x.x","1.y.y","1.x.x.x","1.y.y.y",1), new=c("1","Age","Gender","Race","Ethnicity","UHF","Region"))

#select out demographic vars 
Visit.demo <- Visit.w[, c(1,106:121)]

#Remove extra datasets
rm(Visit.w, Visit.w.age, Visit.w.ethnicity, Visit.w.facility, Visit.w.gender, Visit.w.PIG, Visit.w.race, Visit.w.re, Visit.w.region, Visit.w.UHF)

setwd("D:/12-15-2021")
saveRDS(Visit.demo, file = "Visit_demo.RDS")

Visit.demo$racecat <- NA
Visit.demo$racecat[Visit.demo$Race %in% c("Abenaki","Absentee Shawnee", "Apache", "Arapaho", "Caddo","Acoma", 
                                          "Alamo Navajo", "Canoncito Navajo", "Agdaagux", "Agua Caliente", "Agua Caliente Cahuilla", 
                                          "Augustine", "Bishop", "Bridgeport", "Cabazon", "Cahto", "Cahuilla", 
                                          "California Tribes", "Campo", "Capitan Grande", "Ak-Chin", "Arizona Tewa", 
                                          "Barrio Libre", "Birch Creek", "Brevig Mission", "Ak-Chin", "Arizona Tewa", "Barrio Libre", 
                                          "Birch Creek", "Brevig Mission", "Alabama Coushatta", "Alabama Creek", "Alabama Quassarte",
                                          "Allen Canyon", "Alsea", "Arikara", "Aroostook", "Assiniboine", "Assiniboine Sioux", "Atsina", 
                                          "Blackfoot Sioux", "Attacapa", "Bad River", "Brotherton", "Bannock", "Battle Mountain", 
                                          "Carson", "Bay Mills Chippewa", "Burt Lake Band", "Burt Lake Chippewa", "Burt Lake Ottawa",
                                          "Big Cypress", "Brighton", "Biloxi", "Blackfeet", "Bois Forte", "Brule Sioux", "Burns Paiute",
                                          "Catawba", "Cayuga", "Cayuse", "Cedarville", "Celilo", "Central Pomo", "Chehalis", "Chemakuan",
                                          "Chemehuevi", "Cherokee", "Cherokee Alabama", "Cherokee Shawnee", "Cherokees of Northeast Alabama",
                                          "Cherokees of Southeast Alabama", "Cheyenne", "Cheyenne River Sioux", "Cheyenne-Arapaho",
                                          "Chickahominy", "Chickasaw", "Chimariko", "Chinook", "Chippewa", "Chippewa Cree", 
                                          "Chiricahua", "Chitimacha", "Choctaw", "Chukchansi", "Chumash", "Citizen Band Potawatomi",
                                          "Clatsop", "Clear Lake", "Clifton Choctaw", "Coast Miwok", "Coast Yurok", "Cochiti", "Cocopah",
                                          "Coeur D'Alene", "Coharie", "Colorado River", "Columbia River Chinook", "Colville",
                                          "Comanche", "Coos", "Coos; Lower Umpqua; Siuslaw", "Coquilles", "Costanoan", "Coushatta",
                                          "Cow Creek Umpqua", "Cowlitz", "Craig", "Cree", "Creek", "Croatan", "Crow", "Crow Creek Sioux",
                                          "Cupeno", "Cuyapaipe", "Dakota Sioux", "Delaware", "Diegueno", "Digger", "Dresslerville",
                                          "Dry Creek", "Duck Valley", "Duckwater", "Duwamish", "Eastern Cherokee", "Eastern Chickahominy",
                                          "Eastern Creek", "Eastern Delaware", "Eastern Muscogee", "Eastern Pomo", "Eastern Shawnee",
                                          "Echota Cherokee", "Elko", "Ely", "Esselen", "Etowah Cherokee", "Fallon", "Flandreau Santee",
                                          "Florida Seminole", "Fond du Lac", "Forest County", "Fort Belknap", "Fort Berthold", "Fort Bidwell",
                                          "Fort Hall", "Fort Independence", "Fort McDermitt", "Fort Mcdowell", "Fort Peck", 
                                          "Fort Peck Assiniboine Sioux", "Fort Sill Apache", "French American Indian", "Gabrieleno",
                                          "Gay Head Wampanoag", "Georgetown (Eastern Tribes)", "Gila Bend", "Gila River Pima-Maricopa",
                                          "Goshute", "Grand Portage", "Grand Ronde", "Grand Traverse Band of Ottawa/Chippewa",
                                          "Gros Ventres", "Haliwa", "Hannahville", "Havasupai", "Hidatsa", "Ho-chunk", "Hoh", "Hollywood Seminole",
                                          "Hoopa", "Hoopa Extension", "Hopi", "Houma", "Hualapai", "Huron Potawatomi", "Illinois Miami",
                                          "Inaja-Cosmit", "Indian Township", "Indiana Miami", "Iowa", "Iowa of Kansas-Nebraska", 
                                          "Iowa of Oklahoma", "Iowa Sac and Fox", "Iroquois", "Isleta", "Jamestown", "Jemez",
                                          "Jena Choctaw", "Jicarilla Apache", "Juaneno", "Kaibab", "Kalapuya", "Kalispel", 
                                          "Karuk", "Kashia", "Kathlamet", "Kaw", "Kawaiisu", "Keres", "Kern River", "Keweenaw",
                                          "Kialegee", "Kickapoo", "Kikiallus", "Kiowa", "Klallam", "Klamath", "Konkow", "Kootenai",
                                          "La Jolla", "La Posta", "Lac Courte Oreilles", "Lac du Flambeau", "Lac Vieux Desert Chippewa",
                                          "Laguna", "Lake Superior", "Lake Traverse Sioux", "Las Vegas", "Lassik", "Leech Lake", 
                                          "Lenni-Lenape", "Lipan Apache", "Little Shell Chippewa", "Lone Pine", "Long Island", "Los Coyotes",
                                          "Lovelock", "Lower Brule Sioux", "Lower Elwha", "Lower Muscogee", "Lower Sioux", "Lower Skagit", 
                                          "Luiseno", "Lumbee", "Lummi", "Machis Lower Creek Indian", "Maidu", "Makah", "Malheur Paiute",
                                          "Maliseet", "Mandan", "Manzanita", "Maricopa", "Marshantucket Pequot", "Mashpee Wampanoag",
                                          "Matinecock", "Mattaponi", "Mattole", "Mdewakanton Sioux", "Menominee", "Mesa Grande", 
                                          "Mescalero Apache", "Miami", "Miccosukee", "Michigan Ottawa", "Algonquian", "Beaver", 
                                          "Canadian Indian", "Greenland Eskimo", "Haida", "Micmac", "Mille Lacs", "Miniconjou",
                                          "Minnesota Chippewa", "Mission Indians", "Mississippi Choctaw", "Missouri Sac and Fox", 
                                          "Miwok", "Modoc", "Mohave", "Mohawk", "Mohegan", "Molala", "Mono", "Montauk", "Morongo",
                                          "Mountain Maidu", "Mowa Band of Choctaw", "Muckleshoot", "Munsee", "Nambe", "Narragansett",
                                          "Natchez", "Nausu Waiwash", "Navajo", "Nebraska Ponca", "Nebraska Winnebago", "Nez Perce", 
                                          "Nipmuc", "Nishinam", "Nisqually", "Nomalaki", "Nooksack", "Northern Arapaho", "Northern Cherokee",
                                          "Northern Cheyenne", "Northern Paiute", "Northern Pomo", "Northwest Tribes", "Oglala Sioux",
                                          "Oklahoma Apache", "Oklahoma Cado", "Oklahoma Choctaw", "Oklahoma Comanche", "Oklahoma Kickapoo",
                                          "Oklahoma Kiowa", "Oklahoma Miami", "Oklahoma Ottawa", "Oklahoma Pawnee", "Oklahoma Peoria",
                                          "Oklahoma Ponca", "Oklahoma Sac and Fox", "Oklahoma Seminole", "Omaha", "Oneida", "Onondaga",
                                          "Ontonagon", "Oregon Athabaskan", "Osage", "Otoe-Missouria", "Ottawa", "Owens Valley", "Paiute", 
                                          "Pala", "Palauan", "Pamunkey", "Panamint", "Pascua Yaqui", "Passamaquoddy", "Paugussett", "Pauma", 
                                          "Pawnee", "Payson Apache", "Pawnee", "Payson Apache", "Pechanga", "Pelican", "Penobscot", "Peoria",
                                          "Pequot", "Perryville", "Picuris", "Pima", "Pine Ridge Sioux", "Pipestone Sioux", "Piro", 
                                          "Piscataway", "Pit River", "Pleasant Point Passamaquoddy", "Poarch Band", "Pocomoke Acohonock", 
                                          "Pojoaque", "Pokagon Potawatomi", "Pomo", "Ponca", "Poospatuck", "Port Madison", "Potawatomi", 
                                          "Powhatan", "Prairie Band", "Prairie Island Sioux", "Principal Creek Indian Nation", "Prior Lake Sioux",
                                          "Pueblo", "Puget Sound Salish", "Puyallup", "Pyramid Lake", "Quapaw", "Quechan", "Quileute", 
                                          "Quinault", "Ramah Navajo", "Rampough Mountain", "Red Cliff Chippewa", "Red Lake Chippewa", 
                                          "Red Wood", "Reno-Sparks", "Rocky Boy's Chippewa Cree", "Rosebud Sioux", "Round Valley",
                                          "Sac and Fox", "Saginaw Chippewa", "Salinan", "Salish", "Salish and Kootenai", "Salt River Pima-Maricopa",
                                          "Samish", "San Carlos Apache", "San Felipe", "San Ildefonso", "San Juan Pueblo", "San Juan Southern Paiute",
                                          "San Manual", "San Pasqual", "Sand Hill", "Sand Point", "Sandia", "Santa Ana", "Santa Clara",
                                          "Santa Rosa", "Santa Rosa Cahuilla", "Santa Ynez", "Santa Ysabel", "Santee Sioux", "Sauk-Suiattle",
                                          "Sault Ste. Marie Chippewa", "Schaghticoke", "Scotts Valley", "Seminole", "Seneca", "Seneca Nation",
                                          "Serrano", "Setauket", "Shasta", "Shawnee", "Shinnecock", "Shoshone", "Shoshone Paiute", "Sioux", 
                                          "Sisseton-Wahpeton", "Skokomish", "Skull Valley", "Snohomish", "Soboba", "Sokoagon Chippewa",
                                          "South Fork Shoshone", "Southeastern Indians", "Southern Arapaho", "Southern Cheyenne",
                                          "Southern Paiute", "Spirit Lake Sioux", "Spokane", "Squaxin Island", "St. Croix Chippewa",
                                          "Standing Rock Sioux", "Star Clan of Muscogee Creeks", "Steilacoom", "Stillaguamish",
                                          "Stockbridge", "Sulphur Bank", "Summit Lake", "Suquamish", "Susanville", "Susquehanock",
                                          "Sycuan", "Table Bluff", "Tachi", "Takelma", "Taos", "Te-Moak Western Shoshone", "Temecula",
                                          "Tenino", "Tesuque", "Teton Sioux", "Tewa", "Texas Kickapoo", "Thlopthlocco", "Tigua", 
                                          "Timbi-Sha Shoshone", "Tohono O'Odham", "Tolowa", "Tonawanda Seneca", "Torres-Martinez",
                                          "Tsimshian", "Tuckabachee", "Tulalip", "Tule River", "Tunica Biloxi", "Turtle Mountain",
                                          "Tuscarora", "Tuscola", "Twenty-Nine Palms", "Two Kettle Sioux", "Tygh", "Uintah Ute", 
                                          "Umatilla", "Umpqua", "United Keetowah Band of Cherokee, Upper Chinook", "Upper Sioux",
                                          "Upper Skagit", "Ute", "Ute Mountain Ute", "Utu Utu Gwaitu Paiute", "Waccamaw-Siousan", 
                                          "Wahpekute Sioux", "Wahpeton Sioux", "Wailaki", "Wakiakum Chinook", "Walker River", 
                                          "Walla-Walla", "Wampanoag", "Wappo", "Warm Springs", "Wascopum", "Washakie", "Washoe",
                                          "Wazhaza Sioux", "Wenatchee", "Western Cherokee", "Western Chickahominy", "Whilkut", "White Earth",
                                          "White Mountain", "White Mountain Apache", "White Mountain Inupiat", "Wichita", "Wicomico",
                                          "Willapa Chinook", "Wind River", "Wind River Arapaho", "Wind River Shoshone", "Winnebago",
                                          "Winnemucca", "Wintun", "Wisconsin Potawatomi", "Wishram", "Wiyot", "Wyandotte", "Yahooskin",
                                          "Yakama", "Yakama Cowlitz", "Yana", "Yankton Sioux", "Yanktonai Sioux", "Yaqui", "Yavapai",
                                          "Yavapai Apache", "Yerington Paiute", "Yokuts", "Yomba", "Yuchi", "Yuki", "Yuman", "Yurok",
                                          "Zia", "Zuni", "Eastern Tribes","Ahtna", "Akhiok", "Akiachak", "Akiak", "Akutan", "Alakanuk", "Alanvik", "Alaska Indian", 
                                          "Alaska Native", "Alaskan Athabascan", "Alatna", "Aleknagik", "Aleut", "Aleut Corporation",
                                          "Aleutian", "Aleutian Islander", "Alexander", "Allakaket", "Alutiiq Aleut", "Ambler", 
                                          "Anaktuvuk", "Anaktuvuk Pass", "Andreafsky", "Angoon", "Aniak", "Anvik", "Arctic", 
                                          "Arctic Slope Corporation", "Arctic Slope Inupiat", "Atka", "Atmautluak", "Atqasuk",
                                          "Barrow", "Belkofski", "Bering Straits Inupiat", "Bethel", "Bill Moore's Slough", "Bristol Bay Aleut", 
                                          "Bristol Bay Yupik", "Buckland", "Calista Yupik", "Cantwell", "Central Council of Tlingit and Haida Tribes",
                                          "Chefornak", "Chalkyitsik","Chenega", "Chevak", "Chickaloon", "Chignik", "Chignik Lagoon",
                                          "Chignik Lagoon", "Chignik Lake", "Chilkat", "Chilkoot", "Chinik", "Chistochina", "Chitina",
                                          "Chuathbaluk", "Chugach Aleut", "Chugach Corporation", "Clark's Point", "Cook Inlet", 
                                          "Copper Center", "Copper River", "Crooked Creek", "Deering", "Dillingham", "Dot Lake",
                                          "Doyon", "Eek", "Egegik", "Eklutna", "Ekuk", "Ekwok", "Elim", "Emmonak", "English Bay",
                                          "Eskimo", "Evansville", "Eyak", "False Pass", "Fort Yukon", "Gakona", "Galena", "Gambell", 
                                          "Georgetown (Yupik-Eskimo)", "Golovin", "Goodnews Bay", "Grayling", "Gulkana", "Healy Lake",
                                          "Holy Cross", "Hoonah", "Hooper Bay", "Hughes", "Huslia", "Hydaburg", "Igiugig", "Iliamna",
                                          "Inalik Diomede", "Inupiaq", "Inupiat Eskimo", "Iqurmuit (Russian Mission)", "Ivanof Bay",
                                          "Kake", "Kalskag", "Kaltag", "Kasaan", "Kasigluk", "Kawerak", "Kenaitze", "Ketchikan", "Kiana",
                                          "King Cove", "King Salmon", "Kipnuk", "Kivalina", "Klawock", "Knik", "Kobuk", 
                                          "Kodiak", "Kokhanok", "Koliganek", "Kongiganak", "Koniag Aleut", "Kotlik", "Kotzebue",
                                          "Koyuk", "Koyukuk", "Kwethluk", "Kwigillingok", "Kwiguk", "Lake Minchumina", "Larsen Bay",
                                          "Levelock", "Manley Hot Springs", "Manokotak", "Mary's Igloo", "Mauneluk Inupiat", "Mekoryuk", 
                                          "Mentasta Lake", "Metlakatla", "Minto", "Mountain Village", "Nana Inupiat", "Napakiak", 
                                          "Napaskiak", "Napaumute", "Nelson Lagoon", "Nenana", "New Stuyahok", "Newhalen", "Newtok", "Nikolai",
                                          "Ninilchik", "Noatak", "Nome", "Nondalton", "Noorvik", "Northway", "Nulato", "Nunapitchukv", 
                                          "Old Harbor", "Oscarville", "Ouzinkie", "Pauloff Harbor", "Pedro Bay", "Petersburg", "Pilot Point",
                                          "Pitkas Point", "Point Hope", "Point Lay", "Port Graham", "Port Heiden", "Port Lions", "Portage Creek",
                                          "Qagan Toyagungin", "Qawalangin", "Quinhagak", "Rampart", "Ruby", "Ruby Valley", "Salamatof", "Savoonga",
                                          "Saxman", "Scammon Bay", "Selawik", "Seldovia", "Shageluk", "Shaktoolik", "Sheldon's Point", "Shishmaref",
                                          "Shungnak", "Siberian Eskimo", "Siberian Yupik", "Sitka", "Slana", "Sleetmute", "South Naknek", 
                                          "Southeast Alaska", "St. George", "St. Mary's", "St. Michael", "St. Paul", "Stebbins", "Stevens",
                                          "Stony River", "Sugpiaq", "Tanaina", "Tanana", "Tanana Chiefs", "Tazlina", "Telida", "Teller",
                                          "Tenakee Springs", "Tlingit", "Tlingit-Haida", "Tok", "Toksook", "Tulukskak", "Tuntutuliak", "Tununak",
                                          "Twin Hills", "Tyonek", "Ugashik", "Umkumiate", "Unalakleet", "Unalaska", "Unangan Aleut", "Unga",
                                          "Venetie", "Wainwright", "Wrangell", "Yakutat", "Yupik Eskimo","Central American Indian", "Mexican American Indian",
                                          "South American Indian","American Indian", "American Indian or Alaska Native", "Canadian and Latin American Indian", 
                                          "Chamorro", "Chuukese", "Fijian", "Guamanian", "Kiribati", "Kosraean", "Mariana Islander",
                                          "Marshall", "Marshallese", "Melanesian", "Micronesian", "Native Hawaiian", 
                                          "Native Hawaiian or Other Pacific Islander", "New Hebrides", "Other Pacific Islander",
                                          "Papua New Guinean", "Pohnpeian", "Polynesian", "Saipanese", "Samoan", "Solomon", "Solomon Islander",
                                          "Tahitian", "Tokelauan", "Tongan", "Yapese", "Guamanian or Chamorro", "Spanish American Indian",
                                          "United Keetowah Band of Cherokee","Red Devil","Upper Chinook", "Kluti Kaah"," Lower Kalskag", "Nanticoke",
                                          "Nightmute","Nuiqsut"," Port Gamble Klallam","San Xavier","Scott Valley","Seneca-Cayuga","Siuslaw","Talakamish",
                                          "Tanacross","Togiak", "Lower Kalskag", "Port Gamble Klallam", "Tetlin")] <- "Native American/Alaskan Native/Pacific Islander"

Visit.demo$racecat[Visit.demo$Race %in% c("Asian","Bangladeshi", "Bhutanese", "Asian Indian", "Maldivian", "Nepalese", "Pakistani",
                                          "Sri Lankan","Burmese", "Cambodian", "Indonesian", "Hmong", "Laotian", "Malaysian", "Singaporean",
                                          "Thailand", "Vietnamese","Chinese", "Iwo Jiman", "Japanese", "Korean", "Okinawan", "Taiwanese","Thai")] <- "Asian"                     

Visit.demo$racecat[Visit.demo$Race %in% c("African", "Botswanan", "Ethiopian", "Liberian", "Madagascar", "Namibian", "Nigerian",
                                          "Zairean","African American","Bahamian", "Barbadian", "Douglas", "Haitian", "Jamaican", "Tobagoan", "Trinidadian",
                                          "West Indian","Black", "Black or African American")] <- "Black/AfrAm"

Visit.demo$racecat[Visit.demo$Race %in% c("Alpine", "English", "European", "French", "German", "Irish", "Italian", "Moor",
                                          "Polish", "Scottish", "Wales","Iranian", "Iraqi", "Armenian", "Arab", "Assyrian", "Afghanistani", 
                                          "Israeili", "Karluk", "Lebanese", "Egyptian", "Middle Eastern or North African", 
                                          "Palestinian", "Syrian","White")] <-"White"


Visit.demo$racecat[Visit.demo$Race %in% c("Columbia","Dominica Islander", "Dominican", "Santo Domingo","Filipino","San Juan","Hispanic", "San Juan De")] <-"Hispanic"

Visit.demo$racecat[Visit.demo$Race %in% c("Declined to Report", "Declined to Specify", "Unreported/Refuse to Report", "Unreported/Refused to Report",
                                          "Unreported/Refused To Report","Other Race","Carolinian", "Circle", "Council", "Eagle", "Lime", "Mcgrath", "Platinum", "Stewart",
                                          "Trinity", "Wiseman","Oklahoma Delaware","Siletz","Stonyford","", "Suqpigaq", "Unreported/Refuse To Report")] <- "Other/Unknown"
# If ethnicity is Hispanic, change race to Hispanic
Visit.demo$Ethnicity[Visit.demo$Ethnicity == "Unreported/Refused to Report"] <- NA 
Visit.demo$Ethnicity[Visit.demo$Ethnicity == ""] <- NA 

Visit.demo$racecat[!is.na(Visit.demo$Ethnicity) & Visit.demo$Ethnicity != "Not Hispanic or Latino"] <- "Hispanic"
summary(as.factor(Visit.demo$racecat))
Visit.demo$racecat[is.na(Visit.demo$racecat)] <- "Other/Unknown"
summary(as.factor(Visit.demo$racecat))

####### Updating race variable for citymd data based on Saba's code####
updatedrace_orig_race_Oct<-Visit.demo%>%
  select(PatientID, racecat, Race)
updatedrace_orig_race_Oct$racecat <- NA
updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Abenaki","Absentee Shawnee", "Apache", "Arapaho", "Caddo","Acoma", 
                                                                        "Alamo Navajo", "Canoncito Navajo", "Agdaagux", "Agua Caliente", "Agua Caliente Cahuilla", 
                                                                        "Augustine", "Bishop", "Bridgeport", "Cabazon", "Cahto", "Cahuilla", 
                                                                        "California Tribes", "Campo", "Capitan Grande", "Ak-Chin", "Arizona Tewa", 
                                                                        "Barrio Libre", "Birch Creek", "Brevig Mission", "Ak-Chin", "Arizona Tewa", "Barrio Libre", 
                                                                        "Birch Creek", "Brevig Mission", "Alabama Coushatta", "Alabama Creek", "Alabama Quassarte",
                                                                        "Allen Canyon", "Alsea", "Arikara", "Aroostook", "Assiniboine", "Assiniboine Sioux", "Atsina", 
                                                                        "Blackfoot Sioux", "Attacapa", "Bad River", "Brotherton", "Bannock", "Battle Mountain", 
                                                                        "Carson", "Bay Mills Chippewa", "Burt Lake Band", "Burt Lake Chippewa", "Burt Lake Ottawa",
                                                                        "Big Cypress", "Brighton", "Biloxi", "Blackfeet", "Bois Forte", "Brule Sioux", "Burns Paiute",
                                                                        "Catawba", "Cayuga", "Cayuse", "Cedarville", "Celilo", "Central Pomo", "Chehalis", "Chemakuan",
                                                                        "Chemehuevi", "Cherokee", "Cherokee Alabama", "Cherokee Shawnee", "Cherokees of Northeast Alabama",
                                                                        "Cherokees of Southeast Alabama", "Cheyenne", "Cheyenne River Sioux", "Cheyenne-Arapaho",
                                                                        "Chickahominy", "Chickasaw", "Chimariko", "Chinook", "Chippewa", "Chippewa Cree", 
                                                                        "Chiricahua", "Chitimacha", "Choctaw", "Chukchansi", "Chumash", "Citizen Band Potawatomi",
                                                                        "Clatsop", "Clear Lake", "Clifton Choctaw", "Coast Miwok", "Coast Yurok", "Cochiti", "Cocopah",
                                                                        "Coeur D'Alene", "Coharie", "Colorado River", "Columbia River Chinook", "Colville",
                                                                        "Comanche", "Coos", "Coos; Lower Umpqua; Siuslaw", "Coquilles", "Costanoan", "Coushatta",
                                                                        "Cow Creek Umpqua", "Cowlitz", "Craig", "Cree", "Creek", "Croatan", "Crow", "Crow Creek Sioux",
                                                                        "Cupeno", "Cuyapaipe", "Dakota Sioux", "Delaware", "Diegueno", "Digger", "Dresslerville",
                                                                        "Dry Creek", "Duck Valley", "Duckwater", "Duwamish", "Eastern Cherokee", "Eastern Chickahominy",
                                                                        "Eastern Creek", "Eastern Delaware", "Eastern Muscogee", "Eastern Pomo", "Eastern Shawnee",
                                                                        "Echota Cherokee", "Elko", "Ely", "Esselen", "Etowah Cherokee", "Fallon", "Flandreau Santee",
                                                                        "Florida Seminole", "Fond du Lac", "Forest County", "Fort Belknap", "Fort Berthold", "Fort Bidwell",
                                                                        "Fort Hall", "Fort Independence", "Fort McDermitt", "Fort Mcdowell", "Fort Peck", 
                                                                        "Fort Peck Assiniboine Sioux", "Fort Sill Apache", "French American Indian", "Gabrieleno",
                                                                        "Gay Head Wampanoag", "Georgetown (Eastern Tribes)", "Gila Bend", "Gila River Pima-Maricopa",
                                                                        "Goshute", "Grand Portage", "Grand Ronde", "Grand Traverse Band of Ottawa/Chippewa",
                                                                        "Gros Ventres", "Haliwa", "Hannahville", "Havasupai", "Hidatsa", "Ho-chunk", "Hoh", "Hollywood Seminole",
                                                                        "Hoopa", "Hoopa Extension", "Hopi", "Houma", "Hualapai", "Huron Potawatomi", "Illinois Miami",
                                                                        "Inaja-Cosmit", "Indian Township", "Indiana Miami", "Iowa", "Iowa of Kansas-Nebraska", 
                                                                        "Iowa of Oklahoma", "Iowa Sac and Fox", "Iroquois", "Isleta", "Jamestown", "Jemez",
                                                                        "Jena Choctaw", "Jicarilla Apache", "Juaneno", "Kaibab", "Kalapuya", "Kalispel", 
                                                                        "Karuk", "Kashia", "Kathlamet", "Kaw", "Kawaiisu", "Keres", "Kern River", "Keweenaw",
                                                                        "Kialegee", "Kickapoo", "Kikiallus", "Kiowa", "Klallam", "Klamath", "Konkow", "Kootenai",
                                                                        "La Jolla", "La Posta", "Lac Courte Oreilles", "Lac du Flambeau", "Lac Vieux Desert Chippewa",
                                                                        "Laguna", "Lake Superior", "Lake Traverse Sioux", "Las Vegas", "Lassik", "Leech Lake", 
                                                                        "Lenni-Lenape", "Lipan Apache", "Little Shell Chippewa", "Lone Pine", "Long Island", "Los Coyotes",
                                                                        "Lovelock", "Lower Brule Sioux", "Lower Elwha", "Lower Muscogee", "Lower Sioux", "Lower Skagit", 
                                                                        "Luiseno", "Lumbee", "Lummi", "Machis Lower Creek Indian", "Maidu", "Makah", "Malheur Paiute",
                                                                        "Maliseet", "Mandan", "Manzanita", "Maricopa", "Marshantucket Pequot", "Mashpee Wampanoag",
                                                                        "Matinecock", "Mattaponi", "Mattole", "Mdewakanton Sioux", "Menominee", "Mesa Grande", 
                                                                        "Mescalero Apache", "Miami", "Miccosukee", "Michigan Ottawa", "Algonquian", "Beaver", 
                                                                        "Canadian Indian", "Greenland Eskimo", "Haida", "Micmac", "Mille Lacs", "Miniconjou",
                                                                        "Minnesota Chippewa", "Mission Indians", "Mississippi Choctaw", "Missouri Sac and Fox", 
                                                                        "Miwok", "Modoc", "Mohave", "Mohawk", "Mohegan", "Molala", "Mono", "Montauk", "Morongo",
                                                                        "Mountain Maidu", "Mowa Band of Choctaw", "Muckleshoot", "Munsee", "Nambe", "Narragansett",
                                                                        "Natchez", "Nausu Waiwash", "Navajo", "Nebraska Ponca", "Nebraska Winnebago", "Nez Perce", 
                                                                        "Nipmuc", "Nishinam", "Nisqually", "Nomalaki", "Nooksack", "Northern Arapaho", "Northern Cherokee",
                                                                        "Northern Cheyenne", "Northern Paiute", "Northern Pomo", "Northwest Tribes", "Oglala Sioux",
                                                                        "Oklahoma Apache", "Oklahoma Cado", "Oklahoma Choctaw", "Oklahoma Comanche", "Oklahoma Kickapoo",
                                                                        "Oklahoma Kiowa", "Oklahoma Miami", "Oklahoma Ottawa", "Oklahoma Pawnee", "Oklahoma Peoria",
                                                                        "Oklahoma Ponca", "Oklahoma Sac and Fox", "Oklahoma Seminole", "Omaha", "Oneida", "Onondaga",
                                                                        "Ontonagon", "Oregon Athabaskan", "Osage", "Otoe-Missouria", "Ottawa", "Owens Valley", "Paiute", 
                                                                        "Pala", "Palauan", "Pamunkey", "Panamint", "Pascua Yaqui", "Passamaquoddy", "Paugussett", "Pauma", 
                                                                        "Pawnee", "Payson Apache", "Pawnee", "Payson Apache", "Pechanga", "Pelican", "Penobscot", "Peoria",
                                                                        "Pequot", "Perryville", "Picuris", "Pima", "Pine Ridge Sioux", "Pipestone Sioux", "Piro", 
                                                                        "Piscataway", "Pit River", "Pleasant Point Passamaquoddy", "Poarch Band", "Pocomoke Acohonock", 
                                                                        "Pojoaque", "Pokagon Potawatomi", "Pomo", "Ponca", "Poospatuck", "Port Madison", "Potawatomi", 
                                                                        "Powhatan", "Prairie Band", "Prairie Island Sioux", "Principal Creek Indian Nation", "Prior Lake Sioux",
                                                                        "Pueblo", "Puget Sound Salish", "Puyallup", "Pyramid Lake", "Quapaw", "Quechan", "Quileute", 
                                                                        "Quinault", "Ramah Navajo", "Rampough Mountain", "Red Cliff Chippewa", "Red Lake Chippewa", 
                                                                        "Red Wood", "Reno-Sparks", "Rocky Boy's Chippewa Cree", "Rosebud Sioux", "Round Valley",
                                                                        "Sac and Fox", "Saginaw Chippewa", "Salinan", "Salish", "Salish and Kootenai", "Salt River Pima-Maricopa",
                                                                        "Samish", "San Carlos Apache", "San Felipe", "San Ildefonso", "San Juan Pueblo", "San Juan Southern Paiute",
                                                                        "San Manual", "San Pasqual", "Sand Hill", "Sand Point", "Sandia", "Santa Ana", "Santa Clara",
                                                                        "Santa Rosa", "Santa Rosa Cahuilla", "Santa Ynez", "Santa Ysabel", "Santee Sioux", "Sauk-Suiattle",
                                                                        "Sault Ste. Marie Chippewa", "Schaghticoke", "Scotts Valley", "Seminole", "Seneca", "Seneca Nation",
                                                                        "Serrano", "Setauket", "Shasta", "Shawnee", "Shinnecock", "Shoshone", "Shoshone Paiute", "Sioux", 
                                                                        "Sisseton-Wahpeton", "Skokomish", "Skull Valley", "Snohomish", "Soboba", "Sokoagon Chippewa",
                                                                        "South Fork Shoshone", "Southeastern Indians", "Southern Arapaho", "Southern Cheyenne",
                                                                        "Southern Paiute", "Spirit Lake Sioux", "Spokane", "Squaxin Island", "St. Croix Chippewa",
                                                                        "Standing Rock Sioux", "Star Clan of Muscogee Creeks", "Steilacoom", "Stillaguamish",
                                                                        "Stockbridge", "Sulphur Bank", "Summit Lake", "Suquamish", "Susanville", "Susquehanock",
                                                                        "Sycuan", "Table Bluff", "Tachi", "Takelma", "Taos", "Te-Moak Western Shoshone", "Temecula",
                                                                        "Tenino", "Tesuque", "Teton Sioux", "Tewa", "Texas Kickapoo", "Thlopthlocco", "Tigua", 
                                                                        "Timbi-Sha Shoshone", "Tohono O'Odham", "Tolowa", "Tonawanda Seneca", "Torres-Martinez",
                                                                        "Tsimshian", "Tuckabachee", "Tulalip", "Tule River", "Tunica Biloxi", "Turtle Mountain",
                                                                        "Tuscarora", "Tuscola", "Twenty-Nine Palms", "Two Kettle Sioux", "Tygh", "Uintah Ute", 
                                                                        "Umatilla", "Umpqua", "United Keetowah Band of Cherokee, Upper Chinook", "Upper Sioux",
                                                                        "Upper Skagit", "Ute", "Ute Mountain Ute", "Utu Utu Gwaitu Paiute", "Waccamaw-Siousan", 
                                                                        "Wahpekute Sioux", "Wahpeton Sioux", "Wailaki", "Wakiakum Chinook", "Walker River", 
                                                                        "Walla-Walla", "Wampanoag", "Wappo", "Warm Springs", "Wascopum", "Washakie", "Washoe",
                                                                        "Wazhaza Sioux", "Wenatchee", "Western Cherokee", "Western Chickahominy", "Whilkut", "White Earth",
                                                                        "White Mountain", "White Mountain Apache", "White Mountain Inupiat", "Wichita", "Wicomico",
                                                                        "Willapa Chinook", "Wind River", "Wind River Arapaho", "Wind River Shoshone", "Winnebago",
                                                                        "Winnemucca", "Wintun", "Wisconsin Potawatomi", "Wishram", "Wiyot", "Wyandotte", "Yahooskin",
                                                                        "Yakama", "Yakama Cowlitz", "Yana", "Yankton Sioux", "Yanktonai Sioux", "Yaqui", "Yavapai",
                                                                        "Yavapai Apache", "Yerington Paiute", "Yokuts", "Yomba", "Yuchi", "Yuki", "Yuman", "Yurok",
                                                                        "Zia", "Zuni", "Eastern Tribes","Ahtna", "Akhiok", "Akiachak", "Akiak", "Akutan", "Alakanuk", "Alanvik", "Alaska Indian", 
                                                                        "Alaska Native", "Alaskan Athabascan", "Alatna", "Aleknagik", "Aleut", "Aleut Corporation",
                                                                        "Aleutian", "Aleutian Islander", "Alexander", "Allakaket", "Alutiiq Aleut", "Ambler", 
                                                                        "Anaktuvuk", "Anaktuvuk Pass", "Andreafsky", "Angoon", "Aniak", "Anvik", "Arctic", 
                                                                        "Arctic Slope Corporation", "Arctic Slope Inupiat", "Atka", "Atmautluak", "Atqasuk",
                                                                        "Barrow", "Belkofski", "Bering Straits Inupiat", "Bethel", "Bill Moore's Slough", "Bristol Bay Aleut", 
                                                                        "Bristol Bay Yupik", "Buckland", "Calista Yupik", "Cantwell", "Central Council of Tlingit and Haida Tribes",
                                                                        "Chefornak", "Chalkyitsik","Chenega", "Chevak", "Chickaloon", "Chignik", "Chignik Lagoon",
                                                                        "Chignik Lagoon", "Chignik Lake", "Chilkat", "Chilkoot", "Chinik", "Chistochina", "Chitina",
                                                                        "Chuathbaluk", "Chugach Aleut", "Chugach Corporation", "Clark's Point", "Cook Inlet", 
                                                                        "Copper Center", "Copper River", "Crooked Creek", "Deering", "Dillingham", "Dot Lake",
                                                                        "Doyon", "Eek", "Egegik", "Eklutna", "Ekuk", "Ekwok", "Elim", "Emmonak", "English Bay",
                                                                        "Eskimo", "Evansville", "Eyak", "False Pass", "Fort Yukon", "Gakona", "Galena", "Gambell", 
                                                                        "Georgetown (Yupik-Eskimo)", "Golovin", "Goodnews Bay", "Grayling", "Gulkana", "Healy Lake",
                                                                        "Holy Cross", "Hoonah", "Hooper Bay", "Hughes", "Huslia", "Hydaburg", "Igiugig", "Iliamna",
                                                                        "Inalik Diomede", "Inupiaq", "Inupiat Eskimo", "Iqurmuit (Russian Mission)", "Ivanof Bay",
                                                                        "Kake", "Kalskag", "Kaltag", "Kasaan", "Kasigluk", "Kawerak", "Kenaitze", "Ketchikan", "Kiana",
                                                                        "King Cove", "King Salmon", "Kipnuk", "Kivalina", "Klawock", "Knik", "Kobuk", 
                                                                        "Kodiak", "Kokhanok", "Koliganek", "Kongiganak", "Koniag Aleut", "Kotlik", "Kotzebue",
                                                                        "Koyuk", "Koyukuk", "Kwethluk", "Kwigillingok", "Kwiguk", "Lake Minchumina", "Larsen Bay",
                                                                        "Levelock", "Manley Hot Springs", "Manokotak", "Mary's Igloo", "Mauneluk Inupiat", "Mekoryuk", 
                                                                        "Mentasta Lake", "Metlakatla", "Minto", "Mountain Village", "Nana Inupiat", "Napakiak", 
                                                                        "Napaskiak", "Napaumute", "Nelson Lagoon", "Nenana", "New Stuyahok", "Newhalen", "Newtok", "Nikolai",
                                                                        "Ninilchik", "Noatak", "Nome", "Nondalton", "Noorvik", "Northway", "Nulato", "Nunapitchukv", 
                                                                        "Old Harbor", "Oscarville", "Ouzinkie", "Pauloff Harbor", "Pedro Bay", "Petersburg", "Pilot Point",
                                                                        "Pitkas Point", "Point Hope", "Point Lay", "Port Graham", "Port Heiden", "Port Lions", "Portage Creek",
                                                                        "Qagan Toyagungin", "Qawalangin", "Quinhagak", "Rampart", "Ruby", "Ruby Valley", "Salamatof", "Savoonga",
                                                                        "Saxman", "Scammon Bay", "Selawik", "Seldovia", "Shageluk", "Shaktoolik", "Sheldon's Point", "Shishmaref",
                                                                        "Shungnak", "Siberian Eskimo", "Siberian Yupik", "Sitka", "Slana", "Sleetmute", "South Naknek", 
                                                                        "Southeast Alaska", "St. George", "St. Mary's", "St. Michael", "St. Paul", "Stebbins", "Stevens",
                                                                        "Stony River", "Sugpiaq", "Tanaina", "Tanana", "Tanana Chiefs", "Tazlina", "Telida", "Teller",
                                                                        "Tenakee Springs", "Tlingit", "Tlingit-Haida", "Tok", "Toksook", "Tulukskak", "Tuntutuliak", "Tununak",
                                                                        "Twin Hills", "Tyonek", "Ugashik", "Umkumiate", "Unalakleet", "Unalaska", "Unangan Aleut", "Unga",
                                                                        "Venetie", "Wainwright", "Wrangell", "Yakutat", "Yupik Eskimo","Central American Indian", "Mexican American Indian",
                                                                        "South American Indian","American Indian", "American Indian or Alaska Native", "Canadian and Latin American Indian", 
                                                                        "Chamorro", "Chuukese", "Fijian", "Guamanian", "Kiribati", "Kosraean", "Mariana Islander",
                                                                        "Marshall", "Marshallese", "Melanesian", "Micronesian", "Native Hawaiian", 
                                                                        "Native Hawaiian or Other Pacific Islander", "New Hebrides", "Other Pacific Islander",
                                                                        "Papua New Guinean", "Pohnpeian", "Polynesian", "Saipanese", "Samoan", "Solomon", "Solomon Islander",
                                                                        "Tahitian", "Tokelauan", "Tongan", "Yapese", "Guamanian or Chamorro", "Spanish American Indian",
                                                                        "United Keetowah Band of Cherokee","Red Devil","Upper Chinook", "Kluti Kaah"," Lower Kalskag", "Nanticoke",
                                                                        "Nightmute","Nuiqsut"," Port Gamble Klallam","San Xavier","Scott Valley","Seneca-Cayuga","Siuslaw","Talakamish",
                                                                        "Tanacross","Togiak", "Lower Kalskag", "Port Gamble Klallam", "Tetlin")] <- "Native American/Alaskan Native/Pacific Islander"

updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Asian","Bangladeshi", "Bhutanese", "Asian Indian", "Maldivian", "Nepalese", "Pakistani",
                                                                        "Sri Lankan","Burmese", "Cambodian", "Indonesian", "Hmong", "Laotian", "Malaysian", "Singaporean",
                                                                        "Thailand", "Vietnamese","Chinese", "Iwo Jiman", "Japanese", "Korean", "Okinawan", "Taiwanese","Thai")] <- "Asian"                     

updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("African", "Botswanan", "Ethiopian", "Liberian", "Madagascar", "Namibian", "Nigerian",
                                                                        "Zairean","African American","Bahamian", "Barbadian", "Douglas", "Haitian", "Jamaican", "Tobagoan", "Trinidadian",
                                                                        "West Indian","Black", "Black or African American")] <- "Black/AfrAm"

updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Alpine", "English", "European", "French", "German", "Irish", "Italian", "Moor",
                                                                        "Polish", "Scottish", "Wales","Iranian", "Iraqi", "Armenian", "Arab", "Assyrian", "Afghanistani", 
                                                                        "Israeili", "Karluk", "Lebanese", "Egyptian", "Middle Eastern or North African", 
                                                                        "Palestinian", "Syrian","White")] <-"White"


updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Columbia","Dominica Islander", "Dominican", "Santo Domingo","Filipino","San Juan","Hispanic", "San Juan De")] <-"Hispanic"

updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Declined to Report", "Declined to Specify", "Unreported/Refuse to Report", "Unreported/Refused to Report",
                                                                        "Unreported/Refused To Report","Other Race","Carolinian", "Circle", "Council", "Eagle", "Lime", "Mcgrath", "Platinum", "Stewart",
                                                                        "Trinity", "Wiseman","Oklahoma Delaware","Siletz","Stonyford","", "Suqpigaq", "Unreported/Refuse To Report")] <- "Other/Unknown"
# If ethnicity is Hispanic, change race to Hispanic
updatedrace_orig_race_Oct$Ethnicity[updatedrace_orig_race_Oct$Ethnicity == "Unreported/Refused to Report"] <- NA 
updatedrace_orig_race_Oct$Ethnicity[updatedrace_orig_race_Oct$Ethnicity == ""] <- NA 

updatedrace_orig_race_Oct$racecat[!is.na(updatedrace_orig_race_Oct$Ethnicity) & updatedrace_orig_race_Oct$Ethnicity != "Not Hispanic or Latino"] <- "Hispanic"
summary(as.factor(updatedrace_orig_race_Oct$racecat))
updatedrace_orig_race_Oct$racecat[is.na(updatedrace_orig_race_Oct$racecat)] <- "Other/Unknown"
summary(as.factor(updatedrace_orig_race_Oct$racecat))

#Remove rows that are Other/Unknown
updatedrace_orig_race_Oct %>%
  filter(racecat!="Other/Unknown") -> updated.race

#merged updated races with original data
updated.race %>%
  select(PatientID, racecat) -> updated.race

updated.race %>%
  rename(new_racecat = racecat) -> updated.race

Visit.demo <- left_join(Visit.demo, updated.race, by = "PatientID")

#update racecat var
Visit.demo$racecat <- if_else(Visit.demo$racecat=="Other/Unknown" & !(is.na(Visit.demo$new_racecat)), Visit.demo$new_racecat, Visit.demo$racecat)

#Removing updated race tables
rm(updated.race, updatedrace_orig_race_Oct)

#Saving updated visit file
saveRDS(Visit.demo, file = "Visit_demo_updatedrace.RDS")

###CLEANING COVID RESULTS####
COVIDResults <- as.data.table(COVIDResults)

COVIDResults %>%
  rename(Test_date = `Adjusted Visit Date`,
         Lab.Result.Interpretation = `Lab Result Interpretation`,
         Confirmatory.PCR=`Rapid Order Confirmatory PCR Ordered`,
         Result.PCR=`Rapid Order Confirmatory PCR Result`,
         Diff.Results=`Rapid Order Confirmatory PCR Incongruent Results`,
         Order.Name= `Order Name`,
         Symptomatic=`Rapid Order Patient Symptomatic`) -> COVIDResults

COVIDResults$Test_date <- as.Date(COVIDResults$Test_date, format="%m/%d/%Y")

#Filtering on Covid test given, test date, and receipt of confirmatory PCR
#Currently have incomplete data post 11/1, pasting in "symptomatic" for each visit a person reported
#symptoms
COVIDResults %>%
  filter(Lab.Result.Interpretation=="POSITIVE" | Lab.Result.Interpretation=="NEGATIVE") %>%
  mutate_at(vars(Symptomatic), funs('Symptomatic_old' = na_if(., ''))) %>% 
  # set grouping for following operations
  group_by(VisitID) %>% 
  # for added columns, fill values downwards and upwards within each group
  fill(Symptomatic) %>% 
  fill(Symptomatic, .direction = 'up') %>%
  # reinsert empty strings for NAs
  mutate_at(vars(Symptomatic), funs(coalesce(., factor(NA))))%>%
  ungroup()->COVIDResults

COVIDResults%>%
  filter(Confirmatory.PCR=="1")%>%
  filter(Test_date >= "2020-03-01")%>%
  distinct()-> COVIDResults_confPCR     #959750     

COVIDResults_confPCR <- COVIDResults_confPCR %>%
    group_by(PatientID) %>%
    arrange(Test_date, .by_group=TRUE)%>%
    mutate(Visit = 1:n())

#Saving COVID Results file
saveRDS(COVIDResults_confPCR, file = "COVIDResults_confPCR.RDS")
#If we restrict to those who only came to cityMD once, can link their vaccine and symptoms data

#Marking all entries that got a confirmatory PCR test
#COVIDResults_conf<-COVIDResults%>%
#  filter(Confirmatory.PCR=="1")%>%
#  select(PatientID, Confirmatory.PCR)%>%
#  distinct()

#COVIDResults_confPCR<-left_join(COVIDResults_conf, COVIDResults, by="PatientID")

#COVIDResults_confPCR <- COVIDResults_confPCR %>%
#  group_by(PatientID) %>%
#  arrange(Test_date, .by_group=TRUE)%>%
#mutate(Visit = 1:n())%>%
#  select(-Confirmatory.PCR.x)
#Some pts got PCR + antigen tests on the same day with same VistID so don't pull out distinct Visit IDs 

###MERGING DATA####
#Merge Visit data with covid results to get patient IDs for all results 
COVIDResults<-readRDS("COVIDResults_confPCR")
Visit.demo<-readRDS("Visit_demo_updatedrace.RDS")
merged.data3<-readRDS()
merged.data1 <- inner_join(COVIDResults, Visit.demo, by="PatientID") #innerjoin because out of state covid results need to be filtered out (6994684)

merged.data1<-merged.data1%>%
  mutate(antigen_result=case_when(Lab.Result.Interpretation=="NEGATIVE" & Result.PCR=="NEGATIVE" ~ "TN",
                                  Lab.Result.Interpretation=="POSITIVE" & Result.PCR=="POSITIVE" ~ "TP",
                                  Lab.Result.Interpretation=="POSITIVE" & Result.PCR=="NEGATIVE" ~ "FP",
                                  Lab.Result.Interpretation=="NEGATIVE" & Result.PCR=="POSITIVE" ~ "FN"))

#merge vaccine and visit data
merged.data2 <- left_join(merged.data1, ChiefComplaint_Vax_Clean, by="VisitID") #9226028
merged.data3<-merged.data2%>%
  filter(!is.na(antigen_result))%>%
  ungroup()

#Assume those who do not have 'symptomatic' entered had no symptoms
merged.data3<-merged.data3%>%
  mutate(Symptomatic=ifelse(is.na(Symptomatic), "Don't know", Symptomatic),
         Symptomatic_new=ifelse(Symptomatic=="Don't know", "No", Symptomatic))%>%
  ungroup()

#Creating group "don't know" for vaccination statuses
merged.data3<-merged.data3%>%
  mutate(Fully_vax=ifelse(is.na(Fully_vax), "Unknown", Fully_vax),
         Vax_rec=ifelse(is.na(Vax_rec), "Unknown", Vax_rec))
#date blocks
library(lubridate)
cut_dates<-ymd(c("2020-10-01",
                 "2020-11-01",
                 "2020-12-01",
                 "2021-01-01",
                 "2021-02-01",
                 "2021-03-01",
                 "2021-04-01",
                 "2021-05-01",
                 "2021-06-01",
                 "2021-07-01",
                 "2021-08-01",
                 "2021-09-01",
                 "2021-10-01",
                 "2021-10-31"))
new_cut<-cut(merged.data3$Test_date, cut_dates, include.lowest=TRUE)

merged.data3<-merged.data3%>%
  mutate(Updated_Date=factor(new_cut, labels=c("October 2020",
                             "November 2020",
                             "December 2020",
                             "January 2021",
                             "February 2021",
                             "March 2021",
                             "April 2021", 
                             "May 2021",
                             "June 2021",
                             "July 2021",
                             "August 2021",
                             "September 2021",
                             "October 2021")))
#estimating dates for strain dominance using NYT interactive tracker https://www.nytimes.com/interactive/2021/health/coronavirus-variant-tracker.html
merged.data3<-merged.data3%>%
  mutate(Variant=ifelse(Updated_Date %in% c("October 2020","November 2020","December 2020"),"Epsilon",
                        ifelse(Updated_Date %in% c("January 2021","February 2021","March 2021"), "Alpha",
                               ifelse(Updated_Date %in% c("April 2021","May 2021"), "Beta",
                                      ifelse(Updated_Date %in% c("June 2021","July 2021","August 2021","September 2021","October 2021"), "Delta", NA)))))

merged.data3<-merged.data3%>%
  mutate(Variant=relevel(as.factor(Variant), "Epsilon", "Alpha", "Beta", "Delta"))

#creating age groups
merged.data3<-merged.data3%>%
  mutate(Age_Group=ifelse())
saveRDS(merged.data3, file="final.dataset.RDS")

####TABLES AND ANALYSES####
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
 

####Sensitivity and specificity by month####
october_2020<-main%>%
  filter(Updated_Date=="October 2020")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="October 2020")

november_2020<-main%>%
  filter(Updated_Date=="November 2020")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="November 2020")

december_2020<-main%>%
  filter(Updated_Date=="December 2020")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="December 2020")

jan_2021<-main%>%
  filter(Updated_Date=="January 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="January 2021")

feb_2021<-main%>%
  filter(Updated_Date=="February 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="February 2021")

march_2021<-main%>%
  filter(Updated_Date=="March 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="March 2021")

april_2021<-main%>%
  filter(Updated_Date=="April 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="April 2021")

may_2021<-main%>%
  filter(Updated_Date=="May 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="May 2021")

june_2021<-main%>%
  filter(Updated_Date=="June 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="June 2021")

july_2021<-main%>%
  filter(Updated_Date=="July 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="July 2021")

august_2021<-main%>%
  filter(Updated_Date=="August 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="August 2021")


september_2021<-main%>%
  filter(Updated_Date=="September 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="September 2021")

october_2021<-main%>%
  filter(Updated_Date=="October 2021")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(Month="October 2021")

month_all=rbind(october_2020, november_2020, december_2020, jan_2021, feb_2021, march_2021,
                 april_2021, may_2021, june_2021, july_2021, august_2021, september_2021, october_2021)

month_all<-month_all%>%
  ungroup()%>%
  mutate(Month=as.factor(Month))%>%
  mutate(Month=Month, levels=c(
                       "October 2020",
                       "November 2020",
                       "December 2020",
                       "January 2021",
                       "February 2021",
                       "March 2021",
                       "April 2021",
                       "May 2021",
                       "June 2021",
                       "July 2021",
                       "August 2021",
                       "September 2021",
                       "October 2021"))
month_all$Month <- factor(month_all$Month, levels=c(
  "October 2020",
  "November 2020",
  "December 2020",
  "January 2021",
  "February 2021",
  "March 2021",
  "April 2021",
  "May 2021",
  "June 2021",
  "July 2021",
  "August 2021",
  "September 2021",
  "October 2021"))

month_sens=month_all%>%
  select(sensitivity, Month, name)%>%
  mutate(Test=ifelse(name=="antigen_result", "sensitivity", NA))%>%
  rename(Test_result=sensitivity)

month_spec=month_all%>%
  select(specificity, Month, name)%>%
  mutate(Test=ifelse(name=="antigen_result", "specificity", NA))%>%
  rename(Test_result=specificity)

spec_sens<-rbind(month_sens, month_spec)

sensitivity.line<-ggplot(data=month_sens, aes(x=Month, y=Test_result, group=1))+
         geom_line()+
  geom_point()

specificity.line<-ggplot(data=month_spec, aes(x=Month, y=Test_result, group=1))+
  geom_line()+
  geom_point()

all.line<-ggplot(data=spec_sens, aes(x=Month, y=Test_result, group=Test))+
  geom_line(aes(linetype=Test))+
  geom_point()



#####Lab data####
lab_data<-lab_data%>%
  rename(LabPatientID=`Patient ID`)%>%
  mutate(CMD_ID= grepl("CMD|CMDA", LabPatientID))%>%
  distinct() #27889

CMD_ID<-lab_data%>%
  filter(CMD_ID==TRUE) #14573

#Removing CMD in front of CMD IDs and adding 10, renaming as Patient ID to merge with COVID Results
CMD_ID<-CMD_ID%>%
  mutate(PatientID=str_replace(LabPatientID, "CMDA|CMD", ""))%>%
  mutate(PatientID=as.numeric(PatientID))%>%
  mutate(PatientID=PatientID+10)%>%
  distinct() #14573


#Main includes Covid results and lab data
CT_raw<-inner_join(CMD_ID, main, by="PatientID") #10243 matches

CT_clean<-CT_raw%>%
  mutate(lab_date=mdy(`Collection Date`))%>%
  mutate(timebtwn=difftime(Test_date, lab_date, units="days"))%>%
  mutate(acceptable_time=ifelse(timebtwn<=7 & timebtwn>=-7, 1, 0))%>%
  filter(acceptable_time==1 & Test!="COVID-19 Spike Total Ab" & antigen_result!="TN")%>%
  mutate(antigen_result=as.factor(antigen_result))%>%
  mutate(ct_result=as.numeric(`Num Res`))
  
#Table
CT_table<-CT_clean%>%
  select(antigen_result, Test, ct_result)

CT_table<-tbl_summary(CT_table,
                      by=antigen_result,
                      label=list(
                        ct_result ~ "Num Res"))
#Figure: Boxplot of CT values by test results

figure4<-ggplot(CT_clean, aes(x=antigen_result, y=ct_result)) + 
  geom_boxplot(notch=TRUE) + 
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  facet_wrap("Test")
