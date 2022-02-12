##Rapid test sensitivity analysis: Double checking merging and coding.
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

####pull in raw data####
Visit <- read_delim("D:/12-15-2021/CUNY_Visit.csv","|", escape_double = FALSE, trim_ws = TRUE) #10273173 
COVIDResults <- read_delim("D:/12-15-2021/CUNY_COVIDResults.csv", "|",escape_double = FALSE, 
                           col_types = list(col_character(),col_character(),col_character(),
                                            col_character(),col_character(),col_character(),col_character(),
                                            col_character(),col_double(),col_double()),
                           trim_ws = TRUE) #8109375
ChiefComplaint  <- read_delim("D:/12-15-2021/CUNY_ChiefComplaint.csv","|", escape_double = FALSE, trim_ws = TRUE) #4236024

lab_data$collection_date<-as.Date(lab_data$`Collection Date`, format="%Y/%m/%d")


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

#Converting vaccination date
ChiefComplaint$Vax_date <- as.Date(ChiefComplaint$Vax_date, format="%m/%d/%Y")

ChiefComplaint_vax<-ChiefComplaint%>%
  select(VisitID, Fully_vax, Vax_rec)%>%
  distinct()

#Saving COVID Results file
saveRDS(ChiefComplaint_symp_vax_2, file = "ChiefComplaint_Vax_Symp_Clean.RDS")
####CLEANING VISIT DATA. DONE AND SAVED UPDATED DATASET FOR 12/2021 DATA####
Visit <- as.data.table(Visit)

Visit %>%
  rename(Visit_date = `Adjusted Visit Date`,
         Facility_Address = `Facility Address`,
         Facility_Name = `Facility Name`,
         Facility_City =  `Facility City`,
         Facility_State =  `Facility State`) -> Visit

#changing dates to formatting where they can be manipulated
Visit$Visit_date <- as.Date(Visit$Visit_date, format="%m/%d/%Y")

#Picking out eligible IDs from Visit dataset
Visit %>%
  filter(Facility_State=="NY") %>%
  filter(Visit_date >= "2020-03-01")%>%
  select(PatientID)%>%
  distinct()-> Visit 


####Visit race/eth coding####
###Not including race/ethnic coding per 1/10/22 meeting
#Create separate wide datasets for each variable and combine at the end; unwieldy otherwise
# Visit <- Visit %>%
#   group_by(PatientID) %>%
#   arrange(Visit_date, .by_group=TRUE)%>%
#   mutate(Visit = 1:n())
# 
# Visit<- as.data.table(Visit)
# Visit.w <- dcast(Visit, PatientID  ~ Visit, value.var = "Visit_date")
# 
# Visit.w.age <- dcast(Visit, PatientID  ~ Visit, value.var = "PatientAge")
# Visit.w.age <- Visit.w.age[,1:2]
# Visit.w <- left_join(Visit.w, Visit.w.age, by="PatientID")
# 
# Visit.w.gender <- dcast(Visit, PatientID  ~ Visit, value.var = "PatientGender")
# Visit.w.gender <- Visit.w.gender[,1:2]
# Visit.w <- left_join(Visit.w, Visit.w.gender, by="PatientID")
# 
# Visit.w.re <- dcast(Visit, PatientID  ~ Visit, value.var = "Race")
# Visit.w.race <- Visit.w.re[,1:2]
# Visit.w <- left_join(Visit.w, Visit.w.race, by="PatientID")
# 
# Visit.w.ethnicity <-  dcast(Visit, PatientID  ~ Visit, value.var = "Ethnicity")
# Visit.w.ethnicity <- Visit.w.ethnicity[,1:2]
# Visit.w <- left_join(Visit.w, Visit.w.ethnicity, by="PatientID")
# 
# Visit.w.PIG <-  dcast(Visit, PatientID  ~ Visit, value.var = "PrimaryInsuranceGroup")
# Visit.w.PIG <- Visit.w.PIG[,1:6]
# Visit.w.PIG <- Visit.w.PIG %>%
#   rename(PI1 = 2)
# Visit.w.PIG <- Visit.w.PIG %>%
#   rename(PI2 = 3)
# Visit.w.PIG <- Visit.w.PIG %>%
#   rename(PI3 = 4)
# Visit.w.PIG <- Visit.w.PIG %>%
#   rename(PI4 = 5)
# Visit.w.PIG <- Visit.w.PIG %>%
#   rename(PI5 = 6)
# Visit.w <- left_join(Visit.w, Visit.w.PIG, by="PatientID")
# 
# Visit.w.UHF <- dcast(Visit, PatientID  ~ Visit, value.var = "UHF_Neighborhood")
# Visit.w.UHF <- Visit.w.UHF[,1:2]
# 
# Visit.w <- left_join(Visit.w, Visit.w.UHF, by="PatientID")
# 
# Visit.w.facility <- dcast(Visit, PatientID  ~ Visit, value.var = "Facility_City")
# Visit.w.facility <- Visit.w.facility[,1:6]
# Visit.w.facility <- Visit.w.facility %>%
#   rename(Fac1 = 2)
# Visit.w.facility <- Visit.w.facility %>%
#   rename(Fac2 = 3)
# Visit.w.facility <- Visit.w.facility %>%
#   rename(Fac3 = 4)
# Visit.w.facility <- Visit.w.facility %>%
#   rename(Fac4 = 5)
# Visit.w.facility <- Visit.w.facility %>%
#   rename(Fac5 = 6)
# Visit.w <- left_join(Visit.w, Visit.w.facility, by="PatientID")
# 
# Visit.w.region <- dcast(Visit, PatientID  ~ Visit, value.var = "Geograpic Region")
# Visit.w.region <- Visit.w.region[,1:2]
# Visit.w <- left_join(Visit.w, Visit.w.region, by="PatientID")
# 
# #rename vars
# Visit.w <- setnames(Visit.w, old = c("1.x","1.y","1.x.x","1.y.y","1.x.x.x","1.y.y.y",1), new=c("1","Age","Gender","Race","Ethnicity","UHF","Region"))
# 
# #select out demographic vars 
# Visit.demo <- Visit.w[, c(1,106:121)]
# 
# #Remove extra datasets
# rm(Visit.w, Visit.w.age, Visit.w.ethnicity, Visit.w.facility, Visit.w.gender, Visit.w.PIG, Visit.w.race, Visit.w.re, Visit.w.region, Visit.w.UHF)

#Saba's coding
# Visit.demo$racecat <- NA
# Visit.demo$racecat[Visit.demo$Race %in% c("Abenaki","Absentee Shawnee", "Apache", "Arapaho", "Caddo","Acoma", 
#                                           "Alamo Navajo", "Canoncito Navajo", "Agdaagux", "Agua Caliente", "Agua Caliente Cahuilla", 
#                                           "Augustine", "Bishop", "Bridgeport", "Cabazon", "Cahto", "Cahuilla", 
#                                           "California Tribes", "Campo", "Capitan Grande", "Ak-Chin", "Arizona Tewa", 
#                                           "Barrio Libre", "Birch Creek", "Brevig Mission", "Ak-Chin", "Arizona Tewa", "Barrio Libre", 
#                                           "Birch Creek", "Brevig Mission", "Alabama Coushatta", "Alabama Creek", "Alabama Quassarte",
#                                           "Allen Canyon", "Alsea", "Arikara", "Aroostook", "Assiniboine", "Assiniboine Sioux", "Atsina", 
#                                           "Blackfoot Sioux", "Attacapa", "Bad River", "Brotherton", "Bannock", "Battle Mountain", 
#                                           "Carson", "Bay Mills Chippewa", "Burt Lake Band", "Burt Lake Chippewa", "Burt Lake Ottawa",
#                                           "Big Cypress", "Brighton", "Biloxi", "Blackfeet", "Bois Forte", "Brule Sioux", "Burns Paiute",
#                                           "Catawba", "Cayuga", "Cayuse", "Cedarville", "Celilo", "Central Pomo", "Chehalis", "Chemakuan",
#                                           "Chemehuevi", "Cherokee", "Cherokee Alabama", "Cherokee Shawnee", "Cherokees of Northeast Alabama",
#                                           "Cherokees of Southeast Alabama", "Cheyenne", "Cheyenne River Sioux", "Cheyenne-Arapaho",
#                                           "Chickahominy", "Chickasaw", "Chimariko", "Chinook", "Chippewa", "Chippewa Cree", 
#                                           "Chiricahua", "Chitimacha", "Choctaw", "Chukchansi", "Chumash", "Citizen Band Potawatomi",
#                                           "Clatsop", "Clear Lake", "Clifton Choctaw", "Coast Miwok", "Coast Yurok", "Cochiti", "Cocopah",
#                                           "Coeur D'Alene", "Coharie", "Colorado River", "Columbia River Chinook", "Colville",
#                                           "Comanche", "Coos", "Coos; Lower Umpqua; Siuslaw", "Coquilles", "Costanoan", "Coushatta",
#                                           "Cow Creek Umpqua", "Cowlitz", "Craig", "Cree", "Creek", "Croatan", "Crow", "Crow Creek Sioux",
#                                           "Cupeno", "Cuyapaipe", "Dakota Sioux", "Delaware", "Diegueno", "Digger", "Dresslerville",
#                                           "Dry Creek", "Duck Valley", "Duckwater", "Duwamish", "Eastern Cherokee", "Eastern Chickahominy",
#                                           "Eastern Creek", "Eastern Delaware", "Eastern Muscogee", "Eastern Pomo", "Eastern Shawnee",
#                                           "Echota Cherokee", "Elko", "Ely", "Esselen", "Etowah Cherokee", "Fallon", "Flandreau Santee",
#                                           "Florida Seminole", "Fond du Lac", "Forest County", "Fort Belknap", "Fort Berthold", "Fort Bidwell",
#                                           "Fort Hall", "Fort Independence", "Fort McDermitt", "Fort Mcdowell", "Fort Peck", 
#                                           "Fort Peck Assiniboine Sioux", "Fort Sill Apache", "French American Indian", "Gabrieleno",
#                                           "Gay Head Wampanoag", "Georgetown (Eastern Tribes)", "Gila Bend", "Gila River Pima-Maricopa",
#                                           "Goshute", "Grand Portage", "Grand Ronde", "Grand Traverse Band of Ottawa/Chippewa",
#                                           "Gros Ventres", "Haliwa", "Hannahville", "Havasupai", "Hidatsa", "Ho-chunk", "Hoh", "Hollywood Seminole",
#                                           "Hoopa", "Hoopa Extension", "Hopi", "Houma", "Hualapai", "Huron Potawatomi", "Illinois Miami",
#                                           "Inaja-Cosmit", "Indian Township", "Indiana Miami", "Iowa", "Iowa of Kansas-Nebraska", 
#                                           "Iowa of Oklahoma", "Iowa Sac and Fox", "Iroquois", "Isleta", "Jamestown", "Jemez",
#                                           "Jena Choctaw", "Jicarilla Apache", "Juaneno", "Kaibab", "Kalapuya", "Kalispel", 
#                                           "Karuk", "Kashia", "Kathlamet", "Kaw", "Kawaiisu", "Keres", "Kern River", "Keweenaw",
#                                           "Kialegee", "Kickapoo", "Kikiallus", "Kiowa", "Klallam", "Klamath", "Konkow", "Kootenai",
#                                           "La Jolla", "La Posta", "Lac Courte Oreilles", "Lac du Flambeau", "Lac Vieux Desert Chippewa",
#                                           "Laguna", "Lake Superior", "Lake Traverse Sioux", "Las Vegas", "Lassik", "Leech Lake", 
#                                           "Lenni-Lenape", "Lipan Apache", "Little Shell Chippewa", "Lone Pine", "Long Island", "Los Coyotes",
#                                           "Lovelock", "Lower Brule Sioux", "Lower Elwha", "Lower Muscogee", "Lower Sioux", "Lower Skagit", 
#                                           "Luiseno", "Lumbee", "Lummi", "Machis Lower Creek Indian", "Maidu", "Makah", "Malheur Paiute",
#                                           "Maliseet", "Mandan", "Manzanita", "Maricopa", "Marshantucket Pequot", "Mashpee Wampanoag",
#                                           "Matinecock", "Mattaponi", "Mattole", "Mdewakanton Sioux", "Menominee", "Mesa Grande", 
#                                           "Mescalero Apache", "Miami", "Miccosukee", "Michigan Ottawa", "Algonquian", "Beaver", 
#                                           "Canadian Indian", "Greenland Eskimo", "Haida", "Micmac", "Mille Lacs", "Miniconjou",
#                                           "Minnesota Chippewa", "Mission Indians", "Mississippi Choctaw", "Missouri Sac and Fox", 
#                                           "Miwok", "Modoc", "Mohave", "Mohawk", "Mohegan", "Molala", "Mono", "Montauk", "Morongo",
#                                           "Mountain Maidu", "Mowa Band of Choctaw", "Muckleshoot", "Munsee", "Nambe", "Narragansett",
#                                           "Natchez", "Nausu Waiwash", "Navajo", "Nebraska Ponca", "Nebraska Winnebago", "Nez Perce", 
#                                           "Nipmuc", "Nishinam", "Nisqually", "Nomalaki", "Nooksack", "Northern Arapaho", "Northern Cherokee",
#                                           "Northern Cheyenne", "Northern Paiute", "Northern Pomo", "Northwest Tribes", "Oglala Sioux",
#                                           "Oklahoma Apache", "Oklahoma Cado", "Oklahoma Choctaw", "Oklahoma Comanche", "Oklahoma Kickapoo",
#                                           "Oklahoma Kiowa", "Oklahoma Miami", "Oklahoma Ottawa", "Oklahoma Pawnee", "Oklahoma Peoria",
#                                           "Oklahoma Ponca", "Oklahoma Sac and Fox", "Oklahoma Seminole", "Omaha", "Oneida", "Onondaga",
#                                           "Ontonagon", "Oregon Athabaskan", "Osage", "Otoe-Missouria", "Ottawa", "Owens Valley", "Paiute", 
#                                           "Pala", "Palauan", "Pamunkey", "Panamint", "Pascua Yaqui", "Passamaquoddy", "Paugussett", "Pauma", 
#                                           "Pawnee", "Payson Apache", "Pawnee", "Payson Apache", "Pechanga", "Pelican", "Penobscot", "Peoria",
#                                           "Pequot", "Perryville", "Picuris", "Pima", "Pine Ridge Sioux", "Pipestone Sioux", "Piro", 
#                                           "Piscataway", "Pit River", "Pleasant Point Passamaquoddy", "Poarch Band", "Pocomoke Acohonock", 
#                                           "Pojoaque", "Pokagon Potawatomi", "Pomo", "Ponca", "Poospatuck", "Port Madison", "Potawatomi", 
#                                           "Powhatan", "Prairie Band", "Prairie Island Sioux", "Principal Creek Indian Nation", "Prior Lake Sioux",
#                                           "Pueblo", "Puget Sound Salish", "Puyallup", "Pyramid Lake", "Quapaw", "Quechan", "Quileute", 
#                                           "Quinault", "Ramah Navajo", "Rampough Mountain", "Red Cliff Chippewa", "Red Lake Chippewa", 
#                                           "Red Wood", "Reno-Sparks", "Rocky Boy's Chippewa Cree", "Rosebud Sioux", "Round Valley",
#                                           "Sac and Fox", "Saginaw Chippewa", "Salinan", "Salish", "Salish and Kootenai", "Salt River Pima-Maricopa",
#                                           "Samish", "San Carlos Apache", "San Felipe", "San Ildefonso", "San Juan Pueblo", "San Juan Southern Paiute",
#                                           "San Manual", "San Pasqual", "Sand Hill", "Sand Point", "Sandia", "Santa Ana", "Santa Clara",
#                                           "Santa Rosa", "Santa Rosa Cahuilla", "Santa Ynez", "Santa Ysabel", "Santee Sioux", "Sauk-Suiattle",
#                                           "Sault Ste. Marie Chippewa", "Schaghticoke", "Scotts Valley", "Seminole", "Seneca", "Seneca Nation",
#                                           "Serrano", "Setauket", "Shasta", "Shawnee", "Shinnecock", "Shoshone", "Shoshone Paiute", "Sioux", 
#                                           "Sisseton-Wahpeton", "Skokomish", "Skull Valley", "Snohomish", "Soboba", "Sokoagon Chippewa",
#                                           "South Fork Shoshone", "Southeastern Indians", "Southern Arapaho", "Southern Cheyenne",
#                                           "Southern Paiute", "Spirit Lake Sioux", "Spokane", "Squaxin Island", "St. Croix Chippewa",
#                                           "Standing Rock Sioux", "Star Clan of Muscogee Creeks", "Steilacoom", "Stillaguamish",
#                                           "Stockbridge", "Sulphur Bank", "Summit Lake", "Suquamish", "Susanville", "Susquehanock",
#                                           "Sycuan", "Table Bluff", "Tachi", "Takelma", "Taos", "Te-Moak Western Shoshone", "Temecula",
#                                           "Tenino", "Tesuque", "Teton Sioux", "Tewa", "Texas Kickapoo", "Thlopthlocco", "Tigua", 
#                                           "Timbi-Sha Shoshone", "Tohono O'Odham", "Tolowa", "Tonawanda Seneca", "Torres-Martinez",
#                                           "Tsimshian", "Tuckabachee", "Tulalip", "Tule River", "Tunica Biloxi", "Turtle Mountain",
#                                           "Tuscarora", "Tuscola", "Twenty-Nine Palms", "Two Kettle Sioux", "Tygh", "Uintah Ute", 
#                                           "Umatilla", "Umpqua", "United Keetowah Band of Cherokee, Upper Chinook", "Upper Sioux",
#                                           "Upper Skagit", "Ute", "Ute Mountain Ute", "Utu Utu Gwaitu Paiute", "Waccamaw-Siousan", 
#                                           "Wahpekute Sioux", "Wahpeton Sioux", "Wailaki", "Wakiakum Chinook", "Walker River", 
#                                           "Walla-Walla", "Wampanoag", "Wappo", "Warm Springs", "Wascopum", "Washakie", "Washoe",
#                                           "Wazhaza Sioux", "Wenatchee", "Western Cherokee", "Western Chickahominy", "Whilkut", "White Earth",
#                                           "White Mountain", "White Mountain Apache", "White Mountain Inupiat", "Wichita", "Wicomico",
#                                           "Willapa Chinook", "Wind River", "Wind River Arapaho", "Wind River Shoshone", "Winnebago",
#                                           "Winnemucca", "Wintun", "Wisconsin Potawatomi", "Wishram", "Wiyot", "Wyandotte", "Yahooskin",
#                                           "Yakama", "Yakama Cowlitz", "Yana", "Yankton Sioux", "Yanktonai Sioux", "Yaqui", "Yavapai",
#                                           "Yavapai Apache", "Yerington Paiute", "Yokuts", "Yomba", "Yuchi", "Yuki", "Yuman", "Yurok",
#                                           "Zia", "Zuni", "Eastern Tribes","Ahtna", "Akhiok", "Akiachak", "Akiak", "Akutan", "Alakanuk", "Alanvik", "Alaska Indian", 
#                                           "Alaska Native", "Alaskan Athabascan", "Alatna", "Aleknagik", "Aleut", "Aleut Corporation",
#                                           "Aleutian", "Aleutian Islander", "Alexander", "Allakaket", "Alutiiq Aleut", "Ambler", 
#                                           "Anaktuvuk", "Anaktuvuk Pass", "Andreafsky", "Angoon", "Aniak", "Anvik", "Arctic", 
#                                           "Arctic Slope Corporation", "Arctic Slope Inupiat", "Atka", "Atmautluak", "Atqasuk",
#                                           "Barrow", "Belkofski", "Bering Straits Inupiat", "Bethel", "Bill Moore's Slough", "Bristol Bay Aleut", 
#                                           "Bristol Bay Yupik", "Buckland", "Calista Yupik", "Cantwell", "Central Council of Tlingit and Haida Tribes",
#                                           "Chefornak", "Chalkyitsik","Chenega", "Chevak", "Chickaloon", "Chignik", "Chignik Lagoon",
#                                           "Chignik Lagoon", "Chignik Lake", "Chilkat", "Chilkoot", "Chinik", "Chistochina", "Chitina",
#                                           "Chuathbaluk", "Chugach Aleut", "Chugach Corporation", "Clark's Point", "Cook Inlet", 
#                                           "Copper Center", "Copper River", "Crooked Creek", "Deering", "Dillingham", "Dot Lake",
#                                           "Doyon", "Eek", "Egegik", "Eklutna", "Ekuk", "Ekwok", "Elim", "Emmonak", "English Bay",
#                                           "Eskimo", "Evansville", "Eyak", "False Pass", "Fort Yukon", "Gakona", "Galena", "Gambell", 
#                                           "Georgetown (Yupik-Eskimo)", "Golovin", "Goodnews Bay", "Grayling", "Gulkana", "Healy Lake",
#                                           "Holy Cross", "Hoonah", "Hooper Bay", "Hughes", "Huslia", "Hydaburg", "Igiugig", "Iliamna",
#                                           "Inalik Diomede", "Inupiaq", "Inupiat Eskimo", "Iqurmuit (Russian Mission)", "Ivanof Bay",
#                                           "Kake", "Kalskag", "Kaltag", "Kasaan", "Kasigluk", "Kawerak", "Kenaitze", "Ketchikan", "Kiana",
#                                           "King Cove", "King Salmon", "Kipnuk", "Kivalina", "Klawock", "Knik", "Kobuk", 
#                                           "Kodiak", "Kokhanok", "Koliganek", "Kongiganak", "Koniag Aleut", "Kotlik", "Kotzebue",
#                                           "Koyuk", "Koyukuk", "Kwethluk", "Kwigillingok", "Kwiguk", "Lake Minchumina", "Larsen Bay",
#                                           "Levelock", "Manley Hot Springs", "Manokotak", "Mary's Igloo", "Mauneluk Inupiat", "Mekoryuk", 
#                                           "Mentasta Lake", "Metlakatla", "Minto", "Mountain Village", "Nana Inupiat", "Napakiak", 
#                                           "Napaskiak", "Napaumute", "Nelson Lagoon", "Nenana", "New Stuyahok", "Newhalen", "Newtok", "Nikolai",
#                                           "Ninilchik", "Noatak", "Nome", "Nondalton", "Noorvik", "Northway", "Nulato", "Nunapitchukv", 
#                                           "Old Harbor", "Oscarville", "Ouzinkie", "Pauloff Harbor", "Pedro Bay", "Petersburg", "Pilot Point",
#                                           "Pitkas Point", "Point Hope", "Point Lay", "Port Graham", "Port Heiden", "Port Lions", "Portage Creek",
#                                           "Qagan Toyagungin", "Qawalangin", "Quinhagak", "Rampart", "Ruby", "Ruby Valley", "Salamatof", "Savoonga",
#                                           "Saxman", "Scammon Bay", "Selawik", "Seldovia", "Shageluk", "Shaktoolik", "Sheldon's Point", "Shishmaref",
#                                           "Shungnak", "Siberian Eskimo", "Siberian Yupik", "Sitka", "Slana", "Sleetmute", "South Naknek", 
#                                           "Southeast Alaska", "St. George", "St. Mary's", "St. Michael", "St. Paul", "Stebbins", "Stevens",
#                                           "Stony River", "Sugpiaq", "Tanaina", "Tanana", "Tanana Chiefs", "Tazlina", "Telida", "Teller",
#                                           "Tenakee Springs", "Tlingit", "Tlingit-Haida", "Tok", "Toksook", "Tulukskak", "Tuntutuliak", "Tununak",
#                                           "Twin Hills", "Tyonek", "Ugashik", "Umkumiate", "Unalakleet", "Unalaska", "Unangan Aleut", "Unga",
#                                           "Venetie", "Wainwright", "Wrangell", "Yakutat", "Yupik Eskimo","Central American Indian", "Mexican American Indian",
#                                           "South American Indian","American Indian", "American Indian or Alaska Native", "Canadian and Latin American Indian", 
#                                           "Chamorro", "Chuukese", "Fijian", "Guamanian", "Kiribati", "Kosraean", "Mariana Islander",
#                                           "Marshall", "Marshallese", "Melanesian", "Micronesian", "Native Hawaiian", 
#                                           "Native Hawaiian or Other Pacific Islander", "New Hebrides", "Other Pacific Islander",
#                                           "Papua New Guinean", "Pohnpeian", "Polynesian", "Saipanese", "Samoan", "Solomon", "Solomon Islander",
#                                           "Tahitian", "Tokelauan", "Tongan", "Yapese", "Guamanian or Chamorro", "Spanish American Indian",
#                                           "United Keetowah Band of Cherokee","Red Devil","Upper Chinook", "Kluti Kaah"," Lower Kalskag", "Nanticoke",
#                                           "Nightmute","Nuiqsut"," Port Gamble Klallam","San Xavier","Scott Valley","Seneca-Cayuga","Siuslaw","Talakamish",
#                                           "Tanacross","Togiak", "Lower Kalskag", "Port Gamble Klallam", "Tetlin")] <- "Native American/Alaskan Native/Pacific Islander"
# 
# Visit.demo$racecat[Visit.demo$Race %in% c("Asian","Bangladeshi", "Bhutanese", "Asian Indian", "Maldivian", "Nepalese", "Pakistani",
#                                           "Sri Lankan","Burmese", "Cambodian", "Indonesian", "Hmong", "Laotian", "Malaysian", "Singaporean",
#                                           "Thailand", "Vietnamese","Chinese", "Iwo Jiman", "Japanese", "Korean", "Okinawan", "Taiwanese","Thai")] <- "Asian"                     
# 
# Visit.demo$racecat[Visit.demo$Race %in% c("African", "Botswanan", "Ethiopian", "Liberian", "Madagascar", "Namibian", "Nigerian",
#                                           "Zairean","African American","Bahamian", "Barbadian", "Douglas", "Haitian", "Jamaican", "Tobagoan", "Trinidadian",
#                                           "West Indian","Black", "Black or African American")] <- "Black/AfrAm"
# 
# Visit.demo$racecat[Visit.demo$Race %in% c("Alpine", "English", "European", "French", "German", "Irish", "Italian", "Moor",
#                                           "Polish", "Scottish", "Wales","Iranian", "Iraqi", "Armenian", "Arab", "Assyrian", "Afghanistani", 
#                                           "Israeili", "Karluk", "Lebanese", "Egyptian", "Middle Eastern or North African", 
#                                           "Palestinian", "Syrian","White")] <-"White"
# 
# 
# Visit.demo$racecat[Visit.demo$Race %in% c("Columbia","Dominica Islander", "Dominican", "Santo Domingo","Filipino","San Juan","Hispanic", "San Juan De")] <-"Hispanic"
# 
# Visit.demo$racecat[Visit.demo$Race %in% c("Declined to Report", "Declined to Specify", "Unreported/Refuse to Report", "Unreported/Refused to Report",
#                                           "Unreported/Refused To Report","Other Race","Carolinian", "Circle", "Council", "Eagle", "Lime", "Mcgrath", "Platinum", "Stewart",
#                                           "Trinity", "Wiseman","Oklahoma Delaware","Siletz","Stonyford","", "Suqpigaq", "Unreported/Refuse To Report")] <- "Other/Unknown"
# # If ethnicity is Hispanic, change race to Hispanic
# Visit.demo$Ethnicity[Visit.demo$Ethnicity == "Unreported/Refused to Report"] <- NA 
# Visit.demo$Ethnicity[Visit.demo$Ethnicity == ""] <- NA 
# 
# Visit.demo$racecat[!is.na(Visit.demo$Ethnicity) & Visit.demo$Ethnicity != "Not Hispanic or Latino"] <- "Hispanic"
# summary(as.factor(Visit.demo$racecat))
# Visit.demo$racecat[is.na(Visit.demo$racecat)] <- "Other/Unknown"
# summary(as.factor(Visit.demo$racecat))
# 
# ### Updating race variable for citymd data based on Saba's code
# updatedrace_orig_race_Oct<-Visit.demo%>%
#   select(PatientID, racecat, Race)
# updatedrace_orig_race_Oct$racecat <- NA
# updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Abenaki","Absentee Shawnee", "Apache", "Arapaho", "Caddo","Acoma", 
#                                                                         "Alamo Navajo", "Canoncito Navajo", "Agdaagux", "Agua Caliente", "Agua Caliente Cahuilla", 
#                                                                         "Augustine", "Bishop", "Bridgeport", "Cabazon", "Cahto", "Cahuilla", 
#                                                                         "California Tribes", "Campo", "Capitan Grande", "Ak-Chin", "Arizona Tewa", 
#                                                                         "Barrio Libre", "Birch Creek", "Brevig Mission", "Ak-Chin", "Arizona Tewa", "Barrio Libre", 
#                                                                         "Birch Creek", "Brevig Mission", "Alabama Coushatta", "Alabama Creek", "Alabama Quassarte",
#                                                                         "Allen Canyon", "Alsea", "Arikara", "Aroostook", "Assiniboine", "Assiniboine Sioux", "Atsina", 
#                                                                         "Blackfoot Sioux", "Attacapa", "Bad River", "Brotherton", "Bannock", "Battle Mountain", 
#                                                                         "Carson", "Bay Mills Chippewa", "Burt Lake Band", "Burt Lake Chippewa", "Burt Lake Ottawa",
#                                                                         "Big Cypress", "Brighton", "Biloxi", "Blackfeet", "Bois Forte", "Brule Sioux", "Burns Paiute",
#                                                                         "Catawba", "Cayuga", "Cayuse", "Cedarville", "Celilo", "Central Pomo", "Chehalis", "Chemakuan",
#                                                                         "Chemehuevi", "Cherokee", "Cherokee Alabama", "Cherokee Shawnee", "Cherokees of Northeast Alabama",
#                                                                         "Cherokees of Southeast Alabama", "Cheyenne", "Cheyenne River Sioux", "Cheyenne-Arapaho",
#                                                                         "Chickahominy", "Chickasaw", "Chimariko", "Chinook", "Chippewa", "Chippewa Cree", 
#                                                                         "Chiricahua", "Chitimacha", "Choctaw", "Chukchansi", "Chumash", "Citizen Band Potawatomi",
#                                                                         "Clatsop", "Clear Lake", "Clifton Choctaw", "Coast Miwok", "Coast Yurok", "Cochiti", "Cocopah",
#                                                                         "Coeur D'Alene", "Coharie", "Colorado River", "Columbia River Chinook", "Colville",
#                                                                         "Comanche", "Coos", "Coos; Lower Umpqua; Siuslaw", "Coquilles", "Costanoan", "Coushatta",
#                                                                         "Cow Creek Umpqua", "Cowlitz", "Craig", "Cree", "Creek", "Croatan", "Crow", "Crow Creek Sioux",
#                                                                         "Cupeno", "Cuyapaipe", "Dakota Sioux", "Delaware", "Diegueno", "Digger", "Dresslerville",
#                                                                         "Dry Creek", "Duck Valley", "Duckwater", "Duwamish", "Eastern Cherokee", "Eastern Chickahominy",
#                                                                         "Eastern Creek", "Eastern Delaware", "Eastern Muscogee", "Eastern Pomo", "Eastern Shawnee",
#                                                                         "Echota Cherokee", "Elko", "Ely", "Esselen", "Etowah Cherokee", "Fallon", "Flandreau Santee",
#                                                                         "Florida Seminole", "Fond du Lac", "Forest County", "Fort Belknap", "Fort Berthold", "Fort Bidwell",
#                                                                         "Fort Hall", "Fort Independence", "Fort McDermitt", "Fort Mcdowell", "Fort Peck", 
#                                                                         "Fort Peck Assiniboine Sioux", "Fort Sill Apache", "French American Indian", "Gabrieleno",
#                                                                         "Gay Head Wampanoag", "Georgetown (Eastern Tribes)", "Gila Bend", "Gila River Pima-Maricopa",
#                                                                         "Goshute", "Grand Portage", "Grand Ronde", "Grand Traverse Band of Ottawa/Chippewa",
#                                                                         "Gros Ventres", "Haliwa", "Hannahville", "Havasupai", "Hidatsa", "Ho-chunk", "Hoh", "Hollywood Seminole",
#                                                                         "Hoopa", "Hoopa Extension", "Hopi", "Houma", "Hualapai", "Huron Potawatomi", "Illinois Miami",
#                                                                         "Inaja-Cosmit", "Indian Township", "Indiana Miami", "Iowa", "Iowa of Kansas-Nebraska", 
#                                                                         "Iowa of Oklahoma", "Iowa Sac and Fox", "Iroquois", "Isleta", "Jamestown", "Jemez",
#                                                                         "Jena Choctaw", "Jicarilla Apache", "Juaneno", "Kaibab", "Kalapuya", "Kalispel", 
#                                                                         "Karuk", "Kashia", "Kathlamet", "Kaw", "Kawaiisu", "Keres", "Kern River", "Keweenaw",
#                                                                         "Kialegee", "Kickapoo", "Kikiallus", "Kiowa", "Klallam", "Klamath", "Konkow", "Kootenai",
#                                                                         "La Jolla", "La Posta", "Lac Courte Oreilles", "Lac du Flambeau", "Lac Vieux Desert Chippewa",
#                                                                         "Laguna", "Lake Superior", "Lake Traverse Sioux", "Las Vegas", "Lassik", "Leech Lake", 
#                                                                         "Lenni-Lenape", "Lipan Apache", "Little Shell Chippewa", "Lone Pine", "Long Island", "Los Coyotes",
#                                                                         "Lovelock", "Lower Brule Sioux", "Lower Elwha", "Lower Muscogee", "Lower Sioux", "Lower Skagit", 
#                                                                         "Luiseno", "Lumbee", "Lummi", "Machis Lower Creek Indian", "Maidu", "Makah", "Malheur Paiute",
#                                                                         "Maliseet", "Mandan", "Manzanita", "Maricopa", "Marshantucket Pequot", "Mashpee Wampanoag",
#                                                                         "Matinecock", "Mattaponi", "Mattole", "Mdewakanton Sioux", "Menominee", "Mesa Grande", 
#                                                                         "Mescalero Apache", "Miami", "Miccosukee", "Michigan Ottawa", "Algonquian", "Beaver", 
#                                                                         "Canadian Indian", "Greenland Eskimo", "Haida", "Micmac", "Mille Lacs", "Miniconjou",
#                                                                         "Minnesota Chippewa", "Mission Indians", "Mississippi Choctaw", "Missouri Sac and Fox", 
#                                                                         "Miwok", "Modoc", "Mohave", "Mohawk", "Mohegan", "Molala", "Mono", "Montauk", "Morongo",
#                                                                         "Mountain Maidu", "Mowa Band of Choctaw", "Muckleshoot", "Munsee", "Nambe", "Narragansett",
#                                                                         "Natchez", "Nausu Waiwash", "Navajo", "Nebraska Ponca", "Nebraska Winnebago", "Nez Perce", 
#                                                                         "Nipmuc", "Nishinam", "Nisqually", "Nomalaki", "Nooksack", "Northern Arapaho", "Northern Cherokee",
#                                                                         "Northern Cheyenne", "Northern Paiute", "Northern Pomo", "Northwest Tribes", "Oglala Sioux",
#                                                                         "Oklahoma Apache", "Oklahoma Cado", "Oklahoma Choctaw", "Oklahoma Comanche", "Oklahoma Kickapoo",
#                                                                         "Oklahoma Kiowa", "Oklahoma Miami", "Oklahoma Ottawa", "Oklahoma Pawnee", "Oklahoma Peoria",
#                                                                         "Oklahoma Ponca", "Oklahoma Sac and Fox", "Oklahoma Seminole", "Omaha", "Oneida", "Onondaga",
#                                                                         "Ontonagon", "Oregon Athabaskan", "Osage", "Otoe-Missouria", "Ottawa", "Owens Valley", "Paiute", 
#                                                                         "Pala", "Palauan", "Pamunkey", "Panamint", "Pascua Yaqui", "Passamaquoddy", "Paugussett", "Pauma", 
#                                                                         "Pawnee", "Payson Apache", "Pawnee", "Payson Apache", "Pechanga", "Pelican", "Penobscot", "Peoria",
#                                                                         "Pequot", "Perryville", "Picuris", "Pima", "Pine Ridge Sioux", "Pipestone Sioux", "Piro", 
#                                                                         "Piscataway", "Pit River", "Pleasant Point Passamaquoddy", "Poarch Band", "Pocomoke Acohonock", 
#                                                                         "Pojoaque", "Pokagon Potawatomi", "Pomo", "Ponca", "Poospatuck", "Port Madison", "Potawatomi", 
#                                                                         "Powhatan", "Prairie Band", "Prairie Island Sioux", "Principal Creek Indian Nation", "Prior Lake Sioux",
#                                                                         "Pueblo", "Puget Sound Salish", "Puyallup", "Pyramid Lake", "Quapaw", "Quechan", "Quileute", 
#                                                                         "Quinault", "Ramah Navajo", "Rampough Mountain", "Red Cliff Chippewa", "Red Lake Chippewa", 
#                                                                         "Red Wood", "Reno-Sparks", "Rocky Boy's Chippewa Cree", "Rosebud Sioux", "Round Valley",
#                                                                         "Sac and Fox", "Saginaw Chippewa", "Salinan", "Salish", "Salish and Kootenai", "Salt River Pima-Maricopa",
#                                                                         "Samish", "San Carlos Apache", "San Felipe", "San Ildefonso", "San Juan Pueblo", "San Juan Southern Paiute",
#                                                                         "San Manual", "San Pasqual", "Sand Hill", "Sand Point", "Sandia", "Santa Ana", "Santa Clara",
#                                                                         "Santa Rosa", "Santa Rosa Cahuilla", "Santa Ynez", "Santa Ysabel", "Santee Sioux", "Sauk-Suiattle",
#                                                                         "Sault Ste. Marie Chippewa", "Schaghticoke", "Scotts Valley", "Seminole", "Seneca", "Seneca Nation",
#                                                                         "Serrano", "Setauket", "Shasta", "Shawnee", "Shinnecock", "Shoshone", "Shoshone Paiute", "Sioux", 
#                                                                         "Sisseton-Wahpeton", "Skokomish", "Skull Valley", "Snohomish", "Soboba", "Sokoagon Chippewa",
#                                                                         "South Fork Shoshone", "Southeastern Indians", "Southern Arapaho", "Southern Cheyenne",
#                                                                         "Southern Paiute", "Spirit Lake Sioux", "Spokane", "Squaxin Island", "St. Croix Chippewa",
#                                                                         "Standing Rock Sioux", "Star Clan of Muscogee Creeks", "Steilacoom", "Stillaguamish",
#                                                                         "Stockbridge", "Sulphur Bank", "Summit Lake", "Suquamish", "Susanville", "Susquehanock",
#                                                                         "Sycuan", "Table Bluff", "Tachi", "Takelma", "Taos", "Te-Moak Western Shoshone", "Temecula",
#                                                                         "Tenino", "Tesuque", "Teton Sioux", "Tewa", "Texas Kickapoo", "Thlopthlocco", "Tigua", 
#                                                                         "Timbi-Sha Shoshone", "Tohono O'Odham", "Tolowa", "Tonawanda Seneca", "Torres-Martinez",
#                                                                         "Tsimshian", "Tuckabachee", "Tulalip", "Tule River", "Tunica Biloxi", "Turtle Mountain",
#                                                                         "Tuscarora", "Tuscola", "Twenty-Nine Palms", "Two Kettle Sioux", "Tygh", "Uintah Ute", 
#                                                                         "Umatilla", "Umpqua", "United Keetowah Band of Cherokee, Upper Chinook", "Upper Sioux",
#                                                                         "Upper Skagit", "Ute", "Ute Mountain Ute", "Utu Utu Gwaitu Paiute", "Waccamaw-Siousan", 
#                                                                         "Wahpekute Sioux", "Wahpeton Sioux", "Wailaki", "Wakiakum Chinook", "Walker River", 
#                                                                         "Walla-Walla", "Wampanoag", "Wappo", "Warm Springs", "Wascopum", "Washakie", "Washoe",
#                                                                         "Wazhaza Sioux", "Wenatchee", "Western Cherokee", "Western Chickahominy", "Whilkut", "White Earth",
#                                                                         "White Mountain", "White Mountain Apache", "White Mountain Inupiat", "Wichita", "Wicomico",
#                                                                         "Willapa Chinook", "Wind River", "Wind River Arapaho", "Wind River Shoshone", "Winnebago",
#                                                                         "Winnemucca", "Wintun", "Wisconsin Potawatomi", "Wishram", "Wiyot", "Wyandotte", "Yahooskin",
#                                                                         "Yakama", "Yakama Cowlitz", "Yana", "Yankton Sioux", "Yanktonai Sioux", "Yaqui", "Yavapai",
#                                                                         "Yavapai Apache", "Yerington Paiute", "Yokuts", "Yomba", "Yuchi", "Yuki", "Yuman", "Yurok",
#                                                                         "Zia", "Zuni", "Eastern Tribes","Ahtna", "Akhiok", "Akiachak", "Akiak", "Akutan", "Alakanuk", "Alanvik", "Alaska Indian", 
#                                                                         "Alaska Native", "Alaskan Athabascan", "Alatna", "Aleknagik", "Aleut", "Aleut Corporation",
#                                                                         "Aleutian", "Aleutian Islander", "Alexander", "Allakaket", "Alutiiq Aleut", "Ambler", 
#                                                                         "Anaktuvuk", "Anaktuvuk Pass", "Andreafsky", "Angoon", "Aniak", "Anvik", "Arctic", 
#                                                                         "Arctic Slope Corporation", "Arctic Slope Inupiat", "Atka", "Atmautluak", "Atqasuk",
#                                                                         "Barrow", "Belkofski", "Bering Straits Inupiat", "Bethel", "Bill Moore's Slough", "Bristol Bay Aleut", 
#                                                                         "Bristol Bay Yupik", "Buckland", "Calista Yupik", "Cantwell", "Central Council of Tlingit and Haida Tribes",
#                                                                         "Chefornak", "Chalkyitsik","Chenega", "Chevak", "Chickaloon", "Chignik", "Chignik Lagoon",
#                                                                         "Chignik Lagoon", "Chignik Lake", "Chilkat", "Chilkoot", "Chinik", "Chistochina", "Chitina",
#                                                                         "Chuathbaluk", "Chugach Aleut", "Chugach Corporation", "Clark's Point", "Cook Inlet", 
#                                                                         "Copper Center", "Copper River", "Crooked Creek", "Deering", "Dillingham", "Dot Lake",
#                                                                         "Doyon", "Eek", "Egegik", "Eklutna", "Ekuk", "Ekwok", "Elim", "Emmonak", "English Bay",
#                                                                         "Eskimo", "Evansville", "Eyak", "False Pass", "Fort Yukon", "Gakona", "Galena", "Gambell", 
#                                                                         "Georgetown (Yupik-Eskimo)", "Golovin", "Goodnews Bay", "Grayling", "Gulkana", "Healy Lake",
#                                                                         "Holy Cross", "Hoonah", "Hooper Bay", "Hughes", "Huslia", "Hydaburg", "Igiugig", "Iliamna",
#                                                                         "Inalik Diomede", "Inupiaq", "Inupiat Eskimo", "Iqurmuit (Russian Mission)", "Ivanof Bay",
#                                                                         "Kake", "Kalskag", "Kaltag", "Kasaan", "Kasigluk", "Kawerak", "Kenaitze", "Ketchikan", "Kiana",
#                                                                         "King Cove", "King Salmon", "Kipnuk", "Kivalina", "Klawock", "Knik", "Kobuk", 
#                                                                         "Kodiak", "Kokhanok", "Koliganek", "Kongiganak", "Koniag Aleut", "Kotlik", "Kotzebue",
#                                                                         "Koyuk", "Koyukuk", "Kwethluk", "Kwigillingok", "Kwiguk", "Lake Minchumina", "Larsen Bay",
#                                                                         "Levelock", "Manley Hot Springs", "Manokotak", "Mary's Igloo", "Mauneluk Inupiat", "Mekoryuk", 
#                                                                         "Mentasta Lake", "Metlakatla", "Minto", "Mountain Village", "Nana Inupiat", "Napakiak", 
#                                                                         "Napaskiak", "Napaumute", "Nelson Lagoon", "Nenana", "New Stuyahok", "Newhalen", "Newtok", "Nikolai",
#                                                                         "Ninilchik", "Noatak", "Nome", "Nondalton", "Noorvik", "Northway", "Nulato", "Nunapitchukv", 
#                                                                         "Old Harbor", "Oscarville", "Ouzinkie", "Pauloff Harbor", "Pedro Bay", "Petersburg", "Pilot Point",
#                                                                         "Pitkas Point", "Point Hope", "Point Lay", "Port Graham", "Port Heiden", "Port Lions", "Portage Creek",
#                                                                         "Qagan Toyagungin", "Qawalangin", "Quinhagak", "Rampart", "Ruby", "Ruby Valley", "Salamatof", "Savoonga",
#                                                                         "Saxman", "Scammon Bay", "Selawik", "Seldovia", "Shageluk", "Shaktoolik", "Sheldon's Point", "Shishmaref",
#                                                                         "Shungnak", "Siberian Eskimo", "Siberian Yupik", "Sitka", "Slana", "Sleetmute", "South Naknek", 
#                                                                         "Southeast Alaska", "St. George", "St. Mary's", "St. Michael", "St. Paul", "Stebbins", "Stevens",
#                                                                         "Stony River", "Sugpiaq", "Tanaina", "Tanana", "Tanana Chiefs", "Tazlina", "Telida", "Teller",
#                                                                         "Tenakee Springs", "Tlingit", "Tlingit-Haida", "Tok", "Toksook", "Tulukskak", "Tuntutuliak", "Tununak",
#                                                                         "Twin Hills", "Tyonek", "Ugashik", "Umkumiate", "Unalakleet", "Unalaska", "Unangan Aleut", "Unga",
#                                                                         "Venetie", "Wainwright", "Wrangell", "Yakutat", "Yupik Eskimo","Central American Indian", "Mexican American Indian",
#                                                                         "South American Indian","American Indian", "American Indian or Alaska Native", "Canadian and Latin American Indian", 
#                                                                         "Chamorro", "Chuukese", "Fijian", "Guamanian", "Kiribati", "Kosraean", "Mariana Islander",
#                                                                         "Marshall", "Marshallese", "Melanesian", "Micronesian", "Native Hawaiian", 
#                                                                         "Native Hawaiian or Other Pacific Islander", "New Hebrides", "Other Pacific Islander",
#                                                                         "Papua New Guinean", "Pohnpeian", "Polynesian", "Saipanese", "Samoan", "Solomon", "Solomon Islander",
#                                                                         "Tahitian", "Tokelauan", "Tongan", "Yapese", "Guamanian or Chamorro", "Spanish American Indian",
#                                                                         "United Keetowah Band of Cherokee","Red Devil","Upper Chinook", "Kluti Kaah"," Lower Kalskag", "Nanticoke",
#                                                                         "Nightmute","Nuiqsut"," Port Gamble Klallam","San Xavier","Scott Valley","Seneca-Cayuga","Siuslaw","Talakamish",
#                                                                         "Tanacross","Togiak", "Lower Kalskag", "Port Gamble Klallam", "Tetlin")] <- "Native American/Alaskan Native/Pacific Islander"
# 
# updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Asian","Bangladeshi", "Bhutanese", "Asian Indian", "Maldivian", "Nepalese", "Pakistani",
#                                                                         "Sri Lankan","Burmese", "Cambodian", "Indonesian", "Hmong", "Laotian", "Malaysian", "Singaporean",
#                                                                         "Thailand", "Vietnamese","Chinese", "Iwo Jiman", "Japanese", "Korean", "Okinawan", "Taiwanese","Thai")] <- "Asian"                     
# 
# updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("African", "Botswanan", "Ethiopian", "Liberian", "Madagascar", "Namibian", "Nigerian",
#                                                                         "Zairean","African American","Bahamian", "Barbadian", "Douglas", "Haitian", "Jamaican", "Tobagoan", "Trinidadian",
#                                                                         "West Indian","Black", "Black or African American")] <- "Black/AfrAm"
# 
# updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Alpine", "English", "European", "French", "German", "Irish", "Italian", "Moor",
#                                                                         "Polish", "Scottish", "Wales","Iranian", "Iraqi", "Armenian", "Arab", "Assyrian", "Afghanistani", 
#                                                                         "Israeili", "Karluk", "Lebanese", "Egyptian", "Middle Eastern or North African", 
#                                                                         "Palestinian", "Syrian","White")] <-"White"
# 
# 
# updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Columbia","Dominica Islander", "Dominican", "Santo Domingo","Filipino","San Juan","Hispanic", "San Juan De")] <-"Hispanic"
# 
# updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Declined to Report", "Declined to Specify", "Unreported/Refuse to Report", "Unreported/Refused to Report",
#                                                                         "Unreported/Refused To Report","Other Race","Carolinian", "Circle", "Council", "Eagle", "Lime", "Mcgrath", "Platinum", "Stewart",
#                                                                         "Trinity", "Wiseman","Oklahoma Delaware","Siletz","Stonyford","", "Suqpigaq", "Unreported/Refuse To Report")] <- "Other/Unknown"
# # If ethnicity is Hispanic, change race to Hispanic
# updatedrace_orig_race_Oct$Ethnicity[updatedrace_orig_race_Oct$Ethnicity == "Unreported/Refused to Report"] <- NA 
# updatedrace_orig_race_Oct$Ethnicity[updatedrace_orig_race_Oct$Ethnicity == ""] <- NA 
# 
# updatedrace_orig_race_Oct$racecat[!is.na(updatedrace_orig_race_Oct$Ethnicity) & updatedrace_orig_race_Oct$Ethnicity != "Not Hispanic or Latino"] <- "Hispanic"
# summary(as.factor(updatedrace_orig_race_Oct$racecat))
# updatedrace_orig_race_Oct$racecat[is.na(updatedrace_orig_race_Oct$racecat)] <- "Other/Unknown"
# summary(as.factor(updatedrace_orig_race_Oct$racecat))
# 
# #Remove rows that are Other/Unknown
# updatedrace_orig_race_Oct %>%
#   filter(racecat!="Other/Unknown") -> updated.race
# 
# #merged updated races with original data
# updated.race %>%
#   select(PatientID, racecat) -> updated.race
# 
# updated.race %>%
#   rename(new_racecat = racecat) -> updated.race
# 
# Visit.demo <- left_join(Visit.demo, updated.race, by = "PatientID")
# 
# #update racecat var
# Visit.demo$racecat <- if_else(Visit.demo$racecat=="Other/Unknown" & !(is.na(Visit.demo$new_racecat)), Visit.demo$new_racecat, Visit.demo$racecat)
# 
# #Removing updated race tables
# rm(updated.race, updatedrace_orig_race_Oct)
# 
# #Saving updated visit file
# saveRDS(Visit.demo, file = "Visit_demo_updatedrace.RDS")

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

COVIDResults%>%
  filter(Lab.Result.Interpretation=="POSITIVE" | Lab.Result.Interpretation=="NEGATIVE")->COVIDResults

COVIDResults%>%
  filter(Confirmatory.PCR=="1")%>%
  filter(Test_date >= "2020-03-01" & Test_date<="2021-10-31")%>%
  distinct()-> COVIDResults_confPCR     #959750     

COVIDResults_confPCR <- COVIDResults_confPCR %>%
  group_by(PatientID) %>%
  arrange(Test_date, .by_group=TRUE)%>%
  mutate(Visit = 1:n())

#Saving COVID Results file
saveRDS(COVIDResults_confPCR, file = "COVIDResults_confPCR.RDS")

###MERGING DATA####
#Merge Visit data with covid results to get patient IDs for all results 
merged.data1 <- inner_join(COVIDResults_confPCR, Visit, by="PatientID") #innerjoin because out of state covid results need to be filtered out (6994684)

merged.data1<-merged.data1%>%
  mutate(antigen_result=case_when(Lab.Result.Interpretation=="NEGATIVE" & Result.PCR=="NEGATIVE" ~ "TN",
                                  Lab.Result.Interpretation=="POSITIVE" & Result.PCR=="POSITIVE" ~ "TP",
                                  Lab.Result.Interpretation=="POSITIVE" & Result.PCR=="NEGATIVE" ~ "FP",
                                  Lab.Result.Interpretation=="NEGATIVE" & Result.PCR=="POSITIVE" ~ "FN"))

#merge vaccine and visit data
merged.data2 <- left_join(merged.data1, ChiefComplaint_vax, by="VisitID") #9226028

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

merged.data3<-distinct(merged.data3)

#Creating vaccination status: fully, partially, unvaccinated
###Identifying those who have incongruous vax status####
###Cleaning vaccination status###
#First dataset: yes vaccinated
Vacc_yes<-merged.data3%>%
  filter(Vax_rec=="YES")%>%
  select(VisitID, PatientID, Test_date, Vax_rec)%>%
  distinct()

#Second dataset: not vaccinated
Vacc_no<-merged.data3%>%
  filter(Vax_rec=="NO")%>%
  select(VisitID, PatientID, Test_date, Vax_rec)%>%
  distinct()

#Third dataset: No response to vaccination question
Vacc_noresponse<-merged.data3%>%
  filter(Vax_rec=="NO RESPONSE")%>%
  select(VisitID, PatientID, Test_date, Vax_rec)%>%
  distinct()

#Fourth datasaet: NA vaccinated
Vacc_NA<-merged.data3%>%
  filter(Vax_rec=="Unknown")%>%
  select(VisitID, PatientID, Test_date, Vax_rec)%>%
  distinct()

#Did someone that said yes also have no to the vaccination question?
yesnovacc<-inner_join(Vacc_yes, Vacc_no, by="PatientID")
yesnovacc<-yesnovacc%>%
  mutate(timebtwn=difftime(Test_date.x, Test_date.y, units="days"))

update_yes_1<-yesnovacc%>%
  filter(timebtwn<0)%>%
  select(VisitID.y)%>%
  add_column(updated_vax_rec="YES")%>%
  rename(VisitID=VisitID.y)%>%
  distinct()

#Did someone that said yes also have a no response?
#No
yesnoresponsevacc<-inner_join(Vacc_yes, Vacc_noresponse, by="PatientID")

yesnoresponsevacc<-yesnoresponsevacc%>%
  mutate(timebtwn=difftime(Test_date.x, Test_date.y, units="days"))

update_yes_2<-yesnoresponsevacc%>%
  filter(timebtwn<0)%>%
  select(VisitID.y)%>%
  add_column(updated_vax_rec="YES")%>%
  rename(VisitID=VisitID.y)%>%
  distinct()

#Which na IDs respond yes earlier?
anyna<-inner_join(Vacc_yes, Vacc_NA, by="PatientID")

anyna<-anyna%>%
  mutate(timebtwn=difftime(Test_date.x, Test_date.y, units="days"))

update_yes_3<-anyna%>%
  filter(timebtwn<0)%>%
  select(VisitID.y)%>%
  add_column(updated_vax_rec="YES")%>%
  rename(VisitID=VisitID.y)%>%
  distinct()

update_yes<-rbind(update_yes_1, update_yes_2, update_yes_3)

update_yes<-update_yes%>%
  distinct()

vax_1<-left_join(merged.data3, update_yes, by="VisitID")

#Updating vaccination statuses that were incongruous between datasets
main<-vax_1%>%
  mutate(updated_vax_rec=ifelse(is.na(updated_vax_rec), Vax_rec, updated_vax_rec))

main<-main%>%
  mutate(vax_status=case_when(Fully_vax=="YES" ~ "Fully vaccinated",
                              Fully_vax!="YES"& updated_vax_rec=="YES" ~ "Partially vaccinated",
                              Fully_vax!="YES" & (updated_vax_rec=="NO"|updated_vax_rec=="NO RESPONSE") ~ "Unvaccinated"))

###Creating updated vax status in main dataset####
main<-main%>%
  rename(vax_status_raw=vax_status)

main<-main%>%
  mutate(vax_status=ifelse(Test_date<"2021-04-01", "N/A", vax_status_raw))

###Saving cleaned final vax and main datasets####
write_rds(main, "main_2.11.22.RDS")

vax<-main%>%
  filter(Test_date >= "2021-04-01" & !is.na(vax_status))

write_rds(main, "vax_2.11.22.RDS")
####Load cleaned final dataset####
#Whole dataset
main<-readRDS("main_2.11.22.RDS")
#Vaccinated dataset
vax<-readRDS("vax_2.11.22.RDS")
####TABLES AND ANALYSES####
#Table 1 clinical characteristics of patient for each testing visit
#need to add race
look<-main%>%
  filter(Visit>1)
table1<-main%>%
  select(Age, agegrp_20, Gender, Symptomatic, vax_status, antigen_result)
table1<-tbl_summary(table1,
                    by="antigen_result",
                    percent = "row")%>%
  add_overall()
table1

#Table 2, disease characteristics by testing result
table2<-main%>%
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
table3<-main%>%
  select(Symptomatic, Vax_rec, Fully_vax, antigen_result, Updated_Date)%>%
  ungroup()

table3<-tbl_summary(table3,
                    by="Symptomatic",
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

month_sens<-month_all%>%
  select(sensitivity, Month, name)%>%
  mutate(Performance=ifelse(name=="antigen_result", "Sensitivity", NA))%>%
  rename(Test_result=sensitivity)

month_spec<-month_all%>%
  select(specificity, Month, name)%>%
  mutate(Performance=ifelse(name=="antigen_result", "Specificity", NA))%>%
  rename(Test_result=specificity)

spec_sens<-rbind(month_sens, month_spec)

#Making sensitivity and specificity wide - no longer needed
# sens_wide<-spec_sens%>%
#   filter(Performance=="sensitivity")%>%
#   rename(sens=Test_result)
# 
# spec_wide<-spec_sens%>%
#   filter(Performance=="specificity")%>%
#   rename(spec=Test_result)
# 
# sens_spec_wide<-inner_join(sens_wide, spec_wide, by="Month")

#Separate plots of sensitivity and specificity
sensitivity.line<-ggplot(data=month_sens, aes(x=Month, y=Test_result, group=1))+
  geom_line()+
  geom_point()

specificity.line<-ggplot(data=month_spec, aes(x=Month, y=Test_result, group=1))+
  geom_line()+
  geom_point()

#Combined line graph of sensitivity and specificity
spec_sens<-spec_sens%>%
  mutate(Performance=fct_rev(Performance))
all.line<-ggplot(data=spec_sens, aes(x=Month, y=Test_result, group=Performance))+
  geom_line(aes(color=Performance))+
  scale_color_manual(values=c('Blue','Red'))+
  geom_point()+
  xlab("Date")+
  ylab("Performance (%)") + 
  theme(axis.text.x = element_text(size = 11))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  ggtitle("A")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))

####Figure 2: Vaccination status spec sens over time####
#Fully vaccinated
april_2021<-full_vax%>%
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

may_2021<-full_vax%>%
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

june_2021<-full_vax%>%
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

july_2021<-full_vax%>%
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

august_2021<-full_vax%>%
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


september_2021<-full_vax%>%
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

october_2021<-full_vax%>%
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

full_vax_month_all<-rbind(april_2021, may_2021, june_2021, july_2021, august_2021, september_2021, october_2021)

full_vax_month_all<-full_vax_month_all%>%
  ungroup()%>%
  mutate(Month=as.factor(Month))%>%
  mutate(Month=Month, levels=c(
    "April 2021",
    "May 2021",
    "June 2021",
    "July 2021",
    "August 2021",
    "September 2021",
    "October 2021"))
full_vax_month_all$Month <- factor(full_vax_month_all$Month, levels=c(
 "April 2021",
  "May 2021",
  "June 2021",
  "July 2021",
  "August 2021",
  "September 2021",
  "October 2021"))

full_vax_month_sens<-full_vax_month_all%>%
  select(sensitivity, Month, name)%>%
  mutate(Performance=ifelse(name=="antigen_result", "Sensitivity", NA))%>%
  rename(Test_result=sensitivity)

full_vax_month_spec<-full_vax_month_all%>%
  select(specificity, Month, name)%>%
  mutate(Performance=ifelse(name=="antigen_result", "Specificity", NA))%>%
  rename(Test_result=specificity)

full_vax_spec_sens<-rbind(full_vax_month_sens, full_vax_month_spec)

#Combined line graph of sensitivity and specificity
full_vax_spec_sens<-full_vax_spec_sens%>%
  mutate(Performance=fct_rev(Performance))

full_vax_time_graph<-ggplot(data=full_vax_spec_sens, aes(x=Month, y=Test_result, group=Performance))+
  geom_line(aes(color=Performance))+
  scale_color_manual(values=c('Blue','Red'))+
  geom_point()+
  xlab("Date")+
  ylab("Performance (%)") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  ggtitle("B")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))

#Partially vaccinated
part_vax<-vax%>%
  filter(vax_status=="Partially vaccinated")

april_2021<-part_vax%>%
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

may_2021<-part_vax%>%
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

june_2021<-part_vax%>%
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

july_2021<-part_vax%>%
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

august_2021<-part_vax%>%
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


september_2021<-part_vax%>%
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

october_2021<-part_vax%>%
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

part_vax_month_all<-rbind(april_2021, may_2021, june_2021, july_2021, august_2021, september_2021, october_2021)

part_vax_month_all<-part_vax_month_all%>%
  ungroup()%>%
  mutate(Month=as.factor(Month))%>%
  mutate(Month=Month, levels=c(
    "April 2021",
    "May 2021",
    "June 2021",
    "July 2021",
    "August 2021",
    "September 2021",
    "October 2021"))
part_vax_month_all$Month <- factor(part_vax_month_all$Month, levels=c(
  "April 2021",
  "May 2021",
  "June 2021",
  "July 2021",
  "August 2021",
  "September 2021",
  "October 2021"))

part_vax_month_sens<-part_vax_month_all%>%
  select(sensitivity, Month, name)%>%
  mutate(Performance=ifelse(name=="antigen_result", "Sensitivity", NA))%>%
  rename(Test_result=sensitivity)

part_vax_month_spec<-part_vax_month_all%>%
  select(specificity, Month, name)%>%
  mutate(Performance=ifelse(name=="antigen_result", "Specificity", NA))%>%
  rename(Test_result=specificity)

part_vax_spec_sens<-rbind(part_vax_month_sens, part_vax_month_spec)

#Combined line graph of sensitivity and specificity
part_vax_spec_sens<-part_vax_spec_sens%>%
  mutate(Performance=fct_rev(Performance))

part_vax_time_graph<-ggplot(data=part_vax_spec_sens, aes(x=Month, y=Test_result, group=Performance))+
  geom_line(aes(color=Performance))+
  scale_color_manual(values=c('Blue','Red'))+
  geom_point()+
  xlab("Date")+
  ylab("Performance (%)") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  ggtitle("C")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))

#Unvaccinated
un_vax<-vax%>%
  filter(vax_status=="Unvaccinated")

april_2021<-un_vax%>%
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

may_2021<-un_vax%>%
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

june_2021<-un_vax%>%
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

july_2021<-un_vax%>%
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

august_2021<-un_vax%>%
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


september_2021<-un_vax%>%
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

october_2021<-un_vax%>%
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

un_vax_month_all<-rbind(april_2021, may_2021, june_2021, july_2021, august_2021, september_2021, october_2021)

un_vax_month_all<-un_vax_month_all%>%
  ungroup()%>%
  mutate(Month=as.factor(Month))%>%
  mutate(Month=Month, levels=c(
    "April 2021",
    "May 2021",
    "June 2021",
    "July 2021",
    "August 2021",
    "September 2021",
    "October 2021"))
un_vax_month_all$Month <- factor(un_vax_month_all$Month, levels=c(
  "April 2021",
  "May 2021",
  "June 2021",
  "July 2021",
  "August 2021",
  "September 2021",
  "October 2021"))

un_vax_month_sens<-un_vax_month_all%>%
  select(sensitivity, Month, name)%>%
  mutate(Performance=ifelse(name=="antigen_result", "Sensitivity", NA))%>%
  rename(Test_result=sensitivity)

un_vax_month_spec<-un_vax_month_all%>%
  select(specificity, Month, name)%>%
  mutate(Performance=ifelse(name=="antigen_result", "Specificity", NA))%>%
  rename(Test_result=specificity)

un_vax_spec_sens<-rbind(un_vax_month_sens, un_vax_month_spec)

#Combined line graph of sensitivity and specificity
un_vax_spec_sens<-un_vax_spec_sens%>%
  mutate(Performance=fct_rev(Performance))

un_vax_time_graph<-ggplot(data=un_vax_spec_sens, aes(x=Month, y=Test_result, group=Performance))+
  geom_line(aes(color=Performance))+
  scale_color_manual(values=c('Blue','Red'))+
  geom_point()+
  xlab("Date")+
  ylab("Performance (%)") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  ggtitle("D")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))

##Arranging plots so main is on top, 3 vax on bottom.
lay <- rbind(c(1,1,1),
             c(2,3,4))

gridExtra::grid.arrange(all.line, full_vax_time_graph, part_vax_time_graph, un_vax_time_graph, layout_matrix = lay)

###Adding rectangle
grid::grid.rect(.5,.5,width=unit(.99,"npc"), height=unit(0.99,"npc"), 
                gp=grid::gpar(lwd=3, fill=NA, col="black"))

#Supplementary table
#Table by age groups in 20 yr intervals
main<-main%>%
  mutate(agegrp_20=case_when(Age<20 ~ "Under 20 yrs",
                             Age>=20 & Age <40 ~ "Between 20 and 40 yrs",
                             Age>=40 & Age<60 ~ "Between 40 and 60 yrs",
                             Age>=60 & Age<80 ~ "Between 60 and 80 yrs",
                             Age>=80 ~ "Over 80 yrs"))

under_20<-main%>%
  filter(agegrp_20=="Under 20 yrs")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(age_grp="Under 20 yrs")

btwn_20_40<-main%>%
  filter(agegrp_20=="Between 20 and 40 yrs")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(age_grp="Between 20 and 40 yrs")

btwn_40_60<-main%>%
  filter(agegrp_20=="Between 40 and 60 yrs")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(age_grp="Between 40 and 60 yrs")

btwn_60_80<-main%>%
  filter(agegrp_20=="Between 60 and 80 yrs")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(age_grp="Between 60 and 80 yrs")

over_80<-main%>%
  filter(agegrp_20=="Over 80 yrs")%>%
  select(antigen_result)%>%
  pivot_longer(cols=c(antigen_result))%>%
  count(name, value)%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from=value, values_from=n)%>%
  group_by(name)%>%
  mutate(sensitivity=(TP/(TP+FN))*100,
         specificity=(TN/(TN+FP))*100)%>%
  add_column(age_grp="Over 80 yrs")

age_grp_all=rbind(under_20, btwn_20_40, btwn_40_60, btwn_60_80, over_80)

age_grp_all<-age_grp_all%>%
  ungroup()%>%
  mutate(age_grp=as.factor(age_grp))%>%
  mutate(age_grp=age_grp, levels=c(
    "Under 20 yrs",
    "Between 20 and 40 yrs",
    "Between 40 and 60 yrs",
    "Between 60 and 80 yrs",
    "Over 80 yrs"
  ))
age_grp_all$age_grp <- factor(age_grp_all$age_grp, levels=c(
  "Under 20 yrs",
  "Between 20 and 40 yrs",
  "Between 40 and 60 yrs",
  "Between 60 and 80 yrs",
  "Over 80 yrs"))

agegrp_20_sens<-age_grp_all%>%
  select(sensitivity, age_grp, name)%>%
  mutate(Test=ifelse(name=="antigen_result", "sensitivity", NA))%>%
  rename(Test_result=sensitivity)

agegrp_20_spec<-age_grp_all%>%
  select(specificity, age_grp, name)%>%
  mutate(Test=ifelse(name=="antigen_result", "specificity", NA))%>%
  rename(Test_result=specificity)

age_20_spec_sens<-rbind(agegrp_20_sens, agegrp_20_spec)

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
#Creating dataset among those that had true pos or false neg results
TP_FN<-main%>%
  filter(antigen_result=="TP"|antigen_result=="FN")

CT_raw<-inner_join(CMD_ID, TP_FN, by="PatientID") #10243 matches

CT_clean<-CT_raw%>%
  mutate(lab_date=mdy(`Collection Date`))%>%
  mutate(timebtwn=difftime(Test_date, lab_date, units="days"))%>%
  mutate(acceptable_time=ifelse(timebtwn<0 & timebtwn>=-5, 1, 0))%>%
  filter(acceptable_time==1 & Test!="COVID-19 Spike Total Ab")%>%
  mutate(antigen_result=as.factor(antigen_result))%>%
  mutate(ct_result=as.numeric(`Num Res`))%>%
  distinct()

#Creating separate ORF gene dataset
orf<-CT_clean%>%
  filter(Test=="SARS CoV 2 ORF 1 Gene")%>%
  distinct()

egene<-CT_clean%>%
  filter(Test=="SARS CoV 2 E Gene")
#Table
CT_table<-CT_clean%>%
  select(antigen_result, Test, ct_result)

CT_table<-tbl_summary(CT_table,
                      by=antigen_result,
                      label=list(
                        ct_result ~ "Num Res"))
#Figure: Boxplot of CT values by test results

figure4<-ggplot(CT_clean, aes(x=antigen_result, y=ct_result, color=)) + 
  geom_boxplot(notch=TRUE) + 
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  facet_wrap("Test")


###CT values by vaccination status####
vax_ct<-CT_clean%>%
  filter(!is.na(vax_status))

vax_orf<-orf%>%
  filter(!is.na(vax_status))

#Figure 2: Bodxplot of CT values by test results with vaccination status filled
figure5<-ggplot(vax_ct, aes(x=antigen_result, y=ct_result, color=vax_status)) + 
  geom_boxplot(notch=TRUE) + 
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  facet_wrap("Test")

#Figure 6: boxplot of ct values faced by vaccination status
figure6<-ggplot(vax_ct, aes(x=antigen_result, y=ct_result, color=vax_status)) + 
  geom_boxplot(notch=TRUE) + 
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  facet_wrap("vax_status")

#Figure 7: boxplot of ct values faceted by vaccination status binned
vax_ct<-vax_ct%>%
  mutate(vax_status_binned=ifelse(vax_status=="Partially vaccinated", "Unvaccinated", vax_status))

figure7<-ggplot(vax_ct, aes(x=antigen_result, y=ct_result, color=vax_status_binned)) + 
  geom_boxplot(notch=TRUE) + 
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.2)+
  facet_wrap("vax_status_binned")

#Overall table#Table
vax_ct_table<-vax_ct%>%
  select(vax_status, Test, ct_result)

vax_ct_table<-tbl_summary(vax_ct_table,
                          by=vax_status,
                          label=list(
                            ct_result ~ "Num Res"))

#Table of Ct values for vaccinated vs partically vaccianted vs unvaccinated
#unvaccinated
#unvaccinated
unvaccinated<-vax_ct%>%
  filter(vax_status=="Unvaccinated")

vars=c("antigen_result", "Test", "ct_result")

unvacc_table<-tbl_summary(unvaccinated[vars],
                          by=antigen_result,
                          label=list(
                            ct_result ~ "Num Res"))%>%
  add_p()

#partially vaccinated
partially<-vax_ct%>%
  filter(vax_status=="Partially vaccinated")

vars=c("antigen_result", "Test", "ct_result")

partvacc_table<-tbl_summary(partially[vars],
                          by=antigen_result,
                          label=list(
                            ct_result ~ "Num Res"))%>%
  add_p()

vaccinated<-vax_ct%>%
  filter(vax_status=="Fully vaccinated")

vacc_table<-tbl_summary(vaccinated[vars],
                        by=antigen_result,
                        label=list(
                          ct_result ~ "Num Res"))%>%
  add_p()

vax_table_3level<-as_gt(tbl_merge(tbls=list(vacc_table, partvacc_table, unvacc_table),
                                          tab_spanner = c("**Fully Vaccinated**", "**Partially Vaccinated**", "**Unvaccinated**")))%>%
  gt::tab_header(title =  "Ct values among vaccinated and unvaccinated patients stratified by antigen result", 
                 subtitle = paste("as of ",as.character(Sys.Date(),format="%m/%d/%Y"),sep="")) 

#unvaccinated
unvaccinated<-vax_ct%>%
  filter(vax_status_binned=="Unvaccinated")

vars=c("antigen_result", "Test", "ct_result")

unvacc_table<-tbl_summary(unvaccinated[vars],
                      by=antigen_result,
                      label=list(
                        ct_result ~ "Num Res"))%>%
  add_p()

vaccinated<-vax_ct%>%
  filter(vax_status_binned=="Fully vaccinated")

vacc_table<-tbl_summary(vaccinated[vars],
                        by=antigen_result,
                        label=list(
                          ct_result ~ "Num Res"))%>%
  add_p()

vax_table_all<-table2all<-as_gt(tbl_merge(tbls=list(vacc_table, unvacc_table),
                                          tab_spanner = c("**Fully Vaccinated**", "**Un/Partially Vaccinated**")))%>%
  gt::tab_header(title =  "Ct values among vaccinated and unvaccinated patients stratified by antigen result", 
                 subtitle = paste("as of ",as.character(Sys.Date(),format="%m/%d/%Y"),sep="")) 
####Generating CI for sens/spec measurements####
library(DescTools)

###Overall cohort
#sensitivity
BinomCI(19610, 66408 ,
        conf.level = 0.95,
        method = "clopper-pearson")
#specificity
BinomCI(775233, 777695,
        conf.level = 0.95,
        method = "clopper-pearson")
###Vaccination status
#Fully vaccinated
#sensitivity
BinomCI(1653, 4056,
        conf.level = 0.95,
        method = "clopper-pearson")
#specificity
BinomCI(138039, 138200 ,
        conf.level = 0.95,
        method = "clopper-pearson")

#Partially vaccinated
#sensitivity
BinomCI(646, 2162,
        conf.level = 0.95,
        method = "clopper-pearson")
#specificity
BinomCI(42182, 42252,
        conf.level = 0.95,
        method = "clopper-pearson")

#Unvaccinated
#sensitivity
BinomCI(3282, 13335,
        conf.level = 0.95,
        method = "clopper-pearson")
#specificity
BinomCI(167207, 167487,
        conf.level = 0.95,
        method = "clopper-pearson")

###Symptomatic status
#Symptomatic (null values of symptomatic patients excluded)
#sensitivity
BinomCI(8677, 37079 ,
        conf.level = 0.95,
        method = "clopper-pearson")
#specificity
BinomCI(360698, 361162 ,
        conf.level = 0.95,
        method = "clopper-pearson")
#Asymptomatic
#sensitivity
BinomCI(10595,28118,
        conf.level = 0.95,
        method = "clopper-pearson")
#specificity
BinomCI(399573,401510,
        conf.level = 0.95,
        method = "clopper-pearson")
#Asymptomatic (null values assumed asympomatic)
#sensitivity
BinomCI(10933,29329,
        conf.level = 0.95,
        method = "clopper-pearson")
#specificity
BinomCI(414535,416533,
        conf.level = 0.95,
        method = "clopper-pearson")

#Age groups in 20 years
#Under 20 years
#sensitivity
BinomCI(2000,7414,
        conf.level = 0.95,
        method = "clopper-pearson")

#specificity
BinomCI(99938,100186,
        conf.level = 0.95,
        method = "clopper-pearson")

#Between 20 and 40 years
#sensitivity
BinomCI(10142,33567,
        conf.level = 0.95,
        method = "clopper-pearson")

#specificity
BinomCI(403557, 404752,
        conf.level = 0.95,
        method = "clopper-pearson")

#Between 60 and 80 years
#sensitivity
BinomCI(1998, 6266,
        conf.level = 0.95,
        method = "clopper-pearson")

#specificity
BinomCI(70053, 70341,
        conf.level = 0.95,
        method = "clopper-pearson")

#Over 80 years
#sensitivity
BinomCI(209, 556,
        conf.level = 0.95,
        method = "clopper-pearson")

#specificity
BinomCI(5037,5059,
        conf.level = 0.95,
        method = "clopper-pearson")

##Sex
#Male
BinomCI(9961, 31585,
        conf.level = 0.95,
        method = "clopper-pearson")

#specificity
BinomCI(319240,320191,
        conf.level = 0.95,
        method = "clopper-pearson")
#Female
BinomCI(9648, 34822,
        conf.level = 0.95,
        method = "clopper-pearson")

#specificity
BinomCI(455943,457454,
        conf.level = 0.95,
        method = "clopper-pearson")

