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


#Rename vars col_character
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
Visit$Visit_date <- as.Date(Visit$Visit_date, format="%m/%d/%Y")
ChiefComplaint$Vax_date <- as.Date(ChiefComplaint$Vax_date, format="%m/%d/%Y")

Visit %>%
  filter(Facility_State=="NY") %>%
  filter(Visit_date >= "2020-03-01") -> Visit #7555936

#Filtering on Covid test given, test date, and receipt of confirmatory PCR
COVIDResults %>%
  filter(Lab.Result.Interpretation=="POSITIVE" | Lab.Result.Interpretation=="NEGATIVE") %>%
  filter(Test_date >= "2020-03-01") -> COVIDResults     #959750                        

#Marking all entries that got a confirmatory PCR test
COVIDResults_conf<-COVIDResults%>%
  filter(Confirmatory.PCR=="1")%>%
  select(PatientID, Confirmatory.PCR)%>%
  distinct()

COVIDResults_confPCR<-left_join(COVIDResults_conf, COVIDResults, by="PatientID")

COVIDResults_confPCR <- COVIDResults_confPCR %>%
  group_by(PatientID) %>%
  arrange(Test_date, .by_group=TRUE)%>%
  mutate(Visit = 1:n())%>%
  select(-Confirmatory.PCR.x)
#Some pts got PCR + antigen tests on the same day with same VistID so don't pull out distinct Visit IDs 

#Remove duplicates in Visit data
Visit <- distinct(Visit) #7555922

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

names(Visit.w)
#rename vars
Visit.w <- setnames(Visit.w, old = c("1.x","1.y","1.x.x","1.y.y","1.x.x.x","1.y.y.y",1), new=c("1","Age","Gender","Race","Ethnicity","UHF","Region"))

#select out demographic vars 
Visit.demo <- Visit.w[, c(1,106:121)]

#Remove extra datasets
rm(Visit.w, Visit.w.age, Visit.w.ethnicity, Visit.w.facility, Visit.w.gender, Visit.w.PIG, Visit.w.race, Visit.w.re, Visit.w.region, Visit.w.UHF)

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

#Remove duplicates in COVID results
COVIDResults_confPCR<- distinct(COVIDResults_confPCR) #3006164

#Merge Visit data with covid results to get patient IDs for all results 
merged.data1 <- inner_join(COVIDResults_confPCR, Visit.demo, by="PatientID") #innerjoin because out of state covid results need to be filtered out (6994684)

#Remove duplicates from Chief Complaints
ChiefComplaint<-distinct(ChiefComplaint)

#merge vaccine and visit data
merged.data2 <- left_join(merged.data1, ChiefComplaint, by="VisitID") #9226028

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

#Taking Patient IDs from the merged data
COVIDResults_confPCR%>%
  select(PatientID, Lab.Result.Interpretation, Result.PCR, Confirmatory.PCR.y)%>%
  distinct()->listIDs

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
