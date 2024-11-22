# Clear the "Global Environment"
rm(list = ls())

# Adjust the path as needed
setwd("/Users/vetlewisloffsandring/Documents/Altasaken/Testing")

options(scipen = 999)

# Load the necessary package
library(dplyr)

# Define a function to list and import all CSV files containing "MNLI" in the name
dataliste <- function() {
  # List all files with "MNLI" in the name
  csv_files <- list.files(pattern = "MNLI")
  
  # Read each file and store in a list of data frames
  data_list <- lapply(csv_files, read.csv, stringsAsFactors = FALSE)
  
  # Combine all data frames into a single data frame
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

# Example of calling the function and storing the combined data frame
combined_csv_data <- dataliste()
print(combined_csv_data)



# Lager en tom rad for byer
combined_csv_data[ , 'city'] <- NA
combined_csv_data[ , 'dhlabid'] <- NA
combined_csv_data[ , 'date'] <- NA
combined_csv_data[ , 'title'] <- NA


# Nytt navn til URN
names(combined_csv_data)[names(combined_csv_data)=="URN"] <- "urn"
names(combined_csv_data)[names(combined_csv_data)=="sentence"] <- "conc"


#Splitter urn til ulike kolonner
combined_csv_data$title <- sapply(strsplit(combined_csv_data$urn, "_"), `[`, 3)

#Legger inn sted på aviser som mangler NB! Ting mangler. 
combined_csv_data <- combined_csv_data %>%
  mutate(city = ifelse(title == "agder", "Flekkefjord", city)) %>%
  mutate(city = ifelse(title == "dittoslogrunerloekkasagene", "Oslo", city)) %>%
  mutate(city = ifelse(title == "dittoslogamleoslo", "Oslo", city)) %>%
  mutate(city = ifelse(title == "helgelandarbeiderbla", "Mosjøen", city)) %>%
  mutate(city = ifelse(title == "byavisatrondheim", "Trondheim", city)) %>%
  mutate(city = ifelse(title == "fiskeribladettjue", "Bergen", city)) %>%
  mutate(city = ifelse(title == "nordisktidende", "USA", city)) %>%
  mutate(city = ifelse(title == "fjordingen", "Stryn", city)) %>%
  mutate(city = ifelse(title == "gaula", "Melhus", city)) %>%
  mutate(city = ifelse(title == "gauldalsposten", "Støren", city)) %>%
  mutate(city = ifelse(title == "arbeiderbladetoslo", "Oslo", city)) %>%
  mutate(city = ifelse(title == "bladetharstad", "Harstad", city))%>%
  mutate(city = ifelse(title == "helgelandsblad", "Sandnessjøen", city))%>%
  mutate(city = ifelse(title == "eikerbladet", "Mjøndalen", city))%>%
  mutate(city = ifelse(title == "framtidinord", "Storslett", city))%>%
  mutate(city = ifelse(title == "sarpsborgarbeiderblad", "Sarpsborg", city))%>%
  mutate(city = ifelse(title == "rix", "Sandefjord", city))%>%
  mutate(city = ifelse(title == "ullensakerblad", "Eidsvoll", city))%>%
  mutate(city = ifelse(title == "aasavis", "Ås", city))%>%
  mutate(city = ifelse(title == "ytringen", "Kolvereid", city))%>%
  mutate(city = ifelse(title == "nytid", "Oslo", city))%>%
  mutate(city = ifelse(title == "byavisadrammen", "Drammen", city))%>%
  mutate(city = ifelse(title == "marsteinen", "Storebø", city))%>%
  mutate(city = ifelse(title == "lokalavisatrysilengerdal", "Trysil", city))%>%
  mutate(city = ifelse(title == "ullernavisakersposten", "Oslo", city))%>%
  mutate(city = ifelse(title == "lokalsorosterdaltrys", "Elverum", city))%>%
  mutate(city = ifelse(title == "malvikbladet", "Hommelvik", city))%>%
  mutate(city = ifelse(title == "sortrondelag", "Orkanger", city))%>%
  mutate(city = ifelse(title == "mossdagbladtjuenitten", "Moss", city))%>%
  mutate(city = ifelse(title == "kystogfjord", "Kjøllefjord", city))%>%
  mutate(city = ifelse(title == "norroena", "USA", city))%>%
  mutate(city = ifelse(title == "norgeidag", "Bergen", city))%>%
  mutate(city = ifelse(title == "dagsavisenostfold", "Moss", city))%>%
  mutate(city = ifelse(title == "dagsavisenarbeiderbladet", "Oslo", city))%>%
  mutate(city = ifelse(title == "westernviking", "USA", city))%>%
  mutate(city = ifelse(title == "vestbyavismoss", "Moss", city))%>%
  mutate(city = ifelse(title == "vestavind", "Sveio", city))%>%
  mutate(city = ifelse(title == "vestbyavis", "Vestby", city))%>%
  mutate(city = ifelse(title == "sydvesten", "Bergen", city))%>%
  mutate(city = ifelse(title == "varingen", "Nittedaø", city))%>%
  mutate(city = ifelse(title == "snasningen", "Snåsa", city))%>%
  mutate(city = ifelse(title == "decorahposten", "USA", city))%>%
  mutate(city = ifelse(title == "nordlys", "Tromsø", city))%>%
  mutate(city = ifelse(title == "gulatidend", "Bergen", city))%>%
  mutate(city = ifelse(title == "dagenbergen", "Bergen", city))%>%
  mutate(city = ifelse(title == "innherredverdal", "Verdal", city))%>%
  mutate(city = ifelse(title == "ostlendingen", "Elverum", city))%>%
  mutate(city = ifelse(title == "tronderavisa", "Steinkjær", city))%>%
  mutate(city = ifelse(title == "sarpen", "Sarpsborg", city))%>%
  mutate(city = ifelse(title == "tonsbergsblad", "Tønsberg", city))%>%
  mutate(city = ifelse(title == "dittoslosthanshaugen", "Oslo", city))%>%
  mutate(city = ifelse(title == "dittoslofrogner", "Oslo", city))%>%
  mutate(city = ifelse(title == "baerumsavisen", "Bærum", city))%>%
  mutate(city = ifelse(title == "byavisamoss", "Moss", city))%>%
  mutate(city = ifelse(title == "byavisasarpsborg", "Sarpsborg", city))%>%
  mutate(city = ifelse(title == "byavisafredrikstad", "Fredrikstad", city))%>%
  mutate(city = ifelse(title == "askeravisen", "Asker", city))%>%
  mutate(city = ifelse(title == "loerenskogposten", "Lørenskog", city))%>%
  mutate(city = ifelse(title == "drammenstidendeogbus", "Drammen", city))%>%
  mutate(city = ifelse(title == "enebakkavis", "Enebakk", city))%>%
  mutate(city = ifelse(title == "skedsmoposten", "Lillestrøm", city))%>%
  mutate(city = ifelse(title == "igjoevik", "Gjøvik", city))%>%
  mutate(city = ifelse(title == "lierposten", "Lier", city))%>%
  mutate(city = ifelse(title == "osogfusaposten", "Os", city))%>%
  mutate(city = ifelse(title == "romerikspostenlillestroemtjuetolv", "Lillestrøm", city))%>%
  mutate(city = ifelse(title == "samningen", "Samnanger", city))%>%
  mutate(city = ifelse(title == "sovesten", "Kyrksæterøra", city))%>%
  mutate(city = ifelse(title == "lokalgroruddalenab", "Oslo", city))%>%
  mutate(city = ifelse(title == "lokalgroruddalengs", "Oslo", city))%>%
  mutate(city = ifelse(title == "lokalavisaverrannamdalseid", "Namdalseid", city))%>%
  mutate(city = ifelse(title == "meraakerposten", "Meråker", city))%>%
  mutate(city = ifelse(title == "mossbymagasin", "Moss", city))%>%
  mutate(city = ifelse(title == "minnesotaposten", "Minneapolis", city))%>%
  mutate(city = ifelse(title == "klartale", "Oslo", city))%>%
  mutate(city = ifelse(title == "svalbardposten", "Longyearbyen", city))%>%
  mutate(city = ifelse(title == "roykenoghurumsavis", "Slemmestad", city))%>%
  mutate(city = ifelse(title == "vinland", "Chicago", city))%>%
  mutate(city = ifelse(title == "duluthskandinav", "Duluth", city))%>%
  mutate(city = ifelse(title == "kongsbergtidende", "Kongsberg", city))%>%
  mutate(city = ifelse(title == "fosnafolket", "Brekstad", city))%>%
  mutate(city = ifelse(title == "sunnmoerearbeideravis", "Ålesund", city))%>%
  mutate(city = ifelse(title == "fremtiden", "Drammen", city))%>%
  mutate(city = ifelse(title == "firda", "Førde", city))%>%
  mutate(city = ifelse(title == "aftenpostenukensnytt", "Oslo", city))%>%
  mutate(city = ifelse(title == "kvinnheringen", "Husnes", city))%>%
  mutate(city = ifelse(title == "sorvarangeravis", "Kirkenes", city))%>%
  mutate(city = ifelse(title == "friheten", "Oslo", city))%>%
  mutate(city = ifelse(title == "telemarkarbeiderblad", "Skien", city))%>%
  mutate(city = ifelse(title == "laagendalsposten", "Kongsberg", city))%>%
  mutate(city = ifelse(title == "klassekampen", "Oslo", city))%>%
  mutate(city = ifelse(title == "oevresmaalenene", "Askim", city))%>%
  mutate(city = ifelse(title == "firdaposten", "Florø", city))%>%
  mutate(city = ifelse(title == "sunnmorsposten", "Ålesund", city))%>%
  mutate(city = ifelse(title == "porsgrunnsdagblad", "Porsgrunn", city))%>%
  mutate(city = ifelse(title == "samholdvelgeren", "Gjøvik", city))%>%
  mutate(city = ifelse(title == "tidenskrav", "Kristiansund", city))%>%
  mutate(city = ifelse(title == "ostlandetsblad", "Ski", city))%>%
  mutate(city = ifelse(title == "telemarkingen", "Bø", city))%>%
  mutate(city = ifelse(title == "tiden", "Arendal", city))%>%
  mutate(city = ifelse(title == "telen", "Notodden", city))%>%
  mutate(city = ifelse(title == "itromso", "Tromsø", city))%>%
  mutate(city = ifelse(title == "kronstadposten", "Alta", city))%>%
  mutate(city = ifelse(title == "finnmarken", "Vadsø", city))%>%
  mutate(city = ifelse(title == "farsundsavis", "Farsund", city))%>%
  mutate(city = ifelse(title == "sogndagblad", "Høyanger", city))%>%
  mutate(city = ifelse(title == "indresmaalenenesavis", "Ørje", city))%>%
  mutate(city = ifelse(title == "altaposten", "Alta", city))%>%
  mutate(city = ifelse(title == "vikebladet", "Ulsteinvik", city))%>%
  mutate(city = ifelse(title == "dagbladet", "Oslo", city))%>%
  mutate(city = ifelse(title == "firdatidend", "Sandane", city))%>%
  mutate(city = ifelse(title == "tysvaerbygdeblad", "Tysvær", city))%>%
  mutate(city = ifelse(title == "haugesundsdagblad", "Haugesund", city))%>%
  mutate(city = ifelse(title == "romsdalfolkeblad", "Molde", city))%>%
  mutate(city = ifelse(title == "mossavis", "Moss", city))%>%
  mutate(city = ifelse(title == "varden", "Skien", city))%>%
  mutate(city = ifelse(title == "hordaland", "Voss", city))%>%
  mutate(city = ifelse(title == "faedrelandsvennen", "Kristiansand", city))%>%
  mutate(city = ifelse(title == "tronderbladetmelhus", "Melhus", city))%>%
  mutate(city = ifelse(title == "andoyaavis", "Andenes", city))%>%
  mutate(city = ifelse(title == "sykkylvsbladet", "Sykkylven", city))%>%
  mutate(city = ifelse(title == "dagensnaeringsliv", "Oslo", city))%>%
  mutate(city = ifelse(title == "finnmarksposten", "Honningsvåg", city))%>%
  mutate(city = ifelse(title == "meloyavisa", "Ørnes", city))%>%
  mutate(city = ifelse(title == "aandalsnesavis", "Åndalsnes", city))%>%
  mutate(city = ifelse(title == "fanaposten", "Bergen", city))%>%
  mutate(city = ifelse(title == "ringsakerblad", "Brumunddal", city))%>%
  mutate(city = ifelse(title == "sagat", "Lakselv", city))%>%
  mutate(city = ifelse(title == "vaaganavisa", "Svolvær", city))%>%
  mutate(city = ifelse(title == "fredriksstadblad", "Fredrikstad", city))%>%
  mutate(city = ifelse(title == "fjordabladet", "Nordfjordeid", city))%>%
  mutate(city = ifelse(title == "stavangeraftenblad", "Stavanger", city))%>%
  mutate(city = ifelse(title == "sandefjordsblad", "Sandefjord", city))%>%
  mutate(city = ifelse(title == "bergenstidende", "Bergen", city))%>%
  mutate(city = ifelse(title == "finnmarkdagblad", "Hammerfest", city))%>%
  mutate(city = ifelse(title == "nationen", "Oslo", city))%>%
  mutate(city = ifelse(title == "nidaros", "Trondheim", city))%>%
  mutate(city = ifelse(title == "jarlsberg", "Holmestrand", city))%>%
  mutate(city = ifelse(title == "bergensavisen", "Bergen", city))%>%
  mutate(city = ifelse(title == "aftenposten", "Oslo", city))%>%
  mutate(city = ifelse(title == "arbeidetsrett", "Røros", city))%>%
  mutate(city = ifelse(title == "nordsaltenavis", "Drag", city))%>%
  mutate(city = ifelse(title == "gjengangeren", "Horten", city))%>%
  mutate(city = ifelse(title == "folketsframtid", "Oslo", city))%>%
  mutate(city = ifelse(title == "hallingdolen", "Ål", city))%>%
  mutate(city = ifelse(title == "morgenbladet", "Oslo", city))%>%
  mutate(city = ifelse(title == "haugesundsavis", "Haugesund", city))%>%
  mutate(city = ifelse(title == "finansavisen", "Oslo", city))%>%
  mutate(city = ifelse(title == "glaamdalen", "Kongsvinger", city))%>%
  mutate(city = ifelse(title == "vesteraalen", "Sortland", city))%>%
  mutate(city = ifelse(title == "dagogtid", "Oslo", city))%>%
  mutate(city = ifelse(title == "rogalandsavis", "Stavanger", city))%>%
  mutate(city = ifelse(title == "bomlonytt", "Bømlo", city))%>%
  mutate(city = ifelse(title == "fremover", "Narvik", city))%>%
  mutate(city = ifelse(title == "romsdalsbudstikke", "Molde", city))%>%
  mutate(city = ifelse(title == "rakkestadavis", "Rakkestad", city))%>%
  mutate(city = ifelse(title == "tffolkebladet", "Finnsnes", city))%>%
  mutate(city = ifelse(title == "opdalingen", "Oppdal", city))%>%
  mutate(city = ifelse(title == "totensblad", "Toten", city))%>%
  mutate(city = ifelse(title == "grannar", "Etne", city))%>%
  mutate(city = ifelse(title == "harstadtidende", "Harstad", city))%>%
  mutate(city = ifelse(title == "saltenposten", "Fauske", city))%>%
  mutate(city = ifelse(title == "rakkestadbygdeblad", "Rakkestad", city))%>%
  mutate(city = ifelse(title == "ukeavisanytid", "Oslo", city))%>%
  mutate(city = ifelse(title == "buskerudsblad", "Drammen", city))%>%
  mutate(city = ifelse(title == "nordmoersposten", "Kristiansund", city))%>%
  mutate(city = ifelse(title == "sandnesposten", "Sandnes", city))%>%
  mutate(city = ifelse(title == "grenda", "Rosendal", city))%>%
  mutate(city = ifelse(title == "agderposten", "Arendal", city))%>%
  mutate(city = ifelse(title == "nyetroms", "Moen", city))%>%
  mutate(city = ifelse(title == "ryfylke", "Sauda", city))%>%
  mutate(city = ifelse(title == "strilen", "Knarvik", city))%>%
  mutate(city = ifelse(title == "nordstrandsblad", "Oslo", city))%>%
  mutate(city = ifelse(title == "andoyposten", "Andenes", city))%>%
  mutate(city = ifelse(title == "nordlandsposten", "Bodø", city))%>%
  mutate(city = ifelse(title == "verdensgang", "Oslo", city))%>%
  mutate(city = ifelse(title == "lofotposten", "Svolvær", city))%>%
  mutate(city = ifelse(title == "smaalenenesavis", "Askim", city))%>%
  mutate(city = ifelse(title == "sunnhordland", "Stord", city))%>%
  mutate(city = ifelse(title == "kragerobladvestmar", "Kragerø", city))%>%
  mutate(city = ifelse(title == "adresseavisen", "Trondheim", city))%>%
  mutate(city = ifelse(title == "vesteraalensavis", "Stokmarknes", city))%>%
  mutate(city = ifelse(title == "austagderblad", "Risør", city))%>%
  mutate(city = ifelse(title == "bronnoysundsavis", "Brønnøysund", city))%>%
  mutate(city = ifelse(title == "demokraten", "Fredrikstad", city))%>%
  mutate(city = ifelse(title == "lillesandsposten", "Lillesand", city))%>%
  mutate(city = ifelse(title == "vestlandsnytt", "Fosnavåg", city))%>%
  mutate(city = ifelse(title == "ruijankaiku", "Alta", city))%>%
  mutate(city = ifelse(title == "dagsavisen", "Oslo", city))%>%
  mutate(city = ifelse(title == "tvedestrandsposten", "Tvedestrand", city))%>%
  mutate(city = ifelse(title == "nyttiuka", "Ålesund", city))%>%
  mutate(city = ifelse(title == "oyblikk", "Giske", city))%>%
  mutate(city = ifelse(title == "avisanordland", "Bodø", city))%>%
  mutate(city = ifelse(title == "hitrafroya", "Hitra", city))%>%
  mutate(city = ifelse(title == "namdalsavisa", "Namsos", city))%>%
  mutate(city = ifelse(title == "hortenarbeiderblad", "Horten", city))%>%
  mutate(city = ifelse(title == "romerikesblad", "Lillestrøm", city))%>%
  mutate(city = ifelse(title == "tromsfolkeblad", "Finnsnes", city))%>%
  mutate(city = ifelse(title == "fjuken", "Skjåk", city))%>%
  mutate(city = ifelse(title == "lindesnes", "Lindesnes", city))%>%
  mutate(city = ifelse(title == "auraavis", "Sunndalsøra", city))%>%
  mutate(city = ifelse(title == "haramsnytt", "Brattvåg", city))%>%
  mutate(city = ifelse(title == "bergensarbeiderblad", "Bergen", city))%>%
  mutate(city = ifelse(title == "hammerfestingen", "Hammerfest", city))%>%
  mutate(city = ifelse(title == "ostlandsposten", "Larvik", city))%>%
  mutate(city = ifelse(title == "vaartland", "Oslo", city))%>%
  mutate(city = ifelse(title == "helgelendingen", "Mosjøen", city))%>%
  mutate(city = ifelse(title == "samholdgjoevik", "Gjøvik", city))%>%
  mutate(city = ifelse(title == "ranablad", "Mo i Rana", city))%>%
  mutate(city = ifelse(title == "telemarksavisa", "Skien", city))%>%
  mutate(city = ifelse(title == "tysnes", "Tysnes", city))%>%
  mutate(city = ifelse(title == "bygdanytt", "Bergen", city))%>%
  mutate(city = ifelse(title == "vestnesavisa", "Vestnes", city))%>%
  mutate(city = ifelse(title == "ytresogn", "Høyanger", city))%>%
  mutate(city = ifelse(title == "sunnmoringen", "Stranda", city))%>%
  mutate(city = ifelse(title == "selbyggen", "Selbu", city))%>%
  mutate(city = ifelse(title == "friheten2", "Oslo", city))%>%
  mutate(city = ifelse(title == "vestkysten", "Stavanger", city))%>%
  mutate(city = ifelse(title == "dolen", "Vinstra", city))%>%
  mutate(city = ifelse(title == "vigga", "Dombås", city))%>%
  mutate(city = ifelse(title == "drabantposten", "Trondheim", city))%>%
  mutate(city = ifelse(title == "skiensdagblad", "Skien", city))%>%
  mutate(city = ifelse(title == "hordatidend", "Voss ", city))%>%
  mutate(city = ifelse(title == "nyttfranorge", "Oslo", city))%>%
  mutate(city = ifelse(title == "folkeviljennyttinord", "Sjøvegan", city))%>%
  mutate(city = ifelse(title == "drangedalblad", "Drangedal", city))%>%
  mutate(city = ifelse(title == "hurumposten", "Hurum", city))%>%
  mutate(city = ifelse(title == "ulefossavisnittenfemtito", "Ulefoss", city))%>%
  mutate(city = ifelse(title == "stavangeren", "Stavanger", city))%>%
  mutate(city = ifelse(title == "fana", "Bergen", city))%>%
  mutate(city = ifelse(title == "langesundnittenfemti", "Langesund", city))%>%
  mutate(city = ifelse(title == "avisatrondheim", "Trondheim", city))%>%
  mutate(city = ifelse(title == "askoytidend", "Straume", city))%>%
  mutate(city = ifelse(title == "ranaposten", "Mo i Rana", city))%>%
  mutate(city = ifelse(title == "vesttelemarkblad", "Kviteseid", city))%>%
  mutate(city = ifelse(title == "driva", "Sunndalsøra", city))%>%
  mutate(city = ifelse(title == "raumnes", "Nes", city))%>%
  mutate(city = ifelse(title == "akershusamtstidende", "Drøbak", city))%>%
  mutate(city = ifelse(title == "stjordalsnytt", "Stjørdal", city))%>%
  mutate(city = ifelse(title == "nordstrandsbladoslonittenfoerti", "Oslo", city))%>%
  mutate(city = ifelse(title == "vestmar", "Kragerø", city))%>%
  mutate(city = ifelse(title == "akersavisgroruddalen", "Oslo", city))%>%
  mutate(city = ifelse(title == "fjordenestidende", "Måløy", city))%>%
  mutate(city = ifelse(title == "hardangerfolkeblad", "Odda", city))%>%
  mutate(city = ifelse(title == "klaebuposten", "Klæbu", city))%>%
  mutate(city = ifelse(title == "venneslatidende", "Vennesla", city))%>%
  mutate(city = ifelse(title == "christianssandstidende", "Kristiansand", city))%>%
  mutate(city = ifelse(title == "venneslaposten", "Vennesla", city))%>%
  mutate(city = ifelse(title == "sarpsborgavisa", "Sarpsborg", city))%>%
  mutate(city = ifelse(title == "oesterdoelenkoppang", "Koppang", city))%>%
  mutate(city = ifelse(title == "ringerikesblad", "Hønefoss", city))%>%
  mutate(city = ifelse(title == "oestfoldbygdeblad", "Rakkestad", city))%>%
  mutate(city = ifelse(title == "smaalenenesamtstidende", "Halden", city))%>%
  mutate(city = ifelse(title == "velgeren", "Gjøvik", city))%>%
  mutate(city = ifelse(title == "senjensbladnittentrettifem", "Finnsnes ", city))%>%
  mutate(city = ifelse(title == "vaksdalposten", "Vaksdal", city))%>%
  mutate(city = ifelse(title == "lifjell", "Bø", city))%>%
  mutate(city = ifelse(title == "osterdolen", "Stor-Elvdal", city))%>%
  mutate(city = ifelse(title == "oksnesavisa", "Myre", city))%>%
  mutate(city = ifelse(title == "boblad", "Bø", city))%>%
  mutate(city = ifelse(title == "aasanetidende", "Bergen", city))%>%
  mutate(city = ifelse(title == "agdertidend", "Kristiansand", city))%>%
  mutate(city = ifelse(title == "drammenstidendeogbuskerudblad", "Drammen", city))%>%
  mutate(city = ifelse(title == "lokalavisanordsalten", "Drag", city))%>%
  mutate(city = ifelse(title == "breviksposten", "Porsgrunn", city))%>%
  mutate(city = ifelse(title == "nordtroenderenognamdalen", "Namsos", city))%>%
  mutate(city = ifelse(title == "stavangerennittenseksten", "Stavanger", city))%>%
  mutate(city = ifelse(title == "morenytt", "Ørsta", city))%>%
  mutate(city = ifelse(title == "avvir", "Kautokeino", city))%>%
  mutate(city = ifelse(title == "suldalsposten", "Suldal", city))%>%
  mutate(city = ifelse(title == "sognavis", "Leikanger", city))%>%
  mutate(city = ifelse(title == "bladet", "Stjørdal", city))%>%
  mutate(city = ifelse(title == "listertjuefjorten", "Farsund", city))%>%
  mutate(city = ifelse(title == "sorlandsavisenkr", "Kristiansand", city))%>%
  mutate(city = ifelse(title == "drammenstidende", "Drammen", city))%>%
  mutate(city = ifelse(title == "hamararbeiderblad", "Hamar", city))%>%
  mutate(city = ifelse(title == "amagasinet", "Oslo", city))%>%
  mutate(city = ifelse(title == "nordreakerbudstikke", "Oslo", city))%>%
  mutate(city = ifelse(title == "nordstrandostreaker", "Oslo", city))%>%
  mutate(city = ifelse(title == "hamardagblad", "Hamar", city))%>%
  mutate(city = ifelse(title == "synstemore", "Fiskåbygd", city))%>%
  mutate(city = ifelse(title == "avisahemnes", "Korgen", city))%>%
  mutate(city = ifelse(title == "nordvestnytt", "Smøla", city))%>%
  mutate(city = ifelse(title == "storfjordnytt", "Sylte", city))%>%
  mutate(city = ifelse(title == "fjellljom", "Røros", city))%>%
  mutate(city = ifelse(title == "rjukanarbeiderblad", "Rjukan", city))%>%
  mutate(city = ifelse(title == "gudbrandsdolendagnin", "Lillehammer", city))%>%
  mutate(city = ifelse(title == "budstikkaforaskerogb", "Billingstad", city))%>%
  mutate(city = ifelse(title == "valdres", "Fagernes", city))%>%
  mutate(city = ifelse(title == "dalanetidende", "Egersund", city))%>%
  mutate(city = ifelse(title == "grimstadadressetiden", "Grimstad", city))%>%
  mutate(city = ifelse(title == "oppny", "Larvik", city))%>%
  mutate(city = ifelse(title == "arendalstidende", "Arendal", city))%>%
  mutate(city = ifelse(title == "more", "Volda", city))%>%
  mutate(city = ifelse(title == "kulingen", "Ørnes", city))%>%
  mutate(city = ifelse(title == "hadeland", "Gran", city))%>%
  mutate(city = ifelse(title == "svelviksposten", "Svelvik", city))%>%
  mutate(city = ifelse(title == "norddalen", "Otta", city))%>%
  mutate(city = ifelse(title == "bygdeposten", "Vikersund", city))%>%
  mutate(city = ifelse(title == "opplandarbeiderblad", "Gjøvik", city))%>%
  mutate(city = ifelse(title == "jaerbladet", "Bryne", city))%>%
  mutate(city = ifelse(title == "kanalen", "Ulefoss", city))%>%
  mutate(city = ifelse(title == "bygdebladet", "Randaberg", city))%>%
  mutate(city = ifelse(title == "lofottidende", "Leknes", city))%>% 
  mutate(city = ifelse(title == "soerlandet", "Kristiansand", city))%>% 
  mutate(city = ifelse(title == "romsdalsposten", "Kristiansund", city))%>% 
  mutate(city = ifelse(title == "namdalarbeiderblad", "Namsos", city))%>% 
  mutate(city = ifelse(title == "akershusarbeiderblad", "Lillestrøm", city))%>% 
  mutate(city = ifelse(title == "dagningen", "Lillehammer", city))%>% 
  mutate(city = ifelse(title == "helgelandarbeiderblad", "Mosjøen", city))%>% 
  mutate(city = ifelse(title == "gudbrandsdoelen", "Lillehammer", city))%>% 
  mutate(city = ifelse(title == "samiaigi", "Karasjok", city))%>% 
  mutate(city = ifelse(title == "stjordalensblad", "Stjørdalen", city))%>% 
  mutate(city = ifelse(title == "nordlandsframtid", "Bodø", city))%>% 
  mutate(city = ifelse(title == "mossdagblad", "Moss", city))%>% 
  mutate(city = ifelse(title == "lillehammertilskuer", "Lillehammer", city))%>% 
  mutate(city = ifelse(title == "vestfoldfremtid", "Tønsberg", city))%>% 
  mutate(city = ifelse(title == "stjoerdalingen", "Stjørdal", city))%>% 
  mutate(city = ifelse(title == "norgeshandelsogsjoefartstidende", "Oslo", city))%>% 
  mutate(city = ifelse(title == "haldenarbeiderblad", "Halden", city))%>% 
  mutate(city = ifelse(title == "sandebladet", "Sande", city))%>% 
  mutate(city = ifelse(title == "ofotenstidende", "Narvik", city))%>% 
  mutate(city = ifelse(title == "fiskeribladet", "Tromsø", city))%>% 
  mutate(city = ifelse(title == "eidsvoldblad", "Eidsvold", city))%>% 
  mutate(city = ifelse(title == "innherredsfolkebladv", "Verdal", city))%>% 
  mutate(city = ifelse(title == "askerogbaerumsbudstikke", "Billingstad", city))%>% 
  mutate(city = ifelse(title == "levangeravisa", "Levanger", city))%>% 
  mutate(city = ifelse(title == "setesdolen", "Bygland", city))%>% 
  mutate(city = ifelse(title == "sognogfjordane", "Leikanger", city))%>% 
  mutate(city = ifelse(title == "sogningensognsavis", "Leikanger", city))%>% 
  mutate(city = ifelse(title == "firdafolkeblad", "Florø", city))%>% 
  mutate(city = ifelse(title == "indreakershusblad", "Bjørkelangen", city))%>% 
  mutate(city = ifelse(title == "glomfjordposten", "Glomfjord", city))%>% 
  mutate(city = ifelse(title == "krageroblad", "Kragerø", city))%>% 
  mutate(city = ifelse(title == "hitranytt", "Sandstad", city))%>% 
  mutate(city = ifelse(title == "vindafjordingen", "Sandeid", city))%>% 
  mutate(city = ifelse(title == "hordalandfolkeblad", "Norheimsund", city))%>% 
  mutate(city = ifelse(title == "hardanger", "Odda", city))%>% 
  mutate(city = ifelse(title == "folket", "Oslo", city))%>% 
  mutate(city = ifelse(title == "askoyvaeringen", "Askøy", city))%>% 
  mutate(city = ifelse(title == "romeriksnytt", "Skedsmo", city))%>% 
  mutate(city = ifelse(title == "nordhordland", "Knarvik", city))%>% 
  mutate(city = ifelse(title == "strandbuen", "Jørpeland", city))%>% 
  mutate(city = ifelse(title == "fiskaren", "Bergen", city))%>% 
  mutate(city = ifelse(title == "sandnestidende", "Sandenes", city))%>% 
  mutate(city = ifelse(title == "folkogland", "Oslo", city))%>% 
  mutate(city = ifelse(title == "oeynytt", "Sortland", city))%>% 
  mutate(city = ifelse(title == "aasaposten", "Bergen", city))%>% 
  mutate(city = ifelse(title == "arbeiderbladet", "Oslo", city))%>% 
  mutate(city = ifelse(title == "loerenskogvel", "Lørenskog", city))%>% 
  mutate(city = ifelse(title == "nordtromsavis", "Skjervøy", city))%>% 
  mutate(city = ifelse(title == "melhusbladet", "Melhusbladet", city))%>% 
  mutate(city = ifelse(title == "dagbladetrogaland", "Jæren", city))%>% 
  mutate(city = ifelse(title == "arbeideravisa", "Trondheim", city))%>% 
  mutate(city = ifelse(title == "oestfoldposten", "Askim", city))%>% 
  mutate(city = ifelse(title == "fylketmolde", "Molde", city))%>% 
  mutate(city = ifelse(title == "vestposten", "Hareid", city))%>% 
  mutate(city = ifelse(title == "vaksdalnytt", "Bergen", city))%>% 
  mutate(city = ifelse(title == "nordkalott", "Lakselv", city))%>% 
  mutate(city = ifelse(title == "vignett", "Enebakk", city))%>% 
  mutate(city = ifelse(title == "sotranytt", "Straume", city))%>% 
  mutate(city = ifelse(title == "vikebladetnittentjueni", "Ullsteinvik", city))%>% 
  mutate(city = ifelse(title == "osloavisen", "Oslo", city))%>% 
  mutate(city = ifelse(title == "trondheimsavisa", "Trondheim", city))%>% 
  mutate(city = ifelse(title == "gudbrandsdoelenlillehammer", "Lillehammer", city))%>% 
  mutate(city = ifelse(title == "oesterdalbudstikke", "Rendalen", city))%>% 
  mutate(city = ifelse(title == "soendagsoendag", "Drammen", city))%>% 
  mutate(city = ifelse(title == "fremskritt", "Oslo", city))%>% 
  mutate(city = ifelse(title == "solabladet", "Sola", city))%>% 
  mutate(city = ifelse(title == "steinkjeravisa", "Steinkjær", city))%>% 
  mutate(city = ifelse(title == "vestnytt", "Straume", city))%>% 
  mutate(city = ifelse(title == "assu", "Kautokeino", city))%>% 
  mutate(city = ifelse(title == "oeyavis", "Midsund", city))%>% 
  mutate(city = ifelse(title == "inderoyningen", "Straumen", city))%>% 
  mutate(city = ifelse(title == "osthavet", "Vardø", city))%>% 
  mutate(city = ifelse(title == "frostingen", "Åsen", city))%>% 
  mutate(city = ifelse(title == "minaigi", "Karasjok", city))%>% 
  mutate(city = ifelse(title == "oyposten", "Judaberg", city))%>% 
  mutate(city = ifelse(title == "hovaagavisa", "Lillesand", city))%>% 
  mutate(city = ifelse(title == "eidsvollullensakerbl", "Eidsvoll", city))%>% 
  mutate(city = ifelse(title == "gjesdalbuen", "Gjesdal", city))%>% 
  mutate(city = ifelse(title == "fiskeribladetfiskare", "Tromsø", city))%>% 
  mutate(city = ifelse(title == "glaamdalensolor", "Kongsvinger", city))%>% 
  mutate(city = ifelse(title == "glaamdalenodal", "Kongsvinger", city))%>% 
  mutate(city = ifelse(title == "bygdapostenforhjelme", "Hjelmeland", city))%>% 
  mutate(city = ifelse(title == "sortlandsavisa", "Sortland", city))%>% 
  mutate(city = ifelse(title == "dagenmagazinet", "Bergen", city))%>% 
  mutate(city = ifelse(title == "lokalsorosterdalelve", "Elveroum", city))%>% 
  mutate(city = ifelse(title == "sarpsborgarbeiderbla", "Sarpsborg", city))%>% 
  mutate(city = ifelse(title == "opp", "Oppdalen", city))%>% 
  mutate(city = ifelse(title == "varangeren", "Vadsø", city))%>% 
  mutate(city = ifelse(title == "stangeavisa", "Stange", city))%>% 
  mutate(city = ifelse(title == "ullensakerposten", "Oslo", city))%>% 
  mutate(city = ifelse(title == "oyene", "Nøtterøy", city))%>% 
  mutate(city = ifelse(title == "framtida", "Meløy", city))%>% 
  mutate(city = ifelse(title == "sulaposten", "Langevåg", city))%>% 
  mutate(city = ifelse(title == "lillehammerbyavisno", "Lillehammer", city))%>% 
  mutate(city = ifelse(title == "solungavisa", "Flisa", city))%>% 
  mutate(city = ifelse(title == "tronderbladet", "Melhus", city))%>% 
  mutate(city = ifelse(title == "frolendingen", "Froland", city))%>% 
  mutate(city = ifelse(title == "aamliavisa", "Åmli", city))%>% 
  mutate(city = ifelse(title == "byavisasandefjord", "Sandefjord", city))%>% 
  mutate(city = ifelse(title == "nordre", "Brattvåg", city))%>% 
  mutate(city = ifelse(title == "tromso", "Tromsø", city))%>% 
  mutate(city = ifelse(title == "birkenesavisa", "Birkenes", city))
  

#Finne tomme

# Identify rows where 'city' is NA or an empty string
missing_city_rows <- which(is.na(combined_csv_data$city) | combined_csv_data$city == "")


# Extract the titles corresponding to the rows with missing 'city'
titles_missing_city <- combined_csv_data$title[missing_city_rows]

# Optional: Get unique titles to avoid duplicates
unique_titles_missing_city <- unique(titles_missing_city)

# View the unique titles
print(unique_titles_missing_city)


# Split the URN into multiple columns
combined_csv_data <- combined_csv_data %>%
  separate(urn, into = paste0("URN_part", 1:10), sep = "_", fill = "right", remove = FALSE)

# Extract the desired components
combined_csv_data <- combined_csv_data %>%
  mutate(
    timestamp = URN_part6,
    year = substr(timestamp, 1, 4)
  )


library(lubridate)
combined_csv_data <- combined_csv_data %>%
  mutate(
    date = ymd(timestamp)
  )

renset_data_trunkert <- combined_csv_data %>%
  mutate(across(where(is.numeric), ~if_else(. < 0.5, 0, .)))# Function to apply the condition on each row



library(ggplot2)

summary_table <- renset_data_trunkert %>%
  group_by(year) %>%
  summarise(
    average_value_sam = mean(samesaken, na.rm = TRUE),
    average_value_nat = mean(`natur.og.miljø`, na.rm = TRUE)
  )

summary_table$year <- as.numeric(summary_table$year)
ggplot(summary_table, aes(x = year)) +
  geom_smooth(aes(y = average_value_sam, color = "Smoothed Average samesaken"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = average_value_nat, color = "Smoothed Average natur og miljø"), method = "loess", se = FALSE) +
  geom_line(aes(y = average_value_sam, color = "Unsmoothed Average samesaken"), size = 1, alpha = 0.4) +
  geom_line(aes(y = average_value_nat, color = "Unsmoothed Average natur og miljø"), size = 1, alpha = 0.4) +
  scale_x_continuous(breaks=seq(1979, 2023, by = 2)) +  # Adjust breaks to show every 5th year
  labs(
    title = "Text snippets, average yearly values, \"The sami cause\",  N = 87 854",
    x = "Year",
    y = "Average label score, 0 = no relationship 1 = strong relationship",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Smoothed Average samesaken" = "lightsalmon4",
                                 "Smoothed Average natur og miljø" = "darkseagreen4",
                                "Unsmoothed Average samesaken" = "lightsalmon2",
                                     "Unsmoothed Average natur og miljø" = "darkseagreen3"
  )) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
        panel.grid.major = element_line(color = 'bisque3'),
        panel.grid.minor = element_line(color = 'bisque3'),
        legend.background = element_rect(fill ='bisque3'),
        legend.key = element_rect(fill ='bisque3', colour = 'bisque4'),
        plot.background = element_rect(fill ='bisque2'))



#### Traume

summary_table_traume <- renset_data_trunkert %>%
  filter(!is.na(year)) %>%  # Remove rows where year is NA
  group_by(year) %>%
  summarise(
    average_value_uts = mean(`utsatt.og.usikkerhet`, na.rm = TRUE),
    average_value_uno = mean(`unødvendig`, na.rm = TRUE),
    average_value_spl = mean(`splittelse.og.uenighet`, na.rm = TRUE),
    average_value_van = mean(`vanskelig.å.snakke.om`, na.rm = TRUE),
    average_value_ure = mean(`urettferdig`, na.rm = TRUE),
    average_value_opp = mean(`opprivende`, na.rm = TRUE),
    average_value_fors = mean(`forsoning`, na.rm = TRUE),
    average_value_samh = mean(`samhold.og.enighet`, na.rm = TRUE),
    average_value_hand = mean(`handlekraft.og.beslutninger`, na.rm = TRUE),
    average_value_okon = mean(`økonomisk.og.næringsmessig.nytte`, na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    average_traumer = mean(c_across(c(
      average_value_uts, average_value_uno, average_value_opp,
      average_value_spl, average_value_van, average_value_ure
    )), na.rm = TRUE),
    average_ikke_traumer = mean(c_across(c(
      average_value_fors, average_value_samh,
      average_value_hand, average_value_okon
    )), na.rm = TRUE)
  )
summary_table_traume <- summary_table_traume %>%
  mutate(year = as.numeric(as.character(year)))


library(ggplot2)
library(scales)  # Needed for the alpha() function

ggplot(summary_table_traume, aes(x = year)) +
  # Smoothed Average Trauma (plotted first to be behind)
  geom_smooth(
    aes(y = average_traumer, color = "Smoothed Average trauma", group = 1),
    method = "loess",
    se = FALSE,
    size = 1
  ) +
  # Unsmoothed Average Trauma
  geom_line(
    aes(y = average_traumer, color = "Unsmoothed Average trauma", group = 1),
    size = 1
  ) +
  # Add labels
  labs(
    title = "Text snippets rated by trauma, supercategory. N = 112,141",
    x = "Year",
    y = "Average (0 = No connection, 1 = Strong connection)",
    color = "Legend:"
  ) +
  # Set color scheme with adjusted alpha values
  scale_color_manual(
    values = c(
      "Smoothed Average trauma" = alpha("tomato2", 0.4),
      "Unsmoothed Average trauma" = alpha("tomato4", 0.9)
    )
  ) +
  # Customize x-axis
  scale_x_continuous(breaks = seq(1979, 2023, by = 2)) +  # Adjust breaks to show every 2 years
  # Customize legend
  guides(color = guide_legend(override.aes = list(size = 1))) +
  # Set plot theme
  theme(
    panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
    panel.grid.major = element_line(color = 'bisque3'),
    panel.grid.minor = element_line(color = 'bisque3'),
    legend.background = element_rect(fill = 'bisque3'),
    legend.key = element_rect(fill = 'bisque3', colour = 'bisque4'),
    plot.background = element_rect(fill = 'bisque2')
  )


#####
summary_table_syn_sam <- renset_data_trunkert %>%
  filter(!is.na(year)) %>%  # Remove rows where year is NA
  group_by(year) %>%
  summarise(
    average_value_sær = mean(`samiske.særrettigheter`, na.rm = TRUE),
    average_value_trus = mean(`trussel.mot.samisk.kultur.og.næring`, na.rm = TRUE),
    average_value_sam = mean(samesaken, na.rm = TRUE),
    
  )

summary_table_syn_sam$year <- as.numeric(summary_table_syn_sam$year)


library(ggplot2)
library(scales)  # Needed for the alpha() function

ggplot(summary_table_syn_sam, aes(x = year)) +
  geom_vline(aes(xintercept = year), linetype = "solid", alpha = 0.1) +  
  # Add smoothed curves first to place them behind
  geom_smooth(aes(y = average_value_sær, color = "Smoothed Average special privileges"), 
              method = "loess", se = FALSE, size = 1) +
  geom_smooth(aes(y = average_value_trus, color = "Smoothed Average Threat to Sami rights"), 
              method = "loess", se = FALSE, size = 1) +
  # Add unsmoothed lines on top
  geom_line(aes(y = average_value_sær, color = "Unsmoothed Average special privileges"), 
            size = 1) +
  geom_line(aes(y = average_value_trus, color = "Unsmoothed Average Threat to Sami rights"), 
            size = 1) +
  # Add labels
  labs(
    title = "Text snippets scored by perspective on the issue of Sami rights N=112,141, 1979-2023",
    x = "Year",
    y = "Average, 0 = No connection 1 = Strong connection",
    color = "Legend, sorted by value in 2024:"
  ) +
  # Set color scheme with adjusted alpha values
  scale_color_manual(
    values = c(
      "Smoothed Average special privileges" = alpha("lightsalmon2", 0.4),
      "Smoothed Average Threat to Sami rights" = alpha("deepskyblue3", 0.4),
      "Unsmoothed Average special privileges" = alpha("lightsalmon4", 0.9),
      "Unsmoothed Average Threat to Sami rights" = alpha("deepskyblue4", 0.9)
    ),
    limits = c(
      "Smoothed Average Threat to Sami rights",
      "Unsmoothed Average Threat to Sami rights",
      "Smoothed Average special privileges",
      "Unsmoothed Average special privileges"
    )
  ) +
  # Customize x-axis
  scale_x_continuous(breaks = seq(1979, 2023, by = 2)) +  # Adjust breaks to show every 2 years
  # Customize legend
  guides(color = guide_legend(override.aes = list(size = 1))) +
  # Set plot theme
  theme(
    panel.background = element_rect(fill = 'bisque', color = 'bisque3'),
    panel.grid.major = element_line(color = 'bisque3'),
    panel.grid.minor = element_line(color = 'bisque3'),
    legend.background = element_rect(fill = 'bisque3'),
    legend.key = element_rect(fill = 'bisque3', colour = 'bisque4'),
    plot.background = element_rect(fill = 'bisque2')
  )


