rm(list=ls()) # Clear de "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken") # Den ser ikke slik ut på windows
options(scipen = 999)
options(encoding = "UTF-8")
library(tidyverse)
library(lubridate)

#Importere konkordanser med score fra BERT 
data <- read_csv("Resultat.csv")
#Importere kopusdefinisjonen for å hente ut metadata
Korpus <- read_csv("Korpus.csv")

#Merge Korpuslister med konkordanse
merged_metadata <- left_join(Korpus, data, by = "urn")
# Fjerner de som ikke har noen konkordanser knyttet til seg (Weird men OK )
merged_metadata <- merged_metadata %>% filter(!is.na(conc))
# trekker ut år og formaterer dato
merged_metadata <- merged_metadata %>%
  mutate(
    date = ymd(timestamp)
  )


# Småtabeller
city_count <- merged_metadata %>% count(city)
newspaper_count <- merged_metadata %>% count(title)
year_count <- merged_metadata %>% count(year)

#### Testhenting fra Python ####

labeled_data_1 <- read_csv("setninger_labeled_MNLI.csv")

labeled_data_2 <- read_csv("setninger_labeled_MNLI_2.csv")

labeled_data_3 <- read_csv("setninger_labeled_MNLI_3.csv")

labeled_data_4 <- read_csv("setninger_labeled_MNLI_4.csv")


merged_data_labeled <- labeled_data_1 %>%
  left_join(labeled_data_2, by = "sentence") %>%
  left_join(labeled_data_3, by = "sentence") %>%
  left_join(labeled_data_4, by = "sentence")


names(merged_data_labeled)[names(merged_data_labeled) == 'sentence'] <- 'conc'


merged_data_labeled <- left_join(merged_metadata, merged_data_labeled, by = "conc")


# Fjerner de som ikke har noen konkordanser knyttet til seg (Weird men OK )

merged_data_labeled <- merged_data_labeled %>% filter(!is.na(samesaken))

# trekker ut år og formaterer dato
merged_data_labeled <- merged_data_labeled %>%
  mutate(
    date = ymd(timestamp)
  )

merged_data_labeled$year <- as.integer(merged_data_labeled$year)

merged_data_labeled <- merged_data_labeled %>%
  mutate(
    extracted_pattern = str_extract(conc, "\\d\\d\\.\\d\\d\\D|\\d\\d\\s\\.\\d\\d\\D|\\d\\d\\.\\s\\d\\d\\D|\\d\\d\\s\\.\\s\\d\\d\\D|\\d\\d\\s\\d\\d\\D" )
  )

new_df <- merged_data_labeled %>%
  filter(!is.na(extracted_pattern))
  
  
new_df <- new_df %>%
  filter(!grepl("(kl\\.)|(fakkeltog)|(\\bKL\\b)|(\\sKl\\s)|(kl\\s\\d+)|(kl\\s)", new_df$conc, ignore.case = TRUE))



merged_data_labeled_u_tv <- merged_data_labeled %>%
  anti_join(new_df, by = "conc") 

duplicates_specific <- merged_data_labeled_u_tv %>%
  count(conc, sort = TRUE) %>%  # replace column1, column2 with the columns you want to check
  filter(n > 1) %>%
  ungroup()

#Rens
renset_data <- merged_data_labeled_u_tv %>%
  filter(conc != "Vi ledes gjennom året 1979 av Mattis Hætta , samisk kunstner med stort engasjement mot utbygging av Alta-vassdraget , Karin Julsrud , som ledet tv-ungdomsprogrammet") %>%
  filter(conc != "Opptakten til Alta-aksjonen på 1970 - tallet handlet om bygda Masi som kunne bli lagt under vann .") %>%
  filter(conc != "20 ÅR ETTER : Jan Horne har laget program om Altasaken . NRK2 kl 19.05") %>%
  filter(conc != "Alfred Nilsen - den suverene lederen av Folkeaksjonen mot Altautbygginga . Ved Jan Pedersen og Kåre Sørensen .") %>%
  select(!'overdrevet maktbrukt av myndigheter og politi')



write.csv(renset_data, "renset_data.csv", row.names = FALSE)


