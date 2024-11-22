

rm(list=ls()) # Clear de "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken") # Den ser ikke slik ut på windows

library("httr")
library("dplyr")

#Lager en funksjon som snakker med NB 
get_document_corpus <- function(doctype=NULL, author=NULL, ddk=NULL, freetext=NULL, subject=NULL, from_timestamp=NULL, to_timestamp=NULL, publisher=NULL, limit=100000, order_and_limit_by_rank=NULL, title=NULL, from_year=NULL, to_year=NULL, fulltext=NULL, lang=NULL){
  
  url <- "https://api.nb.no/dhlab/build_corpus"
  
  params <- list("author" = author, 
                 "ddk" = ddk, 
                 "doctype" = doctype, 
                 "freetext" = freetext, 
                 "from_timestamp" = from_timestamp, 
                 "from_year" = from_year, 
                 "fulltext" = fulltext, 
                 "lang" = lang, 
                 "limit" = limit, 
                 "order_and_limit_by_rank" = order_and_limit_by_rank, 
                 "publisher" = publisher, 
                 "subject" = subject, 
                 "title" = title, 
                 "to_timestamp" = to_timestamp, 
                 "to_year" = to_year)
  
  query <- POST(url, body = params, encode = "json")
  
  return(as.data.frame(do.call(cbind, content(query))))
}

#Søker gjennom NB og gir oss en liste med aviser. - Korpus 1 før aksjonen 
Korp_per_0 <- get_document_corpus(doctype = "digavis",
                                  fulltext = '
                            "Alta saken" OR "Alta-saken" OR "Alta- saken" OR "Altasaken" OR 
                            "Alta saka" OR "Alta-saka" OR "Alta- saka" OR "Altasaka" OR 
                            "Alta konflikten" OR "Alta-konflikten" OR "Alta- konflikten" OR "Altakonflikten" OR 
                            "konflikten i Alta" OR
                            "Alta utbyggingen" OR "Alta-utbyggingen" OR "Alta- utbyggingen" OR "Altautbyggingen" OR
                            "Alta ut- byggingen" OR "Alta-ut- byggingen" OR "Altaut- byggingen" OR
                            "Alta utbygginga" OR "Alta-utbygginga" OR "Alta- utbygginga" OR "Altautbygginga" OR
                            "utbyggingen av Alta" OR "utbyggingen av Alta-" OR "utbyggingen av Altaelva" OR "utbyggingen av Altaelven" OR
                            "utbygginga av Alta" OR "utbygginga av Alta-" OR "utbygginga av Altaelva" OR "utbygginga av Altaelven" OR
                            "Alta demningen" OR "Alta-demningen" OR "Alta- demingen" OR "Altademningen" OR
                            "Alta demninga" OR "Alta-demninga" OR "Alta- demninga" OR "Altademninga" OR
                            "Alta dammen" OR "Alta-dammen" OR "Alta- dammen" OR "Altadammen" OR
                            "Folkeaksjonen mot utbyggingen" OR "Folke-aksjonen mot utbyggingen" OR
                            "Folkeaksjonen mot utbygginga" OR "Folke-aksjonen mot utbygginga" OR
                            "demonstrantane i Stilla" OR "demonstrantene i Stilla" OR "demonstranter i Stilla" OR "demonstrerte i Stilla" OR
                            "Alta demonstrantene" OR "Alta-demonstrantene" OR "Alta- demonstrantene" OR "Altademonstrantene" OR
                            "Alta aksjonen" OR "Alta-aksjonen" OR "Alta- aksjonen" OR "Altaaksjonen" OR
                            "Aksjonen i alta" OR "Aksjonistene i alta" OR "Aksjonen i stilla" OR
                            "Alta-Kautokeinovassdraget" OR "Alta- Kautokeinovassdraget" OR "Alta-Kautokeino vassdraget" OR "Alta- Kautokeino- vassdraget" OR "Alta-Kautokeino- vassdraget" OR 
                            "Alta-vassdraget" OR "Alta- Vassdraget" OR "Alta vassdraget" OR
                            "Alta-kautokeino utbyggingen" OR "Alta- kautokeino utbyggingen" OR
                            "Slaget i stilla" OR
                            utbyggingen altaelva OR utbyggingen altaelven OR
                            kraftutbygging altaelva OR kraftutbygging altaelven OR
                            kraftverk altaelva OR kraftverk altaelven OR
                            elektrisitet altaelva OR elektrisitet altaelven OR
                            strøm altaelva OR strøm altaelven OR
                            aksjonen altaelva OR aksjonen altaelven OR
                            aksjon altaelva OR aksjon altaelven OR
                            demning altaelva OR demning altaelven OR
                            demning "alta-elva" OR demning "alta-elven"
          ',
                                  from_year = 1978, 
                                  to_year = 1981
)
Korp_per_1 <- get_document_corpus(doctype = "digavis",
                            fulltext = '
                            "Alta saken" OR "Alta-saken" OR "Alta- saken" OR "Altasaken" OR 
                            "Alta saka" OR "Alta-saka" OR "Alta- saka" OR "Altasaka" OR 
                            "Alta konflikten" OR "Alta-konflikten" OR "Alta- konflikten" OR "Altakonflikten" OR 
                            "konflikten i Alta" OR
                            "Alta utbyggingen" OR "Alta-utbyggingen" OR "Alta- utbyggingen" OR "Altautbyggingen" OR
                            "Alta ut- byggingen" OR "Alta-ut- byggingen" OR "Altaut- byggingen" OR
                            "Alta utbygginga" OR "Alta-utbygginga" OR "Alta- utbygginga" OR "Altautbygginga" OR
                            "utbyggingen av Alta" OR "utbyggingen av Alta-" OR "utbyggingen av Altaelva" OR "utbyggingen av Altaelven" OR
                            "utbygginga av Alta" OR "utbygginga av Alta-" OR "utbygginga av Altaelva" OR "utbygginga av Altaelven" OR
                            "Alta demningen" OR "Alta-demningen" OR "Alta- demingen" OR "Altademningen" OR
                            "Alta demninga" OR "Alta-demninga" OR "Alta- demninga" OR "Altademninga" OR
                            "Alta dammen" OR "Alta-dammen" OR "Alta- dammen" OR "Altadammen" OR
                            "Folkeaksjonen mot utbyggingen" OR "Folke-aksjonen mot utbyggingen" OR
                            "Folkeaksjonen mot utbygginga" OR "Folke-aksjonen mot utbygginga" OR
                            "demonstrantane i Stilla" OR "demonstrantene i Stilla" OR "demonstranter i Stilla" OR "demonstrerte i Stilla" OR
                            "Alta demonstrantene" OR "Alta-demonstrantene" OR "Alta- demonstrantene" OR "Altademonstrantene" OR
                            "Alta aksjonen" OR "Alta-aksjonen" OR "Alta- aksjonen" OR "Altaaksjonen" OR
                            "Aksjonen i alta" OR "Aksjonistene i alta" OR "Aksjonen i stilla" OR
                            "Alta-Kautokeinovassdraget" OR "Alta- Kautokeinovassdraget" OR "Alta-Kautokeino vassdraget" OR "Alta- Kautokeino- vassdraget" OR "Alta-Kautokeino- vassdraget" OR 
                            "Alta-vassdraget" OR "Alta- Vassdraget" OR "Alta vassdraget" OR
                            "Alta-kautokeino utbyggingen" OR "Alta- kautokeino utbyggingen" OR
                            "Slaget i stilla" OR
                            utbyggingen altaelva OR utbyggingen altaelven OR
                            kraftutbygging altaelva OR kraftutbygging altaelven OR
                            kraftverk altaelva OR kraftverk altaelven OR
                            elektrisitet altaelva OR elektrisitet altaelven OR
                            strøm altaelva OR strøm altaelven OR
                            aksjonen altaelva OR aksjonen altaelven OR
                            aksjon altaelva OR aksjon altaelven OR
                            demning altaelva OR demning altaelven OR
                            demning "alta-elva" OR demning "alta-elven"
          ',
                            from_year = 1981, 
                            to_year = 1983
)

#Søker gjennom NB og gir oss en liste med aviser. - Korpus 2, Under aksjonen 
Korp_per_2 <- get_document_corpus(doctype = "digavis",
                                  fulltext = '
                            "Alta saken" OR "Alta-saken" OR "Alta- saken" OR "Altasaken" OR 
                            "Alta saka" OR "Alta-saka" OR "Alta- saka" OR "Altasaka" OR 
                            "Alta konflikten" OR "Alta-konflikten" OR "Alta- konflikten" OR "Altakonflikten" OR 
                            "konflikten i Alta" OR
                            "Alta utbyggingen" OR "Alta-utbyggingen" OR "Alta- utbyggingen" OR "Altautbyggingen" OR
                            "Alta ut- byggingen" OR "Alta-ut- byggingen" OR "Altaut- byggingen" OR
                            "Alta utbygginga" OR "Alta-utbygginga" OR "Alta- utbygginga" OR "Altautbygginga" OR
                            "utbyggingen av Alta" OR "utbyggingen av Alta-" OR "utbyggingen av Altaelva" OR "utbyggingen av Altaelven" OR
                            "utbygginga av Alta" OR "utbygginga av Alta-" OR "utbygginga av Altaelva" OR "utbygginga av Altaelven" OR
                            "Alta demningen" OR "Alta-demningen" OR "Alta- demingen" OR "Altademningen" OR
                            "Alta demninga" OR "Alta-demninga" OR "Alta- demninga" OR "Altademninga" OR
                            "Alta dammen" OR "Alta-dammen" OR "Alta- dammen" OR "Altadammen" OR
                            "Folkeaksjonen mot utbyggingen" OR "Folke-aksjonen mot utbyggingen" OR
                            "Folkeaksjonen mot utbygginga" OR "Folke-aksjonen mot utbygginga" OR
                            "demonstrantane i Stilla" OR "demonstrantene i Stilla" OR "demonstranter i Stilla" OR "demonstrerte i Stilla" OR
                            "Alta demonstrantene" OR "Alta-demonstrantene" OR "Alta- demonstrantene" OR "Altademonstrantene" OR
                            "Alta aksjonen" OR "Alta-aksjonen" OR "Alta- aksjonen" OR "Altaaksjonen" OR
                            "Aksjonen i alta" OR "Aksjonistene i alta" OR "Aksjonen i stilla" OR
                            "Alta-Kautokeinovassdraget" OR "Alta- Kautokeinovassdraget" OR "Alta-Kautokeino vassdraget" OR "Alta- Kautokeino- vassdraget" OR "Alta-Kautokeino- vassdraget" OR 
                            "Alta-vassdraget" OR "Alta- Vassdraget" OR "Alta vassdraget" OR
                            "Alta-kautokeino utbyggingen" OR "Alta- kautokeino utbyggingen" OR
                            "Slaget i stilla" OR
                            utbyggingen altaelva OR utbyggingen altaelven OR
                            kraftutbygging altaelva OR kraftutbygging altaelven OR
                            kraftverk altaelva OR kraftverk altaelven OR
                            elektrisitet altaelva OR elektrisitet altaelven OR
                            strøm altaelva OR strøm altaelven OR
                            aksjonen altaelva OR aksjonen altaelven OR
                            aksjon altaelva OR aksjon altaelven OR
                            demning altaelva OR demning altaelven OR
                            demning "alta-elva" OR demning "alta-elven"
          ',
                                  from_year = 1983, 
                                  to_year = 1991
)

#Søker gjennom NB og gir oss en liste med aviser. - Korpus 3, Etter aksjonen 
Korp_per_3 <- get_document_corpus(doctype = "digavis",
                                  fulltext = '
                            "Alta saken" OR "Alta-saken" OR "Alta- saken" OR "Altasaken" OR 
                            "Alta saka" OR "Alta-saka" OR "Alta- saka" OR "Altasaka" OR 
                            "Alta konflikten" OR "Alta-konflikten" OR "Alta- konflikten" OR "Altakonflikten" OR 
                            "konflikten i Alta" OR
                            "Alta utbyggingen" OR "Alta-utbyggingen" OR "Alta- utbyggingen" OR "Altautbyggingen" OR
                            "Alta ut- byggingen" OR "Alta-ut- byggingen" OR "Altaut- byggingen" OR
                            "Alta utbygginga" OR "Alta-utbygginga" OR "Alta- utbygginga" OR "Altautbygginga" OR
                            "utbyggingen av Alta" OR "utbyggingen av Alta-" OR "utbyggingen av Altaelva" OR "utbyggingen av Altaelven" OR
                            "utbygginga av Alta" OR "utbygginga av Alta-" OR "utbygginga av Altaelva" OR "utbygginga av Altaelven" OR
                            "Alta demningen" OR "Alta-demningen" OR "Alta- demingen" OR "Altademningen" OR
                            "Alta demninga" OR "Alta-demninga" OR "Alta- demninga" OR "Altademninga" OR
                            "Alta dammen" OR "Alta-dammen" OR "Alta- dammen" OR "Altadammen" OR
                            "Folkeaksjonen mot utbyggingen" OR "Folke-aksjonen mot utbyggingen" OR
                            "Folkeaksjonen mot utbygginga" OR "Folke-aksjonen mot utbygginga" OR
                            "demonstrantane i Stilla" OR "demonstrantene i Stilla" OR "demonstranter i Stilla" OR "demonstrerte i Stilla" OR
                            "Alta demonstrantene" OR "Alta-demonstrantene" OR "Alta- demonstrantene" OR "Altademonstrantene" OR
                            "Alta aksjonen" OR "Alta-aksjonen" OR "Alta- aksjonen" OR "Altaaksjonen" OR
                            "Aksjonen i alta" OR "Aksjonistene i alta" OR "Aksjonen i stilla" OR
                            "Alta-Kautokeinovassdraget" OR "Alta- Kautokeinovassdraget" OR "Alta-Kautokeino vassdraget" OR "Alta- Kautokeino- vassdraget" OR "Alta-Kautokeino- vassdraget" OR 
                            "Alta-vassdraget" OR "Alta- Vassdraget" OR "Alta vassdraget" OR
                            "Alta-kautokeino utbyggingen" OR "Alta- kautokeino utbyggingen" OR
                            "Slaget i stilla" OR
                            utbyggingen altaelva OR utbyggingen altaelven OR
                            kraftutbygging altaelva OR kraftutbygging altaelven OR
                            kraftverk altaelva OR kraftverk altaelven OR
                            elektrisitet altaelva OR elektrisitet altaelven OR
                            strøm altaelva OR strøm altaelven OR
                            aksjonen altaelva OR aksjonen altaelven OR
                            aksjon altaelva OR aksjon altaelven OR
                            demning altaelva OR demning altaelven OR
                            demning "alta-elva" OR demning "alta-elven"
          ',
                                  from_year = 1991, 
                                  to_year = 2001
)

Korp_per_3.5 <- get_document_corpus(doctype = "digavis",
                                  fulltext = '
                            "Alta saken" OR "Alta-saken" OR "Alta- saken" OR "Altasaken" OR 
                            "Alta saka" OR "Alta-saka" OR "Alta- saka" OR "Altasaka" OR 
                            "Alta konflikten" OR "Alta-konflikten" OR "Alta- konflikten" OR "Altakonflikten" OR 
                            "konflikten i Alta" OR
                            "Alta utbyggingen" OR "Alta-utbyggingen" OR "Alta- utbyggingen" OR "Altautbyggingen" OR
                            "Alta ut- byggingen" OR "Alta-ut- byggingen" OR "Altaut- byggingen" OR
                            "Alta utbygginga" OR "Alta-utbygginga" OR "Alta- utbygginga" OR "Altautbygginga" OR
                            "utbyggingen av Alta" OR "utbyggingen av Alta-" OR "utbyggingen av Altaelva" OR "utbyggingen av Altaelven" OR
                            "utbygginga av Alta" OR "utbygginga av Alta-" OR "utbygginga av Altaelva" OR "utbygginga av Altaelven" OR
                            "Alta demningen" OR "Alta-demningen" OR "Alta- demingen" OR "Altademningen" OR
                            "Alta demninga" OR "Alta-demninga" OR "Alta- demninga" OR "Altademninga" OR
                            "Alta dammen" OR "Alta-dammen" OR "Alta- dammen" OR "Altadammen" OR
                            "Folkeaksjonen mot utbyggingen" OR "Folke-aksjonen mot utbyggingen" OR
                            "Folkeaksjonen mot utbygginga" OR "Folke-aksjonen mot utbygginga" OR
                            "demonstrantane i Stilla" OR "demonstrantene i Stilla" OR "demonstranter i Stilla" OR "demonstrerte i Stilla" OR
                            "Alta demonstrantene" OR "Alta-demonstrantene" OR "Alta- demonstrantene" OR "Altademonstrantene" OR
                            "Alta aksjonen" OR "Alta-aksjonen" OR "Alta- aksjonen" OR "Altaaksjonen" OR
                            "Aksjonen i alta" OR "Aksjonistene i alta" OR "Aksjonen i stilla" OR
                            "Alta-Kautokeinovassdraget" OR "Alta- Kautokeinovassdraget" OR "Alta-Kautokeino vassdraget" OR "Alta- Kautokeino- vassdraget" OR "Alta-Kautokeino- vassdraget" OR 
                            "Alta-vassdraget" OR "Alta- Vassdraget" OR "Alta vassdraget" OR
                            "Alta-kautokeino utbyggingen" OR "Alta- kautokeino utbyggingen" OR
                            "Slaget i stilla" OR
                            utbyggingen altaelva OR utbyggingen altaelven OR
                            kraftutbygging altaelva OR kraftutbygging altaelven OR
                            kraftverk altaelva OR kraftverk altaelven OR
                            elektrisitet altaelva OR elektrisitet altaelven OR
                            strøm altaelva OR strøm altaelven OR
                            aksjonen altaelva OR aksjonen altaelven OR
                            aksjon altaelva OR aksjon altaelven OR
                            demning altaelva OR demning altaelven OR
                            demning "alta-elva" OR demning "alta-elven"
          ',
                                  from_year = 2001, 
                                  to_year = 2011
)
#Søker gjennom NB og gir oss en liste med aviser. - Korpus 4, Lenge Etter aksjonen 
Korp_per_4 <- get_document_corpus(doctype = "digavis",
                                  fulltext = '
                            "Alta saken" OR "Alta-saken" OR "Alta- saken" OR "Altasaken" OR 
                            "Alta saka" OR "Alta-saka" OR "Alta- saka" OR "Altasaka" OR 
                            "Alta konflikten" OR "Alta-konflikten" OR "Alta- konflikten" OR "Altakonflikten" OR 
                            "konflikten i Alta" OR
                            "Alta utbyggingen" OR "Alta-utbyggingen" OR "Alta- utbyggingen" OR "Altautbyggingen" OR
                            "Alta ut- byggingen" OR "Alta-ut- byggingen" OR "Altaut- byggingen" OR
                            "Alta utbygginga" OR "Alta-utbygginga" OR "Alta- utbygginga" OR "Altautbygginga" OR
                            "utbyggingen av Alta" OR "utbyggingen av Alta-" OR "utbyggingen av Altaelva" OR "utbyggingen av Altaelven" OR
                            "utbygginga av Alta" OR "utbygginga av Alta-" OR "utbygginga av Altaelva" OR "utbygginga av Altaelven" OR
                            "Alta demningen" OR "Alta-demningen" OR "Alta- demingen" OR "Altademningen" OR
                            "Alta demninga" OR "Alta-demninga" OR "Alta- demninga" OR "Altademninga" OR
                            "Alta dammen" OR "Alta-dammen" OR "Alta- dammen" OR "Altadammen" OR
                            "Folkeaksjonen mot utbyggingen" OR "Folke-aksjonen mot utbyggingen" OR
                            "Folkeaksjonen mot utbygginga" OR "Folke-aksjonen mot utbygginga" OR
                            "demonstrantane i Stilla" OR "demonstrantene i Stilla" OR "demonstranter i Stilla" OR "demonstrerte i Stilla" OR
                            "Alta demonstrantene" OR "Alta-demonstrantene" OR "Alta- demonstrantene" OR "Altademonstrantene" OR
                            "Alta aksjonen" OR "Alta-aksjonen" OR "Alta- aksjonen" OR "Altaaksjonen" OR
                            "Aksjonen i alta" OR "Aksjonistene i alta" OR "Aksjonen i stilla" OR
                            "Alta-Kautokeinovassdraget" OR "Alta- Kautokeinovassdraget" OR "Alta-Kautokeino vassdraget" OR "Alta- Kautokeino- vassdraget" OR "Alta-Kautokeino- vassdraget" OR 
                            "Alta-vassdraget" OR "Alta- Vassdraget" OR "Alta vassdraget" OR
                            "Alta-kautokeino utbyggingen" OR "Alta- kautokeino utbyggingen" OR
                            "Slaget i stilla" OR
                            utbyggingen altaelva OR utbyggingen altaelven OR
                            kraftutbygging altaelva OR kraftutbygging altaelven OR
                            kraftverk altaelva OR kraftverk altaelven OR
                            elektrisitet altaelva OR elektrisitet altaelven OR
                            strøm altaelva OR strøm altaelven OR
                            aksjonen altaelva OR aksjonen altaelven OR
                            aksjon altaelva OR aksjon altaelven OR
                            demning altaelva OR demning altaelven OR
                            demning "alta-elva" OR demning "alta-elven"
          ',
                                  from_year = 2011, 
                                  to_year = 2024
)


#Tar vekk rader vi ikke trenger 
korp_per_0_t <- Korp_per_0[,-c(4:7,11:17)] 
korp_per_1_t <- Korp_per_1[,-c(4:7,11:17)] 
korp_per_2_t <- Korp_per_2[,-c(4:7,11:17)] 
korp_per_3_t <- Korp_per_3[,-c(4:7,11:17)]
korp_per_3.5_t <- Korp_per_3.5[,-c(4:7,11:17)] 
korp_per_4_t <- Korp_per_4[,-c(4:7,11:17)] 

Korpus_df <- rbind(korp_per_0_t, korp_per_1_t, korp_per_2_t, korp_per_3_t, korp_per_3.5_t, korp_per_4_t)


#Legger inn sted på aviser som mangler NB! Ting mangler. 
Korpus_df <- Korpus_df %>%
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
  mutate(city = ifelse(title == "lofottidende", "Leknes", city))


# Velger de radene som har NULL på sted -- For å lage en liste over aviser som mangler sted. 
Korpus_[Korpus$city == "NULL",]
Korpus_igjen <- korp_per_alle_t[korp_per_alle_t$city == "NULL",] 
unique(Korpus_igjen$title)

#Søker på tittel -- For å feilsøke og sjekke 
korp_per_1_t[korp_per_1_t$title == "arbeiderbladetoslo",]

# Lager en vektor som definerer korpuset 
urn_periode_0 <- pull(korp_per_0_t,"urn")
urn_periode_1 <- pull(korp_per_1_t,"urn")
urn_periode_2 <- pull(korp_per_2_t,"urn")
urn_periode_3 <- pull(korp_per_3_t,"urn")
urn_periode_3.5 <- pull(korp_per_3.5_t,"urn")
urn_periode_4 <- pull(korp_per_4_t,"urn")
urn_periode_alle <- pull(korp_per_alle_t,"urn")

save(urn_periode_0, file = "urn_periode_0.rda")
save(urn_periode_1, file = "urn_periode_1.rda")
save(urn_periode_2, file = "urn_periode_2.rda")
save(urn_periode_3, file = "urn_periode_3.rda")
save(urn_periode_3.5, file = "urn_periode_3.5.rda")
save(urn_periode_4, file = "urn_periode_4.rda")
save(urn_periode_alle, file = "urn_periode_alle.rda")

# Lage en samlet korpusfil

Korpus_df$dhlabid <- unlist(Korpus_df$dhlabid)
Korpus_df$urn <- unlist(Korpus_df$urn)
Korpus_df$title <- unlist(Korpus_df$title)
Korpus_df$city <- unlist(Korpus_df$city)
Korpus_df$timestamp <- unlist(Korpus_df$timestamp)
Korpus_df$year <- unlist(Korpus_df$year)


year_count <- Korpus %>% count(year)


write.csv(Korpus_df, "Korpus.csv")


Korpus <- read_csv("Korpus.csv")
year_count <- Korpus %>% count(year)

