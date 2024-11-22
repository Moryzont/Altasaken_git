# Altasaken, Konkordanser -- Ord før og etter. søkeordet 

#' Retrieve Concordance for Words in Documents
#'
#' This function obtains the concordance for specified words within given documents.
#'
#' @param pids A vector or data frame containing document IDs.
#' @param words A string of words (tokens) for which the concordance will be retrieved. For multiple tokens use keyword OR
#' @param window An optional numeric value specifying the number of characters before and after the matching word (default is 20).
#' @param limit An optional numeric value specifying the maximum number of results to return (default is 5000).
#'
#' @return A data frame containing the concordance results for each word in the specified documents. Returns NULL if the API request fails or no results are found.

#' @eksempler
#' pids <- c("URN:NBN:no-nb_digibok_2008051404065", "URN:NBN:no-nb_digibok_2010092120011") 
#' @Dette skal være en liste over dokumenter du vil finne konkordanser i 
#' 
#' words <- "Norge" 
#' @Her skriver du ordet du vil finne ord rundt. 
#' Dersom du skal bruke fraser så må du sette ordet i amerikanske hermetegn: @' og ordene i norske hermetegn @" inni. Typ: words <- 'einer OR "eine bær"' ¯\_(ツ)_/¯
#' 
#' window <- 20 
#' @Antall KARAKTERER!!!! før og etter søkeordet
#' 
#' limit <- 10000 
#' @Antall ganger du skal få et ord
#' 
#' result <- get_concordance(document_ids, tokens, window, limit) 
#' @Se på resultatet ditt

rm(list=ls()) # Clear de "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken") # Den ser ikke slik ut på windows
options(scipen = 999)



library("httr")
library("jsonlite")
library(tibble)
library(tidyverse)

load("urn_periode_1.rda")
load("urn_periode_2.rda")
load("urn_periode_3.rda")
load("urn_periode_3.5.rda")
load("urn_periode_4.rda")
load("urn_periode_0.rda")

#Pids skal være korpuset ditt f.eks urn_periode_1
pids_0 <- urn_periode_0
pids_1 <- urn_periode_1
pids_2 <- urn_periode_2
pids_3 <- urn_periode_3
pids_3.5 <- urn_periode_3.5
pids_4 <- urn_periode_4

get_concordance <- function(pids, words, window=window, limit=limit) {
  if (is.data.frame(pids)) {
    pids <- unname(pids$urn)
  } else {
    pids <- unname(pids)
  }
  
  url <- "https://api.nb.no/dhlab/conc"
  
  params <- list("urns" = pids, "query" = words, "window" = window, "limit" = limit)
  
  json_params <- jsonlite::toJSON(params, auto_unbox = TRUE)
  
  query <- POST(url, body = json_params, encode = "raw", content_type("application/json"))
  
  if (http_status(query)$category != "Success") {
    warning("API request failed: ", http_status(query)$message)
    return(NULL)
  }
  
  query_content <- content(query)
  
  if (length(query_content) == 0) {
    warning("No concordance results found.")
    return(NULL)
  }
  
  return(as.data.frame(do.call(cbind, query_content)))
}

window <- 600
limit <- 100000


#Søkeord, hver linje er ulike stavelser. Ikke case sensitiv
words <- '"Alta saken" OR "Alta-saken" OR "Alta- saken" OR "Altasaken" OR 
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
          '
#Selve søket
Resultat_per_0 <- get_concordance(pids_0, words, window, limit)
Resultat_per_1 <- get_concordance(pids_1, words, window, limit)
Resultat_per_2 <- get_concordance(pids_2, words, window, limit)
Resultat_per_3 <- get_concordance(pids_3, words, window, limit)
Resultat_per_3.5 <- get_concordance(pids_3.5, words, window, limit)
Resultat_per_4 <- get_concordance(pids_4, words, window, limit)

Resultat <- rbind(Resultat_per_0, Resultat_per_1, Resultat_per_2, Resultat_per_3, Resultat_per_3.5, Resultat_per_4)
sample(Resultat$conc, 50)

Resultat$conc


# Remove </b> tags
Resultat$conc <- gsub("</b>|<b>", "", Resultat$conc)

# Remove starting and closing ellipses
Resultat$conc <- gsub("\\s*\\.\\.\\.\\s*", "", Resultat$conc)

#####EKSPORTERING TIL PYTHON OG FOR R######

#Eksporterer konkordanser og URN
Resultat_df <- data.frame("urn" = unlist(Resultat[[2]]),
                          "conc" = unlist(Resultat[[3]]))
write.csv(Resultat_df, "Resultat.csv", row.names = FALSE)

#####Prepping for Python######
Resultat <- read_csv("Resultat.csv")
Resultat$conc <- gsub('["\']', "", Resultat$conc)

# Fjerner duplikater
CleanedResultat <- data.frame(conc = Resultat$conc)
CleanedResultat <- CleanedResultat[!duplicated(CleanedResultat$conc), , drop = FALSE]

# Trekker ut et subset
SubsetCleanedResultat <- as.data.frame(sample(CleanedResultat$conc, 50))

#eksporterer ett uttrekk
write.csv(SubsetCleanedResultat, "sampleResultat.csv", row.names = FALSE)

#eksporterer alt, NB! Kan bli krøll med Python, denne filen må flyttes til WD for python for at det skal skje noe i det hele tatt. 14k setninger tok 8 minutt å sette i gang. 
write.csv(CleanedResultat, "CleanedResultat.csv", row.names = FALSE)

