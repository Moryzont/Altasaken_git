#' Get collocations for word in corpus
#'
#' This function retrieves collocation data from a corpus using a given word and a list of unique identifiers (pids) of corpus data frame.
#'
#' @param pids A vector or data frame containing the unique identifiers of the texts in the corpus.
#' @param word The target word for which you want to find concordances.
#' @param before The number of words before the target word to include in the context (default is 10).
#' @param after The number of words after the target word to include in the context (default is 10).
#' @param sample_size The number of samples to retrieve from the API (default is 5000).
#'
#' @return A data frame of concordances.
#'
#' @import httr jsonlite
#' @export
#'
#' @examples
#'pids <- c("URN:NBN:no-nb_digibok_2008051404065", "URN:NBN:no-nb_digibok_2010092120011")
#'word <- "."
#'collocations <- get_collocations(pids, word)
#'

rm(list=ls()) # Clear de "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken") # Den ser ikke slik ut på windows


library(jsonlite)
library(httr)
library(rlang)
library(flipTables)
library(dplyr)

install.packages("devtools")
require(devtools)
install_github("Displayr/flipTables", dependencies = NA)

#Ikke standardform på taøø
options(scipen = 999)

load("urn_periode_1.rda")
load("urn_periode_2.rda")
load("urn_periode_3.rda")
load("urn_periode_4.rda")


#Pids skal være korpuset ditt f.eks urn_periode_1
pids <- urn_periode_3
word <- "Altasaken"
word <- "Alta-utbyggingen"


# NB! Denne er case-sensitive

get_collocations_n10 <- function(pids, word, before=10, after=10, sample_size=1000000){
  
  if (is.data.frame(pids)) {
    pids <- unname(pids$urn)
  } else {
    pids <- unname(pids)
  }
  
  url <- "https://api.nb.no/dhlab/urncolldist_urn"
  
  params <- list("urn" = pids, "word" = word, "before" = before, "after" = after, "samplesize" = sample_size)
  
  query <- POST(url, body = params, encode = "json")
  
  return(as.data.frame(do.call(cbind, fromJSON(content(query)))))
}

word <- "Altasaken"
Altasaken_C <- get_collocations_n10(pids, word)
word <- "Alta-utbyggingen"
Alta_utbyggingen_C <- get_collocations_n10(pids, word)

kombo <- Cbind(Alta_utbyggingen_C, Altasaken_C)

# Altasaken data 
Altasaken_C$counts <- as.numeric(Altasaken_C$counts)
Altasaken_CS <- Altasaken_C %>% arrange(desc(counts))
# Alta-utbyggingen data
Alta_utbyggingen_C$counts <- as.numeric(Alta_utbyggingen_C$counts)
Alta_utbyggingen_CS <- Alta_utbyggingen_C %>% arrange(desc(counts))
# Kombinert data
kombo <- kombo %>% replace(.=="NULL", NA)
names(kombo)[1] = "anntall_AU"
names(kombo)[4] = "anntall_AS"
kombo$anntall_AU <- as.numeric(unlist(kombo$anntall_AU))
kombo$anntall_AS <- as.numeric(unlist(kombo$anntall_AS))
kombo_S <- kombo %>% arrange(desc(anntall_AU))
#Legger ordene sammen 
kombo_S$anntall_tot_n10 <- kombo_S$anntall_AU + kombo_S$anntall_AS
#Sorterer dem 
kombo_S <- kombo_S %>% arrange(desc(anntall_tot_n10))

#Kontekster 

get_collocations_n20 <- function(pids, word, before=20, after=20, sample_size=1000000){
  
  if (is.data.frame(pids)) {
    pids <- unname(pids$urn)
  } else {
    pids <- unname(pids)
  }
  
  url <- "https://api.nb.no/dhlab/urncolldist_urn"
  
  params <- list("urn" = pids, "word" = word, "before" = before, "after" = after, "samplesize" = sample_size)
  
  query <- POST(url, body = params, encode = "json")
  
  return(as.data.frame(do.call(cbind, fromJSON(content(query)))))
}

# Lage søket
word <- "Altasaken"
Altasaken_U <- get_collocations_n20(pids, word)
word <- "Alta-utbyggingen"
Alta_utbyggingen_U <- get_collocations_n20(pids, word)

#Slå sammen 
kombo_U <- Cbind(Alta_utbyggingen_U, Altasaken_U)
kombo_U <- kombo_U %>% replace(.=="NULL", NA)
names(kombo_U)[1] = "anntall_AU_20"
names(kombo_U)[4] = "anntall_AS_20"
kombo_U$anntall_AU_20 <- as.numeric(unlist(kombo_U$anntall_AU_20))
kombo_U$anntall_AS_20 <- as.numeric(unlist(kombo_U$anntall_AS_20))
kombo_US <- kombo_U %>% arrange(desc(anntall_AU_20))

#Legger ordene sammen 
kombo_US$anntall_tot_n20 <- kombo_US$anntall_AU_20 + kombo_US$anntall_AS_20
#Sorterer dem 
kombo_US <- kombo_US %>% arrange(desc(anntall_tot_n20))

#Normalisering

Norm <- Cbind(kombo_S, kombo_US)

Norm_t <- Norm[,-c(1:6,8:13)] 

ord_C <- sum(Norm_t$kombo_S.anntall_tot_n10, na.rm = TRUE)
ord_U <- sum(Norm_t$kombo_US.anntall_tot_n20, na.rm = TRUE)

Norm_t$C_frekv <- (Norm_t$kombo_S.anntall_tot_n10 / ord_C) * 100
Norm_t$U_frekv <- (Norm_t$kombo_US.anntall_tot_n20 / ord_U) * 100

Norm_t$Relv <- (Norm_t$C_frekv / Norm_t$U_frekv)



Norm_t <- Norm_t %>% arrange(desc(Relv))

View(Norm_t)
