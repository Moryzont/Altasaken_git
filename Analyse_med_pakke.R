

rm(list=ls()) # Clear de "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken") # Den ser ikke slik ut på windows

library("httr")
library("dplyr")
library(dhlabR)
library(tibble)

nyliste <- rownames_to_column(Norm_t, "ord")

words <- nyliste$ord 


get_reference_words <- function(doctype = "digibok", from_year = 1983, to_year = 2010, words = NULL){
  
  url <- "https://api.nb.no/dhlab/reference_words"
  
  params <- list("doctype" = doctype, "from_year" = from_year, "to_year" = to_year, "words" = words)
  query <- POST(url, body = params, encode = "json")
  
  return(content(query))
  
}

largeref <- get_reference_words(
  doctype = "digavis",
  from_year = 1983,
  to_year = 2010,
  words = words
)

# krølle det ut 
flat_list <- unlist(largeref)

# Create a matrix and then transform it into a dataframe
df <- as.data.frame(matrix(flat_list, ncol=3, byrow=TRUE))

# Rename the columns
colnames(df) <- c("ord", "int", "num")

# Convert columns to appropriate classes
df$int <- as.integer(df$int)
df$num <- as.numeric(df$num)

LargeREF_mm <-merge(df, nyliste, by = "ord", all = TRUE)

LargeREF_mm$Relv <- (LargeREF_mm$U_frekv / LargeREF_mm$num)
