#Clear the "Global Environment"
rm(list = ls())

# Adjust the path as needed
setwd("/Users/vetlewisloffsandring/Documents/Altasaken")

options(scipen = 999)

# Install and load required packages
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")

library(httr)
library(jsonlite)

# Base URL of the API
base_url <- "https://api.nb.no/catalog/v1/items"


# Define your search terms as a character vector
search_terms <- c(
  "Alta saken", "Alta-saken", "Alta- saken", "Altasaken",
  "Alta saka", "Alta-saka", "Alta- saka", "Altasaka",
  "Alta konflikten", "Alta-konflikten", "Alta- konflikten", "Altakonflikten",
  "konflikten i Alta",
  "Alta utbyggingen", "Alta-utbyggingen", "Alta- utbyggingen", "Altautbyggingen",
  "Alta ut- byggingen", "Alta-ut- byggingen", "Altaut- byggingen",
  "Alta utbygginga", "Alta-utbygginga", "Alta- utbygginga", "Altautbygginga",
  "utbyggingen av Alta", "utbyggingen av Alta-", "utbyggingen av Altaelva", "utbyggingen av Altaelven",
  "utbygginga av Alta", "utbygginga av Alta-", "utbygginga av Altaelva", "utbygginga av Altaelven",
  "Alta demningen", "Alta-demningen", "Alta- demingen", "Altademningen",
  "Alta demninga", "Alta-demninga", "Alta- demninga", "Altademninga",
  "Alta dammen", "Alta-dammen", "Alta- dammen", "Altadammen",
  "Folkeaksjonen mot utbyggingen", "Folke-aksjonen mot utbyggingen",
  "Folkeaksjonen mot utbygginga", "Folke-aksjonen mot utbygginga",
  "demonstrantane i Stilla", "demonstrantene i Stilla", "demonstranter i Stilla", "demonstrerte i Stilla",
  "Alta demonstrantene", "Alta-demonstrantene", "Alta- demonstrantene", "Altademonstrantene",
  "Alta aksjonen", "Alta-aksjonen", "Alta- aksjonen", "Altaaksjonen",
  "Aksjonen i Alta", "Aksjonistene i Alta", "Aksjonen i Stilla",
  "Alta-Kautokeinovassdraget", "Alta- Kautokeinovassdraget", "Alta-Kautokeino vassdraget", "Alta- Kautokeino- vassdraget", "Alta-Kautokeino- vassdraget",
  "Alta-vassdraget", "Alta- Vassdraget", "Alta vassdraget",
  "Alta-kautokeino utbyggingen", "Alta- kautokeino utbyggingen",
  "Slaget i Stilla"
)

# Initialize an empty vector to store IDs
all_ids <- c()

# Loop over each search term
for (term in search_terms) {
  
  # Print the current search term
  cat("Searching for term:", term, "\n")
  
  # Add double quotes around the term for exact phrase search
  quoted_term <- paste0('"', term, '"')
  
  # Initialize pagination variables
  page_number <- 0  # Zero-based index
  total_pages <- 1  # Default value, will be updated after first request
  
  # Loop over pages
  while (page_number < total_pages) {
    
    # Query parameters without the 'filter' parameter
    params <- list(
      q = quoted_term,  # Use the quoted term directly
      searchType = "FULL_TEXT_SEARCH",
      digitalAccessibleOnly = "false",
      expand = "metadata",
      fragments = "2",
      fragSize = "500",
      profile = "wwwnbno",
      page = as.character(page_number),
      size = "100"
    )
    
    # Handle 'filter' parameters separately
    filter_values <- c("year:2022", "mediatype:aviser")
    filters_list <- setNames(as.list(filter_values), rep("filter", length(filter_values)))
    
    # Combine the parameters
    query_params <- c(params, filters_list)
    
    # For debugging: print the full URL being requested
    full_url <- modify_url(base_url, query = query_params)
    cat("Requesting URL:", full_url, "\n")
    
    # Make the GET request
    response <- GET(
      url = base_url,
      query = query_params,
      add_headers(`accept` = "application/hal+json")
    )
    
    # Check the HTTP status code
    if (http_status(response)$category != "Success") {
      warning("API request failed for term '", term, "' on page ", page_number, " with status: ", http_status(response)$message)
      break  # Exit the pagination loop and proceed to the next term
    }
    
    # Parse the JSON content
    content_text <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content_text, flatten = TRUE)
    
    # Update total_pages after the first request
    if (page_number == 0) {
      total_pages <- json_data$page$totalPages
      cat("Total pages for term '", term, "': ", total_pages, "\n", sep = "")
    }
    
    # Check if there are items in the response
    if (!is.null(json_data$`_embedded`$items)) {
      # Access the items in the response
      items <- json_data$`_embedded`$items
      
      # Check if 'metadata.identifiers.urn' exists in items
      if ("metadata.identifiers.urn" %in% names(items)) {
        # Extract the IDs
        ids <- items$`metadata.identifiers.urn`
        
        # Append the IDs to the all_ids vector
        all_ids <- c(all_ids, ids)
        
        cat("Collected ", length(ids), " IDs from page ", page_number + 1, " of ", total_pages, "\n", sep = "")
      } else {
        # Handle the case where the column doesn't exist
        cat("No URNs found for term '", term, "' on page ", page_number, "\n", sep = "")
      }
    } else {
      # No items found for this page
      cat("No results found for term '", term, "' on page ", page_number, "\n", sep = "")
    }
    
    # Increment the page number
    page_number <- page_number + 1
    
    # Pause between requests to be polite to the server
    Sys.sleep(0.5)
  }
}

# Remove duplicate IDs
unique_ids <- unique(all_ids)

# Print the total number of unique IDs found
cat("Total unique IDs found:", length(unique_ids), "\n")

# Ensure unique_ids is not empty
if (length(unique_ids) == 0) {
  stop("No URNs found. Please run the first part of the script to collect URNs.")
}

# Define the get_content_fragments function
get_content_fragments <- function(id, query, fragments = 3, frag_size = 300) {
  # Add double quotes around the term for exact phrase search
  quoted_query <- paste0('"', query, '"')
  
  # Construct the URL
  url <- paste0("https://api.nb.no/catalog/v1/items/", id, "/contentfragments")
  
  # Construct the query parameters
  query_params <- list(
    q = quoted_query,
    fragments = fragments,
    fragSize = frag_size
  )
  
  # Send GET request
  response <- GET(
    url,
    query = query_params,
    add_headers("accept" = "application/hal+json")
  )
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    content <- fromJSON(content(response, "text"), flatten = TRUE)
    
    # If contentFragments are empty, return NULL
    if (length(content$contentFragments) == 0) {
      return(NULL)
    }
    
    return(content$contentFragments)
  } else {
    # Handle request failure
    return(NULL)
  }
}

# Initialize counters
total_urns_with_fragments <- 0
total_fragments <- 0

# Initialize a list to store the results
all_results <- list()

# Loop over each URN
for (id in unique_ids) {
  
  # Print the current URN
  cat("Processing URN:", id, "\n")
  
  # Initialize a list to store results for this URN
  urn_results <- list()
  
  # Initialize fragment count for this URN
  urn_fragment_count <- 0
  
  # Loop over each search term
  for (term in search_terms) {
    
    # Fetch content fragments
    fragments <- get_content_fragments(id, term)
    
    if (!is.null(fragments)) {
      # Store the fragments in the urn_results list under the search term
      urn_results[[term]] <- fragments
      
      # Update fragment counts
      urn_fragment_count <- urn_fragment_count + length(fragments)
      total_fragments <- total_fragments + length(fragments)
    }
    
    # Pause between requests to be polite to the server
    Sys.sleep(0.001)
  }
  
  # Only add URN to results if there are any fragments
  if (length(urn_results) > 0) {
    # Store the urn_results in the all_results list under the URN
    all_results[[id]] <- urn_results
    
    # Update URN count
    total_urns_with_fragments <- total_urns_with_fragments + 1
  }
  
  # Print fragment count for this URN
  cat("Total fragments for URN", id, ":", urn_fragment_count, "\n")
}

# Print total counts
cat("Total URNs with fragments:", total_urns_with_fragments, "\n")
cat("Total content fragments collected:", total_fragments, "\n")

# Example: Print the results
# print(all_results)


# Load necessary library
library(dplyr)

# Initialize an empty data frame
result_df <- data.frame(URN = character(), Text = character(), stringsAsFactors = FALSE)

# Iterate over each URN in all_results
for (urn in names(all_results)) {
  
  # Get the list of fragments for this URN
  urn_results <- all_results[[urn]]
  
  # Initialize a vector to hold all fragments for this URN
  all_fragments <- c()
  
  # Loop over each search term
  for (term in names(urn_results)) {
    # Get the fragments for this term
    fragments <- urn_results[[term]]
    
    # Check if fragments are not empty
    if (!is.null(fragments) && length(fragments) > 0) {
      
      # Depending on the structure of fragments, extract the 'text' field
      if (is.data.frame(fragments) && "text" %in% names(fragments)) {
        fragment_texts <- fragments$text
      } else if (is.list(fragments)) {
        # If fragments is a list, extract 'text' from each element
        fragment_texts <- sapply(fragments, function(x) x$text)
      } else {
        # If the structure is different, print a warning
        warning(paste("Unexpected structure in fragments for URN:", urn, "and term:", term))
        next
      }
      
      # Collect all fragments
      all_fragments <- c(all_fragments, fragment_texts)
    }
  }
  
  # Remove duplicate fragments for this URN
  unique_fragments <- unique(all_fragments)
  
  # Remove '<em>' tags from the text using regex
  cleaned_fragments <- gsub("<[^>]+>", "", unique_fragments)
  
  # Create a data frame with URN and cleaned text
  urn_df <- data.frame(URN = rep(urn, length(cleaned_fragments)), Text = cleaned_fragments, stringsAsFactors = FALSE)
  
  # Append to the result_df
  result_df <- bind_rows(result_df, urn_df)
}

# Save the results to a CSV file
write.csv(result_df, "2022_conc_urn.csv", row.names = FALSE)


result_df <- read.csv("2022_conc_urn.csv")

##### Kombinering og legge til metadata
# Read the CSV files
result_df <- read.csv("2022_conc_urn.csv")
combined_df <- read.csv("setninger_labeled_MNLI_2022.csv", stringsAsFactors = FALSE)


# Merge the data frames

names(result_df)[names(result_df)=="Text"] <- "sentence"

#add URN 
combined_URN <- merge(result_df, combined_df, by = "sentence", all.x = TRUE)

library(dplyr)
library(tidyr)
library(tidyverse)

# Split the URN into multiple columns
combined_URN <- combined_URN %>%
  separate(URN, into = paste0("URN_part", 1:10), sep = "_", fill = "right", remove = FALSE)

# Extract the desired components
combined_URN <- combined_URN %>%
  mutate(
    title = URN_part3,
    timestamp = URN_part6,
    year = substr(timestamp, 1, 4)
  )

# Remove the temporary URN_part columns
combined_URN <- combined_URN %>%
  select(-starts_with("URN_part"))



#Fjerner duplikater
combined_URN <- combined_URN %>%
  distinct(.keep_all = TRUE)

# Lager en tom rad for byer
combined_URN[ , 'city'] <- NA
combined_URN[ , 'dhlabid'] <- NA
combined_URN[ , 'date'] <- NA

# Nytt navn til URN
names(combined_URN)[names(combined_URN)=="URN"] <- "urn"
names(combined_URN)[names(combined_URN)=="sentence"] <- "conc"

#Legger inn sted på aviser som mangler NB! Ting mangler. 
combined_URN <- combined_URN %>%
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
  mutate(city = ifelse(title == "lofottidende", "Leknes", city))


colnames(combined_URN)
combined_URN_sortert = combined_URN[,c(73,2,69,72,70,71,1,74,3:68)] 


library(lubridate)
combined_URN_sortert <- combined_URN_sortert %>%
  mutate(
    date = ymd(timestamp)
  )

write.csv(combined_URN_sortert, "2022_conc_renset.csv", row.names = FALSE)

