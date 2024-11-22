rm(list = ls())  # Clear the "Global Environment"
setwd("/Users/vetlewisloffsandring/Documents/Altasaken")  # Adjust the path as needed
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
  
  # Encode the search term for URL
  encoded_term <- URLencode(quoted_term, reserved = TRUE)
  
  # Initialize pagination variables
  page_number <- 0  # Zero-based index
  total_pages <- 1  # Default value, will be updated after first request
  
  # Loop over pages
  while (page_number < total_pages) {
    
    # Query parameters without the 'filter' parameter
    params <- list(
      q = encoded_term,  # Your search term
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
    filter_values <- c("year:2023", "mediatype:aviser")
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
  
  # Optional: Remove duplicates after each term
  # all_ids <- unique(all_ids)
}

# Remove duplicate IDs
unique_ids <- unique(all_ids)

# Print the total number of unique IDs found
cat("Total unique IDs found:", length(unique_ids), "\n")

# Print the unique IDs
print(unique_ids)

# Optionally, save the IDs to a file
# writeLines(unique_ids, "urn_list.txt")


# Ensure unique_ids is not empty
if (length(unique_ids) == 0) {
  stop("No URNs found. Please run the first part of the script to collect URNs.")
}

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
  
  # For debugging: print the full URL being requested
  full_url <- modify_url(url, query = query_params)
  cat("Requesting URL:", full_url, "\n")
  
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


# Loop over each URN
for (id in unique_ids) {
  
  # Print the current URN
  cat("Processing URN:", id, "\n")
  
  # Initialize a list to store results for this URN
  urn_results <- list()
  
  # Loop over each search term
  for (term in search_terms) {
    
    # Fetch content fragments
    fragments <- get_content_fragments(id, term)
    
    if (!is.null(fragments)) {
      # Store the fragments in the urn_results list under the search term
      urn_results[[term]] <- fragments
    }
    
    # Pause between requests to be polite to the server
    Sys.sleep(0.1)
  }
  
  # Only add URN to results if there are any fragments
  if (length(urn_results) > 0) {
    # Store the urn_results in the all_results list under the URN
    all_results[[id]] <- urn_results
  }
}

# Now all_results contains content fragments for each URN and each search term
# You can inspect or process the results as needed

# Example: Print the results
print(all_results)