# Clear the "Global Environment"
rm(list = ls())

# Adjust the path as needed
setwd("/Users/vetlewisloffsandring/Documents/Altasaken/Testing")

options(scipen = 999)

# Install and load required packages
required_packages <- c("httr", "jsonlite", "progress", "data.table", "lubridate", "tidyr", "dplyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
}

library(httr)
library(jsonlite)
library(progress)
library(data.table)
library(lubridate)
library(tidyr)
library(dplyr)

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

# Initialize a data table to store failed requests
failed_requests <- data.table(URN = character(), Term = character())

# Loop over each year from 1990 to 1992
for (year in 1979:2023) {
  
  cat("\nProcessing year:", year, "\n")
  
  # Initialize per-year variables
  all_ids <- list()  # Use a list to associate URNs with search terms
  
  # Adjust filter_values for the current year
  filter_values <- c(paste0("year:", year), "mediatype:aviser")
  
  # Initialize progress bar for search terms
  pb_terms <- txtProgressBar(min = 0, max = length(search_terms), style = 3)
  term_index <- 0
  
  # Loop over each search term
  for (term in search_terms) {
    
    term_index <- term_index + 1
    setTxtProgressBar(pb_terms, term_index)
    
    # Add double quotes around the term for exact phrase search
    quoted_term <- paste0('"', term, '"')
    
    # Initialize pagination variables
    page_number <- 0  # Zero-based index
    total_pages <- 1  # Default value, will be updated after first request
    
    # Loop over pages
    repeat {
      
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
      filters_list <- setNames(as.list(filter_values), rep("filter", length(filter_values)))
      
      # Combine the parameters
      query_params <- c(params, filters_list)
      
      # Make the GET request with HTTP/1.1
      response <- tryCatch({
        GET(
          url = base_url,
          query = query_params,
          add_headers(`accept` = "application/hal+json"),
          config = httr::config(http_version = 1.1)  # Force HTTP/1.1
        )
      }, error = function(e) {
        warning("Failed to fetch data for term '", term, "' on page ", page_number, ": ", e$message)
        return(NULL)
      })
      
      if (is.null(response)) break  # Exit if request failed
      
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
      }
      
      # Check if there are items in the response
      if (!is.null(json_data$`_embedded`$items)) {
        # Access the items in the response
        items <- json_data$`_embedded`$items
        
        # Check if 'metadata.identifiers.urn' exists in items
        if ("metadata.identifiers.urn" %in% names(items)) {
          # Extract the IDs
          ids <- items$`metadata.identifiers.urn`
          
          # Associate each URN with the search term
          for (id in ids) {
            if (id %in% names(all_ids)) {
              all_ids[[id]] <- unique(c(all_ids[[id]], term))
            } else {
              all_ids[[id]] <- term
            }
          }
        }
      }
      
      # Break the loop if we've reached the last page
      page_number <- page_number + 1
      if (page_number >= total_pages) break
      
      # Pause between requests to be polite to the server
      Sys.sleep(0.1)
    }
  }
  
  close(pb_terms)
  
  # Get unique URNs
  unique_ids <- names(all_ids)
  
  # Print the total number of unique IDs found
  cat("\nTotal unique IDs found for year ", year, ": ", length(unique_ids), "\n", sep = "")
  
  # Ensure unique_ids is not empty
  if (length(unique_ids) == 0) {
    warning("No URNs found for year ", year, ". Skipping to next year.")
    next
  }
  
  # Define the get_content_fragments function with enhanced error handling
  get_content_fragments <- function(id, terms, fragments = 3, frag_size = 300, max_retries = 5) {
    # Initialize a list to store fragments per term
    fragments_list <- list()
    
    for (term in terms) {
      # Add double quotes around the term for exact phrase search
      quoted_query <- paste0('"', term, '"')
      
      # Construct the URL
      url <- paste0("https://api.nb.no/catalog/v1/items/", id, "/contentfragments")
      
      # Construct the query parameters
      query_params <- list(
        q = quoted_query,
        fragments = fragments,
        fragSize = frag_size
      )
      
      # Retry logic parameters
      retry_delay <- 2  # Starting delay in seconds
      
      for (attempt in 1:max_retries) {
        # Send GET request with HTTP/1.1
        response <- tryCatch({
          GET(
            url,
            query = query_params,
            add_headers("accept" = "application/hal+json"),
            config = httr::config(http_version = 1.1)  # Force HTTP/1.1
          )
        }, error = function(e) {
          warning("Failed to fetch fragments for URN '", id, "', term '", term, "' (Attempt ", attempt, "): ", e$message)
          return(NULL)
        })
        
        if (is.null(response)) {
          Sys.sleep(retry_delay)
          retry_delay <- retry_delay * 2  # Exponential backoff
          next
        }
        
        # Check if the request was successful
        if (status_code(response) == 200) {
          content_json <- content(response, "text", encoding = "UTF-8")
          content <- suppressMessages(fromJSON(content_json, flatten = TRUE))
          
          # If contentFragments are empty, store an empty list
          if (!is.null(content$contentFragments) && length(content$contentFragments) > 0) {
            fragments_list[[term]] <- content$contentFragments
          } else {
            fragments_list[[term]] <- list()
          }
          break  # Exit retry loop on success
        } else {
          warning("Failed to fetch fragments for URN '", id, "', term '", term, "' with status: ", status_code(response))
          Sys.sleep(retry_delay)
          retry_delay <- retry_delay * 2  # Exponential backoff
        }
      }
      
      # If all attempts failed, log the failed URN and term
      if (attempt == max_retries && is.null(fragments_list[[term]])) {
        cat("All attempts failed for URN '", id, "', term '", term, "'. Logging for retry.\n", sep = "")
        # Append to the failed_requests data table
        failed_requests <- rbind(failed_requests, data.table(URN = id, Term = term))
      }
    }
    return(fragments_list)
  }
  
  # Initialize counters
  total_urns_with_fragments <- 0
  total_fragments <- 0
  
  # Initialize a list to store the results
  all_results <- list()
  
  # Process URNs sequentially to reduce load on the server
  pb_urns <- txtProgressBar(min = 0, max = length(unique_ids), style = 3)
  urn_index <- 0
  
  for (id in unique_ids) {
    terms <- all_ids[[id]]
    urn_index <- urn_index + 1
    setTxtProgressBar(pb_urns, urn_index)
    
    # Fetch content fragments with error handling
    result <- get_content_fragments(id, terms)
    
    # Check if any fragments were found
    if (length(result) > 0) {
      all_results[[id]] <- result
      total_urns_with_fragments <- total_urns_with_fragments + 1
      
      # Update total_fragments count
      for (term_fragments in result) {
        if (!is.null(term_fragments)) {
          total_fragments <- total_fragments + length(term_fragments)
        }
      }
    }
    
    # Pause between requests to be polite to the server
    Sys.sleep(0.1)
  }
  
  close(pb_urns)
  
  # After processing all URNs, check if there are any failed requests
  if (nrow(failed_requests) > 0) {
    cat("\nNumber of failed requests for year ", year, ": ", nrow(failed_requests), "\n", sep = "")
    
    # Optionally, save the failed requests to a CSV file for manual inspection or later retry
    fwrite(failed_requests, paste0("failed_requests_", year, ".csv"))
    
    # Retry fetching fragments for failed requests
    cat("Retrying failed requests...\n")
    
    for (i in 1:nrow(failed_requests)) {
      id <- failed_requests$URN[i]
      term <- failed_requests$Term[i]
      
      # Fetch content fragments with fewer retries to avoid infinite loops
      result <- get_content_fragments(id, term, max_retries = 5)
      
      # Check if fragments were fetched successfully
      if (length(result) > 0 && !is.null(result[[term]])) {
        if (is.null(all_results[[id]])) {
          all_results[[id]] <- result
        } else {
          all_results[[id]][[term]] <- result[[term]]
        }
        cat("Successfully fetched fragments for URN '", id, "', term '", term, "' on retry.\n", sep = "")
      } else {
        cat("Failed to fetch fragments for URN '", id, "', term '", term, "' after retry.\n", sep = "")
      }
    }
  }
  
  # Print total counts
  cat("\nTotal URNs with fragments for year ", year, ": ", total_urns_with_fragments, "\n", sep = "")
  cat("Total content fragments collected for year ", year, ": ", total_fragments, "\n", sep = "")
  
  # Initialize an empty data table with consistent column names
  result_dt <- data.table(URN = character(), text = character())
  
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
    
    # Create a data table with URN and cleaned text
    urn_dt <- data.table(URN = urn, text = cleaned_fragments)
    
    # Append to the result_dt
    result_dt <- rbind(result_dt, urn_dt)
  }
  
  # Save the results to a CSV file
  fwrite(result_dt, paste0(year, "_conc_urn.csv"))
  
  # Read the CSV file (if needed)
  result_dt <- fread(paste0(year, "_conc_urn.csv"))
  
  # If you have a labeled file to merge, define 'labeled_file' and perform merging
  # If not, you can comment out or remove the following lines
  
  # labeled_file <- paste0("setninger_labeled_MNLI_", year, ".csv")
  # if (file.exists(labeled_file)) {
  #   combined_dt <- fread(labeled_file)
  #   # Merge result_dt with combined_dt if necessary
  #   # merged_dt <- merge(result_dt, combined_dt, by = "URN", all.x = TRUE)
  #   # Proceed with processing merged_dt
  # } else {
  #   warning("Labeled file ", labeled_file, " does not exist. Skipping merging step for year ", year, ".")
  # }
  
  cat("Completed processing for year:", year, "\n")
}
