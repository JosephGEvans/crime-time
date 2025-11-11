# Download the Crimes - 2001 to Present CSV

# Libraries
library(RSocrata)

#' Get Chicago Crimes Data
#' 
#' This function only returns data regarding violent crimes from 2015-2024.
#' First it will check if the data is already stored locally in a csv file.
#' If not, it will download the data from the Chicago Data Portal API
#' and create the csv file locally.  Then, when the function is called again
#' in the future it will reference that local file.
#' 
#' @return a data frame containing Chicago violent crime data 2025-2024
get_crime_data <- function() {
  
  data_dir <- "data"
  file_name <- "violent_crimes_2015_2024.csv"
  file_path <- file.path(data_dir, file_name)
  
  # Create the directory if it doesn't exist
  dir.create(data_dir, showWarnings = FALSE)
  
  if (file.exists(file_path)) {
    
    message(paste("Data file exists at ", file_path, ". Reading CSV."))
    df <- read.csv(file_path)

  } else {
    
    message("Data not found in local data folder.  Downloading...")
    
    source("R/easy_street.R")
    app_token <- easy_street()
    # app_token <- Sys.getenv("SOCRATA_APP_TOKEN")
    if (app_token == "") {
      stop("SOCRATA_APP_TOKEN not found. An app token is required.")
    }
    
    start_year <- 2015
    end_year <- 2024
    fbi_codes <- c("01A", "02", "03", "04A", "04B")
    fbi_code_filter <- paste0("('", paste(fbi_codes, collapse = "','"), "')")
    
    soql_query <- paste0(
      "`year` >= ", start_year, " AND `year` <= ", end_year,
      " AND `fbi_code` IN ", fbi_code_filter
    )
    
    api_url <- paste0(
      "https://data.cityofchicago.org/resource/ijzp-q8t2.json",
      "?$where=",
      URLencode(soql_query)
    )
    
    df <- read.socrata(api_url, app_token = app_token)
    
    # Save the csv to the data/ directory
    write.csv(df, file_path, row.names = FALSE)
    message(paste("Data downloaded and saved to:", data_file_path))
  }
  
  return(df)
}


