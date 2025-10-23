# Download the Crimes - 2001 to Present CSV

# Libraries
library(RSocrata)

# Create a data storage directory
dir.create("data", showWarnings = FALSE)

# Get your private Chicago Data Portal api app token
# Add it to a file called .Renviron in the working directory of this project
# Add a line:  SOCRATA_APP_TOKEN=YourSecretPersonalAppToken
app_token <- Sys.getenv("SOCRATA_APP_TOKEN")
if (app_token == "") {
  stop("SOCRATA_APP_TOKEN not in .Renviron file.")
}

# Choose start and end years from 2001 to the present
# I elected 2015 to 2024 for a full 10 year period
start_year <- 2015
end_year <- 2024

# Specify the FBI codes for violent crimes
fbi_codes <- c("09A", "11A", "11B", "11C", "13A", "120")
fbi_code_filter <- paste0("('", paste(fbi_codes, collapse = "','"), "')")

# Define the SoQL query we will submit to the api call
soql_query <- paste0(
  "SELECT * ",
  "WHERE year >= '", start_year, "' AND year <= '", end_year, "'",
  "AND fbi_code IN ", fbi_code_filter
)

# Build the api request url
api_url <- paste0(
  "https://data.cityofchicago.org/resource/ijzp-q8t2.json",
  "?$query=",
  URLencode(soql_query)
)

# Build a dataframe directly from the api download
df <- read.socrata(
  api_url,
  app_token = app_token
)

print(head(df))

# Save the data to a csv file in the data/ directory
write.csv(df, "./data/crimes.csv", row.names = FALSE)


