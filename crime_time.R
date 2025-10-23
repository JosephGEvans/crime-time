# Download the Crimes - 2001 to Present CSV if it doesn't already exist locally
source("R/get_data.R")
df <- get_crime_data()

# Take a look at the first few rows of the data set
print(head(df))

# Check the types of crimes to see if they seem "violent"
crimes <- crimes[order(crimes$fbi_code, crimes$primary_type, crimes$description), ]
print(crimes)

# Check the data type of the `date` column
print(paste("The data type of the 'date' column is:", class(df$date)))

# Convert the `date` column to POSIXct date-time object
if(is.character(df$date)) {
  df$date <- as.POSIXct(df$date, format="%Y-%m-%d %H:%M:%S")
  print("'date' column converted to POSIXct type")
}

# Extract hour value from the timestamp
df$hour <- as.numeric(format(df$date, "%H"))

# Create a flag to indicate if a crime took place between 10PM and 2AM
df$crime_time <- ifelse(df$hour >= 22 | df$hour <= 2, 1, 0)
