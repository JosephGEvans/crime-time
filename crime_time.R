library(dplyr)
library(lubridate)
library(ggplot2)

# Download the Crimes - 2001 to Present CSV if it doesn't already exist locally
source("R/get_data.R")
df <- get_crime_data()

# Take a look at the first few rows of the data set
print(head(df))

# Check the types of crimes to see if they seem "violent"
crimes <- crimes[order(crimes$fbi_code, crimes$primary_type, crimes$description), ]
print(crimes)
# A word cloud visualization would help to summarize the descriptions


# Check the data type of the `date` column
print(paste("The data type of the 'date' column is:", class(df$date)))

# Create a new date column with the POSIXct date-time object
# parse_date_time handles cases where the timestamp style varies
if(is.character(df$date)) {
  df$date_time <- parse_date_time(df$date,
                                  orders = c("Y-m-d H:M:S", "Y-m-d"),
                                  tz = "America/Chicago")
  print("New date_time column created with POSIX values from the date column")
}

# Extract hour value from the timestamp
df$hour <- as.numeric(format(df$date_time, "%H"))

# Create a flag to indicate if a crime took place between 10PM and 2AM
df$crime_time <- ifelse(df$hour >= 22 | df$hour <= 2, 1, 0)

#####
# Check if midnight timestamps are overrepresented
#####

# Extract the time of day in "HH:MM:SS" format
crime_data <- df %>%
  mutate(time_of_day = format(date_time, "%H:%M:%S"))

# Count the occurrences of each unique time, and sort to see the most common
time_frequencies <- crime_data %>%
  count(time_of_day, sort = TRUE)

# Display the top 10 most frequent times
cat("Top 10 most frequent timestamps:\n")
print(head(time_frequencies, 10))

crime_data %>%
  filter(is.na(time_of_day)) %>%
  head()

# Filter for midnight times and then count the rows
midnight_crime_count <- crime_data %>%
  filter(time_of_day == "00:00:00") %>%
  summarise(count = n())

# Print the result
cat("Total number of crimes recorded at exactly midnight:\n")
print(midnight_crime_count)

# Count crimes occurring in each 1 hour bucket
hourly_crime_counts <- df %>%
  # Create an 'hour' column
  mutate(hour = hour(date_time)) %>%
  # Group by and count the occurrences for each hour
  count(hour, sort = TRUE)

print(hourly_crime_counts)

# Visualize the one hour buckets
library(ggplot2)
ggplot(hourly_crime_counts, aes(x = hour, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Crimes by Hour of the Day",
       x = "Hour of Day (0-23)",
       y = "Number of Crimes") +
  theme_minimal()


# Midnight crimes are overrepresented, likely due to midnight being a default
# We will guess the "natural" valid number of midnight based on nearby times
# Then we will redistribute a random number of the excess to other time buckets
#
# 1. Estimate the natural baseline for midnight
# 1a Get counts for crimes 10, 11, 1, and 2 on the hour
n_2200 <- time_frequencies$n[time_frequencies$time_of_day == "22:00:00"]
n_2300 <- time_frequencies$n[time_frequencies$time_of_day == "23:00:00"]
n_0100 <- time_frequencies$n[time_frequencies$time_of_day == "01:00:00"]
n_0200 <- time_frequencies$n[time_frequencies$time_of_day == "02:00:00"]
# 1b Get the mean count from all four of those times
midnight_baseline <- round(mean(c(n_2200[1], n_2300[1], n_0100[1], n_0200[1])))
# 1c Compare the difference between actual midnights and the mean of nearby times
cat("Observed count for midnight:", time_frequencies$n[1], "\n")
cat("Estimated 'natural' baseline for midnight:", midnight_baseline, "\n")
#
# 2. What is the excess between "natural" and "actual" midnight counts?
excess_to_redistribute <- time_frequencies$n[1] - midnight_baseline
cat("Excess count to be redistributed:", excess_to_redistribute, "\n\n")
#
# 3. Redistribute the excess count of crimes among all time buckets
# Saving this step for later
# Don't forget to randomly select 1863 midnight crimes to impute new timestamps
