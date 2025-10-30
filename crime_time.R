library(dplyr)
library(lubridate)
library(ggplot2)

# Download the Crimes - 2001 to Present CSV if it doesn't already exist locally
source("R/get_data.R")
df_raw <- get_crime_data()

# Take a look at the first few rows of the data set
print(head(df_raw))

# Check the types of crimes to see if they seem "violent"
crimes <- df_raw[order(crimes$fbi_code, crimes$primary_type, crimes$description), ]
print(crimes)
# A word cloud visualization would help to summarize the descriptions


# Check the data type of the `date` column
print(paste("The data type of the 'date' column is:", class(df_raw$date)))

# create a new dataframe for processing
df_process <- df_raw

# Create a new date column with the POSIXct date-time object
# parse_date_time handles cases where the timestamp style varies
if(is.character(df_raw$date)) {
  df_process$date_time <- parse_date_time(df_raw$date,
                                  orders = c("Y-m-d H:M:S", "Y-m-d"),
                                  tz = "America/Chicago")
  print("New date_time column created with POSIX values from the date column")
}

# Extract hour value from the timestamp
df_process$hour <- as.numeric(format(df_process$date_time, "%H"))


#####
# Check if midnight timestamps are overrepresented
#####

# Extract the time of day in "HH:MM:SS" format
df_process <- df_process %>%
  mutate(time_of_day = format(date_time, "%H:%M:%S"))

# Count the occurrences of each unique time, and sort to see the most common
time_frequencies <- df_process %>%
  count(time_of_day, sort = TRUE)

# Display the top 10 most frequent times
cat("Top 10 most frequent timestamps:\n")
print(head(time_frequencies, 10))

# preview
df_process %>%
  filter(is.na(time_of_day)) %>%
  head()

# Filter for midnight times and then count the rows
midnight_crime_count <- df_process %>%
  filter(time_of_day == "00:00:00") %>%
  summarise(count = n())

# Print the result
cat("Total number of crimes recorded at exactly midnight:\n")
print(midnight_crime_count)

# Count crimes occurring in each 1 hour bucket
hourly_crime_counts <- df_process %>%
  # Create an 'hour' column
  mutate(hour = hour(date_time)) %>%
  # Group by and count the occurrences for each hour
  count(hour, sort = TRUE)

print(hourly_crime_counts)

# Visualize the one hour buckets
ggplot(hourly_crime_counts, aes(x = hour, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Crimes by Hour of the Day (Original)",
       x = "Hour of Day (0-23)",
       y = "Number of Crimes") +
  theme_minimal()


#########
# Clean excess midnight crimes
#########

# Midnight crimes are overrepresented, likely due to midnight being a default
# We will guess the "natural" valid number of midnight based on nearby times
# Then we will drop a random number of the excess crimes 
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
midnight_original <- time_frequencies$n[1]
cat("Observed count for midnight:", midnight_original, "\n")
cat("Estimated 'natural' baseline for midnight:", midnight_baseline, "\n")
#
# 2. What is the excess between "natural" and "actual" midnight counts?
midnight_excess <- midnight_original - midnight_baseline
cat("Excess count to be redistributed:", midnight_excess, "\n\n")
#
# 3. Randomly select 1863 midnight crimes to drop from the dataset
# (Note: we could do other things like redistribute the excess crimes.)
# 3a Set a random seed for reproducability
set.seed(42)
# 3b Get all the index values for midnight crimes
midnight_indices <- which(df_process$time_of_day == "00:00:00")
# 3c Randomly sample 'midnight_excess' (1863) indices from midnight_indices
crimes_to_drop <- sample(midnight_indices, size = midnight_excess)
# 3d Create a new dataframe without those randomly-selected rows
df_clean <- df_process %>%
  slice(-crimes_to_drop)

# Visualize the one hour buckets on the now-clean data set
hourly_crime_counts_clean <- df_clean %>%
  # Create an 'hour' column
  mutate(hour = hour(date_time)) %>%
  # Group by and count the occurrences for each hour
  count(hour, sort = TRUE)

ggplot(hourly_crime_counts_clean, aes(x = hour, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Crimes by Hour of the Day (Cleaned)",
       x = "Hour of Day (0-23)",
       y = "Number of Crimes") +
  theme_minimal()


####
# Final cleaning steps
####

# Create a flag to indicate if a crime took place between 10PM and 2AM
df_clean$crime_time <- ifelse(df_process$hour >= 22 | df_process$hour <= 2, 1, 0)

# Are there any missing values for the crime_time flag?
print(sum(is.na(df_clean$crime_time)))

# Clean the 5 rows with na flag values
df_clean <- df_clean %>% filter(!is.na(crime_time))


#########
# Perform the One-Proportion Z-Test
#########

# The derived column crime_time has a 1 for every crime from 10pm to 2am
crimes_ten_to_two <- sum(df_clean$crime_time)

# Total number of violent crimes in the data set
total_crimes <- nrow(df_clean)

# The null hypothesis proportion is 4/24, supposing that any given 4-hour window
# has a similar number of crimes
hypothesis_proportion <- 4/24

# Z-Test with 95% confidence level
# H0: The true proportion of crimes is <= 4/24
# H1: The true proportion of crimes is > 4/24
test_result <- prop.test(x = crimes_ten_to_two,
                         n = total_crimes,
                         p = hypothesis_proportion,
                         alternative = "greater",
                         conf.level = 0.95,
                         correct = FALSE # do not want "continuity correction"
                         )

cat("--- One-Proportion Z-Test Results ---\n\n")
cat(sprintf("Violent crimes observed from 10:00 PM to 2:00 AM: %d\n", 
            crimes_ten_to_two))
cat(sprintf("All violent crimes regardless of time: %d\n", total_crimes))
cat(sprintf("Sample Proportion (p-hat): %.4f\n", 
            crimes_ten_to_two / total_crimes))
cat(sprintf("Hypothesized Proportion (p0): %.4f\n\n", hypothesis_proportion))

# prop.test in R returns the chi-squared statistic (X-squared)
# We need the square root of that result to get the Z-statistic
z_statistic <- sqrt(test_result$statistic)
cat(sprintf("Z-statistic: %.4f\n", z_statistic))
cat(sprintf("P-value: %g\n\n", test_result$p.value))

