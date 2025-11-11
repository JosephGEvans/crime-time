library(dplyr)
library(lubridate)
library(ggplot2)
library(wordcloud)
library(tm)

# Set a random seed for reproducibility
set.seed(42)

# Download the Crimes - 2001 to Present CSV if it doesn't already exist locally
source("R/get_data.R")
df_raw <- get_crime_data()

#####
# Exploratory Data Analysis
#####

# Take a look at the first few rows of the data set
print(head(df_raw))

# Check the types of crimes to see if they seem "violent"
crimes <- df_raw[order(df_raw$fbi_code, df_raw$primary_type, df_raw$description), ]
print(crimes)


# A word cloud visualization would help to summarize the descriptions

# First I need a corpus from the description column
docs <- Corpus(VectorSource(crimes$description))

# Next we create a term-document matrix from the corpus
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

# Finally, generate the word cloud visualization
par(bg = "black",
    mar = c(0, 0, 0, 0))
wordcloud(words = d$word, 
          freq = d$freq,
          min.freq = 5,
          max.words = 55,
          random.order = FALSE,
          rot.per = 0.2,
          colors = brewer.pal(12, "Set3"),
          scale = c(8, 0.5)
          )


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
# 
# 3a Get all the index values for midnight crimes
midnight_indices <- which(df_process$time_of_day == "00:00:00")
# 3b Randomly sample 'midnight_excess' (1863) indices from midnight_indices
crimes_to_drop <- sample(midnight_indices, size = midnight_excess)
# 3c Create a new dataframe without those randomly-selected rows
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
df_clean$crime_time <- ifelse(df_clean$hour >= 22 | df_clean$hour <= 2, 1, 0)

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

# Interpretation
alpha <- 0.05
if (test_result$p.value < alpha) {
  cat(sprintf("Conclusion: The p-value (%.4f) is less than the significance
              level (%.2f).\n", test_result$p.value, alpha))
  cat("Therefore, we reject the null hypothesis.\n")
  cat("This suggests the proportion of crimes occurring between 10:00 PM\n")
  cat("and 2:00 AM is greater than 4/24 (0.167).\n")
} else {
  cat(sprintf("Conclusion: The p-value (%.4f) is not less than the
              significance level (%.2f).\n", test_result$p.value, alpha))
  cat("Therefore, we fail to reject the null hypothesis.\n")
  cat("There is not enough evidence to conclude that the proportion of crimes\n")
  cat("occurring between 10:00 PM and 2:00 AM is greater than 4/24 (0.167.\n")
}

# Optionally See the raw test result
# print(test_result)


#####
# A proportional histogram can help visualize the result intuitively
# It will be nice if the midnight hours are centered
# It will also be helpful to see the "expected" 1/24 proportion as a line
# Finally, it would be helpful to highlight the 4 hours of interest to the study
#####

# Calculate the proportion of crimes for each hour
hourly_crime_proportions <- df_clean %>%
  mutate(hour = hour(date_time)) %>%
  count(hour) %>%
  mutate(proportion = n / sum(n))

# Set the order of the hours for the x-axis
hour_order <- c(7:23, 0:6)
hourly_crime_proportions$hour <- factor(hourly_crime_proportions$hour, 
                                        levels = hour_order)

# Create a column to identify the hours to highlight
hourly_crime_proportions <- hourly_crime_proportions %>%
  mutate(highlight = ifelse(hour %in% c(22, 23, 0, 1), 
                            "highlight", 
                            "normal"))

# Generate the plot
ggplot(hourly_crime_proportions, 
       aes(x = hour, 
           y = proportion, 
           fill = highlight)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("highlight" = "orange", 
                               "normal" = "steelblue"), 
                    guide = "none") +
  geom_hline(yintercept = 1/24, linetype = "dashed", 
             color = "red", 
             size = 1) +
  labs(title = "Proportion of Crimes by Hour",
       x = "Hour",
       y = "Proportion of Crimes") +
  theme_minimal() 
