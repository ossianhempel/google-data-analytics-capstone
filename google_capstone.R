library(tidyverse)


daily_activity <- read_csv("repos/google-data-analytics-capstone/fitabase_data/dailyActivity_merged.csv")
daily_sleep <- read_csv("repos/google-data-analytics-capstone/fitabase_data/sleepDay_merged.csv")



# How is the data organized? Is it in long or wide format?
head(daily_activity)
head(daily_sleep)

"Each row represents a unique day of activity for a user, 
with different activity metrics (steps, calories, distance, etc.) in separate columns. 
This suggests that the data is in wide format. 
If the data were in long format, we would expect to see multiple rows for each user-day combination, 
each row representing a different variable or measurement."

unique_ids_activity <- unique(daily_activity$Id)
unique_ids_sleep <- unique(daily_sleep$Id)
print(unique_ids_activity)
print(unique_ids_sleep)

num_unique_ids_activity <- length(unique(daily_activity$Id))
num_unique_ids_sleep <- length(unique(daily_sleep$Id))

print(num_unique_ids_activity) # 33 unique IDs
print(num_unique_ids_sleep) # 24 unique IDs

# Check if the IDs in sleep also are in activity (for possible merging)
common_ids <- unique_ids_sleep %in% unique_ids_activity
print(common_ids) # All of the IDs in sleep exist in activity, although they're fewer total

# Check so that the date intervals are the same in both datasets
date_interval_activity <- range(daily_activity$ActivityDate)
date_interval_sleep <- range(daily_sleep$SleepDay)
print(date_interval_activity)
print(date_interval_sleep)
"The datasets are in the same data intervals, altough one is DATE and one DATETIME"

# Are there any problems with the data?
missing_values_activity <- sapply(daily_activity, function(x) sum(is.na(x)))
print(missing_values_activity) # No missing values

missing_values_sleep <- sapply(daily_sleep, function(x) sum(is.na(x)))
print(missing_values_sleep) # No missing values

duplicate_entries_activity <- sum(duplicated(daily_activity))
print(duplicate_entries_activity) # 0 duplicate entries

duplicate_entries_sleep <- sum(duplicated(daily_sleep))
print(duplicate_entries_sleep) # 3 duplicate entries

"The sleep data has 3 duplicate entries, otherwise there are no missing data." 

# Convert both date columns to Date type and sort on date instead of ID 
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate) # Convert the date column to Date type if it's not already
daily_activity_sorted <- df[order(daily_activity$ActivityDate), ]
print(daily_activity_sorted)

daily_sleep$SleepDay <- as.Date(daily_sleep$SleepDay) # Convert the date column to Date type if it's not already
daily_sleep_sorted <- df[order(daily_sleep$SleepDay), ]
print(daily_sleep_sorted)

