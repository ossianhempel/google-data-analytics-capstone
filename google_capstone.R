library(tidyverse)

daily_activity <- read_csv("repos/google-data-analytics-capstone/fitabase_data/daily_activity_data.csv")
daily_sleep <- read_csv("repos/google-data-analytics-capstone/fitabase_data/daily_sleep_data.csv")

"********** Process **********"

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
daily_activity_sorted <- daily_activity[order(daily_activity$ActivityDate), ]
print(daily_activity_sorted)

daily_sleep$SleepDay <- as.Date(daily_sleep$SleepDay) # Convert the date column to Date type if it's not already
daily_sleep_sorted <- daily_sleep[order(daily_sleep$SleepDay), ]
print(daily_sleep_sorted)

"********** Process **********"

# Remove duplicates from the sleep data
daily_sleep_sorted <- daily_sleep_sorted[!duplicated(daily_sleep_sorted), ]

# Rename 'SleepDay' column to 'ActivityDate' in the sleep data
names(daily_sleep_sorted)[names(daily_sleep_sorted) == "SleepDay"] <- "ActivityDate"

# Perform a left join on 'Id' and 'ActivityDate'
merged_df <- merge(daily_activity_sorted, daily_sleep_sorted, by = c("Id", "ActivityDate"), all.x = TRUE)

# Check the type of date column
print(class(merged_df$ActivityDate))

# Convert 'ActivityDate' to Date type
merged_df$ActivityDate <- as.Date(merged_df$ActivityDate, format = "%m/%d/%Y")

# Check the class again to confirm the conversion
print(class(merged_df$ActivityDate))

# Check data types of all columns
str(merged_df)

# Save merged data frame as CSV
#write.csv(merged_df, "merged_data.csv", row.names = FALSE)

"Create two separate data frames, one with only activity data, and one only containing complete data using the sleep data as well"

# Create a dataframe with all data (will focus on activity data)
activity_data <- merged_df

# Create a dataframe with only complete sleep data
sleep_data <- na.omit(merged_df[, c("TotalSleepRecords", "TotalMinutesAsleep", "TotalTimeInBed")])

# Check the dimensions of the original data, activity data, and sleep data
original_shape <- dim(merged_df)
activity_shape <- dim(activity_data)
sleep_shape <- dim(sleep_data)

print(paste("Original data dimensions: ", original_shape[1], "x", original_shape[2]))
print(paste("Activity data dimensions: ", activity_shape[1], "x", activity_shape[2]))
print(paste("Sleep data dimensions: ", sleep_shape[1], "x", sleep_shape[2]))

"This indicates that we have removed 350 rows with missing sleep data."

"********** Analyze **********"

"1. What are some trends in smart device usage?"

"2. How could these trends apply to Bellabeat customers?"

"3. How could these trends help inﬂuence Bellabeat marketing strategy?"

