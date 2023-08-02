library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

daily_activity <- read_csv("repos/google-data-analytics-capstone/fitabase_data/daily_activity_data.csv")
daily_sleep <- read_csv("repos/google-data-analytics-capstone/fitabase_data/daily_sleep_data.csv")
hourly_activity <- read_csv("repos/google-data-analytics-capstone/fitabase_data/hourly_intensity_data.csv")
hourly_calories <- read_csv("repos/google-data-analytics-capstone/fitabase_data/hourly_calories_data.csv")

"********** Process **********"

# How is the data organized? Is it in long or wide format?
head(daily_activity)
head(daily_sleep)
head(hourly_activity)
head(hourly_calories)

"The datasets appears to be in a wide format. 
This is because each row represents a single observation (a unique combination of Id and ActivityDate), 
and each feature or variable (like TotalSteps, TotalDistance, Calories, etc.) is presented as a separate column."

unique_ids_daily_activity <- unique(daily_activity$Id)
unique_ids_daily_sleep <- unique(daily_sleep$Id)
unique_ids_hourly_activity <- unique(hourly_activity$Id)
unique_ids_hourly_calories <- unique(hourly_calories$Id)
print(unique_ids_activity)
print(unique_ids_sleep)
print(unique_ids_hourly_activity)
print(unique_ids_hourly_calories)

# Calculate the number of unique IDs in the daily activity data
num_unique_ids_activity <- length(unique(daily_activity$Id))
print(num_unique_ids_activity) # 33 unique IDs

# Calculate the number of unique IDs in the daily sleep data
num_unique_ids_sleep <- length(unique(daily_sleep$Id))
print(num_unique_ids_sleep) # 24 unique IDs

# Calculate the number of unique IDs in the hourly activity data
num_unique_ids_hourly_activity <- length(unique(hourly_activity$Id))
print(num_unique_ids_hourly_activity) # 33 unique IDs

# Calculate the number of unique IDs in the hourly calories data
num_unique_ids_hourly_calories <- length(unique(hourly_calories$Id))
print(num_unique_ids_hourly_calories) # 33 unique IDs

"Merging daily datasets"
# Check if the IDs in sleep also are in activity (for possible merging)
common_ids <- unique_ids_sleep %in% unique_ids_activity
print(common_ids) # All of the IDs in sleep exist in activity, although they're fewer total

# Check so that the date intervals are the same in both datasets
date_interval_activity <- range(daily_activity$ActivityDate)
date_interval_sleep <- range(daily_sleep$SleepDay)
print(date_interval_activity)
print(date_interval_sleep)
"The datasets are in the same data intervals, altough one is DATE and one DATETIME"

# Check the date range for the hourly_activity dataset
activity_date_range <- range(hourly_activity$ActivityHour)
print(paste("Hourly activity data ranges from ", activity_date_range[1], " to ", activity_date_range[2]))

# Check the date range for the hourly_calories dataset
calories_date_range <- range(hourly_calories$ActivityHour)
print(paste("Hourly calories data ranges from ", calories_date_range[1], " to ", calories_date_range[2]))

"Are there any problems with the data?"
# Check for missing values in the daily activity data
missing_values_activity <- sapply(daily_activity, function(x) sum(is.na(x)))
print(missing_values_activity) # 0 missing values

# Check for missing values in the daily sleep data
missing_values_sleep <- sapply(daily_sleep, function(x) sum(is.na(x)))
print(missing_values_sleep) # 0 missing values

# Check for missing values in the hourly activity data
missing_values_hourly_activity <- sapply(hourly_activity, function(x) sum(is.na(x)))
print(missing_values_hourly_activity) # 0 missing values

# Check for missing values in the hourly calories data
missing_values_hourly_calories <- sapply(hourly_calories, function(x) sum(is.na(x)))
print(missing_values_hourly_calories) # 0 missing values

# Check for duplicate entries in the hourly activity data
duplicate_entries_activity <- sum(duplicated(daily_activity))
print(duplicate_entries_activity) # 0 duplicate entries

# Check for duplicate entries in the hourly activity data
duplicate_entries_sleep <- sum(duplicated(daily_sleep))
print(duplicate_entries_sleep) # 3 duplicate entries

# Check for duplicate entries in the hourly activity data
duplicate_entries_hourly_activity <- sum(duplicated(hourly_activity))
print(duplicate_entries_hourly_activity) # 0 duplicate entries

# Check for duplicate entries in the hourly calories data
duplicate_entries_hourly_calories <- sum(duplicated(hourly_calories))
print(duplicate_entries_hourly_calories) # 0 duplicate entries

"The daily sleep data has 3 duplicate entries, 
otherwise there are no missing data or duplicates." 

# Convert 'ActivityDate' to Date type in daily activity
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")

# Convert 'SleepDay' to datetime type
daily_sleep$SleepDay <- as.POSIXct(daily_sleep$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")

# Convert 'ActivityHour' to datetime type in hourly activity and calories data
hourly_activity$ActivityHour <- as.POSIXct(hourly_activity$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_calories$ActivityHour <- as.POSIXct(hourly_calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

# Check the conversion
head(daily_activity$ActivityDate)
head(hourly_activity$ActivityHour)
head(hourly_calories$ActivityHour)
head(daily_sleep$SleepDay)

"DAILY DATA"
# Convert 'ActivityDate' to Date type
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")

# Convert 'SleepDay' to Date type and rename it to 'ActivityDate'
daily_sleep$SleepDay <- as.Date(daily_sleep$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")
names(daily_sleep)[names(daily_sleep) == "SleepDay"] <- "ActivityDate"

# Perform a left join on 'Id' and 'ActivityDate'
#merged_df <- merge(daily_activity, daily_sleep, by = c("Id", "ActivityDate"))
merged_df <- merge(daily_activity, daily_sleep, by = c("Id", "ActivityDate"), all.x = TRUE)


# Check the type and conversion of date column
print(class(merged_df$ActivityDate))
head(merged_df$ActivityDate)

# Check data types of all columns
str(merged_df)

"Create two separate data frames, one with only activity data, and one only containing complete data using the sleep data as well"

# Create a dataframe with all data (will focus on activity data)
activity_data <- merged_df

# Create a dataframe with only complete sleep data
sleep_data <- na.omit(merged_df[, c("TotalSleepRecords", "TotalMinutesAsleep", "TotalTimeInBed")])

"HOURLY DATA"
# Remove duplicates from the hourly activity data
hourly_activity <- hourly_activity[!duplicated(hourly_activity), ]

# Remove duplicates from the hourly calories data
hourly_calories <- hourly_calories[!duplicated(hourly_calories), ]

# Convert 'ActivityHour' to datetime type
hourly_activity$ActivityHour <- as.POSIXct(hourly_activity$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_calories$ActivityHour <- as.POSIXct(hourly_calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

# Check the type of date column
print(class(hourly_activity$ActivityHour))
print(class(hourly_calories$ActivityHour))

# Check data types of all columns
str(hourly_activity)
str(hourly_calories)


"********** Analyze **********"

"1. What are some trends in smart device usage?"

"DESCRIPTIVE STATISTICS"

# Calculate average daily steps
average_steps <- mean(activity_data$TotalSteps, na.rm = TRUE)
activity_data$TotalSteps %>% summary()

"Analyze activity minutes to understand user's activity levels throughout the day"
# Calculate average very active minutes
activity_data$VeryActiveMinutes %>% summary()

# Calculate average fairly active minutes
activity_data$FairlyActiveMinutes %>% summary()

# Calculate average lightly active minutes
activity_data$LightlyActiveMinutes %>% summary()

"Analyze sleep patterns and sleep quality"
# Calculate average total minutes asleep
sleep_data$TotalMinutesAsleep %>% summary()

# Calculate average total time in bed
sleep_data$TotalTimeInBed %>% summary()

"Calculate average time to fall asleep (while in bed) - doesn't necessarily mean
they're trying to fall asleep for the entire time!"
average_time_to_sleep <- average_time_in_bed - average_minutes_asleep
print(average_time_to_sleep)

# Calculate average calories burned
average_calories <- mean(activity_data$Calories, na.rm = TRUE)
activity_data$Calories %>% summary()

"DISTRIBUTIONS"

"Let's check the distributions of total steps and minutes asleep"
# Histogram of total steps
ggplot(activity_data, aes(x = TotalSteps)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(x = "Total Steps", y = "Count", title = "Histogram of Total Steps")

# Histogram of total minutes asleep
ggplot(sleep_data, aes(x = TotalMinutesAsleep)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(x = "Total Minutes Asleep", y = "Count", title = "Histogram of Total Minutes Asleep")


"STEPS AND CALORIES"
# Scatter plot of total steps vs calories with smoothing line
ggplot(activity_data, aes(x = TotalSteps, y = Calories)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Total Steps", y = "Calories", title = "Scatter Plot of Total Steps vs Calories with Smoothing Line")

# Calculate correlation between total steps and calories
correlation_steps_calories <- cor(activity_data$TotalSteps, activity_data$Calories, use = "complete.obs")
print(correlation_steps_calories) 

"We find a moderate reltionship between steps and expended calories, as expected. Walking burns calories."

"Since we haven't really checked if our variables fullfill the assumptions required for correlation analysis, 
this and the following correlations should be taken with a grain of salt. We're mainly observing the plots and 
using the correlation as a complement."

"ACTIVITY LEVELS AND CALORIES"

"Let's explore the relationship between different activity levels and expended calories"

# Calculate correlation between very active minutes and calories
correlation_very_active_calories <- cor(activity_data$VeryActiveMinutes, activity_data$Calories, use = "complete.obs")
print(correlation_very_active_calories)

# Calculate correlation between fairly active minutes and calories
correlation_fairly_active_calories <- cor(activity_data$FairlyActiveMinutes, activity_data$Calories, use = "complete.obs")
print(correlation_fairly_active_calories)

# Calculate correlation between lightly active minutes and calories
correlation_lightly_active_calories <- cor(activity_data$LightlyActiveMinutes, activity_data$Calories, use = "complete.obs")
print(correlation_lightly_active_calories)

# Scatter plot of very active minutes vs total minutes asleep with smoothing line
ggplot(merged_df, aes(y = VeryActiveMinutes, x = Calories)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Very Active Minutes", y = "Calories", title = "Scatter Plot of Very Active Minutes vs Calories with Smoothing Line")

"We find a quite strong relationship (0.61) between very active minutes and calories.
Observing the plot, we see that the realtionship exists but isn't strong at minutes of 0-3000
active minutes. For people who have 
Fairly active minutes and calories share a weak but postive relationships. 
Lastly, lightly active minutes and calories doesn't appear to have an association."

"STEPS AND SLEEP"
# Scatter plot of total steps vs total minutes asleep with smoothing line
ggplot(merged_df, aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Total Steps", y = "Total Minutes Asleep", title = "Scatter Plot of Total Steps vs Total Minutes Asleep with Smoothing Line")

# Calculate correlation between total steps and total minutes asleep
correlation_steps_sleep <- cor(merged_df$TotalSteps, merged_df$TotalMinutesAsleep, use = "complete.obs")
print(correlation_steps_sleep)

"There is no apparent relationship between total steps and minutes sleeping"

"SEDENTARY MINUTES AND SLEEP"
ggplot(data=merged_df, aes(y=TotalMinutesAsleep, x=SedentaryMinutes)) + 
  geom_point(color='darkblue') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")

# Calculate correlation between sedentary minutes and total minutes asleep
correlation_sedentary_sleep <- cor(merged_df$SedentaryMinutes, merged_df$TotalMinutesAsleep, use = "complete.obs")
print(correlation_sedentary_sleep)

"We find a quite weak but existing negative relationship between sedentary minutes and minutes sleeping.
Looking at the plot it seems there is a 'normal' range of sedentary minutes (approx. 500-1000) where sleep
isn't really correlated. However, for >1000 sedentary minutes, less sleep occurs."

"HOURLY ACTIVITY LEVELS"

# Group by Hour and calculate mean TotalIntensity
hourly_activity %>%
  mutate(Hour = hour(ActivityHour)) %>%
  group_by(Hour) %>%
  summarise(mean_total_int = mean(TotalIntensity, na.rm = TRUE)) %>%
  # Create bar plot
  ggplot(aes(x = factor(Hour), y = mean_total_int)) +
  geom_col(fill = 'darkblue') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Hour of Day", y = "Average Total Intensity", title = "Average Total Intensity vs. Time")

"People are most active between 5 pm to 7 pm"

"HOURLY CALORIE LEVELS"

# Group by Hour and calculate mean Calories
hourly_calories %>%
  mutate(Hour = hour(ActivityHour)) %>%
  group_by(Hour) %>%
  summarise(mean_calories = mean(Calories, na.rm = TRUE)) %>%
  # Create bar plot
  ggplot(aes(x = factor(Hour), y = mean_calories)) +
  geom_col(fill = 'darkblue') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Hour of Day", y = "Average Calories", title = "Average Calories vs. Time")

"Similarly to the activity levels, most calories are burned between 5-7pm"

"ACTIVITY LEVELS WEEKDAYS VS WEEKENDS"
# Create a new column 'day_of_week' that represents the day of the week, where Monday is 1 and Sunday is 7
merged_df$day_of_week <- weekdays(merged_df$ActivityDate)

# Define weekdays as 1-5 and weekend as 6-7
merged_df$day_type <- ifelse(merged_df$day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

# Calculate the average steps for weekdays and weekends
average_steps_weekday <- mean(merged_df[merged_df$day_type == "Weekday",]$TotalSteps, na.rm = TRUE)
average_steps_weekend <- mean(merged_df[merged_df$day_type == "Weekend",]$TotalSteps, na.rm = TRUE)

# Print the average steps
print(paste("Average steps on weekdays: ", average_steps_weekday))
print(paste("Average steps on weekends: ", average_steps_weekend))

# Plot the step count for weekdays vs weekends
ggplot(merged_df, aes(x = day_type, y = TotalSteps)) +
  geom_boxplot() +
  labs(x = "Type of Day", y = "Step Count", title = "Step Count for Weekdays vs Weekends")

"There's no meaningful difference in step counts between weekends and weekdays."
