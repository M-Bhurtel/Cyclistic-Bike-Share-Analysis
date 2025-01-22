library(tidyverse) #helps wrangle data
# Use the conflicted package to manage conflicts
library(conflicted)
library(janitor)
library(ggplot2)


# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#=====================
# STEP 1: COLLECT DATA
#=====================
# # Upload Divvy datasets (csv files) here
january_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202401-divvy-tripdata.csv")
february_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202402-divvy-tripdata.csv")
march_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202403-divvy-tripdata.csv")
april_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202404-divvy-tripdata.csv")
may_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202405-divvy-tripdata.csv")
june_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202406-divvy-tripdata.csv")
july_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202407-divvy-tripdata.csv")
august_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202408-divvy-tripdata.csv")
september_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202409-divvy-tripdata.csv")
october_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202410-divvy-tripdata.csv")
november_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202411-divvy-tripdata.csv")
december_2024 <- read_csv("C:/Users/Ashis/OneDrive/Desktop/Coursera/Project 2024/202412-divvy-tripdata.csv")


#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before
#we can use a command to join them into one file
colnames(january_2024)
colnames(february_2024)
colnames(march_2024)
colnames(april_2024)
colnames(may_2024)
colnames(june_2024)
colnames(july_2024)
colnames(august_2024)
colnames(september_2024)
colnames(october_2024)
colnames(november_2024)
colnames(december_2024)

# Stack all individual month's data frames into one big data frame

all_trips <- bind_rows(january_2024,february_2024,march_2024,april_2024,may_2024,june_2024,
                        july_2024,august_2024,september_2024,october_2024,november_2024,december_2024)

# Remove lat, long as they are no needed for analysis
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

# Inspect the new dataframe and look for inconguencies
str(all_trips)

# Drop NA fields
all_trips <- drop_na(all_trips)

# Convert ride_id and rideable_type to character so that they can stack correctly
all_trips <-  mutate(all_trips, ride_id = as.character(ride_id))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. Also tail(all_trips)
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics


# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing
#these operations we could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and
#checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarize(number_of_rides = n()  #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>%  # calculates the average duration
  arrange(member_casual, weekday)  # sorts the trip by member and weekdays

#=====================================

# STEP 5: Share Findings Through Visuals
#=====================================

# Let's visualize the number of rides by rider type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides), position = position_dodge(0.9), vjust = -0.3) + 
  labs(title = "Number of Rides by Rider Type and Weekday", x = "Weekday", y = "Number of Rides") +
  theme_minimal()


# Let's create a visualization for average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(average_duration, 1)), position = position_dodge(0.9), vjust = -0.3) +
  labs(title = "Average Ride Duration by Rider Type and Weekday", x = "Weekday", y = "Average Duration (seconds)") +
  theme_minimal()

# Heatmap of average ride length by hour of day and day of the week
all_trips_v2 %>%
  mutate(hour = format(as.POSIXct(started_at), "%H"),
         weekday = wday(started_at, label = TRUE)) %>%
  group_by(hour, weekday) %>%
  summarise(avg_duration = mean(ride_length)) %>%
  ggplot(aes(x = hour, y = weekday, fill = avg_duration)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Heatmap of Average Ride Duration by Hour and Day of Week", 
       x = "Hour of Day", y = "Day of Week", fill = "Avg. Duration (seconds)") +
  theme_minimal()

#Average Duration by Ryder Type

all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual) %>%
  ggplot(aes(x=member_casual, y=average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(average_duration, 1)), position = position_dodge(0.9), vjust = -0.3) +
  labs(title = "Average Ride Duration by Rider Type", x = "Ryder Type", y = "Average Duration (seconds)") +
  theme_minimal()

#Number of rides by rider type

all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual) %>%
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides), position = position_dodge(0.9), vjust = -0.3) + 
  labs(title = "Ride Frequency by Rider Type", x = "Ryder Type", y = "Number of Rides") +
  theme_minimal()

#Ride Frequency by Bike Type in Percentage

all_trips_v2 %>% 
  group_by(rideable_type) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(rideable_type) %>% 
  ggplot(aes(x = "", y = number_of_rides, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = pi/3) +  # Adjust start for similar orientation
  theme_void() +
  geom_text(aes(label = paste0(round((number_of_rides / sum(number_of_rides)) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4, color = "white") +  # Add percentage labels
  labs(title = "Ride Frequency by Bike Type in Percentage", fill = "Bike Type") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

