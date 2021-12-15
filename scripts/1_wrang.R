## Data Source and Credibility

# Although the case is fictitious, the data source 
#(https://divvy-tripdata.s3.amazonaws.com/index.html) is reliable.

# There is no bias in the process of collecting the data. All genres and ages 
#are included.



### Step 1 - Loading packages



# install.packages("readr")
# install.packages("tidyverse")
# install.packages("lubridate")

library(readr)
library(tidyverse)
library(lubridate)

### Step 2 - Merging the database

raw_data_2020_11 <- read_csv("data/raw/202011-divvy-tripdata.csv")
raw_data_2020_12 <- read_csv("data/raw/202012-divvy-tripdata.csv")
raw_data_2021_01 <- read_csv("data/raw/202101-divvy-tripdata.csv")
raw_data_2021_02 <- read_csv("data/raw/202102-divvy-tripdata.csv")
raw_data_2021_03 <- read_csv("data/raw/202103-divvy-tripdata.csv")
raw_data_2021_04 <- read_csv("data/raw/202104-divvy-tripdata.csv")
raw_data_2021_05 <- read_csv("data/raw/202105-divvy-tripdata.csv")
raw_data_2021_06 <- read_csv("data/raw/202106-divvy-tripdata.csv")
raw_data_2021_07 <- read_csv("data/raw/202107-divvy-tripdata.csv")
raw_data_2021_08 <- read_csv("data/raw/202108-divvy-tripdata.csv")
raw_data_2021_09 <- read_csv("data/raw/202109-divvy-tripdata.csv")
raw_data_2021_10 <- read_csv("data/raw/202110-divvy-tripdata.csv")

data1_merged <- rbind(
  raw_data_2020_11,
  raw_data_2020_12,
  raw_data_2021_01,
  raw_data_2021_02,
  raw_data_2021_03,
  raw_data_2021_04,
  raw_data_2021_05,
  raw_data_2021_06,
  raw_data_2021_07,
  raw_data_2021_08,
  raw_data_2021_09,
  raw_data_2021_10
  )

rm(list = c("raw_data_2020_11",
            "raw_data_2020_12",
            "raw_data_2021_01",
            "raw_data_2021_02",
            "raw_data_2021_03",
            "raw_data_2021_04",
            "raw_data_2021_05",
            "raw_data_2021_06",
            "raw_data_2021_07",
            "raw_data_2021_08",
            "raw_data_2021_09",
            "raw_data_2021_10"
            ))

save(data1_merged, file = "data/processed/data1_merged.RData")

### Step 3 - Discovering database



glimpse(data1_merged)


round(sum(is.na(data1_merged)) / 
        (nrow(data1_merged) * ncol(data1_merged)), 
      digits =  4) * 100

# NAs variables percentile from total 3.58%. It's smaller than 10% of 
#statistical significance, this metric will be reference for this study.


### Step 4 - Adapting database for study



## Filtering not relevant variables

#ride_id it's not relevant for the study

data2_adapted <- select(data1_merged, 
                       everything(), 
                       - ride_id)

# The variables: 
#"start_station_id" and "end_station_id" 
#can be the primary key for the variables: 
#"start_station_name", "end_station_name". 

data2_adapted <- select(data2_adapted, 
                       everything(), 
                       - start_station_name,
                       - end_station_name)

# And all latitude and longitude data must match with "start_station_id" 
#and "end_station_id". So, we will not use the following variables: 
#"start_lat", "start_lng", "end_lat", "end_lng".

data2_adapted <- select(data2_adapted, 
                       everything(),
                       - start_lat,
                       - start_lng,
                       - end_lat,
                       - end_lng)

## Adding relevant variables 
#According to the project's tasks
ride_length <- data2_adapted$ended_at - data2_adapted$started_at

day_of_week <- as.Date(data2_adapted$started_at)  
day_of_week <- format(day_of_week,"%u")
day_of_week <- as.numeric(day_of_week)
day_of_week <- day_of_week + 1
day_of_week <- replace(day_of_week, day_of_week == 8, 1)

#On my own
hour <- hour(data2_adapted$started_at)

month_number <- month(data2_adapted$started_at)

data2_adapted <- mutate(data2_adapted,
                       ride_length,
                       hour,
                       day_of_week,
                       month_number
                       )

#Checking out
glimpse(data2_adapted)

#Removing used data
rm(list = c("data1_merged",
            "ride_length",
            "day_of_week",
            "hour",
            "month_number"
            ))


### Step 5 - Identiying problems and cleaning

## Identifying problems
summary(data2_adapted)

# The variable ride length must be bigger then zero
min(data2_adapted$ride_length, na.rm = T)

# Investigating the percentile from total

round(nrow(filter(data2_adapted, 
       ride_length <= 0)
     ) / nrow(data2_adapted), digits = 4) * 100


# Cleaning
data3_cleaned <- mutate(data2_adapted,
                       ride_length = replace(
                         ride_length, ride_length <= 0, NA))

# Checking out

min(data3_cleaned$ride_length, na.rm = T)

round(sum(is.na(data3_cleaned)) / 
        (nrow(data3_cleaned) * ncol(data3_cleaned)), 
      digits =  4) * 100

rm(data2_adapted)

# Saving
save(data3_cleaned, file = "data/processed/data2_cleaned.RData")

