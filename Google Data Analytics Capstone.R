library(tidyverse)
library(data.table)

jan_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202201-divvy-tripdata.csv")
feb_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202202-divvy-tripdata.csv")
mar_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202203-divvy-tripdata.csv")
apr_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202204-divvy-tripdata.csv")
may_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202205-divvy-tripdata.csv")
jun_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202206-divvy-tripdata.csv")
jul_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202207-divvy-tripdata.csv")
aug_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202208-divvy-tripdata.csv")
sep_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202209-divvy-tripdata.csv")
oct_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202210-divvy-tripdata.csv")
nov_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202211-divvy-tripdata.csv")
dec_2022 <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\202212-divvy-tripdata.csv")

# checking structures of everydataset before merging them into one large datset
str(jan_2022)
str(feb_2022)
str(mar_2022)
str(apr_2022)
str(may_2022)
str(jun_2022)
str(jul_2022)
str(aug_2022)
str(sep_2022)
str(oct_2022)
str(nov_2022)
str(dec_2022)

# merging monthly dataset into one and checking structure of new dataset created

all_bike_trip <- bind_rows(jan_2022,feb_2022,mar_2022,apr_2022,may_2022,jun_2022,jul_2022,aug_2022,sep_2022,oct_2022,nov_2022,dec_2022)
str(all_bike_trip)

# Adding month and date column in dataset
all_bike_trip$date <- as.Date(all_bike_trip$started_at)
all_bike_trip$month <- format(as.Date(all_bike_trip$date), "%m")
str(all_bike_trip)
tail(all_bike_trip)

#calculating ride distance from latitudes and longitudes
install.packages("geosphere")
library(geosphere)

all_bike_trip$ride_dist <- distGeo(matrix(c(all_bike_trip$start_lng, all_bike_trip$start_lat),ncol = 2), matrix(c(all_bike_trip$end_lng,all_bike_trip$end_lat),ncol=2))
str(all_bike_trip)

# distance in kilometer
all_bike_trip$ride_dist <- all_bike_trip$ride_dist/1000
str(all_bike_trip, )

#checking for bad data where ride length is 0.
all_bike_trip %>% 
  count(all_bike_trip$ride_length == 0)

#removing rows where ride length is 0.
all_bike_trip_clean <- all_bike_trip[!(all_bike_trip$ride_length == 0 ),]

#saving the clean dataset for future use
fwrite(all_bike_trip_clean, 
       "C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\2022_all_bike_trip_clean.csv",
       col.names = TRUE,
       row.names = FALSE
)

all_bike_trip_clean <- read.csv("C:\\Users\\ASUS\\Desktop\\Cyclistic Trip Data\\Cleaned\\2022_all_bike_trip_clean.csv")
str(all_bike_trip)

#finding out min, max, mean and median of ride length based on member type

all_bike_trip_clean %>% 
  group_by(member_casual) %>% 
  summarise(max_ride_length = max(ride_length), min_ride_length = min(ride_length),
            average_ride_length = mean(ride_length), median_ride_length = median(ride_length))

# visualizing members and casuals by total ride taken

all_bike_trip_clean %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id))

