library(dplyr)
library(tidyverse)
library(lubridate)
library(tidycensus)
library(sf)
library(janitor)
library(fuzzyjoin)

# Importing trip data ----------------------------------

# getting station xwalk
station_name_xwalk <- read_rds('data/mst/b00_station_names.rds')

# getting stations data
stations <- read_rds("data/mst/b00a_census_stations_xwalk.rds")


# getting 5 stations highest/lowest income above 50 capacity
census_stations_enriched_adj_pop <- read_rds('data/mst/b01_census_stations_data.rds')
stations_hl_income <- rbind(census_stations_enriched_adj_pop %>% filter(capacity >= 50) %>% arrange(income) %>% head(5),
                            census_stations_enriched_adj_pop %>% filter(capacity >= 50) %>% arrange(-income) %>% head(5))

# getting trip data
# trip_data_2311 <- read_csv('data/raw/202311-capitalbikeshare-tripdata.csv')
trip_data_2203 <- read_csv('data/raw/202203-capitalbikeshare-tripdata.csv') %>% 
  # setNames(c("duration", "started_at", "ended_at", "start_station_id", "start_station_name", "end_station_id", "end_station_name", "bike_num", "member_type")) %>% 
  mutate(across(c(ended_at, started_at), function(col){force_tz(col, tzone="EST")}))

trip_data_sample <- read_csv('data/raw/bks_tripdata_sample.csv') %>% 
  mutate(across(c(ended_at, started_at), function(col){force_tz(col, tzone="EST")}))

# processing trip data ---------------------------------------------

tripdata_filtered <- trip_data_2203 %>% 
  select(started_at
         , ended_at
         , start_station_name
         , start_station_id
         , end_station_name
         , end_station_id) %>% na.omit() %>% filter(start_station_name != '' & end_station_name != '') %>% 
  mutate(# date = paste0(format(as.Date(started_at), "%Y-%m"), '-01') %>% as.Date()) 
    year = year(as.Date(started_at)),
    quarter = floor(month(as.Date(started_at))/4) + 1,
    across(c(started_at, ended_at), 
           function(col) {round_date(as.POSIXct(col), unit = '1 hour') %>% format(format = "%D %H:%M")},
           .names = "{.col}_rounded_time"),
    across(c(start_station_id, end_station_id), ~ifelse(.==31135, 31103, as.integer(.)))) %>% 
  filter(year!=2020)


station_time <- as.POSIXct(stations$last_reported[1])

# filtering trip data by time, standardizing names, and joining on station attributes
tripdata_new <- tripdata_filtered %>%  # %>% filter(started_at > station_time) %>% 
  select(-start_station_name, -end_station_name) %>% 
  left_join(station_name_xwalk %>% rename(start_station_id = station_id, start_station_name = station_name)) %>% 
  left_join(station_name_xwalk %>% rename(end_station_id = station_id, end_station_name = station_name)) %>% 
  filter(!is.na(start_station_name) & !is.na(end_station_name)) %>% 
  left_join(stations %>% select(station, start_census_tract = census_tract),
            by = c('start_station_name' = 'station')) %>% 
  left_join(stations %>% select(station, end_census_tract = census_tract),
            by = c('end_station_name' = 'station')) 

tripdata_new %>% write_rds("data/mst/b05_data_for_investigation_2203.rds")

tripdata_vertical <- rbind(tripdata_new %>% select(name = start_station_name, timestamp = started_at) %>% 
                             transform(type = 'start'),
                           tripdata_new %>% select(name = end_station_name, timestamp = ended_at) %>% 
                             transform(type = 'end')) %>% 
  left_join(stations %>% rename(name = station) %>% select(name, 
                                capacity,
                                num_bikes_available,
                                num_docks_available)) 

# Calculating docks available with each trip -----------------------------------------

tripdata_calc <- tripdata_vertical %>% 
  group_by(name) %>% arrange(timestamp, .by_group = T) %>% 
  mutate(num_bikes_change = ifelse(type == 'start', 1, -1),
         `Number of Bikes Available` = -cumsum(num_bikes_change),
         `Number of Docks Available` = cumsum(num_bikes_change),
         start_end_diff = sum(type == 'start') - sum(type == 'end'))

# exportin a specific station

tripdata_calc %>% filter(abs(start_end_diff) >= 100) %>% write_rds('data/mst/b04b_bike_avail_2203.rds')


# For a station or census tract, get instances over time ------------------

group_instances_over_time <- function (group_col) {
  
  rounded_time <- ifelse(grepl('start', group_col), 'started_at_rounded_time', 'ended_at_rounded_time')
  station_or_census <- ifelse(grepl('station', group_col), 'station', 'census')
  
  instances_over_time <- tripdata_new %>% 
    mutate(datestamp = as.Date(started_at),
           weekend_flag = ifelse(wday(datestamp) %in% c(1, 7), 1, 0)) %>% 
    group_by(get(group_col), get(rounded_time), quarter, weekend_flag) %>% 
    summarise(instances = n(),
              type = ifelse(grepl('start', group_col), 'start', 'finish')) %>% 
    rename({{station_or_census}} := 'get(group_col)',
           rounded_time = 'get(rounded_time)')
  
}

instances_over_days_census_tracts <- map(.x = c('start_census_tract', 'end_census_tract'), 
                                         group_instances_over_time) %>% reduce(rbind) %>% ungroup()

instances_over_time_stations <- map(.x = c('start_station_name', 'end_station_name'),
                                    group_instances_over_time) %>% reduce(rbind) %>% ungroup()

# group instances over days -------------------

group_instances_over_days<- function (group_col) {
  
  rounded_time <- ifelse(grepl('start', group_col), 'started_at_rounded_time', 'ended_at_rounded_time')
  station_or_census <- ifelse(grepl('station', group_col), 'station', 'census')
  
  instances_over_days <- tripdata_new %>% 
    mutate(datestamp = as.Date(started_at),
           weekend_flag = ifelse(wday(datestamp) %in% c(1, 7), 1, 0)) %>% 
    group_by(get(group_col), get(rounded_time), datestamp, weekend_flag) %>% 
    summarise(instances = n(),
              type = ifelse(grepl('start', group_col), 'start', 'finish')) %>% 
    rename({{station_or_census}} := 'get(group_col)',
           rounded_time = 'get(rounded_time)')
  
}

instances_over_days_census_tracts <- map(.x = c('start_census_tract', 'end_census_tract'), 
                                         group_instances_over_days) %>% reduce(rbind) %>% ungroup()

# exportin  -------------------

instances_over_time_census_tracts %>% write_rds('data/mst/b03_census_stations_instances_over_time.rds')
instances_over_time_stations %>% write_rds('data/mst/b03_stations_instances_over_time.rds')

instances_over_days_census_tracts %>% write_rds('data/mst/b03f_census_stations_instances_over_days.rds')
                                        