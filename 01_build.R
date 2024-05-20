library(dplyr)
library(tidyverse)
library(lubridate)
library(tidycensus)
library(sf)
library(janitor)
library(fuzzyjoin)
library(elevatr)

# Importing datasets -------------------------

# tripdata <- read.csv("data/raw/bks_tripdata_v2.csv")

## getting DC income data
options(tigris_use_cache = TRUE)

dc_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "DC", 
  year = 2020,
  geometry = TRUE
)

dc_census_tracts <- dc_income %>% arrange(GEOID) %>% 
  rename(geoid = GEOID,
         name = NAME,
         income = estimate,
         income_moe = moe) %>% 
  mutate(index = row_number())

# Saving a sample of the data ----------------------------------------

# tripdata_sample <- tripdata %>% sample_n(500000)
# tripdata_sample %>% write_csv("bks_tripdata_sample.csv")
tripdata_sample <- read.csv('data/raw/bks_tripdata_sample.csv')

# trip data within time frame
trip_data_2203 <- read_csv('data/raw/202203-capitalbikeshare-tripdata.csv') %>% 
  mutate(across(c(ended_at, started_at), function(col){force_tz(col, tzone="EST")}))

# saving dc income data
# dc_income %>% write_csv("data/raw/dc_income_geom.csv")
dc_income <- read.csv('data/raw/dc_income_geom.csv') 

# getting dc population data
dc_population <- read.csv('data/raw/Census_Tracts_in_2020.csv') %>% 
  select(TRACT, GEOID, P0020001) %>% mutate(GEOID = as.character(GEOID)) %>% 
  rename(tract_num = TRACT
         , geoid = GEOID
         , population = P0020001) 

## getting station data
stations <- read.csv("data/raw/Capital_Bikeshare_Locations.csv") %>% clean_names() %>% arrange(name)

# making income brackets -------------------------

dc_income_brackets <- dc_income %>% 
  mutate(income_brackets = cut(estimate, breaks = quantile(estimate, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)),
         geoid = as.character(GEOID)) %>% 
  select(geoid, income_brackets)

# constructing a xwalk between coords and sf polygons --------------------------------

coords <- stations %>% select(longitude = x, latitude = y) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 'NAD83')

stations <- cbind(stations, coords)

# joining with census tract poly
census_tract_shapes <- dc_census_tracts %>% select(geometry) %>% st_sf() 

coords_xwalk_1 <- st_within(coords, census_tract_shapes) 
coords_xwalk <- map(1:nrow(coords), 
                    function (i) {if (length(coords_xwalk_1[[i]]) == 0) coords_xwalk_1[[i]] = 0
                    else coords_xwalk_1[[i]]}) %>% unlist()

# Merging stations data with census data -----------------------------

census_stations <- cbind(stations %>% select(-x,-y, geometry) %>% 
                           rename(station = name, point = geometry), index = coords_xwalk) %>% 
  filter(index != 0) %>% 
  full_join(dc_census_tracts %>% rename(census_tract = name), by = 'index') %>% 
  full_join(dc_population) %>% 
  mutate(capacity = ifelse(grepl('Convention Center', station), 23, capacity))

census_stations %>% write_rds('data/mst/b00a_census_stations_xwalk.rds')

census_stations_grouped <- census_stations %>% 
  group_by(geoid, census_tract, income, population, geometry) %>% 
  summarise(stations = paste(station[!is.na(station)], collapse = ', ')
            , n_stations = n_distinct(station, na.rm = T)
            , across(c(capacity, num_docks_available,
                     num_bikes_available, num_ebikes_available, 
                     num_bikes_disabled), 
                     function (col) {ifelse(!is.na(sum(col)), sum(col), 0)})) 

# ggplot(census_stations_grouped) + geom_sf(aes(fill = income))

# Grouping trip data ------------------------

tripdata_filtered <- trip_data_sample %>% 
  select(started_at
         , ended_at
         , start_station_name
         , start_station_id
         , end_station_name
         , end_station_id
         , start_lat
         , start_lng
         , end_lat
         , end_lng) %>% na.omit() %>% filter(start_station_name != '' & end_station_name != '') %>% 
  mutate(# date = paste0(format(as.Date(started_at), "%Y-%m"), '-01') %>% as.Date()) 
         year = year(as.Date(started_at)),
         quarter = floor(month(as.Date(started_at))/4) + 1,
         across(c(started_at, ended_at), 
                function(col) {round_date(as.POSIXct(col), unit = '1 hour') %>% format(format = "%H:%M")},
                .names = "{.col}_rounded_time"),
         across(c(start_station_id, end_station_id), ~ifelse(.==31135, 31103, as.integer(.))))

# coords <- tripdata_filtered_1 %>% # converting long lat to sf points
#   select(longitude = start_lng, latitude = start_lat) %>% 
#   st_as_sf(coords = c('longitude', 'latitude'))

# tripdata_filtered <- cbind(tripdata_filtered_1, coords)

# standardizing station names ----------------------------------------

# station_name_distinct <- tripdata %>% 
#   select(station_id = start_station_id,
#          station_name = start_station_name) %>% 
#   mutate(station_id = station_id %>% as.integer) %>% distinct() %>% na.omit()
# 
# station_name_xwalk <- station_name_distinct %>% 
#   full_join(stations %>% select(name, x, y), by = c('station_name' = 'name')) %>% 
#   filter(!is.na(station_id)) %>% group_by(station_id) %>% 
#   filter(!(is.na(x) & n() > 1) & station_id != 31974) %>% select(-c(x,y))

station_name_xwalk  <- read_rds("data/mst/b00_station_names.rds")

tripdata_standardized <- tripdata_filtered %>% 
  select(-start_station_name, -end_station_name) %>% 
  left_join(station_name_xwalk %>% rename(start_station_id = station_id, start_station_name = station_name)) %>% 
  left_join(station_name_xwalk %>% rename(end_station_id = station_id, end_station_name = station_name)) %>% 
  filter(!is.na(start_station_name) & !is.na(end_station_name))

# identified_stations <- station_name_xwalk %>% pull(station_id)
# all_stations <- station_name_distinct %>% pull(station_id) %>% unique
# 
# full_join(station_name_xwalk, census_stations %>% select(station, station_id, capacity), by = c('station_name' = 'station')) %>% View
# 
# station_name_xwalk_2 <- tripdata_filtered %>% 
#   select(station_id = end_station_id,
#          station_name = end_station_name) %>% distinct() %>% 
#   full_join(stations %>% select(name, x, y), by = c('station_name' = 'name')) %>% 
#   na.omit()

# Grouping by station ------------------------------------

get_station_direction <- function (grouped_station = 'start_station') {
  
  other_station <- ifelse(grouped_station == 'start_station', 'end_station', 'start_station')
  
  station_direction <- tripdata_standardized %>%
    # left_join(census_stations %>% select(station, census_tract),
    #           by = setNames('station', other_station)) %>%
    group_by(station_id = get(paste0(grouped_station, '_id'))) %>%
    summarise(instances = n()
              , {{grouped_station}} := paste(get(paste0(grouped_station, '_name')) %>% unique, collapse = ', ')
              , {{other_station}} := paste(get(paste0(other_station, '_name')) %>% unique, collapse = ' ||| ')
              , n_other = n_distinct(get(paste0(other_station, '_id')))) 
    # ungroup() %>%
    # left_join(census_stations %>% select(station, num_docks_available, point, geoid, census_tract, income, geometry),
    #           by = setNames('station', grouped_station)) %>% filter(!is.na(income))
}

start_stations_grouped <- get_station_direction('start_station')
end_stations_grouped <- get_station_direction('end_station')

stations_grouped_census <- full_join(
  start_stations_grouped %>% select(station_id, start_instances = instances, station = start_station, start_n_other = n_other),
  end_stations_grouped %>% select(station_id, end_instances = instances, station = end_station, end_n_other = n_other),
  by = c('station_id', 'station')
) %>% 
  full_join(census_stations %>% select(station, capacity, point, census_tract, geometry, income, population),
            by = 'station') %>% 
  mutate(starts_per_bike = start_instances/capacity,
            ends_per_bike = end_instances/capacity,
            start_end_diff = starts_per_bike - ends_per_bike)
  
  
  # fuzzy_join(census_stations %>% select(station, capacity, point, census_tract, geometry, income, population),
  #            by = 'station',
  #            match_fun = stringr::str_detect,
  #            mode = 'full') %>% 
  # select(-station.x) %>% rename(station = station.y)
  
  
# Grouping by census tract ------------------------------------

get_direction_data <- function (grouped_station = 'start_station') {
  
  station_data <- get(paste0(grouped_station, 's_grouped'))
  
  other_station <- ifelse(grouped_station == 'start_station', 'end_station', 'start_station')
  n_other_station <- ifelse(grouped_station == 'start_station', 'n_ends', 'n_starts')

  instances_var_name <- paste0(grouped_station, '_instances')
  
  direction <- station_data %>% 
    full_join(census_stations %>% select(station, census_tract),
              by = setNames('station', grouped_station)) %>% 
    filter(!is.na(census_tract)) %>% 
    group_by(census_tract) %>% 
    summarise({{instances_var_name}} := sum(instances, na.rm = T),
              {{n_other_station}} := mean(n_other, na.rm = T))
  
  # 
  # trip_census_tracts <- direction %>% 
  #   group_by(station_id, census_tract) %>% 
  #   summarise(instances = sum(instances),
  #             n_other = mean(n_other)) %>% 
  #   group_by(census_tract) %>% 
  #   summarise({{instances_var_name}} := sum(instances),
  #             {{n_other_station}} := mean(n_other))
  
  # direction <- station_data %>% 
  #   full_join(census_stations %>% select(station, census_tract),
  #             by = setNames('station', paste0(grouped_station, '_name'))) %>% 
  #   full_join(census_stations %>% select(other_station = station, other_census_tract = census_tract),
  #             by = setNames('other_station', paste0(other_station, '_name'))) %>% View
  #   group_by(census_tract) %>% 
  #   summarise({{instances_var_name}} := n(),
  #             {{n_other_station}} := n_distinct(other_census_tract))
  
}
  
starts_grouped <- get_direction_data('start_station')
ends_grouped <- get_direction_data('end_station')

census_stations_enriched <- census_stations_grouped %>% full_join(starts_grouped) %>% 
  full_join(ends_grouped) %>% 
  filter(!(capacity == 0 & start_station_instances > 0)) %>% 
  mutate(across(c(start_station_instances, end_station_instances, n_starts, n_ends), function (col) {ifelse(is.na(col), 0, col)}))

# adj by population --------------------------------------

census_stations_enriched_adj_pop <- census_stations_enriched %>% 
  mutate(across(capacity:end_station_instances, 
         function (col) {col/population},
         .names = "{.col}_population_adjusted"),
         starts_per_bike = start_station_instances/capacity,
         ends_per_bike = end_station_instances/capacity,
         start_end_diff = starts_per_bike - ends_per_bike) %>% 
  mutate(across(c(starts_per_bike, ends_per_bike, start_end_diff), function (col) {ifelse(is.na(col), 0, col)}))

# For a station or census tract, get instances over time ------------------

tripdata_filtered_with_census <- tripdata_standardized %>% 
  left_join(census_stations %>% select(station, start_census_tract = census_tract),
            by = c('start_station_name' = 'station')) %>% 
  left_join(census_stations %>% select(station, end_census_tract = census_tract),
            by = c('end_station_name' = 'station')) 

group_instances_over_time <- function (group_col) {
  
  rounded_time <- ifelse(grepl('start', group_col), 'started_at_rounded_time', 'ended_at_rounded_time')
  station_or_census <- ifelse(grepl('station', group_col), 'station', 'census')
  
  instances_over_time <- tripdata_filtered_with_census %>% 
    group_by(get(group_col), get(rounded_time), quarter) %>% 
    summarise(instances = n(),
              type = ifelse(grepl('start', group_col), 'start', 'finish')) %>% 
    rename({{station_or_census}} := 'get(group_col)',
           rounded_time = 'get(rounded_time)')
  
}

instances_over_time_census_tracts <- map(.x = c('start_census_tract', 'end_census_tract'), 
                                         group_instances_over_time) %>% reduce(rbind) %>% ungroup()

instances_over_time_stations <- map(.x = c('start_station_name', 'end_station_name'),
                                    group_instances_over_time) %>% reduce(rbind) %>% ungroup()

# getting elevation data ---------------------------------

points_sf <- stations_grouped_census %>% select(station, point)  %>% st_as_sf %>% filter(!st_is_empty(.))
stations_elevations <- get_elev_point(locations = points_sf, src = 'aws')

# getting elevation by census tract 

avg_elevs <- stations_grouped_census %>% left_join(stations_elevations) %>% 
  group_by(census_tract) %>% summarise(avg_elevation = mean(elevation, na.rm = T))



# exportin ----------------------------------------------- 

station_name_xwalk %>% write_rds('data/mst/b00_station_names.rds')

census_stations_enriched_adj_pop %>% left_join(avg_elevs) %>%  write_rds('data/mst/b01_census_stations_data.rds')
stations_grouped_census %>% left_join(stations_elevations) %>% write_rds('data/mst/b02_stations_data.rds')

instances_over_time_census_tracts %>% write_rds('data/mst/b03_census_stations_instances_over_time.rds')
instances_over_time_stations %>% write_rds('data/mst/b03a_stations_instances_over_time.rds')





