# getting stations data
stations <- read_rds("data/mst/b00a_census_stations_xwalk.rds")
census_stations <- read_rds('data/mst/b01_census_stations_data.rds') %>% st_as_sf()
instances_over_time_census_tracts <- read_rds('data/mst/b03_census_stations_instances_over_time.rds') 
grouped_stations<-read_rds('data/mst/b02_stations_data.rds')

# ---------------------------------

inv_station <- grouped_stations %>% filter(grepl(' 1.02', census_tract)) %>% pull(station)

inv_station_df <- read_rds('data/mst/b05_data_for_investigation.rds') %>% 
  # filter(start_station_name %in% inv_station) %>% 
  left_join(stations %>% select(station, start_census = census_tract), by = c('start_station_name' = 'station')) %>% 
  left_join(stations %>% select(station, end_census = census_tract), by = c('end_station_name' = 'station')) %>% 
  # filter(started_at_rounded_time %in% c('18:00', '17:00') & wday(as.Date(started_at)) %in% c(2:6)) %>%
  # filter(wday(as.Date(started_at)) %in% c(1, 7)) %>% 
  group_by(start_census, end_census) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  mutate(across(contains("census"), ~str_replace_all(., ", District of Columbia", "")))
  # group_by(end_census) %>% summarise(n = sum(n)) # %>% 
  # left_join(census_stations %>% select(geometry, census_tract), by = c("end_census" = 'census_tract')) %>% st_as_sf()

# start_end_diff_at_time <- full_join(
#   (inv_station_df %>% group_by(census =start_census) %>% summarise(starts = sum(n))),
#   (inv_station_df %>% group_by(census=end_census) %>% summarise(ends = sum(n)))
# ) %>% 
#   full_join(census_stations %>% transmute(census = census_tract %>% str_replace_all(., ", District of Columbia", ""), capacity)) %>% 
#   mutate(across(c(starts,ends), ~replace_na(.,0)),
#          start_end_diff = (starts-ends)) %>% st_as_sf()
          # start_end_diff = (starts-ends)/ifelse(capacity != 0, capacity, 0.1)) %>% st_as_sf()

most_freq<-inv_station_df %>% filter(start_census != end_census | is.na(start_census) | is.na(end_census)) %>% rowwise() %>% 
  mutate(across(c(start_census, end_census), ~replace_na(., '')),
         clean_route = str_c(min(start_census, end_census), '-', max(start_census, end_census))) %>% 
  group_by(clean_route) %>% summarise(n_total = sum(n))
  

abs_routes <- inv_station_df %>% 
  filter(start_census != end_census | is.na(start_census) | is.na(end_census)) %>% rowwise() %>% 
  mutate(across(c(start_census, end_census), ~replace_na(., '')),
         clean_route = str_c(min(start_census, end_census), '-', max(start_census, end_census)),
         n = ifelse(start_census < end_census, n, -n)) %>% 
  group_by(clean_route) %>% summarise(n_net = sum(n)) %>% 
  ungroup() %>% 
  mutate(start = ifelse(n_net >= 0, str_extract(clean_route, '^[^-]*'), str_extract(clean_route, '[^-]*$')),
         end = ifelse(n_net >= 0, str_extract(clean_route, '[^-]*$'), str_extract(clean_route, '^[^-]*')),
         n_net = abs(n_net),
         across(c(start, end), function (col) {ifelse(col == "", "Out of DC", col)}))


most_common <- abs_routes %>% 
  filter(start != end) %>% 
  # na.omit() %>% 
  arrange(-n_net) %>% head(10)

most_common_starts <- most_common %>% pull(start) %>% unique()
most_common_finishes <- most_common %>% pull(end) %>% unique

# m <- ggplot(inv_station_df) + geom_sf(aes(fill = n), colour = 'black') + 
#   theme_void()  + guides(fill = guide_legend(title = legend_title),
#                          colour = guide_colorbar(ticks.colour = 'gray', ticks.linewidth = 1))

# checking --------------------



# function to plot 2 census tracts -----------------------

plot_trips <- function (start_tracts, end_tracts) {
  
  legend_title = "Type of Endpoint in Trip"
  
  for_plt <- census_stations %>% 
    select(census_tract, geometry) %>% 
    mutate(census_tract = str_replace_all(census_tract, ", District of Columbia", ""),
              flg = ifelse(census_tract %in% start_tracts & !(census_tract %in% end_tracts), "Trip Starts",
                           ifelse(census_tract %in% end_tracts & !(census_tract %in% start_tracts), "Trip Finishes", 
                                  ifelse(census_tract %in% start_tracts & census_tract %in% end_tracts, "Both Start and Finish", NA)))) %>% 
    na.omit
  
  ggplot(for_plt) + geom_sf(aes(fill = flg), colour = 'gray') + 
    theme_void()  + scale_fill_manual(values = c("purple", "maroon", "darkblue")) + 
    guides(fill = guide_legend(title = legend_title),
           colour = guide_colorbar(ticks.colour = 'gray', ticks.linewidth = 1))
  
}

# plot_census(tract1 = "Census Tract 53.03", tract2 = "Census Tract 107")
plot_trips(start_tracts = most_common_starts, end_tracts = most_common_finishes)
plot_trips('Census Tract 9800', 'Census Tract 108')


# functiion to plot station of int ------------------------------------

cnss_of_interest <- "Census Tract 1.02"

plot_one_station <- function (cnss_of_interest) {
  
  one_station_inv <- abs_routes %>% 
    filter(if_any(c('start', 'end'), ~grepl(cnss_of_interest, .))) %>% 
    mutate(n_net = abs(n_net),
           census = ifelse(start==cnss_of_interest, end, start)) %>% 
    full_join(census_stations %>% transmute(census = census_tract %>% str_replace_all(., ", District of Columbia", ""), capacity)) %>% 
    mutate(n_net = ifelse(is.na(n_net), 0, n_net),
           direction = ifelse(n_net != 0 & start == cnss_of_interest, "Net Flow Out",
                              ifelse(n_net != 0 & end == cnss_of_interest, "Net Flow In", 
                                     ifelse(census == cnss_of_interest, cnss_of_interest, "0 Net")))) %>% 
    st_as_sf() %>% 
    filter(census != 'Out of DC') %>% 
    transform(n_net = ifelse(census == cnss_of_interest, max(n_net), n_net))
  
  # limit = ceiling(max(one_station_inv$net_out)/10) * 10
  
  palette_df = data.frame(names = one_station_inv %>% arrange(direction) %>% pull(direction) %>% unique(),
                   colors = c("white", "darkblue", "#32a855", "#bf3102"))
  
  palette = palette_df$colors
  names(palette) = palette_df %>% pull(names)
  
  ggplot(one_station_inv) + geom_sf(aes(fill = direction, alpha = n_net), colour = '#323332') + 
    theme_void()  + scale_fill_manual(values = palette) + 
    guides(fill = guide_legend(title = glue("Net Flow From\n{cnss_of_interest}")),
           colour = guide_colorbar(ticks.colour = 'gray', ticks.linewidth = 1))
  
}

plot_one_station("Census Tract 30") + 
  geom_sf(data = grouped_stations %>% na.omit, aes(geometry = point, color = elevation), size = 1) + 
  scale_color_gradientn(colors = c('lightblue','purple', '#8B4513')) + 
  labs(color = "Elevation (Feet)")

# plot start_end_diff at specific time -------------------------------------------

plot_diff_at_spec_time <- function (time, weekend_flag, adj) {

  wknd <- ifelse(weekend_flag == T, 1, 0)
  
  if (is.na(time[1])) time <- instances_over_time_census_tracts$rounded_time %>% unique
  if (is.na(weekend_flag)) wknd = c(0, 1)
  
  start_end_diff_at_time <- instances_over_time_census_tracts %>% 
    filter(weekend_flag == wknd & rounded_time %in% time) %>% 
    group_by(census) %>% 
    summarise(start_end_diff_unadj = sum(instances[type == 'start']) - sum(instances[type == 'finish'])) %>% 
    full_join(census_stations %>% transmute(census = census_tract, capacity)) %>% 
    mutate(census = census %>% str_replace_all(., ", District of Columbia", ""),
           start_end_diff_unadj = start_end_diff_unadj %>% replace_na(., 0),
           start_end_diff = start_end_diff_unadj/ifelse(capacity != 0, capacity, 0.1)) %>% st_as_sf()
  
  limit = ifelse(adj == 'unadj', 2500, 15)
  fill_var = ifelse(adj == 'unadj', 'start_end_diff_unadj', 'start_end_diff')
  
  ggplot(start_end_diff_at_time) + geom_sf(aes(fill = get(fill_var)), colour = 'black') + 
    theme_void()  + scale_fill_gradientn(colors = c("maroon","white", "darkblue"), limits = c(-limit, limit)) +
                                         # breaks = seq(-limit, limit, 200)) + 
    guides(fill = guide_legend(title = ifelse(adj == 'unadj',
                                              "Starts - Finishes",
                                              "Starts - Finishes\n(adjusted for Stations Capacity)")),
           colour = guide_colorbar(ticks.colour = 'lightgray', ticks.linewidth = 1))

}

plot_diff_at_spec_time(c('17:00', '18:00'), F, 'adj')
plot_diff_at_spec_time(c('08:00', '09:00'), F, 'adj')
plot_diff_at_spec_time(NA, T, 'unadj')


census_stations_df <- read_rds('data/mst/b03_census_stations_instances_over_time_2203.rds') %>% 
  filter(grepl(' 1.02', census)) %>% group_by(rounded_time, type) %>% summarise(instances = sum(instances))
