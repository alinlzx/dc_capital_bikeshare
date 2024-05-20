library(dplyr)
library(tidyverse)
library(lubridate)
library(tidycensus)
library(sf)
library(janitor)
library(viridis)
library(scales)
library(glue)

library(ggmap)

#for the aesthetics hehe
library(ggthemes)

library(gridExtra)

# importing datasets ---------------------

census_stations <- read_rds('data/mst/b01_census_stations_data.rds') %>% 
  transform(income_brackets = cut(income, breaks = quantile(income, probs = seq(0, 1, 0.1), na.rm = T)),
            start_end_diff_unadj = start_end_diff * capacity) %>% st_as_sf()

grouped_stations <- read_rds('data/mst/b02_stations_data.rds') %>%
  transform(start_end_diff = (start_instances - end_instances)/capacity) %>% st_as_sf() 

instances_over_time_census_tracts <- read_rds('data/mst/b03_census_stations_instances_over_time.rds') 
instances_over_time_stations <- read_rds('data/mst/b03_stations_instances_over_time.rds') 

bike_availability <- read_rds('data/mst/b04b_bike_avail_2203.rds') 

# constructing xwalk between vars and labels -------------------

var_str = c('Number of Bikes Available'
            , 'Number of Docks Available'
            , 'Stations Capacity'
            , 'Number of Trip Starts'
            , 'Number of Trip Finishes'
            , 'Population'
            , 'Income ($)'
            , 'Number of Trip Starts\n(adjusted for station capacity)'
            , 'Number of Trip Finishes\n(adjusted for station capacity)'
            , 'Average Number of\nOrigins'
            , 'Average Number of\nDestinations'
            , 'Starts - Finishes\n(adjusted for station capacity)'
            , 'Starts - Finishes'
            , 'Station Elevation (feet)')

var_str_expanded <- c(var_str, paste(var_str[1:5], "\n(adjusted for population)"))

vars = c('num_bikes_available'
         , 'num_docks_available'
         , 'capacity'
         , 'start_station_instances'
         , 'end_station_instances'
         , 'population'
         , 'income'
         , 'starts_per_bike'
         , 'ends_per_bike'
         , 'n_starts'
         , 'n_ends'
         , 'start_end_diff'
         , 'start_end_diff_unadj'
         , 'elevation')

vars_expanded <- c(vars, paste0(vars[1:5], '_population_adjusted'))

vars_xwalk <- data.frame(
  var_str = var_str_expanded,
  vars = vars_expanded
)

# making maps -----------------------------

make_maps <- function (varpick = 'num_bikes_available', station_color = NA) {
  
  # if (mean(census_stations %>% select(get(varpick)), na.rm = T) < 1) {
  #   fillmin <- floor(min(census_stations %>% select(get(varpick))) * 100) / 100
  #   fillmax <- ceiling(max(census_stations %>% select(get(varpick))) * 100) / 100
  # } else {
  #   fillmin <- floor(min(census_stations %>% select(get(varpick))) / 100) * 100
  #   fillmax <- ceiling(max(census_stations %>% select(get(varpick))) / 100) * 100
  # }
  
  legend_title <- vars_xwalk %>% filter(vars == varpick) %>% pull(var_str)
  
  map_base <- ggplot(census_stations) + geom_sf(aes(fill = get(varpick)), colour = 'white') + 
    theme_void() + guides(fill = guide_legend(title = legend_title),
                           colour = guide_colorbar(ticks.colour = 'gray', ticks.linewidth = 1))
  
  if (varpick %in% c('income')) {
    
    map <- map_base +
      scale_fill_viridis(option = 'A', direction = -1, labels = scales::dollar_format()) 
    
  } else if (grepl('per|adjusted', varpick)) {
    
    map <- map_base +
      scale_fill_viridis(option = 'B', direction = -1, limits = c(0, 75)) 
    
    print('A')
    
  } else if (grepl('diff', varpick)) {
    
    map <- map_base +
      scale_fill_gradientn(colors = c('maroon', 'white','darkblue'), limits = c(-2500, 2500))
    
  } else {
    
    map <- map_base +
      scale_fill_viridis(option = 'G', direction = -1)
    
  }
  
  if (is.na(station_color)) {
    return(map)
  } else if (station_color == 'black') {
    map + geom_sf(data = grouped_stations, aes(geometry = point), size = 0.5, color = 'black')
  } else {
    map + geom_sf(data = grouped_stations, aes(geometry = point, color = get(station_color)), size = 1) + 
      scale_color_gradient(low = 'lightblue', high = 'purple') + 
      labs(color = "Elevation (Feet)",
           fill = "Starts - Finishes")
  }

  
}

# Applying plotting function ---------------------------

bike_maps <- map(.x = vars_expanded, .f = make_maps)

ggsave(filename = 'outputs/01_capital_bikeshare_maps.pdf',
       plot = gridExtra::marrangeGrob(bike_maps, nrow=1, ncol=1))





# Scatterplot function ---------------------------------

make_point_plots <- function (data, xpick, ypick, colorpick) {
  
  data <- get(data)
  
  xtitle <- vars_xwalk %>% filter(vars == xpick) %>% pull(var_str)
  ytitle <- vars_xwalk %>% filter(vars == ypick) %>% pull(var_str)
  
  legend_title <- vars_xwalk %>% filter(vars == colorpick) %>% pull(var_str)
  
  xmax <- quantile(data %>% pull(get(xpick)), 0.75, na.rm = T) * 2
  
  ymin <- quantile(data %>% pull(get(ypick)), 0.25, na.rm = T) * 2
  ymax <- quantile(data %>% pull(get(ypick)), 0.75, na.rm = T) * 2

  increments <- ifelse((ymax - ymin) < 1, ceiling((ymax - ymin)*10)/100, ceiling((ymax - ymin)/100)*10)
  ymax <- ceiling(ymax/increments) * increments
  ymin <- floor(ymin/increments) * increments
  
  data$off <- "1"
  
  print(ymax)
  
  set_colour_scale <- function (c) {
    if (c == 'income') {
      scale_colour_viridis_c(labels = scales::dollar_format(), direction = -1)
    } else if (c == 'off') {
      
      scale_colour_manual(values = '#856088')
    } else if (grepl('brackets',c)) {
      scale_fill_manual(values = c('#fb607f', '#ff7570', '#c21e56', 'purple'))
    }
    else {
      scale_colour_viridis_b(labels = scales::comma_format(), direction = -1)
    }
  }
  
  # print(colnames(census_stations))
  
  set_x_scale <- function (x) {
    if (x == 'income') {
      scale_x_continuous(labels = scales::dollar_format(), limits = c(0, xmax),
                         breaks = seq(0, xmax, 50000),
                         expand = c(0,0))
    } else {
      scale_x_continuous(limits = c(0, xmax),
                         breaks = seq(0, xmax, floor(xmax/5)),
                         expand = c(0,0))
    }
  }
  
  plot <- ggplot(data ) +
    geom_point(aes(x = get(xpick), y = get(ypick), color = get(colorpick), size = population), alpha = 0.75) +
    # scale_colour_continuous(labels = scales::dollar_format()) +
    set_colour_scale(colorpick) +
    scale_size_continuous(guide = 'none', range = c(0.1, 3)) +
    scale_y_continuous(limits = c(ymin, ymax), 
                       # breaks = seq(0, ymax, ifelse(ymax < 1, round(ymax, 2)/5, ceiling(ymax/10)*10/5))) +
                       breaks = seq(ymin, ymax, increments/2), expand = c(0,increments/10)) +
    set_x_scale(xpick) +
    theme_economist_white(gray_bg = F) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    # theme(legend.position = 'bottom') +
    labs(x = xtitle, y = ytitle) + 
    guides(color = guide_legend(title = legend_title)) + 
    geom_smooth(aes(x = get(xpick), y = get(ypick)), method = 'lm', linewidth = 1, color = 'purple', linetype = 'dashed',
                fill = 'gray')
  
  plot
  
}

# Applying plotting function --------------------------

income_plot <- make_point_plots('capacity', 'starts_per_bike', colorpick = 'income')
test

inputs <- expand.grid(xpick = vars_expanded, ypick = vars_expanded, stringsAsFactors = F)
plots_combined <- pmap(.l = inputs, .f = make_point_plots)

ggsave(filename = 'outputs/01_capital_bikeshare_plots.pdf',
       plot = gridExtra::marrangeGrob(plots_combined, nrow=1, ncol=1))

# Line plot function ---------------------------------------

# color_palette <- c('Start' = '#9acd32', 'Finish' = '#FF6347')
color_palette <- c('Start' = '#001F3F', 'Finish' = '#E87605')

label_xwalk <- data.frame(
  vars = c('type', 'quarter', 'census', 'station', 'weekend', 'instances'),
  var_str = c('Type/Direction of Trip', 'Quarter', 'Census Tract', 'Station', 'Day of Week', 'Frequency')
)

make_lines <- function (filter = '', ypick = 'instances', colorpick = 'type', typepick = 'weekend') {
  
  # if (census_or_station == 'census') {
  #   for_plt <- instances_over_time_census_tracts %>% ungroup()
  # } else {for_plt <- instances_over_time_stations %>% ungroup()}
  
  # date_filter <- as.POSIXct("2022-3-15 00:00", format = "%Y-%m-%d %H:%M")
  
  for_plt <- instances_over_days_census_tracts %>% ungroup() %>% 
    filter(census == filter) %>% 
    mutate(rounded_time = strptime(rounded_time, format = '%H:%M') %>% as.POSIXct(),
           # rounded_time = rounded_time %>% as.POSIXct(format = "%m/%d/%y %H:%M"),
           type = ifelse(type == 'start', 'Start', 'Finish'),
           weekend = ifelse(weekend_flag == 1, 'Weekend', 'Weekday')) # %>% 
  
  ypick_label <- label_xwalk %>% filter(vars == ypick) %>% pull(var_str)
  color_label <- label_xwalk %>% filter(vars == colorpick) %>% pull(var_str)
  type_label <- label_xwalk %>% filter(vars == typepick) %>% pull(var_str)
  
  for_plt_grouped <- for_plt %>% 
    group_by(census, rounded_time, get(typepick), get(colorpick)) %>% 
    summarise(instances = sum(instances)) %>% 
    rename({{colorpick}} := 'get(colorpick)',
           {{typepick}} := 'get(typepick)')
  
  census_tract_title <- for_plt %>% pull(census) %>% 
    unique() %>% str_replace(",.*", "")
  
  ggplot(for_plt_grouped, aes(x = rounded_time, y = get(ypick), 
                              color = get(colorpick))) + 
    geom_line(aes(linetype = get(typepick)), linewidth = 1.25) + 
    theme_economist_white(gray_bg = F) + 
    theme(legend.position = 'bottom',
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) + 
    scale_color_manual(values = color_palette) + 
    labs(x = 'Time of the Day', 
         y = ypick_label,
         title = "{census_tract_title}" %>% glue(),
         # subtitle = "Apr 2020-Feb 2023",
         color = color_label,
         linetype = type_label) + 
    scale_x_datetime(expand = c(0,0), labels = scales::date_format("%H:%M", tz = 'America/New_York')) +
    scale_y_continuous(expand = c(0,0))
  
}

# getting 5 census highest/lowest income above 50 capacity
grouped_instances_over_time <- instances_over_time_census_tracts %>% 
  group_by(census) %>% summarise(start_instances = sum(instances[type == 'start']),
                                        end_instances = sum(instances[type == 'finish'])) %>% 
  transform(start_end_diff = start_instances - end_instances) %>% na.omit()

hl_diff <- rbind(grouped_instances_over_time %>% arrange(start_end_diff) %>% head(5),
                            grouped_instances_over_time %>% arrange(-start_end_diff) %>% head(5)) %>%
  pull(census)

hl_same <- grouped_instances_over_time %>% filter(abs(start_end_diff) < 5 & start_instances >= 300) %>% 
  pull(census)


# getting 5 stations highest/lowest start end diff 
# stations_hl_income <- c((bike_availability %>% arrange(start_end_diff) %>% pull(name) %>% unique)[1:3],
#                         (bike_availability %>% arrange(-start_end_diff) %>% pull(name) %>% unique)[1:3])

hl_diff_lines <- map(hl_diff, .f = make_lines)
grid.arrange(grobs = hl_diff_lines[c(10:15)], ncol = 2, common.legend = T)



#hl_same_lines <- map(hl_same, .f = make_lines)

# for one cns
inv_one_cnss <-grouped_instances_over_time %>% filter(grepl("43", census)) %>% pull(census) %>% unique
make_lines(inv_one_cnss)

# Make lines by day ----------------------------------

make_lines_by_day <- function (filter = '', ypick = 'instances', colorpick = 'type') {
  
  # if (census_or_station == 'census') {
  #   for_plt <- instances_over_time_census_tracts %>% ungroup()
  # } else {for_plt <- instances_over_time_stations %>% ungroup()}
  
  date_filter <- as.POSIXct("2022-3-07 00:00", format = "%Y-%m-%d %H:%M")
  
  for_plt <- instances_over_days_census_tracts %>% ungroup() %>% 
    filter(census == filter) %>% 
    mutate(# rounded_time = strptime(rounded_time, format = '%H:%M') %>% as.POSIXct(),
      rounded_time = rounded_time %>% as.POSIXct(format = "%m/%d/%y %H:%M"),
      type = ifelse(type == 'start', 'Start', 'Finish'),
      weekend = ifelse(weekend_flag == 1, 'Weekend', 'Weekday')) %>% 
    filter(rounded_time  <= date_filter)
  
  ypick_label <- label_xwalk %>% filter(vars == ypick) %>% pull(var_str)
  color_label <- label_xwalk %>% filter(vars == colorpick) %>% pull(var_str)
  # type_label <- label_xwalk %>% filter(vars == typepick) %>% pull(var_str)
  
  for_plt_grouped <- for_plt %>% 
    group_by(census, rounded_time, get(colorpick)) %>% 
    summarise(instances = sum(instances)) %>% 
    rename({{colorpick}} := 'get(colorpick)')
  
  census_tract_title <- for_plt %>% pull(census) %>% 
    unique() %>% str_replace(",.*", "")
  
  ggplot(for_plt_grouped, aes(x = rounded_time, y = get(ypick), 
                              color = get(colorpick))) + 
    geom_line(linewidth = 1.25) + 
    theme_economist_white(gray_bg = F) + 
    theme(legend.position = 'bottom',
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) + 
    scale_color_manual(values = color_palette) + 
    labs(x = 'Time of the Day', 
         y = ypick_label,
         title = "{census_tract_title}" %>% glue(),
         # subtitle = "Apr 2020-Feb 2023",
         color = color_label) + 
    scale_x_datetime(expand = c(0,0), labels = scales::date_format("%m/%d", tz = 'America/New_York')) +
    scale_y_continuous(expand = c(0,0))
  
}

# getting 5 census highest/lowest income adj cap
grouped_instances_over_time <- instances_over_time_census_tracts %>% 
  left_join(census_stations %>% select(census = census_tract, capacity)) %>% 
  group_by(census, capacity) %>% summarise(start_instances = sum(instances[type == 'start']),
                                 end_instances = sum(instances[type == 'finish'])) %>% 
  transform(start_end_diff_cap_adj = (start_instances - end_instances)/capacity) %>% na.omit()

hl_diff_cap_adj <- rbind(grouped_instances_over_time %>% arrange(start_end_diff_cap_adj) %>% head(5),
                 grouped_instances_over_time %>% arrange(-start_end_diff_cap_adj) %>% head(5)) %>%
  pull(census)

hl_diff_lines_by_day <- map(hl_diff, .f = make_lines_by_day)
hl_diff_lines_by_day[[2]]

# plotting bike availability -------------------------------------------

# color_palette <- c('Number of Bikes Available' = 'darkblue', 'Number of Docks Available' = 'maroon')

bike_availability %>% select(name, start_end_diff) %>% distinct() %>% arrange(-start_end_diff) %>% filter(grepl("Canal", name))

stations_of_interest <- c('Georgetown Harbor / 30th St NW', '16th & Harvard St NW')

color_palette <- c('#ff5733', '#800080')

bike_availability %>% ungroup %>% select(name, timestamp, "Number of Bikes Available") %>% 
  left_join(grouped_stations %>% select(station, capacity), by = c('name' = 'station')) %>% 
  filter(timestamp >= as.POSIXct("2022-03-01", tz = "EST") & timestamp <= as.POSIXct("2022-03-15", tz = "EST") &
           name %in% stations_of_interest) %>% 
  # pivot_longer(names_to = "type", values_to = "counts",
  #              cols = c(2:3)) %>% 
  # filter(grepl("Bikes", type)) %>% 
  ggplot(aes(x = timestamp, y = `Number of Bikes Available`, color = name)) + 
  geom_line() + geom_hline(aes(yintercept = capacity), color = color_palette[2], linetype = 'dashed') + 
  geom_hline(aes(yintercept = -capacity), color = color_palette[1], linetype = 'dashed') + 
  theme_economist_white(gray_bg = F) + 
  theme(legend.position = 'bottom',
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.title = element_blank()) +
  scale_color_manual(values = color_palette) + 
  scale_x_datetime(date_breaks = "3 days", date_labels = "%b %d")
# scale_color_manual(values = color_palette)

# regressions ---------------------------------------------------------

bike_reg <- lm(data = grouped_stations, log(start_end_diff ~ starts_per_bike * elevation))
summary(bike_reg)

