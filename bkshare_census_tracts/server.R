#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(lubridate)
library(tidycensus)
library(janitor)

data <- read_rds('census_stations_data.rds') %>% 
    as.data.frame() %>% 
    st_as_sf()

input_xwalk <- data.frame(
    var_str,
    vars = c('num_bikes_available'
              , 'num_docks_available'
              , 'capacity'
              , 'start_station_instances'
              , 'end_station_instances'
              , 'population'
              , 'income')
)

# Define server logic required to draw vis
function(input, output, session) {
    
    output$plotOutput <- renderPlot({
        
        req(input$xvar)
        req(input$yvar)
        req(input$plotType)
        
        xvar <- input_xwalk %>% 
            filter(var_str == input$xvar) %>% pull(vars)

        
        yvar <- input_xwalk %>% 
            filter(var_str == input$yvar) %>% pull(vars)
        
        
        
        if (input$plotType == "Side-by-Side Maps") {
            print(xvar)
            output <- ggplot(data) + geom_point(aes(x = get(xvar), y = get(yvar)))
        }
        
        grid.arrange(output)
        
    })

}
