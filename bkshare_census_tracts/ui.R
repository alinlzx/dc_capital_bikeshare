#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

var_str = c('Number of Bikes Available'
            , 'Number of Docks Available'
            , 'Stations Capacity'
            , 'Number of Trip Starts'
            , 'Number of Trip Finishes'
            , 'Population'
            , 'Income')

# Define UI for application that draws a histogram
fluidPage(

    titlePanel("DC Capital Bikeshare Interactive Exhibit"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("xvar", "Select Variable 1", choices = var_str),
            selectInput("yvar", "Select Variable 2", choices = var_str),
            radioButtons("plotType", "Choose Plot Type:",
                         choices = c("Side-by-Side Maps", "Scatterplot"),
                         selected = "Side-by-Side Maps")
        ),
        
        mainPanel(
            # Output for maps or scatterplot
            uiOutput("plotOutput")
        )
    )
)
