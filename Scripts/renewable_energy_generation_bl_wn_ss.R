##################################################################
#                               
# ESM 244 Final Project
# 
# Version: 1
# February 11, 2024               
# Ben Ladabaum, Wes Noble, Scott Schwartz                  
#                               
# Description:  Build general layout, add placeholder widgets, pick a theme,
# and begin populating the app
#
# Notes:  # add panels: 1 for map, 1 for graph, 1 for table
# 
# Inputs:   
#         
#
# Outputs:   
#
##################################################################


# load libraries
library(tidyverse)
library(shiny)
library(here)
library(openxlsx)
library(readxl)
library(janitor)
library(leaflet)
library(bslib)

# load data (for now, just one sample file)
solar_2023 = read_xlsx(here("Data/eia8602023", "3_3_Solar_Y2023.xlsx"), skip = 1) |>
  clean_names()|>
  mutate(energy_type = "solar")
  
  #figure out how to put all states in a vector to be chosen below
#states = unique(solar_2023$state)


# create user interface
ui = fluidPage(
  theme = bs_theme(  # Apply theme
    version = 5, 
    bootswatch = "flatly",  
    primary = "#2c7c59",  
    secondary = "#158cba",  
    success = "#62c462"
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('solar_background.jpg'); 
        background-size: cover;
        background-attachment: fixed;
        background-position: center;
      }
      .panel {
        background-color: rgba(255, 255, 255, 0.8); /* Slight transparency */
        padding: 15px;
        border-radius: 10px;
      }
    "))
  ),
  
  titlePanel("Wind and Solar Generation Facilities in the US"),
  
  sidebarLayout(
    sidebarPanel("Placeholder Widgets",
                 radioButtons(
                   inputId = "state",
                   label = "Choose State",
                   choices = c("AZ", "CA")
                 ),
                 selectInput(inputId = 'energy_type',
                             label = 'select energy type',
                             choices = c("solar", "wind")
                              )
                             ),
    
    mainPanel("Placeholder Graph",
              plotOutput(outputId = 'solar_plot'),
              h3('placeholder summary table'),
              tableOutput(outputId = 'solar_table'),
              leafletOutput("map")
              )
    
  )
)

# create server function
server = function(input, output) {
  state_select = reactive(
    {
      state_df = solar_2023 |>
        filter(state==input$state) |>
        filter(energy_type == input$energy_type)
    }
  )
  
  output$solar_plot = renderPlot({
    ggplot(data = state_select())+
      geom_point(aes(x=operating_year, y=nameplate_capacity_mw))
  })
  
  energy_sum_table = reactive({
    solar_summary_df = solar_2023 |>
      filter(state == input$state) |>
      filter(energy_type == input$energy_type) |>
      group_by(state) |>
      summarize(
        mean_nameplate_capacity = mean(nameplate_capacity_mw, na.rm = TRUE),
      )
  })
  
  output$solar_table = renderTable({
    energy_sum_table()
  })
  
  # Create a placeholder Leaflet map
  output$map = renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>  # Light-themed base map
      setView(lng = -98.5, lat = 39.8, zoom = 4)  # Centered on the US
  })
}

# combine into app:
shinyApp(ui = ui, server=server)







