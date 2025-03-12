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
# Clean the environment
rm(list=ls())

# Load libraries
library(tidyverse)
library(shiny)
library(here)
library(openxlsx)
library(readxl)
library(janitor)
library(leaflet)
library(bslib)

# Load data 
solar_2023 = read.csv(here("Output", "eia_solar_2016-2023.csv")) |>
  filter(year==2023)
wind_2023 = read.csv(here("Output", "eia_wind_2016-2023.csv")) |>
  filter(year==2023)


# Combine datasets and add energy_type column
energy_data <- bind_rows(
  solar_2023 |> mutate(energy_type = "Solar"),
  wind_2023 |> mutate(energy_type = "Wind")
)

year_range <- range(energy_data$operating_year, na.rm = TRUE)

cv_accuracy_df = read.csv(here("Output", "cv_accuracy_df.csv")) |>
  clean_names()

# Create user interface
ui = navbarPage(
  title = "Wind and Solar Generation in the US",
  theme = bs_theme( 
    version = 5, 
    bootswatch = "flatly",  
    primary = "#2c7c59",  
    secondary = "#158cba",  
    success = "#62c462"
  ),
  
  # Introduction Panel
  tabPanel("Introduction",
           fluidPage(
             wellPanel(
               h3("Project Overview"),
               p("This Shiny app explores wind and solar energy generation across the United States over the past 25 years. 
       Users can analyze energy generation trends, view summary tables, and explore energy facility locations on an interactive map.")
             ),
             
             # Blank Row between summary and citation
             br(),  # Adds a blank line
             
             # Citation Section
             wellPanel(
               h4("Data Citation:"),
               p("U.S. Energy Information Administration (EIA). Electricity Data: EIA-860 Detailed Data Files.", 
                 "U.S. Energy Information Administration. Retrieved January 30th, 2025, from https://www.eia.gov/electricity/data/eia860/
")
             )
           )
  ),
  
  # Energy Generation Plot Panel
  tabPanel("Energy Generation Plot",
           sidebarLayout(
             sidebarPanel(
               selectInput("state_plot", "Choose State", choices = sort(unique(solar_2023$state))),
               checkboxGroupInput("energy_type_plot", "Select Energy Type", 
                                  choices = c("Solar", "Wind"),
                                  selected = c("Solar", "Wind")) 
             ),
             mainPanel(
               plotOutput("solar_plot")
             )
           )
  ),
  
  # Energy Summary Table Panel
  tabPanel("Energy Summary Table",
           sidebarLayout(
             sidebarPanel(
               radioButtons("energy_type_table", "Select Energy Type", 
                            choices = c("Solar", "Wind"),
                            selected = "Solar"),
               
               # Sector Descriptions
               wellPanel(
                 p(strong("Commercial CHP:"), " Combined Heat and Power (CHP) plants serving commercial facilities."),
                 p(strong("Commercial Non-CHP:"), " Non-CHP generation serving commercial entities."),
                 p(strong("Electric Utility:"), " Traditional utility companies that generate and distribute electricity."),
                 p(strong("IPP CHP:"), " Independent Power Producers (IPP) with CHP generation."),
                 p(strong("IPP Non-CHP:"), " Independent Power Producers without CHP generation."),
                 p(strong("Industrial CHP:"), " Industrial facilities using Combined Heat and Power for self-generation."),
                 p(strong("Industrial Non-CHP:"), " Industrial facilities generating power without CHP.")
               )
             ),
             mainPanel(
               tableOutput("solar_table"),
             )
           )
  ),
  
  # Map Panel
  tabPanel("Map of Energy Locations",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year_range", "Select Operating Year Range:",
                           min = year_range[1], max = year_range[2], 
                           value = year_range, step = 1, sep = "")
             ),
             mainPanel(
               h3("New Project Installations"),
               leafletOutput("map")
             )
           )
  ),
  
  
  tabPanel("Cross Validation Results",
           sidebarLayout(
             radioButtons("model_choice", "Select Model", 
                          choices = c("Model 1: All variables", "Model 2: No sector", "Model 3: No sector or year"),
                          selected = "Model 1: All variables"),
             mainPanel(
               tableOutput("cv_table")
             )
           )
  )
  
)
  
  
  


# Create server function
server = function(input, output) {
  
  # Reactive data filtering for plot
  state_select_plot = reactive({
    energy_data |>
      filter(state == input$state_plot) |>
      filter(energy_type %in% input$energy_type_plot)
  })
  
  output$solar_plot = renderPlot({
    ggplot(data = state_select_plot()) +
      geom_point(aes(x = operating_year, y = nameplate_capacity_mw, color=energy_type)) +
      scale_color_manual(
        name = "Energy Type", 
        values = c("Solar" = "orange", "Wind" = "lightblue")  
      ) +
      labs(x = "Operating Year", y = "Nameplate Capacity", 
           title = "Operating Year and Capacity of Solar and Wind Generation Facilities") +
      theme_bw() +
      theme(legend.text = element_text(size = 12),  # Increase legend text size
            legend.title = element_text(size = 14))  # Increase legend title size
  })
  
  
  # sector output table
  energy_sum_table = reactive({
    energy_data |>
      filter(energy_type == input$energy_type_table) |>
      group_by(sector_name) |>
      summarize(
        total_nameplate_capacity = sum(nameplate_capacity_mw, na.rm = TRUE),
        mean_nameplate_capacity = mean(nameplate_capacity_mw, na.rm = TRUE)
      ) |>
      rename(
        Sector = sector_name,
        `Total Capacity (MW)` = total_nameplate_capacity,
        `Mean Capacity (MW)` = mean_nameplate_capacity
      )
  })
  
  output$solar_table = renderTable({
    energy_sum_table()
  })
  
  
  # Reactive data filtering for map
  filtered_data_map <- reactive({
    energy_data |>
      filter(operating_year >= input$year_range[1], 
             operating_year <= input$year_range[2])
  })
  
  # Initial Map Setup (Only runs once)
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -98.5, lat = 39.8, zoom = 4) |>
      
      # Custom Legend using HTML
      addControl(
        html = "
    <div style='background-color: white; padding: 8px; border-radius: 5px; box-shadow: 2px 2px 5px rgba(0,0,0,0.2);'>
      <strong>Energy Type</strong><br>
      <svg width='20' height='20'><circle cx='10' cy='10' r='6' stroke='black' stroke-width='1.5' fill='lightblue' /></svg> Wind<br>
      <svg width='20' height='20'><circle cx='10' cy='10' r='6' stroke='black' stroke-width='1.5' fill='orange' /></svg> Solar
    </div>",
        position = "bottomright"
      )
  })
  
  # Observe updates and refresh the data when year range changes
  observe({
    data <- filtered_data_map()  # Get filtered dataset
    
    leafletProxy("map", data = data) |>
      clearMarkerClusters() |>  # Ensure old clusters are removed
      clearMarkers() |>  # Clear old markers
      clearShapes() |>  # Remove any lingering shapes
      
      # Add Clustered Points for Facilities
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 5,  
        stroke = TRUE,
        weight = 1.5,  
        color = "black",  
        fillColor = ~ifelse(energy_type == "Solar", "orange", "lightblue"),
        fillOpacity = 0.8,  
        popup = ~paste0("<b>Facility:</b> ", utility_name, "<br>",
                        "<b>Operating Year:</b> ", operating_year),
        clusterOptions = markerClusterOptions(
          disableClusteringAtZoom = 9,  
          showCoverageOnHover = FALSE  
        )  
      )
  })
  
  
  # cross validation results table
  cv_results_table = reactive({
    req(input$model_choice)  # Ensure input is available
    cv_accuracy_df |>
      filter(model == input$model_choice) 
  })
  
  output$cv_table = renderTable({
    cv_results_table()
  })
  
  
}

# Combine into Shiny app
shinyApp(ui = ui, server = server)