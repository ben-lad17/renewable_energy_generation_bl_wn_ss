##################################################################
#                               
# ESM 244 Final Project
# 
# Version: 1
# February 27, 2024               
# Ben Ladabaum, Wes Noble, Scott Schwartz                  
#                               
# Description:  Build dataset to be used for shiny app
#
# Notes:  
# 
# Inputs:   
#         
#
# Outputs:   
#
##################################################################

# load libraries
library(tidyverse)
library(here)
library(openxlsx)
library(readxl)
library(janitor)

### load data ###

## 1) Create solar dataset and 2) create wind dataset

# Define the years of the datasets
years <- 2016:2023

# Initialize an empty list to store data frames
solar_list = list()
wind_list = list()
plant_list = list()

# Loop through each year and read the corresponding file
for (year in years) {
  file_path = here(paste0("Data/eia860", year), paste0("3_3_Solar_Y", year, ".xlsx"))
  file_path_w = here(paste0("Data/eia860", year), paste0("3_2_Wind_Y", year, ".xlsx"))
  file_path_p = here(paste0("Data/eia860", year), paste0("2___Plant_Y", year, ".xlsx"))
  
  # Read the file and clean it
  solar_data = read_xlsx(file_path, skip = 1) |>
    clean_names() |>
    mutate(energy_type = "solar", year = year)  # Add a column for year and energy type
  
  wind_data = read_xlsx(file_path_w, skip = 1) |>
    clean_names() |>
    mutate(energy_type = "wind", year = year)  
  
  plant_data = read_xlsx(file_path_p, skip = 1) |>
    clean_names() |>
    mutate(year = year)  
  
  # Append to the list
  solar_list[[as.character(year)]] = solar_data
  wind_list[[as.character(year)]] = wind_data
  plant_list[[as.character(year)]] = plant_data
}

# Combine all data frames into one
solar_combined = bind_rows(solar_list)
wind_combined = bind_rows(wind_list)
plant_combined = bind_rows(plant_list)

# extract relevant information from plant level data
addresses = plant_combined |>
  select(utility_id, plant_code, street_address, city, zip, 
         latitude, longitude, year)

# merge solar and wind data with plant codes
solar_combined_w_address = left_join(solar_combined, addresses, 
                                     by = c("utility_id", "plant_code", "year"))
wind_combined_w_address = left_join(wind_combined, addresses, 
                                     by = c("utility_id", "plant_code", "year"))


### Export data ###
write_csv(solar_combined_w_address, here("Output/eia_solar_2016-2023.csv"))
write_csv(wind_combined_w_address, here("Output/eia_wind_2016-2023.csv"))
write_csv(plant_combined, here("Output/eia_plants_2016-2023.csv"))















