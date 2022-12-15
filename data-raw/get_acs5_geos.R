# Run this three at a time - local memory can't handle all at once
survey <- "acs5"

# 2010 fails, so ignore for now (may be a general issue like 2009)
year <- 2010
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2011
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2012
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2013
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2014
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2015
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2016
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2017
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2018
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2019
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2020
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)
year <- 2021
rstudioapi::jobRunScript("data-raw/find_available_geographies.R", importEnv = TRUE)


# Iterate through the datasets and assemble, then check the size
library(magrittr)

years <- 2011:2021

acs5_geography <- purrr::map_dfr(years, ~{
  readr::read_rds(glue::glue("data-raw/geo_availability/acs5_{.x}.rds")) %>%
    dplyr::mutate(year = .x)
})

usethis::use_data(acs5_geography, overwrite = TRUE)