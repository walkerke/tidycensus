library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)

# states with county subdivisions for migration flows
mcd_states <- c("Connecticut", "Maine", "Massachusetts", "Michigan", "Minnesota",
                "New Hampshire", "New Jersey", "New York", "Pennsylvania",
                "Rhode Island", "Vermont", "Wisconsin")

# all combinations of states and years to iterate over
mcd_state_year <- crossing(mcd_states, year = 2010:2018)

# get polygons of counties, mcds, and cbsas via tigris
# we want 2010 - 2018 because some geographies in the flows data
# may have changed over time
county_poly <- map_dfr(2010:2018, ~ counties(year = .x))
mcd_poly <- map2_dfr(mcd_state_year$mcd_states, mcd_state_year$year,
                     ~ county_subdivisions(state = .x, year = .y))
cbsa_poly <- map_dfr(2010:2018, ~ core_based_statistical_areas(year = .x))

# combine all geographies and keep only one row per GEOID
# obtain centroid of each polygon and create a standard data frame
# with a sfc_POINT column with the centroid of each GEOID
centroids <- bind_rows(list(county_poly, mcd_poly, cbsa_poly)) %>%
  mutate(GEOID = coalesce(GEOID10, GEOID)) %>%
  group_by(GEOID) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(GEOID) %>%
  st_centroid() %>%
  mutate(centroid = geometry) %>%
  st_drop_geometry() %>%
  as_tibble()

usethis::use_data(centroids, internal = TRUE)
