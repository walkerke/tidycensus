library(albersusa)
library(tidyverse)
library(sf)
library(devtools)


state_laea <- usa_sf("laea") %>%
  select(GEOID = fips_state, geometry) %>%
  mutate(GEOID = as.character(GEOID))

county_laea <- counties_sf("laea") %>%
  select(GEOID = fips, geometry) %>%
  mutate(GEOID = as.character(GEOID))

use_data(state_laea, compress = "xz")
use_data(county_laea, compress = "xz")
