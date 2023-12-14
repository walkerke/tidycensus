library(tidycensus)
library(tidyverse)
library(furrr)

plan("multisession")

# Get the first variable in a table for a given year
# We are assuming here (from the Census docs) that variable availability
# is by table
first_var <- load_variables(year, survey) %>%
  filter(str_starts(name, "B|C")) %>%
  mutate(table = str_remove(name, "_.*")) %>%
  group_by(table) %>%
  filter(name == first(name)) %>%
  ungroup() %>%
  pull(name)

# var_list1 <- var_list[1:100]

# Iterate through the var_list and assemble a tibble with the
# var name and the lowest available geography
# Use a large county as performance is decent in parallel, and some large counties
# have data available at the county level that smaller ones don't
table_availability <- future_map_dfr(first_var, function(var) {
  if (year < 2013) {
    geographies <- c("tract", "county", "state", "us")
  } else {
    geographies <- c("block group", "tract", "county", "state", "us")
  }

  for (g in geographies) {

    if (g %in% c("block group", "tract", "county")) {
      county <- "Dallas"
      state <- "TX"
    } else if (g == "state") {
      county <- NULL
      state <- "TX"
    } else {
      county <- NULL
      state <- NULL
    }

    test <- get_acs(
      geography = g,
      variables = var,
      state = state,
      county = county,
      year = year,
      survey = survey
    )

    table <- str_remove(var, "_.*")

    if (!all(is.na(test$estimate))) {
      return(tibble(table = table,
                    geography = g))
    }

  }

})

# Write out to intermediate file
readr::write_rds(table_availability, glue::glue("~/Dropbox/dev/tidycensus/data-raw/geo_availability/{survey}_{year}.rds"))