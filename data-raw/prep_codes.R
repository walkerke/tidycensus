library(dplyr)

county_codes <- read.csv('data-raw/national_county.txt', header = FALSE, colClasses = "character")

state_codes <- read.table('data-raw/state.txt', header = TRUE, sep = "|", colClasses = "character")

names(county_codes) <- c("state", "state_code", "county_code", "county", "type")

names(state_codes) <- c("state_code", "state", "state_name", "statens")

fips_codes <- left_join(county_codes, state_codes, by = "state_code")

fips_codes <- select(fips_codes, state = state.x, state_code, state_name, county_code, county)

new_counties <- data.frame(state = c('SD', 'AK', 'AK', 'AK'),
                           state_code = c('46', '02', '02', '02'),
                           state_name = c('South Dakota', 'Alaska', 'Alaska', 'Alaska'),
                           county_code = c('102', '158', '063', '066'),
                           county = c('Oglala Lakota County', 'Kusilvak Census Area',
                                      'Chugach Census Area', 'Copper River Census Area'))

defunct_counties <- data.frame(state = c("AK", "AK", "AK", "VA", "AK", "FL", "MT", "VA"),
                               state_code = c("02", "02", "02", "51", "02", "12", "30", "51"),
                               state_name = c("Alaska", "Alaska", "Alaska", "Virginia", "Alaska",
                                              "Florida", "Montana", "Virginia"),
                               county_code = c("201", "232", "280", "560", "231",
                                               "025", "113", "780"),
                               county = c("Prince of Wales-Outer Ketchikan Census Area",
                                          "Skagway-Hoonah-Angoon Census Area",
                                          "Wrangell-Petersburg Census Area", "Clifton Forge city",
                                          "Skagway-Yakutat-Angoon Census Area", "Dade County",
                                          "Yellowstone National Park", "South Boston city"))

new_ct_counties <- tigris::counties(state = "CT", year = 2022, refresh = TRUE) |>
  sf::st_drop_geometry() |>
  transmute(
    state = "CT",
    state_code = STATEFP,
    state_name = "Connecticut",
    county_code = COUNTYFP,
    county = NAME
  )

fips_codes <- rbind(fips_codes, new_counties, defunct_counties, new_ct_counties) %>%
  arrange(state_code, county_code) %>%
  unique()


usethis::use_data(fips_codes, overwrite = TRUE)
