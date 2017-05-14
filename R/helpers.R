# Function to get the correct geom for a Census dataset
# if geometry = TRUE


use_tigris <- function(geography, year, cb = TRUE, resolution = "500k",
                       state = NULL, county = NULL, starts_with = NULL) {

  if (geography == "state") {

    states(cb = cb, resolution = resolution, year = year, class = "sf")

  } else if (geography == "county") {

    counties(cb = cb, state = state, resolution = resolution, year = year,
             class = "sf")

  } else if (geography == "tract") {

    tr <- tracts(cb = cb, state = state, county = county, year = year,
           class = "sf")

    if (year == 1990) {
      tr <- tr %>%
        mutate(TRACTSUF = ifelse(is.na(TRACTSUF), "00", TRACTSUF)) %>%
        mutate(GEOID = paste0(ST, CO, TRACTBASE, TRACTSUF))
    } else if (year %in% c(2000, 2010)) {
      if (year == 2000) {
        tr <- mutate(tr, TRACT = str_pad(TRACT, 6, "right", "0"))
      }
      tr <- mutate(tr, GEOID = paste0(STATE, COUNTY, TRACT))
    }
    return(tr)

  } else if (geography == "block group") {

    block_groups(cb = cb, state = state, county = county, year = year,
                 class = "sf")

  } else if (geography == "zcta" | geography == "zip code tabulation area") {

    zctas(cb = cb, starts_with = starts_with, year = year, class = "sf")

  } else {

    stop("Other geographies are not yet supported, but are coming soon.  Use the tigris package and join as normal instead.")

  }
}

#' Title
#'
#' @param api_key
#'
#' @return
#' @export
#'
#' @examples
census_api_key <- function(api_key) {
  Sys.setenv(CENSUS_API_KEY = api_key)
}