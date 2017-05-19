# Function to get the correct geom for a Census dataset
# if geometry = TRUE


use_tigris <- function(geography, year, cb = TRUE, resolution = "500k",
                       state = NULL, county = NULL, starts_with = NULL) {

  if (geography == "state") {

    st <- states(cb = cb, resolution = resolution, year = year, class = "sf")

    if (year == 1990) {
      st <- mutate(st, GEOID = ST)
    } else if (year %in% c(2000, 2010)) {
      st <- mutate(st, GEOID = STATE)
    }

    return(st)

  } else if (geography == "county") {

    ct <- counties(cb = cb, state = state, resolution = resolution, year = year,
             class = "sf")

    if (year == 1990) {
      ct <- mutate(ct, GEOID = paste0(ST, CO))
    } else if (year %in% c(2000, 2010)) {
      ct <- mutate(ct, GEOID = paste0(STATE, COUNTY))
    }

    return(ct)

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

    bg <- block_groups(cb = cb, state = state, county = county, year = year,
                 class = "sf")

    if (year == 2000) {
      bg <- bg %>%
        mutate(TRACT = str_pad(TRACT, 6, "right", "0")) %>%
        mutate(GEOID = paste0(STATE, COUNTY, TRACT, BLKGROUP))
    } else if (year == 2010) {
      bg <- mutate(bg, GEOID = paste0(STATE, COUNTY, TRACT, BLKGRP))
    }

    return(bg)

  } else if (geography == "zcta" | geography == "zip code tabulation area") {

    # For right now, to get it to work, it has to be cb = FALSE for 2010
    # Re-visit this in the future.

    if (year == 2010) cb <- FALSE

    z <- zctas(cb = cb, starts_with = starts_with, year = year,
               class = "sf", state = state)

    if (year %in% c(2000, 2010)) {
      z <- mutate(z, GEOID = NAME)
    } else {
      z <- rename(z, GEOID = GEOID10)
    }

    return(z)

  } else if (geography == "block") {

    bl <- blocks(state = state, county = county, year = year, class = "sf")

    if (year > 2000) {
      bl <- rename(bl, GEOID = GEOID10)
    } else if (year == 2000) {
      bl <- rename(bl, GEOID = BLKIDFP00)
    }

  } else {

    stop(sprintf("Geometry for %s is not yet supported.  Use the tigris package and join as normal instead.",
                 geography), call. = FALSE)

  }
}

#' Set the Census API key
#'
#' @param api_key A character string representing a user's Census API key.
#'
#' @export
census_api_key <- function(api_key) {
  Sys.setenv(CENSUS_API_KEY = api_key)
}