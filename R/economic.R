#' Title
#'
#' @param geography
#' @param variables
#' @param naics_code
#' @param year
#' @param state
#' @param county
#' @param key
#' @param output
#' @param geometry
#' @param keep_geo_vars
#'
#' @return
#' @export
#'
#' @examples
get_economic <- function(geography, variables, naics_code, year = 2012,
                         state = NULL, county = NULL, key = NULL, output = "tidy",
                         geometry = FALSE, keep_geo_vars = FALSE) {

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }

  if (!geography %in% c("state", "county", "us", "place", "csa", "combined statistical area",
                        "metropolitan statistical area/micropolitan statistical area", "region",
                        "puerto rico planning area")) {

    stop('Invalid geography. Supported geographies are "state", "county", "us", "place", "csa", "combined statistical area", "metropolitan statistical area/micropolitan statistical area", "region", and "puerto rico planning area"',
         call. = FALSE)

  }

  cache <- getOption("tigris_use_cache", FALSE)

  if (!cache && geometry) {
    message("Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.")
  }

  # Get the data

  dat <- load_data_economic(geography, variables, key, year,
                            naics_code, state, county)

  if (output == "tidy") {

    naics_yr <- paste0("NAICS", year)

    naics_ttl <- paste0("NAICS", year, "_TTL")

    sub <- dat[c("GEOID", "NAME", "OPTAX", naics_yr, naics_ttl, variables)]

    dat2 <- sub %>%
      gather(key = variable, value = value, -GEOID, -NAME, -!!naics_yr, -!!naics_ttl, -OPTAX) %>%
      select(GEOID, NAME, everything())

  } else if (output == "wide") {

    dat2 <- dat

  }

  if (geometry) {

    geom <- suppressMessages(use_tigris(geography = geography, year = year,
                                        state = state, county = county, ...))

    if (! keep_geo_vars) {

      geom <- select(geom, GEOID, geometry)

    }

    # Merge and return the output
    out <- inner_join(geom, dat2, by = "GEOID") %>%
      as_tibble() %>%
      st_as_sf()

    return(out)

  } else {

    return(dat2)

  }




}