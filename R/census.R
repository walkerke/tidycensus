#' Obtain data and feature geometry for the decennial Census
#'
#' @param geography The geography of your data.
#' @param variables Character string or vector of character strings of variable
#'                  IDs.#'
#' @param year The year for which you are requesting data.  1990, 2000, and 2010 are available.
#' @param sumfile The Census summary file.  Defaults to sf1; the function will look in sf3 if it
#'                cannot find a variable in sf1.
#' @param state The state for which you are requesting data. State
#'              names, postal codes, and FIPS codes are accepted.
#'              Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#'               FIPS codes are accepted. Must be combined with a value supplied
#'               to `state`.  Defaults to NULL.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'                 if TRUE, uses the tigris package to return an sf tibble
#'                 with simple feature geometry in the `geometry` column.  state, county, tract, and block group are
#'                 supported for 1990 through 2010; block and ZCTA geometry are supported for 2000 and 2010.
#' @param output One of "tidy" (the default) in which each row represents an
#'               enumeration unit-variable combination, or "wide" in which each
#'               row represents an enumeration unit and the variables are in the
#'               columns.
#' @param keep_geo_vars if TRUE, keeps all the variables from the Census
#'                      shapefile obtained by tigris.  Defaults to FALSE.
#' @param summary_var Character string of a "summary variable" from the decennial Census
#'                    to be included in your output. Usually a variable (e.g. total population)
#'                    that you'll want to use as a denominator or comparison.
#' @param key Your Census API key.
#'            Obtain one at \url{http://api.census.gov/data/key_signup.html}
#' @param ... Other keyword arguments
#'
#' @return a tibble or sf tibble of decennial Census data
#' @export
get_decennial <- function(geography, variables, year = 2010, sumfile = "sf1",
                   state = NULL, county = NULL, geometry = FALSE, output = "tidy",
                   keep_geo_vars = FALSE, summary_var = NULL, key = NULL, ...) {

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }

  if (geography %in% c("tract", "block group") & year == 1990 & is.null(county)) {
    stop("At the moment, tracts and block groups for 1990 require specifying a county.",
         call. = FALSE)
  }

  if (length(variables) > 50) {
    stop("The maximum number of variables supported by the Census API at one time is 50. I'm working on a fix; in the meantime consider splitting your variables into multiple calls and using cbind/rbind to combine them.", call. = FALSE)
  }

  if (geography == "zcta") geography <- "zip code tabulation area"

  if (geography == "zip code tabulation area" & is.null(state)) {
    stop("ZCTA data for the decennial Census is only available by state from tidycensus.",
         call. = FALSE)
  }

  if (geography == "zip code tabulation area" & year == 2000 & geometry == TRUE) {
    stop("Linked ZCTA geometry and attributes for 2000 are not currently available in tidycensus.",
         call. = FALSE)
  }

  dat <- try(load_data_decennial(geography, variables, key, year, sumfile, state, county),
             silent = TRUE)

  # If sf1 fails, try to get it from sf3
  if ("try-error" %in% class(dat)) {
    dat <- try(load_data_decennial(geography, variables, key, year, sumfile = "sf3", state, county))
  }

  if (output == "tidy") {

    sub <- dat[c("GEOID", "NAME", variables)]

    dat2 <- sub %>%
      gather(key = variable, value = value, -GEOID, -NAME)

  } else if (output == "wide") {

    dat2 <- dat

  }

  if (!is.null(summary_var)) {

    sumdat <- suppressMessages(try(load_data_decennial(geography, summary_var, key, year,
                                                   sumfile, state, county)))

    if ("try-error" %in% class(sumdat)) {
      sumdat <- suppressMessages(try(load_data_decennial(geography, summary_var, key, year,
                                        sumfile = "sf3", state, county)))
    }

    dat2 <- dat2 %>%
      inner_join(sumdat, by = "GEOID") %>%
      rename_("summary_value" = summary_var,
              NAME = "NAME.x") %>%
      select(-NAME.y)

  }

  if (geometry == TRUE) {

    geom <- suppressMessages(use_tigris(geography = geography, year = year,
                       state = state, county = county, ...))

    if (keep_geo_vars == FALSE) {

      geom <- select(geom, GEOID, geometry)

    }

    # Merge and return the output
    out <- inner_join(geom, dat2, by = "GEOID") %>%
      st_as_sf()

    return(out)

  } else {

    return(dat2)

  }

}