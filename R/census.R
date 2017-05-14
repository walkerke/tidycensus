#' Title
#'
#' @param geography
#' @param variables
#' @param key
#' @param year
#' @param sumfile
#' @param state
#' @param county
#' @param geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
    stop("The maximum number of variables supported by the Census API at one time is 50. Consider splitting your variables into multiple calls and using cbind/rbind to combine them.", call. = FALSE)
  }

  dat <- try(load_data_decennial(geography, variables, key, year, sumfile, state, county))

  # If sf1 fails, try to get it from sf3
  if ("try-error" %in% class(dat)) {
    dat <- try(load_data_decennial(geography, variables, key, year, sumfile = "sf3", state, county))
  }

  if (output == "tidy") {

    sub <- dat[c("GEOID", variables)]

    dat2 <- sub %>%
      gather(key = variable, value = value, -GEOID)

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
      rename_("summary_var" = summary_var)

  }

  if (geometry == TRUE) {

    geom <- suppressMessages(use_tigris(geography = geography, year = year,
                       state = state, county = county, ...))

    if (keep_geo_vars == FALSE) {

      geom <- select(geom, GEOID, geometry)

    }

    # Merge and return the output
    out <- left_join(geom, dat2, by = "GEOID")

    return(out)

  } else {

    return(dat2)

  }

}