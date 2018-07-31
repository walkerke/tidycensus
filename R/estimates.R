#' Get data from the US Census Bureau Population Estimates APIs
#'
#' @param geography The geography of your data.
#' @param product The data product (optional). \code{"population"}, \code{"components"}
#'                \code{"housing"}, and \code{"characteristics"} are supported.
#' @param variables A character string of requested variables to get specific
#'                  variables from the population, components, and housing APIs.
#' @param year The data year (defaults to 2017)
#' @param state The state for which you are requesting data. State
#'              names, postal codes, and FIPS codes are accepted.
#'              Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#'               FIPS codes are accepted. Must be combined with a value supplied
#'               to `state`.  Defaults to NULL.
#' @param output One of "tidy" (the default) in which each row represents an
#'               enumeration unit-variable combination, or "wide" in which each
#'               row represents an enumeration unit and the variables are in the
#'               columns.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'                 if TRUE, uses the tigris package to return an sf tibble
#'                 with simple feature geometry in the `geometry` column.
#' @param keep_geo_vars if TRUE, keeps all the variables from the Census
#'                      shapefile obtained by tigris.  Defaults to FALSE.
#' @param shift_geo if TRUE, returns geometry with Alaska and Hawaii shifted for thematic
#'                  mapping of the entire US.
#' @param key Your Census API key.
#'            Obtain one at \url{http://api.census.gov/data/key_signup.html}.  Can be stored
#'            in your .Renviron with \code{census_api_key("YOUR KEY", install = TRUE)}
#' @param ... other keyword arguments
#'
#' @return A tibble, or sf tibble, of population estimates data
#' @export
get_estimates <- function(geography, product = NULL, variables = NULL,
                          breakdown = NULL,
                          year = 2017, state = NULL, county = NULL,
                          output = "tidy", geometry = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL, ...) {



  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }

  if (product == "characteristics") {
    product <- "charagegroups"
  }

  # If the product is "characteristics", we'll need to do some unique things
  if (product == "charagegroups") {
    if (!is.null(variables)) {
      stop("Use the `breakdown` argument instead of `variables` when requesting population characteristics.", call. = FALSE)
    }
    if (!is.null(breakdown)) {
      variables <- c("POP", breakdown)
    } else {
      stop("Please specify the population breakdown in a vector.  Options include 'SEX', 'AGEGROUP', 'RACE', and 'HISP'.", call. = FALSE)
    }
  }

  # For a variables vector, check to see if the variables cut across multiple products
  if (!is.null(variables) && length(variables) > 1) {

    if (product != "charagegroups") {
      check <- c(any(variables %in% population_estimates_variables),
                 any(variables %in% components_estimates_variables),
                 any(variables %in% housing_estimates_variables))
    } else {
      check <- FALSE
    }

    # if there is more than one TRUE, grab data by variable
    if (length(which(check)) > 1) {
      dat <- map_dfc(variables, function(eachvar) {
        load_data_estimates(geography = geography, product = NULL, variables = eachvar,
                            year = year, state = state, county = county, key = key)
      })

      # Remove any extra GEOID or GEONAME columns
      dat <- dat[, -grep("GEOID[0-9]|GEONAME[0-9]", colnames(dat))]

    } else {
      dat <- load_data_estimates(geography = geography, product = product,
                                 variables = variables,
                                 year = year, state = state,
                                 county = county, key = key)
    }
  } else {
    dat <- load_data_estimates(geography = geography, product = product,
                               variables = variables,
                               year = year, state = state,
                               county = county, key = key)
  }

  if (product == "charagegroups") {
    output <- "wide"
  }

  if (output == "tidy") {

    dat2 <- dat %>%
      rename(NAME = GEONAME) %>%
      gather(key = variable, value = value, -GEOID, -NAME)

    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        dat2[dat2 == variables[i]] <- names(variables)[i]
      }
    }

  } else if (output == "wide") {

    dat <- dat[!duplicated(names(dat), fromLast = TRUE)]

    dat2 <- dat

    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        names(dat2) <- str_replace(names(dat2), variables[i], names(variables)[i])
      }
    }

    dat2 <- dat2 %>%
      select(GEOID, NAME = GEONAME, everything())

  }

  if (product == "charagegroups") {
    dat2 <- rename(dat2, value = POP)
  }

  if (geometry) {

    if (shift_geo) {


      if (!is.null(state)) {
        stop("`shift_geo` is only available when requesting geometry for the entire US", call. = FALSE)
      }

      message("Please note: Alaska and Hawaii are being shifted and are not to scale.")

      if (geography == "state") {

        geom <- tidycensus::state_laea

      } else if (geography == "county") {

        geom <- tidycensus::county_laea

        if (year > 2014) {
          # Account for change from Shannon County, SD to Oglala Lakota County
          # and the new Kusilvak Census Area in AK
          geom$GEOID[geom$GEOID == "46113"] <- "46102"
          geom$GEOID[geom$GEOID == "02270"] <- "02158"
        }

      } else {
        stop("`shift_geo` is only available for states and counties", call. = FALSE)
      }

    } else {

      geom <- suppressMessages(use_tigris(geography = geography, year = year,
                                          state = state, county = county, ...))
    }

    if (! keep_geo_vars) {

      geom <- select(geom, GEOID, geometry)

    }

    if (shift_geo) {
      out <- inner_join(geom, dat2, by = "GEOID") %>%
        as_tibble() %>%
        st_as_sf()
    } else {
      out <- right_join(geom, dat2, by = "GEOID") %>%
        as_tibble() %>%
        st_as_sf()
    }

    return(out)

  } else {

    return(dat2)

  }



}