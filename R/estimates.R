#' Get data from the US Census Bureau Population Estimates APIs
#'
#' @param geography The geography of your data. Available geographies for the most recent data vintage are listed
#'                  \href{https://api.census.gov/data/2019/pep/population/geography.html}{here}. \code{"cbsa"} may
#'                  be used an alias for \code{"metropolitan statistical area/micropolitan statistical area"}.
#' @param product The data product (optional). \code{"population"}, \code{"components"}
#'                \code{"housing"}, and \code{"characteristics"} are supported.
#' @param variables A character string or vector of character strings of requested variables
#'                  to get from either the population, components, or housing API.
#' @param breakdown The population breakdown used when \code{product = "characteristics"}.
#'                  Acceptable values are \code{"AGEGROUP"}, \code{"RACE"}, \code{"SEX"}, and
#'                  \code{"HISP"}, for Hispanic/Not Hispanic.  These values can be combined in
#'                  a vector, returning population estimates in the \code{value} column for all
#'                  combinations of these breakdowns.
#' @param breakdown_labels Whether or not to label breakdown elements returned when
#'                         \code{product = "characteristics"}. Defaults to FALSE.
#' @param year The data vintage (defaults to 2019). It is recommended to use the most recent vintage
#'             available for a given decennial series (so, year = 2019 for the 2010s) and
#'             use \code{time_series = TRUE} to access estimates for prior years.
#' @param state The state for which you are requesting data. State
#'              names, postal codes, and FIPS codes are accepted.
#'              Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#'               FIPS codes are accepted. Must be combined with a value supplied
#'               to `state`.  Defaults to NULL.
#' @param time_series If \code{TRUE}, the function will return a time series of observations back to the decennial Census
#'                    of 2010. The returned column is either "DATE", representing a particular estimate date, or "PERIOD",
#'                    representing a time period (e.g. births between 2016 and 2017), and contains integers representing
#'                    those values.  Integer to date or period mapping is available at
#'                    \url{https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html}.
#' @param output One of "tidy" (the default) in which each row represents an
#'               enumeration unit-variable combination, or "wide" in which each
#'               row represents an enumeration unit and the variables are in the
#'               columns.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'                 if TRUE, uses the tigris package to return an sf tibble
#'                 with simple feature geometry in the `geometry` column.
#' @param keep_geo_vars if TRUE, keeps all the variables from the Census
#'                      shapefile obtained by tigris.  Defaults to FALSE.
#' @param shift_geo (deprecated) if TRUE, returns geometry with Alaska and Hawaii shifted for thematic
#'                  mapping of the entire US.  As of May 2021, we recommend using \code{tigris::shift_geometry()}
#'                  instead.
#' @param key Your Census API key.
#'            Obtain one at \url{https://api.census.gov/data/key_signup.html}.  Can be stored
#'            in your .Renviron with \code{census_api_key("YOUR KEY", install = TRUE)}
#' @param show_call if TRUE, display call made to Census API. This can be very useful
#'                  in debugging and determining if error messages returned are
#'                  due to tidycensus or the Census API. Copy to the API call into
#'                  a browser and see what is returned by the API directly. Defaults to FALSE.
#' @param ... other keyword arguments
#'
#' @return A tibble, or sf tibble, of population estimates data
#' @export
get_estimates <- function(geography = c("us", "region", "division", "state", "county", "county subdivision",
                                        "place/balance (or part)", "place", "consolidated city", "place (or part)",
                                        "metropolitan statistical area/micropolitan statistical area", "cbsa",
                                        "metropolitan division", "combined statistical area"),
                          product = NULL, variables = NULL,
                          breakdown = NULL, breakdown_labels = FALSE,
                          year = 2019, state = NULL, county = NULL,
                          time_series = FALSE,
                          output = "tidy", geometry = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL, show_call = FALSE, ...) {

  geography <- rlang::arg_match(geography)

  if (shift_geo) {
    warning("The `shift_geo` argument is deprecated and will be removed in a future release. We recommend using `tigris::shift_geometry()` instead.", call. = FALSE)
  }

  if (year < 2015) {
    stop("The Population Estimates API is not available in tidycensus for years prior to 2015. Consider using `time_series = TRUE` or the censusapi package for earlier estimates.")
  }


  # Check for a Census API key and warn if missing
  key <- get_census_api_key(key)

  if (geography == "cbsa") geography <- "metropolitan statistical area/micropolitan statistical area"

  insist_get_estimates <- purrr::insistently(get_estimates)

  # Allow for characteristics products to be pulled for the entire US by county
  # Come back to this later
  if (!is.null(product)) {

    if (is.null(state) && geography == "county" && product == "characteristics") {

      message("Fetching characteristics data by state and combining the result.")

      state <- unique(fips_codes$state_code)[1:51]
      # mc <- match.call(expand.dots = TRUE)
      if (geometry) {
        result <- map(state,~{
          suppressMessages(
            insist_get_estimates(geography = geography,
                                 product = product,
                                 variables = variables,
                                 breakdown = breakdown,
                                 breakdown_labels = breakdown_labels,
                                 year = year,
                                 state = .x,
                                 county = county,
                                 time_series = time_series,
                                 output = output,
                                 geometry = geometry,
                                 keep_geo_vars = keep_geo_vars,
                                 shift_geo = shift_geo,
                                 key = key,
                                 show_call = show_call))
        }) %>%
          reduce(rbind)
        geoms <- unique(st_geometry_type(result))
        if (length(geoms) > 1) {
          result <- st_cast(result, "MULTIPOLYGON")
        }
        result <- result %>%
          as_tibble() %>%
          st_as_sf()
      } else {
        result <- map_df(state, ~{
          suppressMessages(
            insist_get_estimates(geography = geography,
                                 product = product,
                                 variables = variables,
                                 breakdown = breakdown,
                                 breakdown_labels = breakdown_labels,
                                 year = year,
                                 state = .x,
                                 county = county,
                                 time_series = time_series,
                                 output = output,
                                 geometry = geometry,
                                 keep_geo_vars = keep_geo_vars,
                                 shift_geo = shift_geo,
                                 key = key,
                                 show_call = show_call))
        })
      }
      return(result)

    }

  }

  if (!is.null(product) && product == "characteristics") {
    product <- "charagegroups"
  }

  # If the product is "characteristics", we'll need to do some unique things
  if (!is.null(product) && product == "charagegroups") {
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

    if (!is.null(product) && product != "charagegroups") {
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
                            year = year, state = state, county = county,
                            time_series = time_series, key = key, show_call = show_call)
      })

      # Remove any extra GEOID or GEONAME columns
      dat <- dat[, -grep("GEOID[0-9]|GEONAME[0-9]|NAME[0-9]", colnames(dat))]

    } else {
      dat <- load_data_estimates(geography = geography, product = product,
                                 variables = variables,
                                 year = year, state = state,
                                 time_series = time_series,
                                 county = county, key = key, show_call = show_call)
    }
  } else {
    dat <- load_data_estimates(geography = geography, product = product,
                               variables = variables,
                               year = year, state = state,
                               time_series = time_series,
                               county = county, key = key, show_call = show_call)
  }

  if (!is.null(product) && product == "charagegroups") {
    output <- "wide"
  }

  if ("PERIOD_CODE" %in% names(dat)) {
    dat <- rename(dat, PERIOD = PERIOD_CODE)
  }

  if ("DATE_CODE" %in% names(dat)) {
    dat <- rename(dat, DATE = DATE_CODE)
  }

  if ("DATE_" %in% names(dat)) {
    dat <- rename(dat, DATE = DATE_)
  }

  if (output == "tidy") {

    if (time_series) {
      if ("PERIOD" %in% names(dat)) {
        if ("GEONAME" %in% names(dat)) {
          dat2 <- dat %>%
            rename(NAME = GEONAME) %>%
            gather(key = variable, value = value, -GEOID, -NAME, -PERIOD)
        } else {
          dat2 <- dat %>%
            gather(key = variable, value = value, -GEOID, -NAME, -PERIOD)
        }

      } else {
        if ("GEONAME" %in% names(dat)) {
          dat2 <- dat %>%
            rename(NAME = GEONAME) %>%
            gather(key = variable, value = value, -GEOID, -NAME, -DATE)
        } else {
          dat2 <- dat %>%
            gather(key = variable, value = value, -GEOID, -NAME, -DATE)
        }
      }
    } else {

      if ("GEONAME" %in% names(dat)) {
        dat2 <- dat %>%
          rename(NAME = GEONAME) %>%
          gather(key = variable, value = value, -GEOID, -NAME)
      } else {
        dat2 <- dat %>%
          gather(key = variable, value = value, -GEOID, -NAME)
      }


    }

    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        dat2[dat2 == variables[i]] <- names(variables)[i]
      }
    }

  } else if (output == "wide") {

    dat <- dat[!duplicated(names(dat), fromLast = TRUE)]

    dat2 <- dat

    if (!is.null(product) && product == "charagegroups") {
      dat2 <- rename(dat2, value = POP)
    }

    # Handle recodes for breakdown labels if requested
    if (breakdown_labels) {
      if (is.null(breakdown)) {
        stop("A breakdown of population characteristics must be specified for this option to be used.", call. = FALSE)
      }

      if ("AGEGROUP" %in% names(dat2)) {
        dat2$AGEGROUP <- factor(dat2$AGEGROUP)
        dat2$AGEGROUP <- recode(dat2$AGEGROUP,
                                `0` = "All ages", `1` = "Age 0 to 4 years",
                                `2` = "Age 5 to 9 years",
                                `3` = "Age 10 to 14 years",
                                `4` = "Age 15 to 19 years", `5` = "Age 20 to 24 years",
                                `6` = "Age 25 to 29 years",
                                `7` = "Age 30 to 34 years", `8` = "Age 35 to 39 years",
                                `9` = "Age 40 to 44 years",
                                `10` = "Age 45 to 49 years", `11` = "Age 50 to 54 years",
                                `12` = "Age 55 to 59 years",
                                `13` = "Age 60 to 64 years", `14` = "Age 65 to 69 years",
                                `15` = "Age 70 to 74 years",
                                `16` = "Age 75 to 79 years", `17` = "Age 80 to 84 years",
                                `18` = "Age 85 years and older",
                                `19` = "Under 18 years", `20` = "5 to 13 years",
                                `21` = "14 to 17 years", `22` = "18 to 64 years",
                                `23` = "18 to 24 years", `24` = "25 to 44 years",
                                `25` = "45 to 64 years", `26` = "65 years and over",
                                `27` = "85 years and over", `28` = "16 years and over",
                                `29` = "18 years and over",
                                `30` = "15 to 44 years", `31` = "Median age")
      }

      if ("SEX" %in% names(dat2)) {
        dat2$SEX <- recode(dat2$SEX, `0` = "Both sexes", `1` = "Male", `2` = "Female")
      }

      if ("RACE" %in% names(dat2)) {
        dat2$RACE <- recode(dat2$RACE, `0` = "All races", `1` = "White alone",
                            `2` = "Black alone",
                            `3` = "American Indian and Alaska Native alone",
                            `4` = "Asian alone",
                            `5` = "Native Hawaiian and Other Pacific Islander alone",
                            `6` = "Two or more races",
                            `7` = "White alone or in combination",
                            `8` = "Black alone or in combination",
                            `9` = "American Indian and Alaska Native alone or in combination",
                            `10` = "Asian alone or in combination",
                            `11` = "Native Hawaiian and Other Pacific Islander alone or in combination")
      }

      if ("HISP" %in% names(dat2)) {
        dat2$HISP <- recode(dat2$HISP, `0` = "Both Hispanic Origins",
                            `1` = "Non-Hispanic",
                            `2` = "Hispanic")
      }

    }

    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        names(dat2) <- str_replace(names(dat2), variables[i], names(variables)[i])
      }
    }

    if ("GEONAME" %in% names(dat2)) {
      dat2 <- dat2 %>%
        select(GEOID, NAME = GEONAME, everything())
    } else {
      dat2 <- dat2 %>%
        select(GEOID, NAME, everything())
    }



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

      geom <- try(suppressMessages(use_tigris(geography = geography, year = year,
                                          state = state, county = county, ...)))

      if ("try-error" %in% class(geom)) {
        stop("Your geometry data download failed. Please try again later or check the status of the Census Bureau website at https://www2.census.gov/geo/tiger/", call. = FALSE)
      }
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
