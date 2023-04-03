#' Obtain data and feature geometry for the decennial US Census
#'
#' @param geography The geography of your data.
#' @param variables Character string or vector of character strings of variable
#'                  IDs.
#' @param table   The Census table for which you would like to request all variables. Uses
#'                lookup tables to identify the variables; performs faster when variable
#'                table already exists through \code{load_variables(cache = TRUE)}.
#'                Only one table may be requested per call.
#' @param cache_table Whether or not to cache table names for faster future access.
#'                    Defaults to FALSE; if TRUE, only needs to be called once per
#'                    dataset.  If variables dataset is already cached via the
#'                    \code{load_variables} function, this can be bypassed.
#' @param year The year for which you are requesting data. Defaults to 2010; 2000,
#'             2010, and 2020 are available.
#' @param sumfile The Census summary file; defaults to \code{"sf1"} but will switch to \code{"pl"} if the year supplied is 2020.  Not all summary files are available for each decennial Census year.
#' @param state The state for which you are requesting data. State
#'              names, postal codes, and FIPS codes are accepted.
#'              Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#'               FIPS codes are accepted. Must be combined with a value supplied
#'               to `state`.  Defaults to NULL.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'                 if TRUE, uses the tigris package to return an sf tibble
#'                 with simple feature geometry in the `geometry` column.  state, county, tract, and block group are
#'                 supported for 2000 through 2020; block and ZCTA geometry are supported for 2000 and 2010.
#' @param output One of "tidy" (the default) in which each row represents an
#'               enumeration unit-variable combination, or "wide" in which each
#'               row represents an enumeration unit and the variables are in the
#'               columns.
#' @param keep_geo_vars if TRUE, keeps all the variables from the Census
#'                      shapefile obtained by tigris.  Defaults to FALSE.
#' @param shift_geo (deprecated) if TRUE, returns geometry with Alaska and Hawaii
#'                  shifted for thematic mapping of the entire US.
#'                  Geometry was originally obtained from the albersusa R package.  As of May 2021,
#'                  we recommend using \code{tigris::shift_geometry()} instead.
#' @param summary_var Character string of a "summary variable" from the decennial Census
#'                    to be included in your output. Usually a variable (e.g. total population)
#'                    that you'll want to use as a denominator or comparison.
#' @param key Your Census API key.
#'            Obtain one at \url{https://api.census.gov/data/key_signup.html}
#' @param show_call if TRUE, display call made to Census API. This can be very useful
#'                  in debugging and determining if error messages returned are
#'                  due to tidycensus or the Census API. Copy to the API call into
#'                  a browser and see what is returned by the API directly. Defaults to FALSE.
#' @param ... Other keyword arguments
#'
#' @return a tibble or sf tibble of decennial Census data
#' @examples \dontrun{
#' # Plot of race/ethnicity by county in Illinois for 2010
#' library(tidycensus)
#' library(tidyverse)
#' library(viridis)
#' census_api_key("YOUR KEY GOES HERE")
#' vars10 <- c("P005003", "P005004", "P005006", "P004003")
#'
#' il <- get_decennial(geography = "county", variables = vars10, year = 2010,
#'                     summary_var = "P001001", state = "IL", geometry = TRUE) %>%
#'   mutate(pct = 100 * (value / summary_value))
#'
#' ggplot(il, aes(fill = pct, color = pct)) +
#'   geom_sf() +
#'   facet_wrap(~variable)
#'
#'
#' }
#' @export
get_decennial <- function(geography,
                          variables = NULL,
                          table = NULL,
                          cache_table = FALSE,
                          year = 2010,
                          sumfile = c("sf1", "sf2", "sf3", "sf4",
                                      "sf2profile", "sf3profile",
                                      "sf4profile",
                                      "pl", "plnat", "aian",
                                      "aianprofile",
                                      "as", "gu", "vi", "mp",
                                      "responserate", "pes",
                                      "dpvi", "dpgu", "dpas",
                                      "dpmp", "sldh"),
                          state = NULL,
                          county = NULL,
                          geometry = FALSE,
                          output = "tidy",
                          keep_geo_vars = FALSE,
                          shift_geo = FALSE,
                          summary_var = NULL,
                          key = NULL,
                          show_call = FALSE,
                          ...
                          ) {

  if (shift_geo) {
    warning("The `shift_geo` argument is deprecated and will be removed in a future release. We recommend using `tigris::shift_geometry()` instead.", call. = FALSE)
  }

  if (geography == "cbg") geography <- "block group"

  message(sprintf("Getting data from the %s decennial Census", year))

  sumfile <- rlang::arg_match(sumfile)

  # If the summary file is not supplied for 2020, switch to the PL file (until new data
  # are released in 2023)
  if (year == 2020 && sumfile == "sf1") {
    sumfile <- "pl"
  }

  if (year == 2020 && sumfile == "pl" && geography == "public use microdata area") {
    stop("PUMAs are not defined yet for the 2020 decennial Census.", call. = FALSE)
  }

  if (geography == "voting district" && year != 2020) {
    stop("`year` must be 2020 for voting districts.", call. = FALSE)
  }

  # Check for a Census API key and warn if missing
  key <- get_census_api_key(key)

  if (year == 1990) {
    stop("The 1990 decennial Census endpoint has been removed by the Census Bureau. We will support 1990 data again when the endpoint is updated; in the meantime, we recommend using NHGIS (https://nhgis.org) and the ipumsr R package.", call. = FALSE)
  }

  if (is.null(variables) && is.null(table)) {
    stop("Either a vector of variables or an table must be specified.", call. = FALSE)
  }

  if (!is.null(variables) && !is.null(table)) {
    stop("Specify variables or a table to retrieve; they cannot be combined.",
         call. = FALSE)
  }

  if (length(table) > 1) {
    stop("Only one table may be requested per call.", call. = FALSE)
  }

  if (sumfile == "sf3" && year > 2001) {
    stop("Summary File 3 was not released in 2010. Use Summary File 1 or tables from the American Community Survey via get_acs() instead.", call. = FALSE)
  }

  if (geography == "block" && year == 1990) {
    stop("At the moment, block data is not available for 1990. I recommend using NHGIS (http://www.nhgis.org) and the ipumsr package for block data for other years.", call. = FALSE)
  }

  # if (geography %in% c("tract", "block group") && year == 1990 && is.null(county)) {
  #   stop("At the moment, tracts and block groups for 1990 require specifying a county.",
  #        call. = FALSE)
  # }

  if (geography == "cbsa") geography <- "metropolitan statistical area/micropolitan statistical area"

  if (geography == "zcta") geography <- "zip code tabulation area"

  if (geography == "zip code tabulation area" && !is.null(state)) {
    geography <- "zip code tabulation area (or part)"
  }

  if (year == 2020 && sumfile == "pl" && geography == "zip code tabulation area") {
    stop("ZCTAs are not currently available for the 2020 decennial Census.", call. = FALSE)
  }

  # if (geography == "zip code tabulation area" && is.null(state)) {
  #   stop("ZCTA data for the decennial Census is only available by state from tidycensus.",
  #        call. = FALSE)
  # }
  #
  # if (geography == "zip code tabulation area" && geometry) {
  #   stop("Linked ZCTA geometry and attributes for `get_decennial` are not currently available in tidycensus.",
  #        call. = FALSE)
  # }

  if (shift_geo && !geometry) {
    stop("`shift_geo` is only available when requesting feature geometry with `geometry = TRUE`",
         call. = FALSE)
  }

  cache <- getOption("tigris_use_cache", FALSE)

  if (geometry) {

    if (shift_geo) {

      if (year != 2010) {
        stop("`shift_geo` is currently only available for 2010 data in `get_decennial()` due to county boundary changes.",
             call. = FALSE)
      }

      message("Using feature geometry obtained from the albersusa package")
    } else if (!shift_geo && !cache) {
      message("Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.")
    }

  }



  # Allow users to get all block groups in a state
  # Keep this here when county wildcards for block groups are fixed
  if (geography == "block group" && is.null(county)) {
    st <- suppressMessages(validate_state(state))

    # Get year-specific county IDs from tigris

    cty_year <- suppressMessages(counties(state = st, cb = TRUE,
                                          year = year, class = "sf"))

    county <- cty_year$COUNTYFP


  }

  insist_get_decennial <- purrr::insistently(get_decennial)

  # If more than one state specified for tracts, block groups, or blocks
  # take care of this under the hood by having the function
  # call itself and return the result
  if ((geography == "tract" || geography == "block group" || geography == "block") && length(state) > 1) {
    # mc <- match.call(expand.dots = TRUE)
    if (geometry) {
      result <- map(state, function(s, ...) {
        suppressMessages(
          insist_get_decennial(geography = geography,
                               variables = variables,
                               table = table,
                               cache_table = cache_table,
                               year = year,
                               sumfile = sumfile,
                               output = output,
                               state = s,
                               county = county,
                               geometry = geometry,
                               keep_geo_vars = keep_geo_vars,
                               shift_geo = FALSE,
                               summary_var = summary_var,
                               key = key,
                               show_call = show_call,
                               ...)
        )
      }, ...) %>%
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
          insist_get_decennial(geography = geography,
                               variables = variables,
                               table = table,
                               cache_table = cache_table,
                               year = year,
                               sumfile = sumfile,
                               output = output,
                               state = .x,
                               county = county,
                               geometry = geometry,
                               keep_geo_vars = keep_geo_vars,
                               shift_geo = FALSE,
                               summary_var = summary_var,
                               key = key,
                               show_call = show_call))
      })
    }
    return(result)
  }

  # Get data for an entire table if needed
  if (!is.null(table)) {
    variables <- variables_from_table_decennial(table, year, sumfile, cache_table)
  }

  silent <- ifelse(year == 2010, FALSE, TRUE)

  if (length(variables) > 48) {
    l <- split(variables, ceiling(seq_along(variables) / 48))

    dat <- map(l, function(x) {
      d <- try(load_data_decennial(geography, x, key, year, sumfile, state, county, show_call = show_call),
               silent = silent)
      # If sf1 fails, try to get it from sf3
      if (inherits(d, "try-error") && year < 2010) {

        # stop("The 2000 decennial Census SF3 endpoint has been removed by the Census Bureau. We will support this data again when the endpoint is updated; in the meantime, we recommend using NHGIS (https://nhgis.org) and the ipumsr R package.", call. = FALSE)

        d <- try(suppressMessages(load_data_decennial(geography, x, key, year, sumfile = "sf3", state, county, show_call = show_call)))
        message("Variables not found in Summary File 1. Trying Summary File 3...")
      } else {
        if (sumfile == "sf3") {
          message("Using Census Summary File 3")
        } else if (sumfile == "sf1") {
          message("Using Census Summary File 1")
        } else if (sumfile == "pl") {
          message("Using the PL 94-171 Redistricting Data summary file")
        }
      }
      d
    }) %>%
      reduce(left_join, by = c("GEOID", "NAME"))
  } else {
    dat <- try(load_data_decennial(geography, variables, key, year, sumfile, state, county, show_call = show_call),
               silent = silent)

    # If sf1 fails, try to get it from sf3
    # Keep this code in here, but throw an error for now (if sumfile is not provided)
    if (inherits(dat, "try-error") && year < 2010) {

      # stop("The 2000 decennial Census SF3 endpoint has been removed by the Census Bureau. We will support this data again when the endpoint is updated; in the meantime, we recommend using NHGIS (https://nhgis.org) and the ipumsr R package.", call. = FALSE)

      dat <- try(suppressMessages(load_data_decennial(geography, variables, key, year, sumfile = "sf3", state, county, show_call = show_call)))
      message("Variables not found in Summary File 1. Trying Summary File 3...")
    } else {
      if (sumfile == "sf3") {
        message("Using Census Summary File 3")
      } else if (sumfile == "sf1") {
        message("Using Census Summary File 1")
      } else if (sumfile == "pl") {
        message("Using the PL 94-171 Redistricting Data summary file")
      }
    }

  }

  if (output == "tidy") {

    sub <- dat[c("GEOID", "NAME", variables)]

    dat2 <- sub %>%
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
      select(GEOID, NAME, everything())

  }

  if (!is.null(summary_var)) {

    sumdat <- suppressMessages(try(load_data_decennial(geography, summary_var, key, year,
                                                       sumfile, state, county, show_call = show_call)))

    if (inherits(sumdat, "try-error")) {
      sumdat <- suppressMessages(try(load_data_decennial(geography, summary_var, key, year,
                                                         sumfile = "sf3", state, county, show_call = show_call)))
    }

    dat2 <- dat2 %>%
      inner_join(sumdat, by = "GEOID") %>%
      rename("summary_value" = !! summary_var,
             NAME = "NAME.x") %>%
      select(-NAME.y)

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

    # Give users a heads-up about differential privacy in the 2020 decennial data
    # This should print as the final message before data are returned
    # For right now, this pertains to the PL file; adjust when new data come out in 2023
    if (year == 2020 && sumfile == "pl") {

      msg <- c(crayon::cyan(stringr::str_wrap("Note: 2020 decennial Census data use differential privacy, a technique that introduces errors into data to preserve respondent confidentiality.")),
               i = crayon::magenta("Small counts should be interpreted with caution."),
               i = crayon::magenta("See https://www.census.gov/library/fact-sheets/2021/protecting-the-confidentiality-of-the-2020-census-redistricting-data.html for additional guidance."))

      rlang::inform(msg, .frequency = "once",
                    .frequency_id = "msg2020")

    }

    return(out)

  } else {

    # Give users a heads-up about differential privacy in the 2020 decennial data
    # This should print as the final message before data are returned
    # For right now, this pertains to the PL file; adjust when new data come out in 2023
    if (year == 2020 && sumfile == "pl") {

      msg <- c(crayon::cyan(stringr::str_wrap("Note: 2020 decennial Census data use differential privacy, a technique that introduces errors into data to preserve respondent confidentiality.")),
               i = crayon::magenta("Small counts should be interpreted with caution."),
               i = crayon::magenta("See https://www.census.gov/library/fact-sheets/2021/protecting-the-confidentiality-of-the-2020-census-redistricting-data.html for additional guidance."))

      rlang::inform(msg, .frequency = "once",
                    .frequency_id = "msg2020")

    }

    return(dat2)

  }

}