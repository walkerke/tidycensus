#' Obtain data and feature geometry for the decennial Census
#'
#' @param geography The geography of your data.
#' @param variables Character string or vector of character strings of variable
#'                  IDs.
#' @param table   The Census table for which you would like to request all variables.  Uses
#'                lookup tables to identify the variables; performs faster when variable
#'                table already exists through \code{load_variables(cache = TRUE)}.
#' @param cache_table Whether or not to cache table names for faster future access.
#'                    Defaults to FALSE; if TRUE, only needs to be called once per
#'                    dataset.  If variables dataset is already cached via the
#'                    \code{load_variables} function, this can be bypassed.
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
#' @param shift_geo if TRUE, returns geometry with Alaska and Hawaii shifted for thematic mapping of the entire US.
#'                  Geometry was originally obtained from the albersusa R package.
#' @param summary_var Character string of a "summary variable" from the decennial Census
#'                    to be included in your output. Usually a variable (e.g. total population)
#'                    that you'll want to use as a denominator or comparison.
#' @param key Your Census API key.
#'            Obtain one at \url{http://api.census.gov/data/key_signup.html}
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
get_decennial <- function(geography, variables = NULL, table = NULL, cache_table = FALSE, year = 2010,
                          sumfile = "sf1", state = NULL, county = NULL, geometry = FALSE, output = "tidy",
                          keep_geo_vars = FALSE, shift_geo = FALSE, summary_var = NULL, key = NULL, ...) {

  message(sprintf("Getting data from the %s decennial Census", year))

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }

  if (is.null(variables) && is.null(table)) {
    stop("Either a vector of variables or an table must be specified.", call. = FALSE)
  }

  if (!is.null(variables) && !is.null(table)) {
    stop("Specify variables or a table to retrieve; they cannot be combined.",
         call. = FALSE)
  }

  if (geography == "block" && year != 2010) {
    stop("At the moment, block data is only available for 2010. I recommend using NHGIS (http://www.nhgis.org) and the ipumsr package for block data for other years.", call. = FALSE)
  }

  # if (geography %in% c("tract", "block group") && year == 1990 && is.null(county)) {
  #   stop("At the moment, tracts and block groups for 1990 require specifying a county.",
  #        call. = FALSE)
  # }

  if (geography == "zcta") geography <- "zip code tabulation area"

  if (geography == "zip code tabulation area" && is.null(state)) {
    stop("ZCTA data for the decennial Census is only available by state from tidycensus.",
         call. = FALSE)
  }

  if (geography == "zip code tabulation area" && geometry) {
    stop("Linked ZCTA geometry and attributes for `get_decennial` are not currently available in tidycensus.",
         call. = FALSE)
  }

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
  if (geography == "block group" && is.null(county)) {
    st <- suppressMessages(validate_state(state))

    # Get year-specific county IDs from tigris

    cty_year <- suppressMessages(counties(state = st, cb = TRUE,
                                          year = year, class = "sf"))

    county <- cty_year$COUNTYFP


  }

  # If more than one state specified for tracts - or more than one county
  # for block groups - take care of this under the hood by having the function
  # call itself and return the result
  if (geography == "tract" && length(state) > 1) {
    # mc <- match.call(expand.dots = TRUE)
    if (geometry) {
      result <- map(state, ~{
        suppressMessages(get_decennial(geography = geography,
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
                                       key = key))
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
        suppressMessages(get_decennial(geography = geography,
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
                                       key = key))
      })
    }
    return(result)
  }

  if ((geography %in% c("block group", "block") && length(county) > 1) || (geography == "tract" && length(county) > 1)) {
    # mc <- match.call(expand.dots = TRUE)
    if (geometry) {
      result <- map(county, ~{
        suppressMessages(get_decennial(geography = geography,
                                       variables = variables,
                                       table = table,
                                       cache_table = cache_table,
                                       year = year,
                                       sumfile = sumfile,
                                       output = output,
                                       state = state,
                                       county = .x,
                                       geometry = geometry,
                                       keep_geo_vars = keep_geo_vars,
                                       shift_geo = FALSE,
                                       summary_var = summary_var,
                                       key = key))
      }) %>%
        reduce(rbind)
      geoms <- unique(st_geometry_type(result))
      if (length(geoms) > 1) {
        st_cast(result, "MULTIPOLYGON")
      }
      result <- result %>%
        as_tibble() %>%
        st_as_sf()
    } else {
      result <- map_df(county, ~{
        suppressMessages(get_decennial(geography = geography,
                                       variables = variables,
                                       table = table,
                                       cache_table = cache_table,
                                       year = year,
                                       sumfile = sumfile,
                                       output = output,
                                       state = state,
                                       county = .x,
                                       geometry = geometry,
                                       keep_geo_vars = keep_geo_vars,
                                       shift_geo = FALSE,
                                       summary_var = summary_var,
                                       key = key))
      })
    }
    return(result)
  }

  # Get data for an entire table if needed
  if (!is.null(table)) {
    variables <- variables_from_table_decennial(table, year, sumfile, cache_table)
  }


  if (length(variables) > 48) {
    l <- split(variables, ceiling(seq_along(variables) / 48))

    dat <- map(l, function(x) {
      d <- try(load_data_decennial(geography, x, key, year, sumfile, state, county),
                 silent = TRUE)
      # If sf1 fails, try to get it from sf3
      if (inherits(d, "try-error")) {
        d <- try(suppressMessages(load_data_decennial(geography, x, key, year, sumfile = "sf3", state, county)))
      }
      d
    }) %>%
      bind_cols()
  } else {
    dat <- try(load_data_decennial(geography, variables, key, year, sumfile, state, county),
               silent = TRUE)

    # If sf1 fails, try to get it from sf3
    if (inherits(dat, "try-error")) {
      dat <- try(suppressMessages(load_data_decennial(geography, variables, key, year, sumfile = "sf3", state, county)))
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
                                                   sumfile, state, county)))

    if (inherits(sumdat, "try-error")) {
      sumdat <- suppressMessages(try(load_data_decennial(geography, summary_var, key, year,
                                        sumfile = "sf3", state, county)))
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