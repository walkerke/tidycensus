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
#' @param year The year for which you are requesting data. Defaults to 2020; 2000,
#'             2010, and 2020 are available.
#' @param sumfile The Census summary file; if \code{NULL}, defaults to \code{"pl"} when the year is 2020 and \code{"sf1"} for 2000 and 2010.  Not all summary files are available for each decennial Census year.  Make sure you are using the correct summary file for your requested variables, as variable IDs may be repeated across summary files and represent different topics.
#' @param state The state for which you are requesting data. State
#'              names, postal codes, and FIPS codes are accepted.
#'              Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#'               FIPS codes are accepted. Must be combined with a value supplied
#'               to `state`.  Defaults to NULL.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'                 if TRUE, uses the tigris package to return an sf tibble
#'                 with simple feature geometry in the `geometry` column.
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
#' @param pop_group The population group code for which you'd like to request data.  Applies to summary files for which population group breakdowns are available like the Detailed DHC-A file.
#' @param pop_group_label If \code{TRUE}, return a \code{"pop_group_label"} column that contains the label for the population group.  Defaults to \code{FALSE}.
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
                          year = 2020,
                          sumfile = NULL,
                          state = NULL,
                          county = NULL,
                          geometry = FALSE,
                          output = "tidy",
                          keep_geo_vars = FALSE,
                          shift_geo = FALSE,
                          summary_var = NULL,
                          pop_group = NULL,
                          pop_group_label = FALSE,
                          key = NULL,
                          show_call = FALSE,
                          ...
                          ) {

  if (shift_geo) {
    warning("The `shift_geo` argument is deprecated and will be removed in a future release. We recommend using `tigris::shift_geometry()` instead.", call. = FALSE)
  }

  if (geography == "cbg") geography <- "block group"

  message(sprintf("Getting data from the %s decennial Census", year))

  # Use the default summary file for a given year
  if (is.null(sumfile)) {
    sumfile <- summary_files(year = year)[1]
  } else {

  }

  sumfile <- rlang::arg_match(sumfile, values = summary_files(year))

  # If the summary file is not supplied for 2020, switch to the PL file (until new data
  # are released in 2023)
  if (year == 2020 && sumfile == "sf1") {
    sumfile <- "pl"
  }

  if (sumfile == "ddhca" && is.null(pop_group)) {
    rlang::abort("You must specify a population group to use the DDHC-A file. Look up codes with `get_pop_groups()` or specify `pop_group = 'all' to get all available population groups for a given geography / variable combination.")
  }

  if (year == 2020 && sumfile == "pl" && geography == "public use microdata area") {
    stop("PUMAs are not available in the 2020 PL file.", call. = FALSE)
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

  if (year == 2020 && sumfile == "pl" && geography == "zip code tabulation area") {
    stop("ZCTAs are not available in the 2020 PL file.", call. = FALSE)
  }

  if (geography == "zip code tabulation area" && !is.null(state)) {
    geography <- "zip code tabulation area (or part)"
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
                               pop_group = pop_group,
                               pop_group_label = pop_group_label,
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
                               pop_group = pop_group,
                               pop_group_label = pop_group_label,
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

    if (!is.null(pop_group)) {
      join_vars <- c("GEOID", "NAME", "POPGROUP")
    } else {
      join_vars <- c("GEOID", "NAME")
    }

    dat <- map(l, function(x) {
      d <- try(load_data_decennial(geography, x, key, year, sumfile, pop_group, state, county, show_call = show_call),
               silent = silent)

      # If sf1 fails, try to get it from sf3
      if (inherits(d, "try-error") && year < 2010) {

        # stop("The 2000 decennial Census SF3 endpoint has been removed by the Census Bureau. We will support this data again when the endpoint is updated; in the meantime, we recommend using NHGIS (https://nhgis.org) and the ipumsr R package.", call. = FALSE)

        d <- try(suppressMessages(load_data_decennial(geography, x, key, year, sumfile = "sf3", pop_group, state, county, show_call = show_call)))
        message("Variables not found in Summary File 1. Trying Summary File 3...")
      } else {

        if (sumfile == "sf3") {
          message("Using Census Summary File 3")
        } else if (sumfile == "sf1") {
          message("Using Census Summary File 1")
        } else if (sumfile == "pl") {
          message("Using the PL 94-171 Redistricting Data Summary File")
        } else if (sumfile == "dhc") {
          message("Using the Demographic and Housing Characteristics File")
        } else if (sumfile == "dp") {
          message("Using the Demographic Profile")
        } else if (sumfile == "ddhca") {
          message("Using the Detailed DHC-A File")
        }

      }
      d
    }) %>%
      reduce(left_join, by = join_vars)
  } else {
    dat <- try(load_data_decennial(geography, variables, key, year, sumfile, pop_group, state, county, show_call = show_call),
               silent = silent)

    # If sf1 fails, try to get it from sf3
    # Keep this code in here, but throw an error for now (if sumfile is not provided)
    if (inherits(dat, "try-error") && year < 2010) {

      # stop("The 2000 decennial Census SF3 endpoint has been removed by the Census Bureau. We will support this data again when the endpoint is updated; in the meantime, we recommend using NHGIS (https://nhgis.org) and the ipumsr R package.", call. = FALSE)

      dat <- try(suppressMessages(load_data_decennial(geography, variables, key, year, sumfile = "sf3", pop_group, state, county, show_call = show_call)))
      message("Variables not found in Summary File 1. Trying Summary File 3...")
    } else {
      if (sumfile == "sf3") {
        message("Using Census Summary File 3")
      } else if (sumfile == "sf1") {
        message("Using Census Summary File 1")
      } else if (sumfile == "pl") {
        message("Using the PL 94-171 Redistricting Data Summary File")
      } else if (sumfile == "dhc") {
        message("Using the Demographic and Housing Characteristics File")
      } else if (sumfile == "dp") {
        message("Using the Demographic Profile")
      } else if (sumfile == "ddhca") {
        message("Using the Detailed DHC-A File")
      }

    }

  }

  if (inherits(dat, "try-error")) {
    rlang::abort(message = dat)
  }

  if (output == "tidy") {

    if (is.null(pop_group)) {
      sub <- dat[c("GEOID", "NAME", variables)]

      dat2 <- sub %>%
        gather(key = variable, value = value, -GEOID, -NAME)
    } else {
      sub <- dat[c("GEOID", "NAME", "POPGROUP", variables)]

      dat2 <- sub %>%
        gather(key = variable, value = value, -GEOID, -NAME, -POPGROUP) %>%
        dplyr::rename(pop_group = POPGROUP)
    }

    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        dat2[dat2 == variables[i]] <- names(variables)[i]
      }
    }

    # Convert missing values to NA
    dat2[dat2 == -111111111] <- NA
    dat2[dat2 == -222222222] <- NA
    dat2[dat2 == -333333333] <- NA
    dat2[dat2 == -444444444] <- NA
    dat2[dat2 == -555555555] <- NA
    dat2[dat2 == -666666666] <- NA
    dat2[dat2 == -777777777] <- NA
    dat2[dat2 == -888888888] <- NA
    dat2[dat2 == -999999999] <- NA

  } else if (output == "wide") {

    dat <- dat[!duplicated(names(dat), fromLast = TRUE)]

    dat2 <- dat

    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        names(dat2) <- str_replace(names(dat2), variables[i], names(variables)[i])
      }
    }

    # Convert missing values to NA
    dat2[dat2 == -111111111] <- NA
    dat2[dat2 == -222222222] <- NA
    dat2[dat2 == -333333333] <- NA
    dat2[dat2 == -444444444] <- NA
    dat2[dat2 == -555555555] <- NA
    dat2[dat2 == -666666666] <- NA
    dat2[dat2 == -777777777] <- NA
    dat2[dat2 == -888888888] <- NA
    dat2[dat2 == -999999999] <- NA

    if ("POPGROUP" %in% names(dat2)) {
      dat2 <- dat2 %>%
        select(GEOID, NAME, POPGROUP, everything()) %>%
        dplyr::rename(pop_group = POPGROUP)
    } else {
      dat2 <- dat2 %>%
        select(GEOID, NAME, everything())
    }

  }

  # If label is requested, join it here
  if (pop_group_label) {
    if (is.null(pop_group)) {
      rlang::abort("This argument is only available when specifying a population group, which is only available for selected datasets.")
    }

    labels = get_pop_groups(year = year, sumfile = sumfile)
    dat2 <- dat2 %>%
      dplyr::left_join(labels, by = "pop_group") %>%
      dplyr::select(GEOID, NAME, pop_group, pop_group_label,
                    dplyr::everything())
  }

  # For ZCTAs, strip the state code from GEOID (issue #338 and #358)
  # Should only happen if the GEOID is 7 characters
  if (geography == "zip code tabulation area (or part)" && year == 2020 && unique(nchar(dat2$GEOID)) == 7) {
    dat2 <- dat2 %>%
      dplyr::mutate(
        GEOID = stringr::str_sub(GEOID, start = 3L)
      )
  }

  if (!is.null(summary_var)) {

    sumdat <- suppressMessages(try(load_data_decennial(geography, summary_var, key, year,
                                                       sumfile, pop_group, state, county, show_call = show_call)))

    if (inherits(sumdat, "try-error")) {
      sumdat <- suppressMessages(try(load_data_decennial(geography, summary_var, key, year,
                                                         sumfile = "sf3", pop_group, state, county, show_call = show_call)))
    }

    if (geography == "zip code tabulation area (or part)" && year == 2020 && unique(nchar(dat2$GEOID)) == 7) {
      dat2 <- dat2 %>%
        dplyr::mutate(
          GEOID = stringr::str_sub(GEOID, start = 3L)
        )
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

      if (geography == "urban area" && year == 2020) {
        geom <- try(suppressMessages(use_tigris(geography = geography, year = year,
                                                state = state, county = county, criteria = "2020", ...)))
      } else if (sumfile == "cd118") {
        # stop("Geometry is not yet available for this sumfile in tidycensus.")

        geom <- try(suppressMessages(use_tigris(geography = geography, year = 2022,
                                        state = state, county = county, ...)))
      } else {
        geom <- try(suppressMessages(use_tigris(geography = geography, year = year,
                                                state = state, county = county, ...)))
      }

      if ("try-error" %in% class(geom)) {
        stop("Your geometry data download failed. Please try again later or check the status of the Census Bureau website at https://www2.census.gov/geo/tiger/", call. = FALSE)
      }
    }

    if (! keep_geo_vars) {

      geom <- select(geom, GEOID, geometry)

    }

    args <- list(...)

    if (shift_geo || "filter_by" %in% names(args)) {
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
    if (year == 2020) {

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
    if (year == 2020) {

      msg <- c(crayon::cyan(stringr::str_wrap("Note: 2020 decennial Census data use differential privacy, a technique that introduces errors into data to preserve respondent confidentiality.")),
               i = crayon::magenta("Small counts should be interpreted with caution."),
               i = crayon::magenta("See https://www.census.gov/library/fact-sheets/2021/protecting-the-confidentiality-of-the-2020-census-redistricting-data.html for additional guidance."))

      rlang::inform(msg, .frequency = "once",
                    .frequency_id = "msg2020")

    }

    return(dat2)

  }

}

