#' Obtain data and feature geometry for American Community Survey Migration
#' Flows
#'
#' @param geography The geography of your requested data. Possible values are
#'   `"county"`, `"county subdivision"`, and `"cbsa"`.
#' @param variables Character string or vector of character strings of variable
#'   names. By default, `get_flows()` returns the GEOID and names of the
#'   geographies as well as the number of people who moved in and out of each
#'   geography (`MOVEDIN` and `MOVEDOUT`). If additional variables are
#'   specified, they are pulled in addition to the default variables.
#' @param year The year, or endyear, of the ACS sample. The Migration Flows API
#'   is available for 5-year ACS samples from 2010 to 2018. Defaults to 2018.
#' @param output One of "tidy" (the default) in which each row represents an
#'   enumeration unit-variable combination, or "wide" in which each row
#'   represents an enumeration unit and the variables are in the columns.
#' @param state An optional vector of states for which you are requesting data.
#'   State names, postal codes, and FIPS codes are accepted. When requesting
#'   county subdivision data, you must specify at least one state. Defaults to
#'   NULL.
#' @param county The county for which you are requesting data. County names and
#'   FIPS codes are accepted. Must be combined with a value supplied to `state`.
#'   Defaults to NULL.
#' @param cbsa The core-based statistical area (metropolitan/micropolitan
#'   statistical area) for which you are requesting data. Specify a single value
#'   or a vector of values to get data for more than one CBSA. Numeric or
#'   character CBSA GEOIDs are accepted. When specifying CBSAs, geography must
#'   be set to `"cbsa"` and `state` and `county` must be `NULL`. Defaults to
#'   NULL.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'   if TRUE, uses the tigris package to return an sf tibble with simple feature
#'   geometry in the `geometry` column. **NOT YET IMPLEMENTED**
#' @param shift_geo if TRUE, returns geometry with Alaska and Hawaii shifted for
#'   thematic mapping of the entire US. Geometry was originally obtained from
#'   the albersusa R package. **NOT YET IMPLEMENTED**
#' @param key Your Census API key. Obtain one at
#'   \url{http://api.census.gov/data/key_signup.html}
#' @param moe_level The confidence level of the returned margin of error.  One
#'   of 90 (the default), 95, or 99.
#' @param show_call if TRUE, display call made to Census API. This can be very
#'   useful in debugging and determining if error messages returned are due to
#'   tidycensus or the Census API. Copy to the API call into a browser and see
#'   what is returned by the API directly. Defaults to FALSE.
#' @param ... Other keyword arguments
#'
#' @return A tibble or sf tibble of ACS Migration Flows data
#' @examples \dontrun{
#' get_flows(
#'   geography = "county",
#'   state = "VT",
#'   county = c("Washington", "Chittenden")
#'   )
#' }
#' @export
get_flows <- function(geography, variables = NULL, year = 2018, output = "tidy",
                      state = NULL, county = NULL, cbsa = NULL, geometry = FALSE,
                      shift_geo = FALSE, key = NULL, moe_level = 90,
                      show_call = FALSE, ...) {

  if (Sys.getenv('CENSUS_API_KEY') != '') {
    key <- Sys.getenv('CENSUS_API_KEY')
  } else if (is.null(key)) {
    stop('A Census API key is required. Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')
  }

  if (geography == "cbsa") {
    geography <- "metropolitan statistical area/micropolitan statistical area"
  }

  if (geography == "mcd") {
    geography <- "county subdivision"
  }

  if (!geography %in% c("county", "county subdivision", "metropolitan statistical area/micropolitan statistical area")) {
    stop('ACS Migration Flows API provides data at "county", "county subdivision", and "cbsa" levels only', call. = FALSE)
  }

  if (geography == "county" && !is.null(county)) {
    if (is.null(state)) {
      stop("Must specify one state when requesting data from specified counties", call. = FALSE)
    }
    if (length(state) > 1) {
      stop("Cannot specify more than one state when requesting county data", call. = FALSE)
    }
  }

  if (geography == "county subdivision" && is.null(state)) {
    stop("Must specify one or more states when requesting county subdivision data", call. = FALSE)
  }

  if (geography == "metropolitan statistical area/micropolitan statistical area" && !(is.null(state) && is.null(county))) {
    stop("When requesting CBSA data, state and county must be NULL.", call. = FALSE)
  }

  if (year < 2010) {
    stop("Migration flows are available via API beginning in 2010", call. = FALSE)
  }

  if (moe_level == 90) {
    moe_factor <- 1
  } else if (moe_level == 95) {
    moe_factor <- (1.96 / 1.645)
  } else if (moe_level == 99) {
    moe_factor <- (2.56 / 1.645)
  } else {
    stop("`moe_level` must be one of 90, 95, or 99.", call. = FALSE)
  }

  cache <- getOption("tigris_use_cache", FALSE)

  # if (geometry) {
  #   if (shift_geo) {
  #     message("Using feature geometry obtained from the albersusa package")
  #   } else if (!shift_geo && !cache) {
  #     message("Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.")
  #   }
  # }
  #
  # if (shift_geo && !geometry) {
  #   stop("`shift_geo` is only available when requesting feature geometry with `geometry = TRUE`",
  #        call. = FALSE)
  # }

  # for every call we want these variables
 always_vars <- c("GEOID1", "GEOID2", "FULL1_NAME", "FULL2_NAME",
                  "MOVEDIN", "MOVEDIN_M", "MOVEDOUT", "MOVEDOUT_M")

  # if additional variables are requested, combine with always vars
  # and remove vars duplicated in variables specified
  variables <- c(always_vars, variables[!variables %in% always_vars])

  dat <- load_data_flows(
    geography = geography,
    variables = variables,
    key = key,
    year = year,
    state = state,
    county = county,
    cbsa = cbsa,
    show_call = show_call
    )

  if (output == "tidy") {
    dat <- dat %>%
      tidyr::pivot_longer(cols = where(is.numeric), names_to = "variable") %>%
      tidyr::separate(.data$variable, into = c("variable", "type"), sep = "_", fill = "right") %>%
      mutate(type = ifelse(is.na(.data$type), "estimate", "moe")) %>%
      tidyr::pivot_wider(names_from = .data$type, values_from = .data$value) %>%
      mutate(moe = .data$moe * moe_factor)
  } else if (output == "wide") {
    dat <- dat %>%
      mutate(dplyr::across(dplyr::ends_with("_M"), ~ .x * moe_factor))
  }

return(dat)
#
#   if (geometry) {
#
#     if (shift_geo) {
#
#       if (!is.null(state)) {
#         stop("`shift_geo` is only available when requesting geometry for the entire US", call. = FALSE)
#       }
#
#       message("Please note: Alaska and Hawaii are being shifted and are not to scale.")
#
#       if (geography == "state") {
#
#         geom <- tidycensus::state_laea
#
#       } else if (geography == "county") {
#
#         geom <- tidycensus::county_laea
#
#         if (year > 2014) {
#           # Account for change from Shannon County, SD to Oglala Lakota County
#           # and the new Kusilvak Census Area in AK
#           geom$GEOID[geom$GEOID == "46113"] <- "46102"
#           geom$GEOID[geom$GEOID == "02270"] <- "02158"
#         }
#
#       } else {
#         stop("`shift_geo` is only available for states and counties", call. = FALSE)
#       }
#
#     } else {
#
#       geom <- suppressMessages(use_tigris(geography = geography, year = year,
#                                           state = state, county = county, ...))
#     }
#
#     if (! keep_geo_vars) {
#
#       geom <- select(geom, GEOID, geometry)
#
#     }
#
#     if (shift_geo) {
#       out <- inner_join(geom, dat2, by = "GEOID") %>%
#         st_as_sf()
#     } else {
#       out <- right_join(geom, dat2, by = "GEOID") %>%
#         st_as_sf()
#     }
#
#     return(out)
#
#   } else {
#
#     return(dat2)
#
#   }

}
