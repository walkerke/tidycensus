#' Obtain data and feature geometry for American Community Survey Migration
#' Flows
#'
#' @param geography The geography of your requested data. Possible values are
#'   \code{"county"}, \code{"county subdivision"}, and \code{"metropolitan statistical area"}.
#'   MSA data is only available beginning with the 2009-2013 5-year ACS.
#' @param variables Character string or vector of character strings of variable
#'   names. By default, \code{get_flows()} returns the GEOID and names of the
#'   geographies as well as the number of people who moved in, out, and net
#'   movers of each geography (\code{"MOVEDIN"}, \code{"MOVEDOUT"}, \code{"MOVEDNET"}). If additional
#'   variables are specified, they are pulled in addition to the default
#'   variables. The names of additional variables can be found in the Census
#'   Migration Flows API
#'   documentation at \url{https://api.census.gov/data/2018/acs/flows/variables.html}.
#' @param breakdown A character vector of the population breakdown
#'   characteristics to be crossed with migration flows data. For datasets
#'   between 2006-2010 and 2011-2015, selected demographic characteristics such
#'   as age, race, employment status, etc. are available. Possible values are
#'   "AGE", "SEX", "RACE", "HSGP", "REL", "HHT", "TEN", "ENG",
#'   "POB", "YEARS", "ESR", "OCC", "WKS", "SCHL", "AHINC",
#'   "APINC", and "HISP_ORIGIN". For more information and to see which
#'   characteristics are available in each year, visit the Census Migration
#'   Flows
#'   documentation at \url{https://www.census.gov/data/developers/data-sets/acs-migration-flows.html}.
#'    Note: not all characteristics are available in all years.
#' @param breakdown_labels Whether or not to add columns with labels for the
#'   breakdown characteristic codes. Defaults to \code{FALSE}.
#' @param year The year, or endyear, of the ACS sample. The Migration Flows API
#'   is available for 5-year ACS samples from 2010 to 2018. Defaults to 2018.
#' @param output One of "tidy" (the default) in which each row represents an
#'   enumeration unit-variable combination, or "wide" in which each row
#'   represents an enumeration unit and the variables are in the columns.
#' @param state An optional vector of states for which you are requesting data.
#'   State names, postal codes, and FIPS codes are accepted. When requesting
#'   county subdivision data, you must specify at least one state.
#' @param county The county for which you are requesting data. County names and
#'   FIPS codes are accepted. Must be combined with a value supplied to `state`.
#' @param msa The metropolitan statistical area for which you are requesting
#'   data. Specify a single value or a vector of values to get data for more
#'   than one MSA. Numeric or character MSA GEOIDs are accepted. When specifying
#'   MSAs, geography must be set to \code{"metropolitan statistical area"} and
#'   \code{state} and \code{county} must be \code{NULL}.
#' @param geometry if FALSE (the default), return a tibble of ACS Migration
#'   Flows data. If TRUE, return an sf object with the centroids of both origin
#'   and destination as \code{sfc_POINT} columns. The origin point feature is
#'   returned in a column named \code{centroid1} and is the active geometry column in
#'   the sf object. The destination point feature is returned in the \code{centroid2}
#'   column.
#' @param key Your Census API key. Obtain one at
#'   \url{https://api.census.gov/data/key_signup.html}
#' @param moe_level The confidence level of the returned margin of error.  One
#'   of 90 (the default), 95, or 99.
#' @param show_call if TRUE, display call made to Census API. This can be very
#'   useful in debugging and determining if error messages returned are due to
#'   tidycensus or the Census API. Copy to the API call into a browser and see
#'   what is returned by the API directly. Defaults to FALSE.
#'
#' @return A tibble or sf tibble of ACS Migration Flows data
#' @examples \dontrun{
#' get_flows(
#'   geography = "county",
#'   state = "VT",
#'   county = c("Washington", "Chittenden")
#'   )
#'
#' get_flows(
#'   geography = "county subdivision",
#'   breakdown = "RACE",
#'   breakdown_labels = TRUE,
#'   state = "NY",
#'   county = "Westchester",
#'   output = "wide",
#'   year = 2015
#'   )
#'
#' get_flows(
#'    geography = "metropolitan statistical area",
#'    variables = c("POP1YR", "POP1YRAGO"),
#'    geometry = TRUE,
#'    output = "wide",
#'    show_call = TRUE
#'   )
#' }
#' @export
get_flows <- function(geography, variables = NULL, breakdown = NULL,
                      breakdown_labels = FALSE, year = 2018, output = "tidy",
                      state = NULL, county = NULL, msa = NULL, geometry = FALSE,
                      key = NULL, moe_level = 90, show_call = FALSE) {

  # a bunch of checks to make sure the get_flows() call is specified correctly
  if (Sys.getenv('CENSUS_API_KEY') != '') {
    key <- Sys.getenv('CENSUS_API_KEY')
  } else if (is.null(key)) {
    stop('A Census API key is required. Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key()` function to use it throughout your tidycensus session.')
  }

  if (geography %in% c("cbsa", "msa", "metropolitan statistical area")) {
    geography <- "metropolitan statistical area/micropolitan statistical area"
    if (year <= 2012) {
      stop("Data at the MSA-level is only avaialable beginning with the 2009-2013 5-year ACS", .call = FALSE)
    }
  }

  if (geography == "mcd") {
    geography <- "county subdivision"
  }

  if (!geography %in% c("county", "county subdivision", "metropolitan statistical area/micropolitan statistical area")) {
    stop('ACS Migration Flows API provides data at "county", "county subdivision", and "metropolitan statistical area" levels only', call. = FALSE)
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
    stop("When requesting MSA data, `state` and `county` must be NULL", call. = FALSE)
  }

  if (!is.null(breakdown) && year > 2015) {
    stop("Breakdown characteristics are only available for surveys before 2016", call. = FALSE)
  }

  if (year < 2010) {
    stop("Migration flows are available via API beginning in 2010", call. = FALSE)
  }

  # if different moe level is specified, calculate factor to adjust by
  if (moe_level == 90) {
    moe_factor <- 1
  } else if (moe_level == 95) {
    moe_factor <- (1.96 / 1.645)
  } else if (moe_level == 99) {
    moe_factor <- (2.56 / 1.645)
  } else {
    stop("`moe_level` must be one of 90, 95, or 99.", call. = FALSE)
  }

  # for every call we want these variables
  always_vars <- c("GEOID1", "GEOID2", "FULL1_NAME", "FULL2_NAME",
                   "MOVEDIN", "MOVEDIN_M", "MOVEDOUT", "MOVEDOUT_M",
                   "MOVEDNET", "MOVEDNET_M")

  # these are all the possible characteristic variables available
  # need to know them to pivot returned data to tidy format later
  all_breakdown_vars <- tidycensus::mig_recodes %>%
    dplyr::distinct(.data$characteristic) %>%
    dplyr::pull()

  # if additional variables are requested, combine with always vars, breakdown_vars
  # and remove vars duplicated in variables specified
  variables <- c(
    always_vars,
    breakdown,
    variables[!variables %in% c(always_vars, all_breakdown_vars)]
    )

  # pull data from api
  dat <- load_data_flows(
    geography = geography,
    variables = variables,
    key = key,
    year = year,
    state = state,
    county = county,
    msa = msa,
    show_call = show_call
    )

  if (!is.null(breakdown)) {

    # breakdown vars returned need to be padded with 0
    dat <- dat %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(all_breakdown_vars),
                                    ~ stringr::str_pad(.x, 2, side = "left", pad = "0")))

    if (breakdown_labels) {

      # vector of characteristics that are possible to recode
      vars_to_recode <- tidycensus::mig_recodes %>%
        dplyr::distinct(.data$characteristic) %>%
        dplyr::pull()

      # add id so we can join after adding label cols
      to_recode <- dat %>%
        dplyr::mutate(id = dplyr::row_number())

      # pivot to long format and join variable codes to lookup table with labels
      recoded_long <- suppressWarnings(
        to_recode %>%
          dplyr::select(.data$id, dplyr::any_of(vars_to_recode)) %>%
          tidyr::pivot_longer(
            cols = -c(.data$id),
            names_to = "characteristic",
            values_to = "code"
          ) %>%
          dplyr::left_join(tidycensus::mig_recodes, by = c("characteristic", "code")) %>%
          dplyr::select(-.data$ordered, -.data$code)
        )

      # pivot back to wide format with recoded label cols
      recoded_wide <- recoded_long %>%
        tidyr::pivot_wider(
          names_from = .data$characteristic,
          values_from = .data$desc,
          names_glue = "{characteristic}_label",
        )

      # characteristic vars that should be ordered factors
      breakdown_ordered <- tidycensus::mig_recodes %>%
        dplyr::filter(ordered) %>%
        dplyr::distinct(.data$characteristic) %>%
        dplyr::pull() %>%
        paste0("_label")

      # get col names to be turned into factors
      factor_v <- names(recoded_wide)[names(recoded_wide) %in% breakdown_ordered]

      # create ordered factors of each label column should be ordered
      if (length(factor_v) > 0) {

        for (var in factor_v) {

          # get correct order from lookup table
          order_v <- tidycensus::mig_recodes %>%
            dplyr::filter(.data$characteristic == stringr::str_remove(var, "_label")) %>%
            dplyr::pull(.data$desc)

          # replace label col with ordered factor label
          recoded_wide[[var]] <- factor(recoded_wide[[var]], ordered = TRUE, levels = order_v)
          }
        }

      # join back to original data and drop id col
      dat <- to_recode %>%
        dplyr::left_join(recoded_wide, by = "id") %>%
        dplyr::select(-id)

    # # we need to know all the possible vars NOT to pivot if ouput = tidy in next steop
    # all_breakdown_vars <- c(all_breakdown_vars, breakdown_ordered)
      }
    }

  # do some reshaping if requested and adjust moe if needed
  if (output == "tidy") {
    dat <- dat %>%
      tidyr::pivot_longer(cols = where(is.numeric), names_to = "variable") %>%
      tidyr::separate(.data$variable, into = c("variable", "type"), sep = "_", fill = "right") %>%
      dplyr::mutate(type = ifelse(is.na(.data$type), "estimate", "moe")) %>%
      tidyr::pivot_wider(names_from = .data$type, values_from = .data$value) %>%
      dplyr::mutate(moe = .data$moe * moe_factor)
  } else if (output == "wide") {
    dat <- dat %>%
      dplyr::mutate(dplyr::across(dplyr::ends_with("_M"), ~ .x * moe_factor))
  }

  # move all numeric vars to the end
  dat <- dat %>%
    dplyr::select(where(~ is.character(.x) || is.factor(.x)), dplyr::everything())

  # join to centroid data file twice to get point geometry of origin and destination centroids
  # convert to sf object making origin centroid active geometry col
  if (geometry) {
    dat <- dat %>%
      dplyr::left_join(centroids, by = c("GEOID1" = "GEOID")) %>%
      dplyr::left_join(centroids, by = c("GEOID2" = "GEOID"), suffix = c("1", "2")) %>%
      sf::st_as_sf(sf_column_name = "centroid1")
  }
  return(dat)
}
