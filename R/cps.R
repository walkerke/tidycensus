#' Load data from the Basic Monthly Current Population Survey Microdata API
#'
#' @param variables A vector of variables from the CPS API. Household and person
#'   IDs, household and person weights, state, year, and month are always
#'   returned.
#' @param state A state, or vector of states, for which you would like to
#'   request data. The entire US can be requested with \code{state = "all"}.
#'   Defaults to \code{"all"}.
#' @param year The year or years of the CPS data requested.
#' @param month The month number or numbers of the CPS data requested.
#' @param date_range A vector of two dates (start month and end month). All
#'   months of data between the two dates will be requested You can use either a
#'   date range or a combination of year and month, but not both.
#' @param var_filter A named list of CPS variable names and values to be used as
#'   filter when requesting data from the Census API.
#' @param show_call If TRUE, display call made to Census API. This can be very
#'   useful in debugging and determining if error messages returned are due to
#'   tidycensus or the Census API. Copy to the API call into a browser and see
#'   what is returned by the API directly. Defaults to FALSE.
#' @param key Your Census API key. Obtain one at
#'   \url{http://api.census.gov/data/key_signup.html}.
#'
#' @return A tibble of microdata from the CPS API.
#' @export
#'
#' @examples
#' \dontrun{
#' get_cps(variables = "PRTAGE", state = "VT", year = 2020, month = 9)
#'
#' # All months between July 2018 and and June 2020
#' # Only return observations where age is between 16 and 24 and
#' # employment status is 1, 2, or 5
#' get_cps(
#'   variables = c("PRTAGE", "PEMLR", "PTDTRACE", "PEHSPNON"),
#'   state = c("VT", "NH"),
#'   date_range = c(as.Date("2018-07-01"), as.Date("2020-06-01")),
#'   var_filter = list(PRTAGE = 16:24, PEMLR = c(1, 2, 5))
#'   )
#' }
#'
get_cps <- function(variables,
                    state = "all",
                    year = NULL,
                    month = NULL,
                    date_range = NULL,
                    var_filter = NULL,
                    show_call = FALSE,
                    key = NULL) {

  # check if api key is installed or provided
  if (Sys.getenv('CENSUS_API_KEY') != '') {
    key <- Sys.getenv('CENSUS_API_KEY')
  } else if (is.null(key)) {
    stop("A Census API key is required. Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.",
         call. = FALSE)
  }

  # check that there is either a year/month or date range specified
  if (
    (!is.null(date_range) && (!is.null(year) || !is.null(month))) ||
    (is.null(date_range) && (is.null(year) || is.null(month)))
    ) {
    stop("Specify either a combination of year and month or a date range.",
         call. = FALSE)
    }

  # allow users to get all states by specifying "all"
  if (all(state == "all")) {
    message("Requesting data for the entire United States")
    state <- NULL
  }

  # remove a handful of variables that may be requested by user because they
  # are always returned by load_data_cps() and will return duplicate columns
  # if included in vars requested
  vars_exclude <- c("GESTFIPS", "PWCMPWGT", "HWHHWGT",
                    "HRHHID", "HRHHID2", "PULINENO",
                    "HRMONTH", "HRYEAR4", "HRYEAR")

  # if we request the variable in the var list as well as in filters
  # it will be duplicated, so add the vars with filters to the exclude list
  if (!is.null(var_filter)) {
    vars_exclude <- c(vars_exclude, unique(names(var_filter)))
  }

  variables <- variables[!variables %in% vars_exclude]

  # always get household and person weight vars
  variables <- c(variables, "PWCMPWGT", "HWHHWGT")

  # if no states specified api does not automatically return state var so add it
  if (is.null(state)) {
    variables <- c(variables, "GESTFIPS")
  }

  if (is.null(date_range)) {
    # when year/month is specified
    # create all combinations of years and months
    # if only one year/month requested this will be a one row df
    # and will only need one call to the api
    year_month <- tidyr::crossing(year = year, month = month)

  } else {

    if (length(date_range) > 2) {
      stop("Provide a vector of two dates to specify beginning and end of date range",
           call. = FALSE)
    }
    if (length(date_range) == 1) {
      date_range[2] <- date_range[1]
    }

    # when date range specified, get first of the month for start and end
    floor_month <- as.Date(cut(date_range, "month"))

    # get all year months between start and end
    d <- seq(from = floor_month[1], to = floor_month[2], by = "1 month")

    # create df of years and months to iterate through
    year_month <- data.frame(
      year = as.numeric(format(d, "%Y")),
      month = as.numeric(format(d, "%m"))
      )
  }

  # create date variable to check min date requested
  year_month <- year_month %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-")))

  # before may 2004 HRHHID2 didn't exist and it was more difficult to identify
  # unique observations. we can fix this but seems low priority
  # so for now, can't get more than 42 vars in a single call because we won't
  # be able to join the results of multiple api calls by unique identifiers
  # see: https://cps.ipums.org/cps-action/variables/HRHHID2#description_section
  # and: https://cps.ipums.org/cps-action/variables/HRHHID2#comparability_section
  if (length(variables) > 42 && (min(year_month$date) < as.Date("2004-05-01"))) {
    stop("Cannot request more than 42 variables in a single call for months before May 2004. Break your variables into smaller chunks and try again.",
         call. = FALSE)
  }

  message("Getting data from the Basic Monthly CPS Public Use Microdata files")
  message(paste("First month:", format(min(year_month$date), "%b %Y")))
  message(paste("Last month:", format(max(year_month$date), "%b %Y")))
  message(paste("Total months:", nrow(year_month)))

  # api can only handle 50 vars in a single call, so if more than 42 vars requested
  # we need to iterate over subsets of variables and make separate api calls

  # split vars into a list with each element 42 vars or less
  # if fewer than 42 variables requested, this will create a list of length 1
  # and will only need a single api call
  l <- split(variables, ceiling(seq_along(variables) / 42))

  # iterate over each element of vars list
  cps_data <- map(l, function(variables) {

    # iterate over years and months and get data for each year/month
    # bind all rows returned into a single data frame
    purrr::map2_dfr(year_month$year, year_month$month, function(x, y) {
      load_data_cps(variables = variables,
                    state = state,
                    year = x,
                    month = y,
                    var_filter = var_filter,
                    show_call = show_call,
                    key = key)
      })
    })

  # left join to combine all the variables returned
  # join by variables that are returned from every call to load_data_cps()
  # to avoid duplicate vars
  cps_data <- reduce(
      cps_data,
      left_join,
      by = c("HRYEAR4", "HRMONTH", "HRHHID", "HRHHID2", "PULINENO", "GESTFIPS")
      )

  # convert all columns to numeric except id cols
  # join with state fips table to recode create state fips in abbreviation
  # have to fix up the api return fips code so that it can be joined
  cps_data <- cps_data %>%
    mutate(
      GESTFIPS = str_pad(.data$GESTFIPS, width = 2, side = "left", pad = "0"),
      dplyr::across(!dplyr::any_of(c("HRHHID", "HRHHID2", "PULINENO", "GESTFIPS")), as.numeric)
      ) %>%
    left_join(fips_state_table, by = c("GESTFIPS" = "fips")) %>%
    mutate(ST = toupper(.data$abb)) %>%
    select(-.data$name, -.data$abb) %>%
    dplyr::relocate(.data$GESTFIPS, .data$ST, .after = .data$PULINENO) %>%
    dplyr::relocate(.data$HRMONTH, .data$HRYEAR4)

  # if county is requested, be helpful and add the county name to the data
  # have to fix up the api return fips code so that it can be joined
  if ("GTCO" %in% colnames(cps_data)) {
    cps_data <- cps_data %>%
      mutate(
        GTCO = ifelse(.data$GTCO == 0, NA, .data$GTCO),
        GTCO = str_pad(.data$GTCO, width = 3, side = "left", pad = "0")
      ) %>%
      left_join(fips_codes, by = c("GESTFIPS" = "state_code", "GTCO" = "county_code")) %>%
      rename(COUNTY = .data$county) %>%
      select(-.data$state, -.data$state_name) %>%
      dplyr::relocate(.data$GTCO, .data$COUNTY, .after = .data$ST)
  }
  return(cps_data)
}
