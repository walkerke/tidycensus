#' Load data from the Basic Monthly Current Population Survey Microdata API
#'
#' @param variables A vector of variables from the CPS API.
#' @param state A state, or vector of states, for which you would like to
#'   request data. The entire US can be requested with \code{state = "all"}.
#'   Defaults to \code{"all"}.
#' @param year The year of the CPS data requested. Defaults to 2020.
#' @param month The month number of the CPS data requested Defaults to 9 (September).
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
#' get_pums(variables = "AGEP", state = "VT")
#' get_pums(variables = c("AGEP", "ANC1P"), state = "VT", recode = TRUE)
#' get_pums(variables = "AGEP", state = "VT", survey = "acs1", rep_weights = "person")
#' }
#'
get_cps <- function(variables,
                    state = "all",
                    year = NULL,
                    month = NULL,
                    date_range = NULL,
                    show_call = FALSE,
                    key = NULL) {

  if (Sys.getenv('CENSUS_API_KEY') != '') {
    key <- Sys.getenv('CENSUS_API_KEY')
  } else if (is.null(key)) {
    stop("A Census API key is required. Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.",
         call. = FALSE)
  }

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

  # # if no year or month specified, let's get data from 2 months ago
  # if (is.null(year) || is.null(month)) {
  #   floor_month <- as.Date(cut(Sys.Date(), "month"))
  #   last_month <- seq(floor_month, length = 2, by = "-2 month")[2]
  #
  #   if (is.null(year)) {
  #     year <- as.numeric(format(last_month, "%Y"))
  #     message(paste("No year specified, defaulting to", year))
  #   }
  #
  #   if (is.null(month)) {
  #     month <- as.numeric(format(last_month, "%m"))
  #     message(paste("No month specified, defaulting to", month.name[month]))
  #   }
  # }

  # remove a handful of variables that may be requested by user because they
  # are always returned by load_data_cps() and will return duplicate columns
  # if included in vars requested
  vars_exclude <- c("GESTFIPS", "PWCMPWGT", "HWHHWGT",
                    "HRHHID", "HRHHID2", "PULINENO",
                    "HRMONTH", "HRYEAR4", "HRYEAR")
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

  # # use insistently to try loading data up to 3 times if api response fails
  # insist_load_data_cps <- purrr::insistently(load_data_cps)

  # iterate over each element of vars list
  cps_data <- map(l, function(variables) {

    # iterate over years and months and get data for each year/month
    # bind all rows returned into a single data frame
    purrr::map2_dfr(year_month$year, year_month$month, function(x, y) {
      load_data_cps(variables = variables,
                                  state = state,
                                  year = x,
                                  month = y,
                                  show_call = show_call,
                                  key = key)

      # if (x == min(year_month$year) && y == min(year_month$month)) {
      #   message(ret$messages)
      #   }
      # ret$result
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
     # dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), ~ ifelse(.x == -1, NA, .x))
      ) %>%
    left_join(fips_state_table, by = c("GESTFIPS" = "fips")) %>%
    mutate(ST = toupper(abb)) %>%
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


#' #' Convert a data frame returned by get_pums() to a survey object
#' #'
#' #' @description This helper function takes a data frame returned by
#' #'   \code{\link{get_pums}} and converts it to a tbl_svy from the srvyr
#' #'   \code{\link[srvyr]{as_survey}} package or a svyrep.design object from the
#' #'   \code{\link[survey]{svrepdesign}} package. You can then use functions from the
#' #'   srvyr or survey to calculate weighted estimates with replicate weights
#' #'   included to provide accurate standard errors.
#' #'
#' #' @param df A data frame with PUMS person or housing weight variables, most
#' #'   likely returned by \code{\link{get_pums}}.
#' #' @param type Whether to use person or housing-level weights; either
#' #'   \code{"housing"} or \code{"person"} (the default).
#' #' @param design Whether to use a cluster or replicate weight survey design;
#' #'   either \code{"cluster"} or \code{"rep_weights"} (the default).
#' #' @param class Whether to convert to a srvyr or survey object; either
#' #'   \code{"survey"} or \code{"srvyr"} (the default).
#' #'
#' #' @return A tbl_svy or svyrep.design object.
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' pums <- get_pums(variables = "AGEP", state = "VT", rep_weights = "person")
#' #' pums_design <- to_survey(pums, type = "person", class = "srvyr")
#' #' survey::svymean(~AGEP, pums_design)
#' #' }
#' to_survey <- function(df,
#'                       type = c("person", "housing"),
#'                       class = c("srvyr", "survey"),
#'                       design = c("rep_weights", "cluster")) {
#'
#'   type <- match.arg(type)
#'   class <- match.arg(class)
#'   design <- match.arg(design)
#'
#'   if (class == "srvyr" && !"srvyr" %in% installed.packages()) {
#'     stop('srvyr package must be installed to convert to a srvyr object. Please install using install.packages("srvyr") and try again.',
#'          call. = FALSE)
#'   }
#'
#'   if (!"survey" %in% installed.packages()) {
#'     stop('survey package must be installed to convert to a survey object. Please install using install.packages("survey") and try again.',
#'          call. = FALSE)
#'   }
#'
#'   if (design == "cluster") {
#'     if (!all(c("SERIALNO", "PUMA") %in% names(df))) {
#'       stop('"SERIALNO" and "PUMA" are present in input data.', call. = FALSE)
#'     }
#'   }
#'
#'   if (type == "person") {
#'
#'     variables <- df[, !names(df) %in% c(person_weight_variables, "PWGTP")]
#'     weights <- df$PWGTP
#'
#'     if (design == "rep_weights") {
#'       if (!all(person_weight_variables %in% names(df))) {
#'         stop("Not all person replicate weight variables are present in input data.", call. = FALSE)
#'       }
#'       if (!"PWGTP" %in% names(df)) {
#'         stop("Person weight variable is not present in input data.", call. = FALSE)
#'       }
#'       repweights <- df[, person_weight_variables]
#'     }
#'   }
#'
#'   if (type == "housing") {
#'     if (anyDuplicated(df$SERIALNO) != 0) {
#'       warning("You have duplicate values in the SERIALNO column of your input data, are you sure you wish to proceed?",
#'               call. = FALSE)
#'     }
#'
#'     variables <- df[, !names(df) %in% c(housing_weight_variables, "WGTP")]
#'     weights <- df$WGTP
#'
#'     if (design == "rep_weights") {
#'       if (!all(housing_weight_variables %in% names(df))) {
#'         stop("Not all housing replicate weight variables are present in input data.", call. = FALSE)
#'       }
#'       if (!"WGTP" %in% names(df)) {
#'         stop("Housing weight variable is not present in input data.", call. = FALSE)
#'       }
#'     repweights <- df[, housing_weight_variables]
#'     }
#'   }
#'
#'   if (design == "cluster") {
#'     survey <- survey::svydesign(
#'       variables = variables,
#'       weights = weights,
#'       ids = df$SERIALNO,
#'       strata = df$PUMA
#'     )
#'   }
#'
#'   if (design == "rep_weights"){
#'     survey <- survey::svrepdesign(
#'       variables = variables,
#'       weights = weights,
#'       repweights = repweights,
#'       scale = 4 / 80,
#'       rscales = rep(1 , 80),
#'       mse = TRUE,
#'       type = "JK1"
#'       )
#'     }
#'
#'   if (class == "srvyr") {
#'     return(srvyr::as_survey(survey))
#'   } else {
#'     survey
#'   }
#' }
