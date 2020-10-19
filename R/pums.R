#' Load data from the American Community Survey Public Use Microdata Series API
#'
#' @param variables A vector of variables from the PUMS API.
#' @param state A state, or vector of states, for which you would like to
#'   request data.  The entire US can be requested with \code{state = "all"} - though be patient with the data download!
#' @param puma A vector of PUMAs from a single state, for which you would like
#'   to request data. To get data from PUMAs in more than one state, specify a
#'   named vector of state/PUMA pairs and set \code{state = "multiple"}.
#' @param year The data year of the 1-year ACS sample or the endyear of the
#'   5-year sample. Defaults to 2018.
#' @param survey The ACS survey; one of either \code{"acs1"} or \code{"acs5"}
#'   (the default).
#' @param rep_weights Whether or not to return housing unit, person, or both
#'   housing and person-level replicate weights for calculation of standard
#'   errors; one of \code{"person"}, \code{"housing"}, or \code{"both"}.
#' @param recode If TRUE, recodes variable values using Census data dictionary
#'   and creates a new \code{*_label} column for each variable that is recoded.
#'   Available for 2017 - 2019 data. Defaults to FALSE.
#' @param show_call If TRUE, display call made to Census API. This can be very
#'   useful in debugging and determining if error messages returned are due to
#'   tidycensus or the Census API. Copy to the API call into a browser and see
#'   what is returned by the API directly. Defaults to FALSE.
#' @param key Your Census API key. Obtain one at
#'   \url{http://api.census.gov/data/key_signup.html}
#'
#' @return A tibble of microdata from the ACS PUMS API.
#' @export
#'
#' @examples
#' \dontrun{
#' get_pums(variables = "AGEP", state = "VT")
#' get_pums(variables = "AGEP", state = "multiple", puma = c("UT" = 35008, "NV" = 00403))
#' get_pums(variables = c("AGEP", "ANC1P"), state = "VT", recode = TRUE)
#' get_pums(variables = "AGEP", state = "VT", survey = "acs1", rep_weights = "person")
#' }
#'
get_pums <- function(variables,
                     state = NULL,
                     puma = NULL,
                     year = 2018,
                     survey = "acs5",
                     rep_weights = NULL,
                     recode = FALSE,
                     show_call = FALSE,
                     key = NULL) {

  if (is.null(state)) {
    stop("You must specify a state by name, postal code, or FIPS code. To request data for the entire United States, specify `state = 'all'`.", call. = FALSE)
  }

  if (survey == "acs1") {
    message(sprintf("Getting data from the %s 1-year ACS Public Use Microdata Sample",
                    year))
  } else if (survey == "acs5") {
    startyear <- year - 4
    message(sprintf("Getting data from the %s-%s 5-year ACS Public Use Microdata Sample",
                    startyear, year))
  }

  # Allow users to get all states by specifying "all"
  if (all(state == "all")) {
    message("Requesting data for the entire United States. This can take several minutes to complete.")
    state <- NULL
  }

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }

  # Avoid double-requesting variables
  join_vars <- c("SERIALNO", "SPORDER", "WGTP", "PWGTP", "ST")
  variables <- variables[!variables %in% join_vars]

  if (!is.null(rep_weights)) {
    if (rep_weights == "housing") {
      variables <- c(variables, housing_weight_variables)
    }
    if (rep_weights == "person") {
      variables <- c(variables, person_weight_variables)
    }
    if (rep_weights == "both") {
      variables <- c(variables, housing_weight_variables, person_weight_variables)
    }
  }

  ## If more than 46 vars requested, split into multiple API calls and join the result
  ## this works, but repeats pulling the weight and ST vars
  if (length(variables) > 46) {
    l <- split(variables, ceiling(seq_along(variables) / 46))
    pums_data <- map(l, function(x) {
      load_data_pums(variables = x,
                     state = state,
                     year = year,
                     puma = puma,
                     survey = survey,
                     recode = recode,
                     show_call = show_call,
                     key = key)
        })

    # to combine the multiple API calls, we need to join using the repeated
    # variables so they don't get duplicated in the final data frame
    # the repeated variables will depend on how we requested data

    if (recode) {
      if (!is.null(puma)) {
        join_vars <- c(join_vars, "ST_label", "PUMA")
      } else {
        join_vars <- c(join_vars, "ST_label")
      }
    } else {
      if (!is.null(puma)) {
        join_vars <- c(join_vars, "PUMA")
      }
    }

    pums_data <- reduce(pums_data, left_join, by = join_vars)


    pums_data <- select(pums_data, -contains("WGTP"), everything(), contains("WGTP"))
  } else {
    pums_data <- load_data_pums(variables = variables,
                                state = state,
                                puma = puma,
                                year = year,
                                survey = survey,
                                recode = recode,
                                show_call = show_call,
                                key = key)
  }

  # Replace variable names if supplied
  # Suspending this functionality (for now) as it needs to be re-worked for use with labeling
  # if (!is.null(names(variables))) {
  #   for (i in 1:length(variables)) {
  #     names(pums_data) <- str_replace(names(pums_data),
  #                                     variables[i],
  #                                     names(variables)[i])
  #   }
  # }

  return(pums_data)
}


#' Convert a data frame returned by get_pums() to a survey object
#'
#' @description This helper function takes a data frame returned by
#'   \code{\link{get_pums}} and converts it to a tbl_svy from the srvyr
#'   \code{\link[srvyr]{as_survey}} package or a svyrep.design object from the
#'   \code{\link[survey]{svrepdesign}} package. You can then use functions from the
#'   srvyr or survey to calculate weighted estimates with replicate weights
#'   included to provide accurate standard errors.
#'
#' @param df A data frame with PUMS person or housing weight variables, most
#'   likely returned by \code{\link{get_pums}}.
#' @param type Whether to use person or housing-level weights; either
#'   \code{"housing"} or \code{"person"} (the default).
#' @param design Whether to use a cluster or replicate weight survey design;
#'   either \code{"cluster"} or \code{"rep_weights"} (the default).
#' @param class Whether to convert to a srvyr or survey object; either
#'   \code{"survey"} or \code{"srvyr"} (the default).
#'
#' @return A tbl_svy or svyrep.design object.
#' @export
#'
#' @examples
#' \dontrun{
#' pums <- get_pums(variables = "AGEP", state = "VT", rep_weights = "person")
#' pums_design <- to_survey(pums, type = "person", class = "srvyr")
#' survey::svymean(~AGEP, pums_design)
#' }
to_survey <- function(df,
                      type = c("person", "housing"),
                      class = c("srvyr", "survey"),
                      design = c("rep_weights", "cluster")) {

  type <- match.arg(type)
  class <- match.arg(class)
  design <- match.arg(design)

  if (class == "srvyr" && !"srvyr" %in% installed.packages()) {
    stop('srvyr package must be installed to convert to a srvyr object. Please install using install.packages("srvyr") and try again.',
         call. = FALSE)
  }

  if (!"survey" %in% installed.packages()) {
    stop('survey package must be installed to convert to a survey object. Please install using install.packages("survey") and try again.',
         call. = FALSE)
  }

  if (design == "cluster") {
    if (!all(c("SERIALNO", "PUMA") %in% names(df))) {
      stop('"SERIALNO" and "PUMA" are present in input data.', call. = FALSE)
    }
  }

  if (type == "person") {

    variables <- df[, !names(df) %in% c(person_weight_variables, "PWGTP")]
    weights <- df$PWGTP

    if (design == "rep_weights") {
      if (!all(person_weight_variables %in% names(df))) {
        stop("Not all person replicate weight variables are present in input data.", call. = FALSE)
      }
      if (!"PWGTP" %in% names(df)) {
        stop("Person weight variable is not present in input data.", call. = FALSE)
      }
      repweights <- df[, person_weight_variables]
    }
  }

  if (type == "housing") {
    if (anyDuplicated(df$SERIALNO) != 0) {
      warning("You have duplicate values in the SERIALNO column of your input data, are you sure you wish to proceed?",
              call. = FALSE)
    }

    variables <- df[, !names(df) %in% c(housing_weight_variables, "WGTP")]
    weights <- df$WGTP

    if (design == "rep_weights") {
      if (!all(housing_weight_variables %in% names(df))) {
        stop("Not all housing replicate weight variables are present in input data.", call. = FALSE)
      }
      if (!"WGTP" %in% names(df)) {
        stop("Housing weight variable is not present in input data.", call. = FALSE)
      }
    repweights <- df[, housing_weight_variables]
    }
  }

  if (design == "cluster") {
    survey <- survey::svydesign(
      variables = variables,
      weights = weights,
      ids = df$SERIALNO,
      strata = df$PUMA
    )
  }

  if (design == "rep_weights"){
    survey <- survey::svrepdesign(
      variables = variables,
      weights = weights,
      repweights = repweights,
      scale = 4 / 80,
      rscales = rep(1 , 80),
      mse = TRUE,
      type = "JK1"
      )
    }

  if (class == "srvyr") {
    return(srvyr::as_survey(survey))
  } else {
    survey
  }
}
