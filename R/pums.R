#' Load data from the American Community Survey Public Use Microdata Series API
#'
#' @param variables A vector of variables from the PUMS API.
#' @param state A state, or vector of states, for which you would like to
#'   request data.
#' @param year The data year of the 1-year ACS sample or the endyear of the
#'   5-year sample. Defaults to 2017.
#' @param survey The ACS survey; one of either \code{"acs1"} or \code{"acs5"}
#'   (the default).
#' @param rep_weights Whether or not to return housing, person, or both housing
#'   and person-level replicate weights for calculation of standard errors; one
#'   of \code{"person"}, \code{"housing"}, or \code{"both"}.
#' @param recode (only works for 2017 for now) If TRUE, recodes variable values
#'   using Census data dictionary and creates a new \code{*_label} column for
#'   each variable that is recoded. Defaults to FALSE.
#' @param show_call If TRUE, display call made to Census API. This can be very
#'   useful in debugging and determining if error messages returned are due to
#'   tidycensus or the Census API. Copy to the API call into a browser and see
#'   what is returned by the API directly. Defaults to FALSE.
#' @param key Your Census API key. Obtain one at
#'   \url{http://api.census.gov/data/key_signup.html}
#'
#' @return A tibble of microdata from the ACS PUMS API.
#' @export
get_pums <- function(variables,
                     state,
                     year = 2017,
                     survey = "acs5",
                     rep_weights = NULL,
                     recode = FALSE,
                     show_call = FALSE,
                     key = NULL) {

  if (year == 2018) {
    warning("The 2018 PUMS API has limited functionality as household serial numbers do not come through correctly at the moment. Use at your own risk.")
  }

  if (survey == "acs1") {
    message(sprintf("Getting data from the %s 1-year ACS Public Use Microdata Sample",
                    year))
  } else if (survey == "acs5") {
    startyear <- year - 4
    message(sprintf("Getting data from the %s-%s 5-year ACS Public Use Microdata Sample",
                    startyear, year))
  }

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }

  if(!is.null(rep_weights)) {
    if(year == 2018) {
    stop("Cannot request replicate weights for 2018 PUMS because household serial numbers are not available from the API at the moment and each API call is limited to 50 variables.",
         call. = FALSE)
    } else {
      if(rep_weights == "housing") {
        variables <- c(variables, housing_weight_variables)
      }
      if(rep_weights == "person") {
        variables <- c(variables, person_weight_variables)
        }
      if(rep_weights == "both") {
        variables <- c(variables, housing_weight_variables, person_weight_variables)
        }
      }
  }

  ## If more than 46 vars requested, split into multiple API calls and join the result
  ## this works, but repeats pulling the weight and ST vars
  if (length(variables) > 46) {
    if(year == 2018) {
      stop("Cannot request more than 46 variables in a single call for 2018 PUMS because household serial numbers are not available from the API at the moment.",
           call. = FALSE)
    }
    l <- split(variables, ceiling(seq_along(variables) / 46))
    pums_data <- map(l, function(x) {
      load_data_pums(variables = x,
                     state = state,
                     year = year,
                     survey = survey,
                     recode = recode,
                     show_call = show_call,
                     key = key)
        })

    if(recode) {
      pums_data <- reduce(pums_data, left_join, by = c("SERIALNO", "SPORDER", "WGTP", "PWGTP", "ST", "ST_label"))
    } else {
      pums_data <- reduce(pums_data, left_join, by = c("SERIALNO", "SPORDER", "WGTP", "PWGTP"))
    }

    pums_data <- select(pums_data, -contains("WGTP"), everything(), contains("WGTP"))
      } else {
        pums_data <- load_data_pums(variables = variables,
                                    state = state,
                                    year = year,
                                    survey = survey,
                                    recode = recode,
                                    show_call = show_call,
                                    key = key)
        }

  # Replace variable names if supplied
  if (!is.null(names(variables))) {
    for (i in 1:length(variables)) {
      names(pums_data) <- str_replace(names(pums_data),
                                      variables[i],
                                      names(variables)[i])
    }
  }
  return(pums_data)
}


#' Convert a data frame returned by get_pums() to a survey design object
#'
#' @description This helper function takes a data frame returned by
#'   \code{\link{get_pums}} and converts it to a svyrep.design object from the
#'   \code{\link[survey]{svrepdesign}} package. You can then use functions from survey (or
#'   srvyr) package to calculte weighted estimates with replicate weights
#'   included to provide accurate standard errors.
#'
#' @param df A data frame with PUMS person or housing replicate weight
#'   variables, most likely returned by \code{\link{get_pums}}.
#' @param type Whether to use person or housing-level replicate weights; either
#'   \code{"housing"} or \code{"person"} (the default).
#'
#' @return A svyrep.design object.
#' @export
#'
#' @examples
#' \dontrun{
#' pums <- get_pums(variables = "AGEP", state = "VT", rep_weights = "person")
#' pums_design <- df_to_svyrep(pums, type = "person")
#' survey::svymean(~AGEP, pums_design)
#' }
df_to_svyrep <- function(df, type = "person") {

  if(!"survey" %in% installed.packages()) {
    stop('survey package must be installed to convert to a svyrep object. Please install using install.packages("survey") and try again.',
         call. = FALSE)
    }
  if(type == "person") {
    if(!all(person_weight_variables %in% names(df))) {
      stop("Not all person replicate weight variables are present in input data.", call. = FALSE)
    }
    if(!"PWGTP" %in% names(df)) {
      stop("Person weight variable is not present in input data.", call. = FALSE)
    }
    variables <- df[, !names(df) %in% c(person_weight_variables, "PWGTP")]
    weights <- df$PWGTP
    repweights <- df[, person_weight_variables]
  }

  if(type == "housing") {

    if(anyDuplicated(df$SERIALNO) != 0) {
      warning("You have duplicate values in the SERIALNO column of your input data, are you sure you wish to proceed?",
              call. = FALSE)
    }
    if(!all(housing_weight_variables %in% names(df))) {
      stop("Not all housing replicate weight variables are present in input data.", call. = FALSE)
    }
    if(!"WGTP" %in% names(df)) {
      stop("Housing weight variable is not present in input data.", call. = FALSE)
    }
    variables <- df[, !names(df) %in% c(housing_weight_variables, "WGTP")]
    weights <- df$WGTP
    repweights <- df[, housing_weight_variables]
  }

  survey::svrepdesign(
    variables = variables,
    weights = weights,
    repweights = repweights,
    scale = 4 / 80,
    rscales = rep(1 , 80),
    mse = TRUE,
    type = "JK1"
  )
}
