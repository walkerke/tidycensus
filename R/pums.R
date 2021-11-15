#' Load data from the American Community Survey Public Use Microdata Series API
#'
#' @param variables A vector of variables from the PUMS API.
#'   Use \code{View(pums_variables)} to browse variable options.
#' @param state A state, or vector of states, for which you would like to
#'   request data.  The entire US can be requested with \code{state = "all"} - though be patient with the data download!
#' @param puma A vector of PUMAs from a single state, for which you would like
#'   to request data. To get data from PUMAs in more than one state, specify a
#'   named vector of state/PUMA pairs and set \code{state = "multiple"}.
#' @param year The data year of the 1-year ACS sample or the endyear of the
#'   5-year sample. Defaults to 2019.
#' @param survey The ACS survey; one of either \code{"acs1"} or \code{"acs5"}
#'   (the default).
#' @param variables_filter A named list of filters you'd like to return from the
#'   PUMS API.  For example, passing \code{list(AGE = 25:50, SEX = 1)} will return
#'   only males aged 25 to 50 in your output dataset.  Defaults to \code{NULL},
#'   which returns all records. If a housing-only dataset is required,
#'   use \code{list(SPORDER = 1)} to only return householder records (taking care
#'   in your analysis to use the household weight \code{WGTP}).
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
#'   \url{https://api.census.gov/data/key_signup.html}
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
get_pums <- function(variables = NULL,
                     state = NULL,
                     puma = NULL,
                     year = 2019,
                     survey = "acs5",
                     variables_filter = NULL,
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

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key()` function to use it throughout your tidycensus session.')

  }

  # Account for missing PUMAs in 2008-2012 through 2011-2015 ACS samples
  if (year %in% 2012:2015 && survey == "acs5" && (!is.null(puma) || "PUMA" %in% variables)) {
    stop("PUMAs are not available for end-years between 2012 and 2015 due to inconsistent PUMA boundary definitions.", call. = FALSE)
  }


  # Allow users to get all states by specifying "all"
  # To avoid large data timeout errors, iterate through the states instead.
  if (all(state == "all")) {

    state_codes <- unique(fips_codes$state_code)[1:51]

    message("Requesting data for the entire United States by state then combining the result.\nThis can take several minutes to complete.\nFor large data extract workflows, consider downloading data from the Census FTP server\nor from IPUMS (https://ipums.org) instead.")
    output <- purrr::map_dfr(state_codes, function(each_state) {
      suppressMessages(get_pums(
        state = each_state,
        variables = variables,
        puma = NULL,
        year = year,
        survey = survey,
        variables_filter = variables_filter,
        rep_weights = rep_weights,
        recode = recode,
        show_call = TRUE,
        key = key
      ))
    })

    return(output)

  }

  # If variables is NULL, initialize a length-0 vector to store
  # the required variables eventually
  if (is.null(variables)) {
    variables <- c()
  }

  # Avoid double-requesting variables
  # However, if all states are requested, we should still return the state by default
  # as this is expected behavior when requesting data by state

  # Old code - this should get picked up in load_data.R.  To be removed
  # if (all(state == "all")) {
  #   if (!"ST" %in% variables) {
  #     variables <- c("ST", variables)
  #   }
  # }

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

  ## If more than 45 vars requested, split into multiple API calls and join the result
  ## this works, but repeats pulling the weight and ST vars
  if (length(variables) > 45) {
    l <- split(variables, ceiling(seq_along(variables) / 45))
    pums_data <- map(l, function(x) {

      load_data_pums(variables = x,
                     state = state,
                     year = year,
                     puma = puma,
                     survey = survey,
                     variables_filter = variables_filter,
                     recode = recode,
                     show_call = show_call,
                     key = key)
        })

    # to combine the multiple API calls, we need to join using the repeated
    # variables so they don't get duplicated in the final data frame
    # the repeated variables will depend on how we requested data
    #
    # This gets complicated further by the use of a variable filter which
    # is handled differently than the other variables, so we'll need to
    # merge that in as well

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

    if (!is.null(variables_filter)) {
      var_names <- names(variables_filter)
      var_names <- var_names[!var_names == "SPORDER"]

      if (recode) {
        check_type <- pums_variables %>%
          dplyr::filter(var_code %in% var_names,
                        survey == survey,
                        year == year,
                        data_type == "chr") %>%
          dplyr::distinct(var_code) %>%
          dplyr::pull(var_code)

        chr_names <- var_names[var_names %in% check_type]

        if (length(chr_names) > 0) {
          var_labels <- paste0(chr_names, "_label")

          join_vars <- c(join_vars, var_names, var_labels)
        } else {
          join_vars <- c(join_vars, var_names)
        }

      } else {
        join_vars <- c(join_vars, var_names)
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
                                variables_filter = variables_filter,
                                recode = recode,
                                show_call = show_call,
                                key = key)
  }

  return(pums_data)
}


#' Convert a data frame returned by get_pums() to a survey object
#'
#' @description This helper function takes a data frame returned by
#'   \code{\link{get_pums}} and converts it to a tbl_svy from the srvyr
#'   \code{\link[srvyr]{as_survey}} package or a svyrep.design object from the
#'   \code{\link[survey]{svrepdesign}} package. You can then use functions from
#'   the srvyr or survey to calculate weighted estimates with replicate weights
#'   included to provide accurate standard errors.
#'
#' @param df A data frame with PUMS person or housing weight variables, most
#'   likely returned by \code{\link{get_pums}}.
#' @param type Whether to use person or housing-level weights; either
#'   \code{"housing"} or \code{"person"} (the default).
#' @param design The survey design to use when creating a survey object.
#'   Currently the only option is code{"rep_weights"}/.
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
                      design = "rep_weights") {

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

  # if (design == "cluster") {
  #   if (!all(c("SERIALNO", "PUMA") %in% names(df))) {
  #     stop('"SERIALNO" and "PUMA" must both be present in the input data.', call. = FALSE)
  #   }
  # }

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

  # # remove option for cluster design for now pending more research/discussion
  # if (design == "cluster") {
  #   survey <- survey::svydesign(
  #     variables = variables,
  #     weights = weights,
  #     ids = df$SERIALNO,
  #     strata = df$PUMA
  #   )
  # }

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