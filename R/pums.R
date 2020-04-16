#' Load data from the American Community Survey Public Use Microdata Series API
#'
#' @param variables A vector of variables from the PUMS API.
#' @param state A state, or vector of states, for which you would like to request data.
#' @param year The data year of the 1-year ACS sample or the endyear of the 5-year
#'             sample. Defaults to 2017.
#' @param survey The ACS survey; one of either \code{"acs1"} or \code{"acs5"}
#'               (the default)
#' @param rep_weights (not currently implemented) Whether or not to return household,
#'                    person, or both household and person-level replicate weights
#'                    for calculation of standard errors.
#' @param show_call if TRUE, display call made to Census API. This can be very useful
#'                  in debugging and determining if error messages returned are
#'                  due to tidycensus or the Census API. Copy to the API call into
#'                  a browser and see what is returned by the API directly.
#'                  Defaults to FALSE.
#' @param key Your Census API key.
#'            Obtain one at \url{http://api.census.gov/data/key_signup.html}
#'
#' @return A tibble of microdata from the ACS PUMS API.
#' @export
get_pums <- function(variables,
                     state,
                     year = 2017,
                     survey = "acs5",
                     rep_weights = NULL,
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

  pums_data <- load_data_pums(variables = variables,
                              state = state,
                              year = year,
                              survey = survey,
                              show_call = show_call,
                              key = key)

  return(pums_data)



}