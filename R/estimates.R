#' Get data from the US Census Bureau Population Estimates Program
#'
#' The \code{get_estimates()} function requests data from the US Census Bureau's Population Estimates Program (PEP) datasets.  The PEP datasets are defined by the US Census Bureau as follows: "The Census Bureau's Population Estimates Program (PEP) produces estimates of the population for the United States, its states, counties, cities, and towns, as well as for the Commonwealth of Puerto Rico and its municipios. Demographic components of population change (births, deaths, and migration) are produced at the national, state, and county levels of geography. Additionally, housing unit estimates are produced for the nation, states, and counties.  PEP annually utilizes current data on births, deaths, and migration to calculate population change since the most recent decennial census and produce a time series of estimates of population, demographic components of change, and housing units. The annual time series of estimates begins with the most recent decennial census data and extends to the vintage year. As each vintage of estimates includes all years since the most recent decennial census, the latest vintage of data available supersedes all previously-produced estimates for those dates."
#'
#' \code{get_estimates()} requests data from the Population Estimates API for years 2019 and earlier; however the Population Estimates are no longer supported on the API as of 2020.  For recent years, \code{get_estimates()} reads a flat file from the Census website and parses it.  This means that arguments and output for 2020 and later datasets may differ slightly from datasets acquired for 2019 and earlier.
#'
#' As of April 2022, variables available for 2020 and later datasets are as follows: ESTIMATESBASE, POPESTIMATE, NPOPCHG, BIRTHS, DEATHS, NATURALCHG, INTERNATIONALMIG, DOMESTICMIG, NETMIG, RESIDUAL, GQESTIMATESBASE, GQESTIMATES, RBIRTH, RDEATH, RNATURALCHG, RINTERNATIONALMIG, RDOMESTICMIG, and RNETMIG.
#'
#' @param geography The geography of your data. Available geographies for the most recent data vintage are listed
#'                  \href{https://api.census.gov/data/2019/pep/population/geography.html}{here}. \code{"cbsa"} may
#'                  be used an alias for \code{"metropolitan statistical area/micropolitan statistical area"}.
#' @param product The data product (optional). \code{"population"}, \code{"components"}
#'                \code{"housing"}, and \code{"characteristics"} are supported.
#'
#'                For 2020 and later, the only supported product is \code{"characteristics"}.
#' @param variables A character string or vector of character strings of requested variables.  For years 2020 and later, use \code{variables = "all"} to request all available variables.
#' @param breakdown The population breakdown used when \code{product = "characteristics"}.
#'                  Acceptable values are \code{"AGEGROUP"}, \code{"RACE"}, \code{"SEX"}, and
#'                  \code{"HISP"}, for Hispanic/Not Hispanic.  These values can be combined in
#'                  a vector, returning population estimates in the \code{value} column for all
#'                  combinations of these breakdowns.  For years 2020 and later, \code{"AGE"} is also available for single-year age when using \code{geography = "state"}.
#'
#' @param breakdown_labels Whether or not to label breakdown elements returned when
#'                         \code{product = "characteristics"}. Defaults to FALSE.
#' @param vintage It is recommended to use the most recent vintage
#'             available for a given decennial series (so, year = 2019 for the 2010s, and year = 2023 for the 2020s).  Will default to 2022 until the full PEP for 2023 is released.
#' @param year The data year (defaults to the vintage requested). Use \code{time_series = TRUE} to access time-series estimates.
#' @param state The state for which you are requesting data. State
#'              names, postal codes, and FIPS codes are accepted.
#'              Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#'               FIPS codes are accepted. Must be combined with a value supplied
#'               to `state`.  Defaults to NULL.
#' @param time_series If \code{TRUE}, the function will return a time series of observations back to the decennial Census
#'                    of 2010. The returned column is either "DATE", representing a particular estimate date, or "PERIOD",
#'                    representing a time period (e.g. births between 2016 and 2017), and contains integers representing
#'                    those values.  Integer to date or period mapping is available at
#'                    \url{https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html}.
#' @param output One of "tidy" (the default) in which each row represents an
#'               enumeration unit-variable combination, or "wide" in which each
#'               row represents an enumeration unit and the variables are in the
#'               columns.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'                 if TRUE, uses the tigris package to return an sf tibble
#'                 with simple feature geometry in the `geometry` column.
#' @param keep_geo_vars if TRUE, keeps all the variables from the Census
#'                      shapefile obtained by tigris.  Defaults to FALSE.
#' @param shift_geo (deprecated) if TRUE, returns geometry with Alaska and Hawaii shifted for thematic
#'                  mapping of the entire US.  As of May 2021, we recommend using \code{tigris::shift_geometry()}
#'                  instead.
#' @param key Your Census API key.
#'            Obtain one at \url{https://api.census.gov/data/key_signup.html}.  Can be stored
#'            in your .Renviron with \code{census_api_key("YOUR KEY", install = TRUE)}
#' @param show_call if TRUE, display call made to Census API. This can be very useful
#'                  in debugging and determining if error messages returned are
#'                  due to tidycensus or the Census API. Copy to the API call into
#'                  a browser and see what is returned by the API directly. Defaults to FALSE.
#' @param ... other keyword arguments
#'
#' @return A tibble, or sf tibble, of population estimates data
#' @seealso \url{https://www.census.gov/programs-surveys/popest/about.html}
#' @export
get_estimates <- function(geography = c("us", "region", "division", "state", "county", "county subdivision",
                                        "place/balance (or part)", "place", "consolidated city", "place (or part)",
                                        "metropolitan statistical area/micropolitan statistical area", "cbsa",
                                        "metropolitan division", "combined statistical area"),
                          product = NULL, variables = NULL,
                          breakdown = NULL, breakdown_labels = FALSE, vintage = 2022,
                          year = vintage, state = NULL, county = NULL,
                          time_series = FALSE,
                          output = "tidy", geometry = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL, show_call = FALSE, ...) {

  if (missing(vintage) && year > 2020) {
    rlang::warn(c("For post-2020 Census estimates, `get_estimates()` now uses the `vintage` argument to specify the PEP vintage, and the `year` argument to isolate a year within that vintage.",
                  "!" = "This may be a breaking change in your code",
                  "!" = "Omitting `vintage` may lead to incorrect or unexpected results."))
  }

  if (year > 2020) {
    rlang::inform(sprintf("Using the Vintage %s Population Estimates", vintage))
  }


  geography <- rlang::arg_match(geography)

  if (shift_geo) {
    warning("The `shift_geo` argument is deprecated and will be removed in a future release. We recommend using `tigris::shift_geometry()` instead.", call. = FALSE)
  }

  if (year < 2015) {
    stop("The Population Estimates API is not available in tidycensus for years prior to 2015. Consider using `time_series = TRUE` or the censusapi package for earlier estimates.")
  }

  ###### New logic for 2020 and later
  # Adjust as needed over the next few months, and try to keep consistent with
  # previous years that are on the API
  if (year >= 2020) {

      if (!is.null(product) && product == "characteristics") {

        if (!geography %in% c("state", "county")) {
          rlang::abort("The only supported geographies at this time for population characteristics 2020 and later are 'state' and 'county'.")
        }

        if (vintage > 2023) {
          rlang::abort("The Characteristics dataset has not yet been released for vintages beyond 2023")
        }

        if (geography == "state") {

          state_raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/state/asrh/sc-est%s-alldata6.csv", vintage, vintage)))

          if (!is.null(state)) {
            state <- validate_state(state)

            state_raw2 <- state_raw %>%
              dplyr::filter(STATE == state)
          } else {
            state_raw2 <- state_raw
          }

          parsed <- state_raw2 %>%
            dplyr::select(GEOID = STATE,
                          NAME:SEX,
                          HISP = ORIGIN,
                          RACE:AGE,
                          dplyr::contains("POPESTIMATE")) %>%
            dplyr::mutate(AGEGROUP = dplyr::case_when(
              AGE %in% 0:4 ~ 1,
              AGE %in% 5:9 ~ 2,
              AGE %in% 10:14 ~ 3,
              AGE %in% 15:19 ~ 4,
              AGE %in% 20:24 ~ 5,
              AGE %in% 25:29 ~ 6,
              AGE %in% 30:34 ~ 7,
              AGE %in% 35:39 ~ 8,
              AGE %in% 40:44 ~ 9,
              AGE %in% 45:49 ~ 10,
              AGE %in% 50:54 ~ 11,
              AGE %in% 55:59 ~ 12,
              AGE %in% 60:64 ~ 13,
              AGE %in% 65:69 ~ 14,
              AGE %in% 70:74 ~ 15,
              AGE %in% 75:79 ~ 16,
              AGE %in% 80:84 ~ 17,
              AGE == 85 ~ 18
            )) %>%
            tidyr::pivot_longer(
              dplyr::contains("POPESTIMATE"),
              names_to = "year",
              values_to = "value",
              names_prefix = "POPESTIMATE"
            ) %>%
            dplyr::mutate(year = as.integer(year))

        } else if (geography == "county") {

          if (vintage > 2023) {
            rlang::abort("The county characteristics dataset for this vintage has not yet been released.")
          }

          if (!is.null(state)) {
            state <- validate_state(state)

            county_raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/counties/asrh/cc-est%s-alldata-%s.csv", vintage, vintage, state)))

          } else {
            county_raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/counties/asrh/cc-est%s-alldata.csv", vintage, vintage)))
          }



          if (!is.null(county)) {
            county <- purrr::map_chr(county, function(x) {
              validate_county(state, x)
            })
            county_raw <- dplyr::filter(county_raw, COUNTY %in% county)
          }


          total_vals <- c("TOT", "WA", "BA", "IA", "AA", "NA", "TOM", "WAC", "BAC", "IAC", "AAC", "NAC")

          parsed <- county_raw %>%
            tidyr::pivot_longer(
              TOT_POP:HNAC_FEMALE,
              names_to = c("category", "SEX"),
              values_to = "value",
              names_sep = "_"
            ) %>%
            dplyr::mutate(
              category = ifelse(
                category %in% total_vals,
                paste0("BH", category),
                category
              ),
              category = stringr::str_replace(category, "H", "H_")
            ) %>%
            tidyr::separate_wider_delim(category, delim = "_",
                                        names = c("HISP", "RACE")) %>%
            dplyr::filter(SEX != "POP") %>%
            dplyr::mutate(RACE = ifelse(RACE == "", "TOT", RACE)) %>%
            dplyr::mutate(HISP = dplyr::case_when(
              HISP == "BH" ~ 0L,
              HISP == "H" ~ 2L,
              HISP == "NH" ~ 1L
            ),
            RACE = dplyr::case_when(
              RACE == "TOT" ~ 0L,
              RACE == "WA" ~ 1L,
              RACE == "BA" ~ 2L,
              RACE == "IA" ~ 3L,
              RACE == "AA" ~ 4L,
              RACE == "NA" ~ 5L,
              RACE == "TOM" ~ 6L,
              RACE == "WAC" ~ 7L,
              RACE == "BAC" ~ 8L,
              RACE == "IAC" ~ 9L,
              RACE == "AAC" ~ 10L,
              RACE == "NAC" ~ 11L,
            ),
            SEX = dplyr::case_when(
              SEX == "MALE" ~ 1L,
              SEX == "FEMALE" ~ 2L
            ),
            GEOID = paste0(STATE, COUNTY),
            NAME = paste0(CTYNAME, ", ", STNAME)
            ) %>%
            dplyr::rename(AGEGROUP = AGEGRP) %>%
            dplyr::select(GEOID, NAME,
                          YEAR:value) %>%
            dplyr::rename(year = YEAR) %>%
            dplyr::filter(year != 1) %>%
            dplyr::mutate(year = dplyr::case_when(
              year == 2 ~ 2020L,
              year == 3 ~ 2021L,
              year == 4 ~ 2022L,
              year == 5 ~ 2023L
            ))

        } else {
          rlang::abort("The only available geographies for population characteristics years 2020 and later are 'state' and 'county'.")
        }

        # Handle timeseries
        if (!time_series) {
          in_year <- year

          parsed <- dplyr::filter(parsed, year == in_year)
        }

        if (!is.null(breakdown)) {
          parsed <- dplyr::filter(parsed, RACE < 7)

          grouping_vars <- c("GEOID", "NAME", "year", breakdown)

          # Avoid double-counting
          if (!"SEX" %in% grouping_vars) {
            parsed <- dplyr::filter(parsed, SEX != 0)
          }

          if (!"HISP" %in% grouping_vars) {
            parsed <- dplyr::filter(parsed, HISP != 0)
          }

          if (!"RACE" %in% grouping_vars) {
            parsed <- dplyr::filter(parsed, RACE != 0)
          }

          if (!"AGEGROUP" %in% grouping_vars) {
            parsed <- dplyr::filter(parsed, AGEGROUP != 0)
          }

          output <- suppressMessages(parsed %>%
                                       dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
                                       dplyr::summarize(value = sum(value, na.rm = TRUE)) %>%
                                       dplyr::ungroup()
          )

          if (breakdown_labels) {
            if ("AGEGROUP" %in% names(output)) {
              output$AGEGROUP <- factor(output$AGEGROUP)
              output$AGEGROUP <- dplyr::recode(output$AGEGROUP,
                                               `0` = "All ages",
                                               `1` = "Age 0 to 4 years",
                                               `2` = "Age 5 to 9 years",
                                               `3` = "Age 10 to 14 years",
                                               `4` = "Age 15 to 19 years", `5` = "Age 20 to 24 years",
                                               `6` = "Age 25 to 29 years",
                                               `7` = "Age 30 to 34 years", `8` = "Age 35 to 39 years",
                                               `9` = "Age 40 to 44 years",
                                               `10` = "Age 45 to 49 years", `11` = "Age 50 to 54 years",
                                               `12` = "Age 55 to 59 years",
                                               `13` = "Age 60 to 64 years", `14` = "Age 65 to 69 years",
                                               `15` = "Age 70 to 74 years",
                                               `16` = "Age 75 to 79 years", `17` = "Age 80 to 84 years",
                                               `18` = "Age 85 years and older")
            }

            if ("SEX" %in% names(output)) {
              output$SEX <- dplyr::recode(output$SEX, `0` = "Both sexes", `1` = "Male", `2` = "Female")
            }

            if ("RACE" %in% names(output)) {
              output$RACE <- dplyr::recode(output$RACE, `0` = "All races",
                                           `1` = "White alone",
                                           `2` = "Black alone",
                                           `3` = "American Indian and Alaska Native alone",
                                           `4` = "Asian alone",
                                           `5` = "Native Hawaiian and Other Pacific Islander alone",
                                           `6` = "Two or more races",
              )
            }

            if ("HISP" %in% names(output)) {
              output$HISP <- dplyr::recode(output$HISP, `0` = "Both Hispanic Origins",
                                           `1` = "Non-Hispanic",
                                           `2` = "Hispanic")
            }
          }

          return(output)
        } else {
          return(parsed)
        }



      } else if (product == "population" || product == "components" || is.null(product)) {

        if (!is.null(product)) {
          if (product == "population") {
            variables <- population_estimates_variables22
          } else if (product == "components") {
            if (year == 2021) {
              variables <- components_estimates_variables21

            } else {
              variables <- components_estimates_variables22

            }
          }
        }

        # Get the data into a reasonable first format that is consistent for downstream use
        if (geography == "us") {

          if (vintage == 2021) {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/state/totals/NST-EST%s-alldata.csv", vintage, vintage))) %>%
              dplyr::filter(SUMLEV == "010")
          } else {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/state/totals/NST-EST%s-ALLDATA.csv", vintage, vintage))) %>%
              dplyr::filter(SUMLEV == "010")
          }

          raw <- raw[,!(names(raw) %in% c("SUMLEV", "REGION", "DIVISION", "STATE"))]

          base <- raw %>%
            dplyr::mutate(GEOID = "1") %>%
            dplyr::select(GEOID, NAME, dplyr::everything()) %>%
            tidyr::pivot_longer(-c(GEOID, NAME), names_to = c("variable", "year"),
                                names_pattern = "(\\D+)(\\d+)",
                                values_to = "value") %>%
            dplyr::mutate(variable = stringr::str_remove(variable, "_"))



        } else if (geography == "region") {

          if (vintage == 2021) {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/state/totals/NST-EST%s-alldata.csv", vintage, vintage))) %>%
              dplyr::filter(SUMLEV == "020") %>%
              dplyr::mutate(GEOID = REGION)
          } else {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/state/totals/NST-EST%s-ALLDATA.csv", vintage, vintage))) %>%
              dplyr::filter(SUMLEV == "020") %>%
              dplyr::mutate(GEOID = REGION)
          }

          raw <- raw[,!(names(raw) %in% c("SUMLEV", "REGION", "DIVISION", "STATE"))]

          base <- raw %>%
            dplyr::select(GEOID, NAME, dplyr::everything()) %>%
            tidyr::pivot_longer(-c(GEOID, NAME), names_to = c("variable", "year"),
                                names_pattern = "(\\D+)(\\d+)",
                                values_to = "value") %>%
            dplyr::mutate(variable = stringr::str_remove(variable, "_"))

        } else if (geography == "division") {

          if (vintage == 2021) {
            rlang::abort("Divisions are not available in the 2021 vintage dataset.")
          } else {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/state/totals/NST-EST%s-ALLDATA.csv", vintage, vintage))) %>%
              dplyr::filter(SUMLEV == "030") %>%
              dplyr::mutate(GEOID = DIVISION)
          }

          raw <- raw[,!(names(raw) %in% c("SUMLEV", "REGION", "DIVISION", "STATE"))]

          base <- raw %>%
            dplyr::select(GEOID, NAME, dplyr::everything()) %>%
            tidyr::pivot_longer(-c(GEOID, NAME), names_to = c("variable", "year"),
                                names_pattern = "(\\D+)(\\d+)",
                                values_to = "value") %>%
            dplyr::mutate(variable = stringr::str_remove(variable, "_"))

        } else if (geography == "state") {

          if (vintage == 2021) {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/state/totals/NST-EST%s-alldata.csv", vintage, vintage))) %>%
              dplyr::filter(SUMLEV == "040") %>%
              dplyr::mutate(GEOID = STATE)
          } else {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/state/totals/NST-EST%s-ALLDATA.csv", vintage, vintage))) %>%
              dplyr::filter(SUMLEV == "040") %>%
              dplyr::mutate(GEOID = STATE)
          }

          raw <- raw[,!(names(raw) %in% c("SUMLEV", "REGION", "DIVISION", "STATE"))]

          base <- raw %>%
            dplyr::select(GEOID, NAME, dplyr::everything()) %>%
            tidyr::pivot_longer(-c(GEOID, NAME), names_to = c("variable", "year"),
                                names_pattern = "(\\D+)(\\d+)",
                                values_to = "value") %>%
            dplyr::mutate(variable = stringr::str_remove(variable, "_"))

        } else if (geography == "county") {

          raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/counties/totals/co-est%s-alldata.csv", vintage, vintage))) %>%
            dplyr::filter(SUMLEV == "050") %>%
            dplyr::mutate(GEOID = paste0(STATE, COUNTY),
                          NAME = paste0(CTYNAME, ", ", STNAME))

          raw <- raw[,!(names(raw) %in% c("SUMLEV", "REGION", "DIVISION", "STATE",
                                          "COUNTY", "STNAME", "CTYNAME"))]

          base <- raw %>%
            dplyr::select(GEOID, NAME, dplyr::everything()) %>%
            tidyr::pivot_longer(-c(GEOID, NAME), names_to = c("variable", "year"),
                                names_pattern = "(\\D+)(\\d+)",
                                values_to = "value") %>%
            dplyr::mutate(variable = stringr::str_remove(variable, "_"))

        } else if (geography == "cbsa" || geography == "metropolitan statistical area/micropolitan statistical area") {

          if (vintage != 2022) {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/metro/totals/cbsa-est%s-alldata.csv", vintage, vintage))) %>%
              dplyr::filter(LSAD %in% c("Micropolitan Statistical Area", "Metropolitan Statistical Area")) %>%
              dplyr::mutate(GEOID = CBSA)
          } else {
            raw <- suppressMessages(readr::read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/metro/totals/cbsa-est2022.csv")) %>%
              dplyr::filter(LSAD %in% c("Micropolitan Statistical Area", "Metropolitan Statistical Area")) %>%
              dplyr::mutate(GEOID = CBSA)
          }

          raw <- raw[,!(names(raw) %in% c("MDIV", "STCOU", "LSAD", "CBSA"))]

          base <- raw %>%
            dplyr::select(GEOID, NAME, dplyr::everything()) %>%
            tidyr::pivot_longer(-c(GEOID, NAME), names_to = c("variable", "year"),
                                names_pattern = "(\\D+)(\\d+)",
                                values_to = "value") %>%
            dplyr::mutate(variable = stringr::str_remove(variable, "_"))

        } else if (geography == "combined statistical area") {

          if (vintage != 2022) {
            raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/metro/totals/csa-est%s-alldata.csv", vintage, vintage))) %>%
              dplyr::filter(LSAD == "Combined Statistical Area") %>%
              dplyr::mutate(GEOID = CSA)
          } else {
            raw <- suppressMessages(readr::read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/metro/totals/csa-est2022.csv")) %>%
              dplyr::filter(LSAD == "Combined Statistical Area") %>%
              dplyr::mutate(GEOID = CSA)
          }

          raw <- raw[,!(names(raw) %in% c("MDIV", "STCOU", "LSAD", "CBSA", "CSA"))]

          base <- raw %>%
            dplyr::select(GEOID, NAME, dplyr::everything()) %>%
            tidyr::pivot_longer(-c(GEOID, NAME), names_to = c("variable", "year"),
                                names_pattern = "(\\D+)(\\d+)",
                                values_to = "value") %>%
            dplyr::mutate(variable = stringr::str_remove(variable, "_"))

        } else if (geography == "place") {

          # if (vintage > 2022) {
          #   rlang::abort("The most recent PEP release for this geography is 2022.")
          # }

          raw <- suppressMessages(readr::read_csv(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/cities/totals/sub-est%s.csv", vintage, vintage))) %>%
            dplyr::filter(SUMLEV == "162") %>%
            dplyr::mutate(GEOID = paste0(STATE, PLACE),
                          NAME = paste0(NAME, ", ", STNAME))

          raw <- raw[,!(names(raw) %in% c("SUMLEV", "PLACE", "COUSUB", "STATE",
                                          "COUNTY", "CONCIT", "PRIMGEO_FLAG", "STNAME",
                                          "FUNCSTAT"))]

          base <- raw %>%
            dplyr::select(GEOID, NAME, dplyr::everything()) %>%
            tidyr::pivot_longer(-c(GEOID, NAME), names_to = c("variable", "year"),
                                names_pattern = "(\\D+)(\\d+)",
                                values_to = "value") %>%
            dplyr::mutate(variable = stringr::str_remove(variable, "_"))
        } else {
          rlang::abort("Your requested geography is not currently available for this Population Estimates Program dataset. Please modify your request.")
        }

        base$year <- as.integer(base$year)
        year_to_keep <- as.integer(year)
        base$GEOID <- as.character(base$GEOID)

        # Explain available variables
        if (!all(variables %in% unique(base$variable))) {
          vars <- unique(base$variable)

          rlang::abort(stringr::str_wrap(paste0("You have requested one or more variables not currently available in this PEP dataset.\nAvailable variables are as follows:\n", paste(vars, collapse = ", ")), 50))
        }

        # Use `variables = 'all'`
        if (all(variables == "all")) {
          variables <- unique(base$variable)
        }

        # Get the requested variables and handle time series if so
        if (time_series) {
          pep_sub <- base %>%
            dplyr::mutate(year = as.integer(year)) %>%
            dplyr::filter(variable %in% variables,
                          year <= year_to_keep)
        } else {
          pep_sub <- base %>%
            dplyr::mutate(year = as.integer(year)) %>%
            dplyr::filter(variable %in% variables,
                          year == year_to_keep)
        }

        # Handle state / county filters
        if (!is.null(state)) {
          if (geography %in% c("us", "region", "division", "cbsa", "metropolitan statistical area/micropolitan statistical area", "combined statistical area")) {
            rlang::abort("The `state` argument is not available for your chosen geography.")
          }

          if (is.null(county)) {
            state <- purrr::map_chr(state, function(x) {
              validate_state(x)
            })

            pep_sub <- pep_sub %>%
              dplyr::filter(stringr::str_sub(GEOID, 1, 2) %in% state)

          } else {
            state <- purrr::map_chr(state, function(x) {
              validate_state(x)
            })

            county <- map_chr(county, function(x) {
              validate_county(state, x)
            })

            pep_sub <- pep_sub %>%
              dplyr::filter(stringr::str_sub(GEOID, 1, 2) %in% state,
                            stringr::str_sub(GEOID, 3, 5) %in% county)

          }

        }

        if (output == "wide") {

          if (time_series) {
            dat2 <- pep_sub %>%
              tidyr::pivot_wider(id_cols = c("GEOID", "NAME"),
                                 names_from = c("variable", "year"),
                                 names_sep = "",
                                 values_from = "value")
          } else {
            dat2 <- pep_sub %>%
              tidyr::pivot_wider(id_cols = c(GEOID, NAME),
                                 names_from = "variable",
                                 names_sep = "",
                                 values_from = "value")
          }


        } else {
          dat2 <- pep_sub
        }
      } else {
      rlang::abort("Invalid product; use 'population', 'components', or 'characteristics', or leave NULL.")
    }

  } else {
    # Check for a Census API key and warn if missing
    key <- get_census_api_key(key)

    if (geography == "cbsa") geography <- "metropolitan statistical area/micropolitan statistical area"

    insist_get_estimates <- purrr::insistently(get_estimates)

    # Allow for characteristics products to be pulled for the entire US by county
    # Come back to this later
    if (!is.null(product)) {

      if (is.null(state) && geography == "county" && product == "characteristics") {

        message("Fetching characteristics data by state and combining the result.")

        state <- unique(fips_codes$state_code)[1:51]
        # mc <- match.call(expand.dots = TRUE)
        if (geometry) {
          result <- map(state,~{
            suppressMessages(
              insist_get_estimates(geography = geography,
                                   product = product,
                                   variables = variables,
                                   breakdown = breakdown,
                                   breakdown_labels = breakdown_labels,
                                   year = vintage,
                                   state = .x,
                                   county = county,
                                   time_series = time_series,
                                   output = output,
                                   geometry = geometry,
                                   keep_geo_vars = keep_geo_vars,
                                   shift_geo = shift_geo,
                                   key = key,
                                   show_call = show_call))
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
            suppressMessages(
              insist_get_estimates(geography = geography,
                                   product = product,
                                   variables = variables,
                                   breakdown = breakdown,
                                   breakdown_labels = breakdown_labels,
                                   year = year,
                                   state = .x,
                                   county = county,
                                   time_series = time_series,
                                   output = output,
                                   geometry = geometry,
                                   keep_geo_vars = keep_geo_vars,
                                   shift_geo = shift_geo,
                                   key = key,
                                   show_call = show_call))
          })
        }
        return(result)

      }

    }

    if (!is.null(product) && product == "characteristics") {
      product <- "charagegroups"
    }

    # If the product is "characteristics", we'll need to do some unique things
    if (!is.null(product) && product == "charagegroups") {
      if (!is.null(variables)) {
        stop("Use the `breakdown` argument instead of `variables` when requesting population characteristics.", call. = FALSE)
      }
      if (!is.null(breakdown)) {
        variables <- c("POP", breakdown)
      } else {
        stop("Please specify the population breakdown in a vector.  Options include 'SEX', 'AGEGROUP', 'RACE', and 'HISP'.", call. = FALSE)
      }
    }

    # For a variables vector, check to see if the variables cut across multiple products
    if (!is.null(variables) && length(variables) > 1) {

      if (!is.null(product) && product != "charagegroups") {
        check <- c(any(variables %in% population_estimates_variables),
                   any(variables %in% components_estimates_variables),
                   any(variables %in% housing_estimates_variables))
      } else {
        check <- FALSE
      }

      # if there is more than one TRUE, grab data by variable
      if (length(which(check)) > 1) {
        dat <- map_dfc(variables, function(eachvar) {
          load_data_estimates(geography = geography, product = NULL, variables = eachvar,
                              year = year, state = state, county = county,
                              time_series = time_series, key = key, show_call = show_call)
        })

        # Remove any extra GEOID or GEONAME columns
        dat <- dat[, -grep("GEOID[0-9]|GEONAME[0-9]|NAME[0-9]", colnames(dat))]

      } else {
        dat <- load_data_estimates(geography = geography, product = product,
                                   variables = variables,
                                   year = year, state = state,
                                   time_series = time_series,
                                   county = county, key = key, show_call = show_call)
      }
    } else {
      dat <- load_data_estimates(geography = geography, product = product,
                                 variables = variables,
                                 year = year, state = state,
                                 time_series = time_series,
                                 county = county, key = key, show_call = show_call)
    }

    if (!is.null(product) && product == "charagegroups") {
      output <- "wide"
    }

    if ("PERIOD_CODE" %in% names(dat)) {
      dat <- rename(dat, PERIOD = PERIOD_CODE)
    }

    if ("DATE_CODE" %in% names(dat)) {
      dat <- rename(dat, DATE = DATE_CODE)
    }

    if ("DATE_" %in% names(dat)) {
      dat <- rename(dat, DATE = DATE_)
    }

    if (output == "tidy") {

      if (time_series) {
        if ("PERIOD" %in% names(dat)) {
          if ("GEONAME" %in% names(dat)) {
            dat2 <- dat %>%
              rename(NAME = GEONAME) %>%
              gather(key = variable, value = value, -GEOID, -NAME, -PERIOD)
          } else {
            dat2 <- dat %>%
              gather(key = variable, value = value, -GEOID, -NAME, -PERIOD)
          }

        } else {
          if ("GEONAME" %in% names(dat)) {
            dat2 <- dat %>%
              rename(NAME = GEONAME) %>%
              gather(key = variable, value = value, -GEOID, -NAME, -DATE)
          } else {
            dat2 <- dat %>%
              gather(key = variable, value = value, -GEOID, -NAME, -DATE)
          }
        }
      } else {

        if ("GEONAME" %in% names(dat)) {
          dat2 <- dat %>%
            rename(NAME = GEONAME) %>%
            gather(key = variable, value = value, -GEOID, -NAME)
        } else {
          dat2 <- dat %>%
            gather(key = variable, value = value, -GEOID, -NAME)
        }


      }

      if (!is.null(names(variables))) {
        for (i in 1:length(variables)) {
          dat2[dat2 == variables[i]] <- names(variables)[i]
        }
      }

    } else if (output == "wide") {

      dat <- dat[!duplicated(names(dat), fromLast = TRUE)]

      dat2 <- dat

      if (!is.null(product) && product == "charagegroups") {
        dat2 <- rename(dat2, value = POP)
      }

      # Handle recodes for breakdown labels if requested
      if (breakdown_labels) {
        if (is.null(breakdown)) {
          stop("A breakdown of population characteristics must be specified for this option to be used.", call. = FALSE)
        }

        if ("AGEGROUP" %in% names(dat2)) {
          dat2$AGEGROUP <- factor(dat2$AGEGROUP)
          dat2$AGEGROUP <- recode(dat2$AGEGROUP,
                                  `0` = "All ages", `1` = "Age 0 to 4 years",
                                  `2` = "Age 5 to 9 years",
                                  `3` = "Age 10 to 14 years",
                                  `4` = "Age 15 to 19 years", `5` = "Age 20 to 24 years",
                                  `6` = "Age 25 to 29 years",
                                  `7` = "Age 30 to 34 years", `8` = "Age 35 to 39 years",
                                  `9` = "Age 40 to 44 years",
                                  `10` = "Age 45 to 49 years", `11` = "Age 50 to 54 years",
                                  `12` = "Age 55 to 59 years",
                                  `13` = "Age 60 to 64 years", `14` = "Age 65 to 69 years",
                                  `15` = "Age 70 to 74 years",
                                  `16` = "Age 75 to 79 years", `17` = "Age 80 to 84 years",
                                  `18` = "Age 85 years and older",
                                  `19` = "Under 18 years", `20` = "5 to 13 years",
                                  `21` = "14 to 17 years", `22` = "18 to 64 years",
                                  `23` = "18 to 24 years", `24` = "25 to 44 years",
                                  `25` = "45 to 64 years", `26` = "65 years and over",
                                  `27` = "85 years and over", `28` = "16 years and over",
                                  `29` = "18 years and over",
                                  `30` = "15 to 44 years", `31` = "Median age")
        }

        if ("SEX" %in% names(dat2)) {
          dat2$SEX <- recode(dat2$SEX, `0` = "Both sexes", `1` = "Male", `2` = "Female")
        }

        if ("RACE" %in% names(dat2)) {
          dat2$RACE <- recode(dat2$RACE, `0` = "All races", `1` = "White alone",
                              `2` = "Black alone",
                              `3` = "American Indian and Alaska Native alone",
                              `4` = "Asian alone",
                              `5` = "Native Hawaiian and Other Pacific Islander alone",
                              `6` = "Two or more races",
                              `7` = "White alone or in combination",
                              `8` = "Black alone or in combination",
                              `9` = "American Indian and Alaska Native alone or in combination",
                              `10` = "Asian alone or in combination",
                              `11` = "Native Hawaiian and Other Pacific Islander alone or in combination")
        }

        if ("HISP" %in% names(dat2)) {
          dat2$HISP <- recode(dat2$HISP, `0` = "Both Hispanic Origins",
                              `1` = "Non-Hispanic",
                              `2` = "Hispanic")
        }

      }

      if (!is.null(names(variables))) {
        for (i in 1:length(variables)) {
          names(dat2) <- str_replace(names(dat2), variables[i], names(variables)[i])
        }
      }

      if ("GEONAME" %in% names(dat2)) {
        dat2 <- dat2 %>%
          select(GEOID, NAME = GEONAME, everything())
      } else {
        dat2 <- dat2 %>%
          select(GEOID, NAME, everything())
      }



    }
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

        if (year > 2014) {
          # Account for change from Shannon County, SD to Oglala Lakota County
          # and the new Kusilvak Census Area in AK
          geom$GEOID[geom$GEOID == "46113"] <- "46102"
          geom$GEOID[geom$GEOID == "02270"] <- "02158"
        }

      } else {
        stop("`shift_geo` is only available for states and counties", call. = FALSE)
      }

    } else {

      # Handle here until 2023 CB files are released
      if (year == 2023) {
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
