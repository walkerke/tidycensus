#' Obtain data and feature geometry for the five-year American Community Survey
#'
#' @param geography The geography of your data.
#' @param variables Character string or vector of character strings of variable
#'                  IDs. tidycensus automatically returns the estimate and the
#'                  margin of error associated with the variable.
#' @param endyear The endyear of the ACS sample.  2010 through 2015 are
#'                available. Defaults to 2015 (for 2011-2015)
#' @param output One of "tidy" (the default) in which each row represents an
#'               enumeration unit-variable combination, or "wide" in which each
#'               row represents an enumeration unit and the variables are in the
#'               columns.
#' @param state The state for which you are requesting data. State
#'              names, postal codes, and FIPS codes are accepted.
#'              Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#'               FIPS codes are accepted. Must be combined with a value supplied
#'               to `state`.  Defaults to NULL.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'                 if TRUE, uses the tigris package to return an sf tibble
#'                 with simple feature geometry in the `geometry` column.  state, county, tract, block group,
#'                 block, and ZCTA geometry are supported.
#' @param keep_geo_vars if TRUE, keeps all the variables from the Census
#'                      shapefile obtained by tigris.  Defaults to FALSE.
#' @param summary_var Character string of a "summary variable" from the ACS
#'                    to be included
#'                    in your output. Usually a variable (e.g. total population)
#'                    that you'll want to use as a denominator or comparison.
#' @param key Your Census API key.
#'            Obtain one at \url{http://api.census.gov/data/key_signup.html}
#' @param moe_level The confidence level of the returned margin of error.  One of 90 (the default), 95, or 99.
#' @param aggregate_years The ACS can be aggregated into one, three, or five year groups. This argument accepts an
#'                        integer or either 1, 3, or 5. The default is 5.
#' @param ... Other keyword arguments
#'
#' @return A tibble or sf tibble of ACS data
#' @examples \dontrun{
#' library(tidycensus)
#' library(tidyverse)
#' library(viridis)
#' census_api_key("YOUR KEY GOES HERE")
#'
#' tarr <- get_acs(geography = "tract", variables = "B19013_001",
#'                 state = "TX", county = "Tarrant", geometry = TRUE)
#'
#' ggplot(tarr, aes(fill = estimate, color = estimate)) +
#'   geom_sf() +
#'   coord_sf(crs = 26914) +
#'   scale_fill_viridis(option = "magma") +
#'   scale_color_viridis(options = "magma")
#'
#'
#' vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT")
#'
#' vt %>%
#' mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
#'  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
#'   geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
#'   geom_point(color = "red", size = 3) +
#'   labs(title = "Household income by county in Vermont",
#'        subtitle = "2011-2015 American Community Survey",
#'        y = "",
#'        x = "ACS estimate (bars represent margin of error)")
#'
#' }
#' @export
get_acs <- function(geography, variables, endyear = 2015, output = "tidy",
                    state = NULL, county = NULL, geometry = FALSE, keep_geo_vars = FALSE,
                    summary_var = NULL, key = NULL, moe_level = 90, aggregate_years = 5, ...) {

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }
  #
  #   if (length(variables) > 24) {
  #     stop("The maximum number of variables supported by `get_acs` at one time is 25 at the moment.  I'm working on a fix; in the meantime consider splitting your variables into multiple calls and using cbind/rbind to combine them.",
  #          call. = FALSE)
  #   }
  #

  if (aggregate_years < 5) {
    if (geography == "block group") {
      warning("The acs1 and acs3 surveys do not support block group geographies. Please select 'acs5' for block groups.")
    }
  }

  if (geography == "zcta") geography <- "zip code tabulation area"

  # If more than one state specified for tracts - or more than one county
  # for block groups - take care of this under the hood by having the function
  # call itself and return the result
  if (geography == "tract" & length(state) > 1) {
    mc <- match.call(expand.dots = TRUE)
    if (geometry == TRUE) {
      result <- map(state, function(x) {
        mc[["state"]] <- x
        eval(mc)
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
      result <- map_df(state, function(x) {
        mc[["state"]] <- x
        eval(mc)
      })
    }
    return(result)
  }

  if (geography == "block group" & length(county) > 1) {
    mc <- match.call(expand.dots = TRUE)
    if (geometry == TRUE) {
      result <- map(county, function(x) {
        mc[["county"]] <- x
        eval(mc)
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
      result <- map_df(county, function(x) {
        mc[["county"]] <- x
        eval(mc)
      })
    }
    return(result)
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


  # Allow for as many variables in a call as desired
  if (length(variables) > 24) {
    l <- split(variables, ceiling(seq_along(variables) / 24))

    dat <- map(l, function(x) {
      vars <- format_variables_acs(x)
      suppressWarnings(load_data_acs(geography, vars, key, endyear, state, county, aggregate_years))
    }) %>%
      bind_cols()
  } else {
    vars <- format_variables_acs(variables)

    dat <- suppressWarnings(load_data_acs(geography, vars, key, endyear, state, county, aggregate_years))
  }

  vars2 <- format_variables_acs(variables)

  var_vector <- unlist(strsplit(vars2, split = ","))

  if (output == "tidy") {

    sub <- dat[c("GEOID", "NAME", var_vector)]

    dat2 <- sub %>%
      gather(key = variable, value = value, -GEOID, -NAME) %>%
      separate(variable, into = c("variable", "type"), sep = -2) %>%
      mutate(type = ifelse(type == "E", "estimate", "moe")) %>%
      spread(type, value) %>%
      mutate(moe = moe * moe_factor)


  } else if (output == "wide") {

    # Remove duplicated columns

    dat <- dat[!duplicated(names(dat), fromLast = TRUE)]

    # Find MOE vars
    # moe_vars <- grep("*M", names(dat))

    # dat[[moe_vars]] <- apply(dat[[moe_vars]], 2, function(x) round(x * moe_factor, 0))

    moex <- function(x) x * moe_factor

    dat2 <- dat %>%
      mutate_if(grepl("*M$", names(.)), funs(moex))

  }

  if (!is.null(summary_var)) {

    sumvar <- format_variables_acs(summary_var)

    sumdat <- suppressMessages(load_data_acs(geography, sumvar, key, endyear, state, county, aggregate_years))

    sumest <- paste0(summary_var, "E")

    summoe <- paste0(summary_var, "M")

    dat2 <- dat2 %>%
      inner_join(sumdat, by = "GEOID") %>%
      rename_(summary_est = sumest,
              summary_moe = summoe,
              NAME = "NAME.x") %>%
      select(-NAME.y) %>%
      mutate(summary_moe = round(summary_moe * moe_factor, 0))

  }

  if (geometry == TRUE) {

    geom <- suppressMessages(use_tigris(geography = geography, year = endyear,
                                        state = state, county = county))

    if (keep_geo_vars == FALSE) {

      geom <- select(geom, GEOID, geometry)

    }

    # Merge and return the output
    out <- inner_join(geom, dat2, by = "GEOID") %>%
      as_tibble() %>%
      st_as_sf()

    return(out)

  } else {

    return(dat2)

  }

}
