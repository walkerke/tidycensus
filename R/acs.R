#' Obtain data and feature geometry for the five-year American Community Survey
#'
#' @param geography The geography of your data.
#' @param variables Character string or vector of character strings of variable
#'                  IDs. tidycensus automatically returns the estimate and the
#'                  margin of error associated with the variable.
#' @param table   The ACS table for which you would like to request all variables.  Uses
#'                lookup tables to identify the variables; performs faster when variable
#'                table already exists through \code{load_variables(cache = TRUE)}.
#' @param cache_table Whether or not to cache table names for faster future access.
#'                    Defaults to FALSE; if TRUE, only needs to be called once per
#'                    dataset.  If variables dataset is already cached via the
#'                    \code{load_variables} function, this can be bypassed.
#' @param year The year, or endyear, of the ACS sample. 2010 through 2016 are
#'                available. Defaults to 2016.
#' @param endyear Deprecated and will be removed in a future release.
#' @param output One of "tidy" (the default) in which each row represents an
#'               enumeration unit-variable combination, or "wide" in which each
#'               row represents an enumeration unit and the variables are in the
#'               columns.
#' @param state An optional vector of states for which you are requesting data. State
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
#' @param shift_geo if TRUE, returns geometry with Alaska and Hawaii shifted for thematic mapping of the entire US.
#'                  Geometry was originally obtained from the albersusa R package.
#' @param summary_var Character string of a "summary variable" from the ACS
#'                    to be included
#'                    in your output. Usually a variable (e.g. total population)
#'                    that you'll want to use as a denominator or comparison.
#' @param key Your Census API key.
#'            Obtain one at \url{http://api.census.gov/data/key_signup.html}
#' @param moe_level The confidence level of the returned margin of error.  One of 90 (the default), 95, or 99.
#' @param survey The ACS contains one-year, three-year, and five-year surveys expressed as "acs1", "acs3", and "acs5".
#'               The default selection is "acs5."
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
#'        subtitle = "2012-2016 American Community Survey",
#'        y = "",
#'        x = "ACS estimate (bars represent margin of error)")
#'
#' }
#' @export
get_acs <- function(geography, variables = NULL, table = NULL, cache_table = FALSE,
                    year = 2017, endyear = NULL,
                    output = "tidy",
                    state = NULL, county = NULL, geometry = FALSE, keep_geo_vars = FALSE,
                    shift_geo = FALSE,
                    summary_var = NULL, key = NULL, moe_level = 90, survey = "acs5", ...) {

  if (!is.null(endyear)) {
    year <- endyear
    message("The `endyear` parameter is deprecated and will be removed in a future release.  Please use `year` instead.")
  }

  if (survey == "acs1") {
    message(sprintf("Getting data from the %s 1-year ACS", year))
  } else if (survey == "acs3") {
    startyear <- year - 2
    message(sprintf("Getting data from the %s-%s 3-year ACS", startyear, year))
  } else if (survey == "acs5") {
    startyear <- year - 4
    message(sprintf("Getting data from the %s-%s 5-year ACS", startyear, year))
  }

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }


  if (geography == "block") {
    stop("Block data are not available in the ACS. Use `get_decennial()` to access block data from the 2010 Census.", call. = FALSE)
  }

  if (survey == "acs3") {
    if (year > 2013) {
      stop("The three-year ACS ended in 2013. For newer data, use the 1-year or 5-year ACS.", call. = FALSE)
    } else {
      message("The three-year ACS provides data for geographies with populations of 20,000 and greater.")
    }
  }

  if (survey == "acs1") {
    if (year < 2012) {
      stop("The acs1 data is currently available beginning in 2012. Please select a different year.", call. = FALSE)
    }
    message("The one-year ACS provides data for geographies with populations of 65,000 and greater.")
  }

  cache <- getOption("tigris_use_cache", FALSE)

  if (geometry) {

    if (shift_geo) {
      message("Using feature geometry obtained from the albersusa package")
    } else if (!shift_geo && !cache) {
      message("Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.")
    }

  }

  if (shift_geo && !geometry) {
    stop("`shift_geo` is only available when requesting feature geometry with `geometry = TRUE`",
         call. = FALSE)
  }

  if (is.null(variables) && is.null(table)) {
    stop("Either a vector of variables or an ACS table must be specified.", call. = FALSE)
  }

  if (!is.null(variables) && !is.null(table)) {
    stop("Specify variables or a table to retrieve; they cannot be combined.",
         call. = FALSE)
  }

  if (geography == "zcta") geography <- "zip code tabulation area"

  if (geography == "zip code tabulation area" && (!is.null(state) || !is.null(county))) {
    stop("ZCTAs can only be requested for the entire country, not within states or counties.",
         call. = FALSE)
  }

  # Allow users to get all block groups in a state

  if ((geography == "block group" && is.null(county)) || (geography == "tract" && is.null(county) && year < 2015)) {
    st <- suppressMessages(validate_state(state))

    # Get year-specific county IDs from tigris
    if (year < 2013) {
      tigris_yr <- 2010
    } else {
      tigris_yr <- year
    }

    cty_year <- suppressMessages(counties(state = st, cb = TRUE,
                                          resolution = "20m", year = tigris_yr, class = "sf"))

    county <- cty_year$COUNTYFP


  }

  # If more than one state specified for tracts - or more than one county
  # for block groups - take care of this under the hood by having the function
  # call itself and return the result
  if (geography == "tract" && length(state) > 1 && year > 2014) {
    mc <- match.call(expand.dots = TRUE)
    if (geometry) {
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

  if ((geography == "block group" && length(county) > 1) || (geography == "tract" && length(county) > 1 && year < 2015)) {
    mc <- match.call(expand.dots = TRUE)
    if (geometry) {
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

  if (geography == "block group" && length(county) > 1) {
    mc <- match.call(expand.dots = TRUE)
    if (geometry) {
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

  # Logic for fetching data tables
  if (!is.null(table)) {
    if (grepl("^S[0-9].", table)) {
      survey2 <- paste0(survey, "/subject")
    } else if (grepl("^DP[0-9].", table)) {
      survey2 <- paste0(survey, "/profile")
    } else {
      survey2 <- survey
    }
    variables <- variables_from_table_acs(table, year, survey2, cache_table)
  }


  # Allow for as many variables in a call as desired
  if (length(variables) > 24) {
    l <- split(variables, ceiling(seq_along(variables) / 24))

    dat <- map(l, function(x) {
      vars <- format_variables_acs(x)
      suppressWarnings(load_data_acs(geography, vars, key, year, state, county, survey))
    }) %>%
      bind_cols()
  } else {
    vars <- format_variables_acs(variables)

    dat <- suppressWarnings(load_data_acs(geography, vars, key, year, state, county, survey))
  }

  vars2 <- format_variables_acs(variables)

  var_vector <- unlist(strsplit(vars2, split = ","))

  if (output == "tidy") {

    sub <- dat[c("GEOID", "NAME", var_vector)]

    if (packageVersion("tidyr") > "0.7.2") {
      dat2 <- sub %>%
        gather(key = variable, value = value, -GEOID, -NAME) %>%
        separate(variable, into = c("variable", "type"), sep = -1) %>%
        mutate(type = ifelse(type == "E", "estimate", "moe")) %>%
        spread(type, value)
    } else {
      dat2 <- sub %>%
        gather(key = variable, value = value, -GEOID, -NAME) %>%
        separate(variable, into = c("variable", "type"), sep = -2) %>%
        mutate(type = ifelse(type == "E", "estimate", "moe")) %>%
        spread(type, value)
    }



    if ("moe" %in% names(dat2)) {
      dat2 <- mutate(dat2, moe = moe * moe_factor)
    }


    # Convert missing values to NA
    dat2[dat2 == -111111111] <- NA
    dat2[dat2 == -222222222] <- NA
    dat2[dat2 == -333333333] <- NA
    dat2[dat2 == -444444444] <- NA
    dat2[dat2 == -555555555] <- NA
    dat2[dat2 == -666666666] <- NA
    dat2[dat2 == -777777777] <- NA
    dat2[dat2 == -888888888] <- NA
    dat2[dat2 == -999999999] <- NA

    # Change names if supplied
    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        dat2[dat2 == variables[i]] <- names(variables)[i]
      }
    }


  } else if (output == "wide") {

    # Remove duplicated columns

    dat <- dat[!duplicated(names(dat), fromLast = TRUE)]
    dat <- dat[c("GEOID", "NAME", var_vector)]


    # Convert missing values to NA
    dat[dat == -111111111] <- NA
    dat[dat == -222222222] <- NA
    dat[dat == -333333333] <- NA
    dat[dat == -444444444] <- NA
    dat[dat == -555555555] <- NA
    dat[dat == -666666666] <- NA
    dat[dat == -777777777] <- NA
    dat[dat == -888888888] <- NA
    dat[dat == -999999999] <- NA

    # Find MOE vars
    # moe_vars <- grep("*M", names(dat))

    # dat[[moe_vars]] <- apply(dat[[moe_vars]], 2, function(x) round(x * moe_factor, 0))

    moex <- function(x) x * moe_factor

    dat2 <- dat %>%
      mutate_if(grepl("*M$", names(.)), funs(moex))

    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        names(dat2) <- str_replace(names(dat2), variables[i], names(variables)[i])
      }
    }

    dat2 <- dat2 %>%
      select(GEOID, NAME, everything())

  }

  if (!is.null(summary_var)) {

    sumvar <- format_variables_acs(summary_var)

    sumdat <- suppressMessages(load_data_acs(geography, sumvar, key, year, state, county, survey))

    sumest <- paste0(summary_var, "E")

    summoe <- paste0(summary_var, "M")

    dat2 <- dat2 %>%
      inner_join(sumdat, by = "GEOID") %>%
      rename(summary_est = !! sumest,
             summary_moe = !! summoe,
             NAME = "NAME.x") %>%
      select(-NAME.y) %>%
      mutate(summary_moe = round(summary_moe * moe_factor, 0))

    # Convert missing values to NA
    dat2[dat2 == -111111111] <- NA
    dat2[dat2 == -222222222] <- NA
    dat2[dat2 == -333333333] <- NA
    dat2[dat2 == -444444444] <- NA
    dat2[dat2 == -555555555] <- NA
    dat2[dat2 == -666666666] <- NA
    dat2[dat2 == -777777777] <- NA
    dat2[dat2 == -888888888] <- NA
    dat2[dat2 == -999999999] <- NA
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

      geom <- suppressMessages(use_tigris(geography = geography, year = year,
                                          state = state, county = county, ...))
    }

    if (! keep_geo_vars) {

      geom <- select(geom, GEOID, geometry)

    }

    if (shift_geo) {
      out <- inner_join(geom, dat2, by = "GEOID") %>%
        st_as_sf()
    } else {
      out <- right_join(geom, dat2, by = "GEOID") %>%
        st_as_sf()
    }

    return(out)

  } else {

    return(dat2)

  }

}
