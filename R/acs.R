#' Obtain data and feature geometry for the American Community Survey
#'
#' @param geography The geography of your data.
#' @param variables Character string or vector of character strings of variable
#'   IDs. tidycensus automatically returns the estimate and the margin of error
#'   associated with the variable.
#' @param table   The ACS table for which you would like to request all
#'   variables. Uses lookup tables to identify the variables; performs faster
#'   when variable table already exists through \code{load_variables(cache =
#'   TRUE)}. Only one table may be requested per call.
#' @param cache_table Whether or not to cache table names for faster future
#'   access. Defaults to FALSE; if TRUE, only needs to be called once per
#'   dataset.  If variables dataset is already cached via the
#'   \code{load_variables} function, this can be bypassed.
#' @param year The year, or endyear, of the ACS sample. 5-year ACS data is
#'   available from 2009 through 2023; 1-year ACS data is available from 2005
#'   through 2023, with the exception of 2020.  Defaults to 2023.
#' @param output One of "tidy" (the default) in which each row represents an
#'   enumeration unit-variable combination, or "wide" in which each row
#'   represents an enumeration unit and the variables are in the columns.
#' @param state An optional vector of states for which you are requesting data.
#'   State names, postal codes, and FIPS codes are accepted. Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#'   FIPS codes are accepted. Must be combined with a value supplied to `state`.
#'   Defaults to NULL.
#' @param zcta The zip code tabulation area(s) for which you are requesting
#'   data. Specify a single value or a vector of values to get data for more
#'   than one ZCTA. Numeric or character ZCTA GEOIDs are accepted. When
#'   specifying ZCTAs, geography must be set to `"zcta"` and `state` must be specified with
#'   `county` left as `NULL`. Defaults to NULL.
#' @param geometry if FALSE (the default), return a regular tibble of ACS data.
#'   if TRUE, uses the tigris package to return an sf tibble with simple feature
#'   geometry in the `geometry` column.
#' @param keep_geo_vars if TRUE, keeps all the variables from the Census
#'   shapefile obtained by tigris.  Defaults to FALSE.
#' @param shift_geo (deprecated) if TRUE, returns geometry with Alaska and Hawaii shifted for
#'   thematic mapping of the entire US. Geometry was originally obtained from
#'   the albersusa R package. As of May 2021, we recommend using \code{tigris::shift_geometry()}
#'   instead.
#' @param summary_var Character string of a "summary variable" from the ACS to
#'   be included in your output. Usually a variable (e.g. total population) that
#'   you'll want to use as a denominator or comparison.
#' @param key Your Census API key. Obtain one at
#'   \url{https://api.census.gov/data/key_signup.html}
#' @param moe_level The confidence level of the returned margin of error.  One
#'   of 90 (the default), 95, or 99.
#' @param survey The ACS contains one-year, three-year, and five-year surveys
#'   expressed as "acs1", "acs3", and "acs5". The default selection is "acs5."
#' @param show_call if TRUE, display call made to Census API. This can be very
#'   useful in debugging and determining if error messages returned are due to
#'   tidycensus or the Census API. Copy to the API call into a browser and see
#'   what is returned by the API directly. Defaults to FALSE.
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
#'                 state = "TX", county = "Tarrant", geometry = TRUE, year = 2020)
#'
#' ggplot(tarr, aes(fill = estimate, color = estimate)) +
#'   geom_sf() +
#'   coord_sf(crs = 26914) +
#'   scale_fill_viridis(option = "magma") +
#'   scale_color_viridis(option = "magma")
#'
#'
#' vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT", year = 2019)
#'
#' vt %>%
#' mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
#'  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
#'   geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe), width = 0.3, size = 0.5) +
#'   geom_point(color = "red", size = 3) +
#'   labs(title = "Household income by county in Vermont",
#'        subtitle = "2015-2019 American Community Survey",
#'        y = "",
#'        x = "ACS estimate (bars represent margin of error)")
#'
#' }
#' @export
get_acs <- function(geography, variables = NULL, table = NULL, cache_table = FALSE,
                    year = 2023, output = "tidy",
                    state = NULL, county = NULL, zcta = NULL,
                    geometry = FALSE, keep_geo_vars = FALSE,
                    shift_geo = FALSE, summary_var = NULL, key = NULL,
                    moe_level = 90, survey = "acs5", show_call = FALSE, ...) {

  if (survey == "acs1") {
    message(sprintf("Getting data from the %s 1-year ACS", year))
  } else if (survey == "acs3") {
    startyear <- year - 2
    message(sprintf("Getting data from the %s-%s 3-year ACS", startyear, year))
  } else if (survey == "acs5") {
    startyear <- year - 4
    message(sprintf("Getting data from the %s-%s 5-year ACS", startyear, year))
  }

  # Error message for 1-year 2020 ACS
  if (year == 2020 && survey == "acs1") {

    msg_acs <- c(crayon::red(stringr::str_wrap("The regular 1-year ACS for 2020 was not released and is not available in tidycensus.")),
                 i = crayon::cyan(stringr::str_wrap("Due to low response rates, the Census Bureau instead released a set of experimental estimates for the 2020 1-year ACS.")),
                 i = crayon::cyan(stringr::str_wrap("These estimates can be downloaded at https://www.census.gov/programs-surveys/acs/data/experimental-data/1-year.html.")),
                 i = crayon::green(stringr::str_wrap("1-year ACS data can still be accessed for other years by supplying an appropriate year to the `year` parameter.")))

    rlang::abort(msg_acs)

  }

  if (shift_geo) {
    warning("The `shift_geo` argument is deprecated and will be removed in a future release. We recommend using `tigris::shift_geometry()` instead.", call. = FALSE)
  }

  if (survey == "acs5" && year < 2009) {
    stop("5-year ACS support in tidycensus begins with the 2005-2009 5-year ACS. Consider using decennial Census data instead.", call. = FALSE)
  }

  if (survey == "acs1") {
    if (year < 2005) {
      stop("1-year ACS support in tidycensus begins with the 2005 1-year ACS. Consider using decennial Census data instead.", call. = FALSE)
    }
    message("The 1-year ACS provides data for geographies with populations of 65,000 and greater.")
  }

  if (survey == "acs3") {
    if (year < 2007 || year > 2013) {
      stop("3-year ACS support in tidycensus begins with the 2005-2007 3-year ACS and ends with the 2011-2013 3-year ACS. For newer data, use the 1-year or 5-year ACS.", call. = FALSE)
    } else {
      message("The 3-year ACS provides data for geographies with populations of 20,000 and greater.")
    }
  }

  if (length(table) > 1) {
    stop("Only one table may be requested per call.", call. = FALSE)
  }

  if (!is.null(variables)) {
    if (any(grepl("^K[0-9].", variables))) {
      message("Getting data from the ACS 1-year Supplemental Estimates.  Data are available for geographies with populations of 20,000 and greater.")
      survey <- "acsse"

    }
  }

  if (!is.null(table)) {
    if (grepl("^K[0-9].", table)) {
      message("Getting data from the ACS 1-year Supplemental Estimates.  Data are available for geographies with populations of 20,000 and greater.")
      survey <- "acsse"

    }
  }

  # Check for a Census API key and warn if missing
  key <- get_census_api_key(key)

  if (geography == "block") {
    stop("Block data are not available in the ACS. Use `get_decennial()` to access block data from the 2010 Census.", call. = FALSE)
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

  # CBSA alias (fixes #514 by reverting back)
  if (geography == "cbsa") {
    geography <- "metropolitan statistical area/micropolitan statistical area"
  }

  # Other aliases
  if (geography == "cbg") geography <- "block group"

  if (geography == "zcta") geography <- "zip code tabulation area"

  if (geography == "puma") geography <- "public use microdata area"

  # allow underscores and convert back to spaces
  if (grepl("_", geography) & !grepl(" ", geography)) geography <- gsub("_", " ", geography)

  if (any(grepl("^S[0-9]|^DP", variables)) && geography == "block group") {
    stop("Block groups are not an available geography in the Data Profile and Subject Tables datasets.",
         call. = FALSE)
  }


  if (!is.null(zcta) && geography != "zip code tabulation area") {
    stop("ZCTAs can only be specified when requesting data at the zip code tabulation area-level.",
         call. = FALSE)
  }

  # If multiple states and multiple ZCTAs are requested, handle the filtering on the
  # R side rather than the tidycensus side
  if (geography == "zip code tabulation area" && length(state) > 1 && !is.null(zcta)) {
    zips_to_get <- zcta

    zcta <- NULL

  } else {
    zips_to_get <- NULL
  }


  insist_get_acs <- purrr::insistently(get_acs)

  # Allow users to get all block groups in a state (2012 and earlier)
  # If only one state is specified, get all county FIPS codes in state from tigris and continue
  if (year < 2013) {
    if (geography == "block group") {
      stop("Block groups are not currently available for the 2008-2012 ACS and earlier.\nIf access to these datasets is restored, tidycensus will again support them.", call. = FALSE)
    }

    # Leave in all this code if Census restores block group availability for these years
    if ((geography == "block group" && length(state) == 1 && is.null(county))) {
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

    # If more than one state requested, iterate over the states, calling get_acs and combine results

    if (geography == "block group" && length(state) > 1) {

      if (!is.null(county)) {
        stop("Don't know which counties belong to which states. County must be null when requesting multiple states.",
             call. = FALSE)
      } else {

        message("Fetching block group data by state and county and combining the result.")

        if (geometry) {
          result <- map(state, function(s, ...) {
            suppressMessages(
              insist_get_acs(geography = geography,
                             variables = variables,
                             table = table,
                             cache_table = cache_table,
                             year = year,
                             output = output,
                             state = s,
                             county = county,
                             zcta = zcta,
                             summary_var = summary_var,
                             geometry = geometry,
                             keep_geo_vars = keep_geo_vars,
                             shift_geo = FALSE,
                             key = key,
                             moe_level = moe_level,
                             survey = survey,
                             show_call = show_call,
                             ...))
          }, ...) %>%
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
              insist_get_acs(geography = geography,
                             variables = variables,
                             table = table,
                             cache_table = cache_table,
                             year = year,
                             output = output,
                             state = .x,
                             county = county,
                             zcta = zcta,
                             summary_var = summary_var,
                             geometry = geometry,
                             keep_geo_vars = keep_geo_vars,
                             shift_geo = FALSE,
                             key = key,
                             moe_level = moe_level,
                             survey = survey,
                             show_call = show_call))
          })
        }

        return(result)
      }
    }

  }

  # if variables from more than one type of table (e.g. "S1701_C03_002" and "B05002_013"))
  # are requested - take care of this under the hood by having the function
  # call itself for "B" variables, "S" variables and "DP" variables then combining the results

  if (length(unique(substr(variables, 1, 1))) > 1 && !all(unique(substr(variables, 1, 1)) %in% c("B", "C"))) {

    if (any(grepl("^K[0-9].", variables))) {
      stop("At the moment, supplemental estimates variables cannot be combined with variables from other datasets.", call. = FALSE)

    }

    if (any(grepl("^CP[0-9].", variables))) {
      stop("Comparison profiles variables cannot be mixed with variables from other datasets in tidycensus; please request CP data separately.", call. = FALSE)
    }

    message('Fetching data by table type ("B/C", "S", "DP") and combining the result.')

    # split variables by type into list, discard empty list elements
    vars_by_type <- map(c("^B|^C", "^S", "^D"), ~ variables[str_detect(variables, .x)]) %>%
      purrr::compact()

    if (geometry) {
      if (output == "wide") {
        # when output is wide and geometry = TRUE we can't just make three calls to
        # get_acs and left_join the results because joining two sf objects requires
        # a spatial join, which we don't want to do here
        # instead, we'll take the first element of the var list (say, just "B" vars)
        # and get the data and geometry. then, we'll get the just the data without
        # geometry for the remaining var list elements and do a non-spatial
        # left join to the sf object from the first result

        vars_first <- vars_by_type[[1]]
        vars_rest <- vars_by_type[-1]

        # return acs data with geometry for first element of list
        result_geo <- suppressMessages(
          insist_get_acs(
            geography = geography,
            variables = vars_first,
            table = table,
            cache_table = cache_table,
            year = year,
            output = output,
            state = state,
            county = county,
            zcta = zcta,
            summary_var = summary_var,
            geometry = geometry,
            keep_geo_vars = keep_geo_vars,
            shift_geo = FALSE,
            key = key,
            moe_level = moe_level,
            survey = survey,
            show_call = show_call,
            ...
            )
          )

        # return acs data without geometry for remaining elements and join
        result_no_geo <- map(vars_rest, ~
          suppressMessages(
            insist_get_acs(
              geography = geography,
              variables = .x,
              table = table,
              cache_table = cache_table,
              year = year,
              output = output,
              state = state,
              county = county,
              zcta = zcta,
              summary_var = summary_var,
              geometry = FALSE,
              keep_geo_vars = keep_geo_vars,
              shift_geo = FALSE,
              key = key,
              moe_level = moe_level,
              survey = survey,
              show_call = show_call
              )
            )
          )

        if (year == 2010) {
          result_no_geo <- result_no_geo %>%
            reduce(left_join, by = "GEOID")
        } else {
          result_no_geo <- result_no_geo %>%
            reduce(left_join, by = c("GEOID", "NAME"))
        }



        # NAME.x and NAME.y columns exist when keep_geo_vars = TRUE
        if(keep_geo_vars) {
          join_cols <- c("GEOID", "NAME.y" = "NAME")
        } else {
          join_cols <- c("GEOID", "NAME")
        }

        # join non geo result to first result sf object
        result <- result_geo %>%
          left_join(result_no_geo, by = join_cols) %>%
          select(-geometry, geometry)  # move geometry to last column


      } else {
      # if output is tidy, we don't need to worry about this as results can
      # be combined using rbind
      result <- map(vars_by_type, function(v, ...) {
        suppressMessages(
          insist_get_acs(
            geography = geography,
            variables = v,
            table = table,
            cache_table = cache_table,
            year = year,
            output = output,
            state = state,
            county = county,
            zcta = zcta,
            summary_var = summary_var,
            geometry = geometry,
            keep_geo_vars = keep_geo_vars,
            shift_geo = FALSE,
            key = key,
            moe_level = moe_level,
            survey = survey,
            show_call = show_call,
            ...
          )
        )
      }, ...
      ) %>%
        reduce(rbind)
      }

      geoms <- unique(st_geometry_type(result))
      if (length(geoms) > 1) {
        result <- st_cast(result, "MULTIPOLYGON")
      }
      result <- result %>%
        as_tibble() %>%
        st_as_sf()

    } else {
      if (output == "wide") {
        # when output is wide and geometry = FALSE make one call per list element
        # and then left join results into one df
        result <- map(vars_by_type, ~
          suppressMessages(
            insist_get_acs(
              geography = geography,
              variables = .x,
              table = table,
              cache_table = cache_table,
              year = year,
              output = output,
              state = state,
              county = county,
              zcta = zcta,
              summary_var = summary_var,
              geometry = geometry,
              keep_geo_vars = keep_geo_vars,
              shift_geo = FALSE,
              key = key,
              moe_level = moe_level,
              survey = survey,
              show_call = show_call
            )
          )
        )

        if (year == 2010) {
          result <- result %>%
            reduce(left_join, by = "GEOID")
        } else {
          result <- result %>%
            reduce(left_join, by = c("GEOID", "NAME"))
        }

      } else {
        result <- map_df(vars_by_type, ~
          suppressMessages(
            insist_get_acs(
              geography = geography,
              variables = .x,
              table = table,
              cache_table = cache_table,
              year = year,
              output = output,
              state = state,
              county = county,
              zcta = zcta,
              summary_var = summary_var,
              geometry = geometry,
              keep_geo_vars = keep_geo_vars,
              shift_geo = FALSE,
              key = key,
              moe_level = moe_level,
              survey = survey,
              show_call = show_call
            )
          )
        )
      }

    }
    return(arrange(result, GEOID))  # sort so all vars for each GEOID is together
  }


  # If more than one state specified for tracts/block groups take care of
  # this under the hood by having the function
  # call itself and return the result
  if ((geography == "tract" || geography == "block group") && length(state) > 1) {
    message(sprintf("Fetching %s data by state and combining the result.", geography))
    # mc <- match.call(expand.dots = TRUE)
    if (geometry) {
      result <- map(state, function(s, ...) {
        suppressMessages(
          insist_get_acs(geography = geography,
                variables = variables,
                table = table,
                cache_table = cache_table,
                year = year,
                output = output,
                state = s,
                county = county,
                zcta = zcta,
                summary_var = summary_var,
                geometry = geometry,
                keep_geo_vars = keep_geo_vars,
                shift_geo = FALSE,
                key = key,
                moe_level = moe_level,
                survey = survey,
                show_call = show_call,
                ...)) %>%
          st_cast("MULTIPOLYGON")
      }, ...) %>%
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
          insist_get_acs(geography = geography,
                variables = variables,
                table = table,
                cache_table = cache_table,
                year = year,
                output = output,
                state = .x,
                county = county,
                zcta = zcta,
                summary_var = summary_var,
                geometry = geometry,
                keep_geo_vars = keep_geo_vars,
                shift_geo = FALSE,
                key = key,
                moe_level = moe_level,
                survey = survey,
                show_call = show_call))
      })
    }
    return(result)
  }

  # This should be cleaned up and combined with some of the code earlier up
  # but we still need to iterate through counties for block groups earlier than 2013
  if (year < 2013) {
    if ((geography == "block group" && length(county) > 1)) {
      if (geometry) {
        message("Fetching block group data by county and combining the result.")
        result <- map(county, function(co, ...) {
          suppressMessages(
            insist_get_acs(geography = geography,
                           variables = variables,
                           table = table,
                           cache_table = cache_table,
                           year = year,
                           output = output,
                           state = state,
                           county = co,
                           zcta = zcta,
                           summary_var = summary_var,
                           geometry = geometry,
                           keep_geo_vars = keep_geo_vars,
                           shift_geo = FALSE,
                           key = key,
                           moe_level = moe_level,
                           survey = survey,
                           show_call = show_call,
                           ...)) %>%
            st_cast("MULTIPOLYGON")
        }, ...) %>%
          reduce(rbind)
        geoms <- unique(st_geometry_type(result))
        if (length(geoms) > 1) {
          result <- st_cast(result, "MULTIPOLYGON")
        }
        result <- result %>%
          as_tibble() %>%
          st_as_sf()
      } else {
        message("Fetching block group data by county and combining the result.")
        result <- map_df(county, ~{
          suppressMessages(
            insist_get_acs(geography = geography,
                           variables = variables,
                           table = table,
                           cache_table = cache_table,
                           year = year,
                           output = output,
                           state = state,
                           county = .x,
                           zcta = zcta,
                           summary_var = summary_var,
                           geometry = geometry,
                           keep_geo_vars = keep_geo_vars,
                           shift_geo = FALSE,
                           key = key,
                           moe_level = moe_level,
                           survey = survey,
                           show_call = show_call))
        })
      }
      return(result)
    }
  }

  if (moe_level == 90) {
    moe_factor <- 1
  } else if (moe_level == 95) {
    moe_factor <- (1.96 / 1.645)
  } else if (moe_level == 99) {
    moe_factor <- (2.576 / 1.645)
  } else {
    stop("`moe_level` must be one of 90, 95, or 99.", call. = FALSE)
  }

  # Logic for fetching data tables
  if (!is.null(table)) {
    if (grepl("^S[0-9].", table)) {
      survey2 <- paste0(survey, "/subject")
    } else if (grepl("^DP[0-9].", table)) {
      survey2 <- paste0(survey, "/profile")
    } else if (grepl("^K[0-9].", table)) {
      survey2 <- "acsse"
    } else if (grepl("^CP[0-9].", table)) {
      survey2 <- paste0(survey, "/cprofile")
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
      suppressWarnings(load_data_acs(geography, vars, key, year, state, county,
                                     zcta, survey, show_call = show_call))
    }) %>%
    Reduce(function(x, y) full_join(x, y, by = "GEOID", suffix = c("", ".y")), .)
  } else {
    vars <- format_variables_acs(variables)

    dat <- suppressWarnings(load_data_acs(geography, vars, key, year, state, county,
                                          zcta, survey, show_call = show_call))
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

    if ("moe" %in% names(dat2)) {
      dat2 <- mutate(dat2, moe = moe * moe_factor)
    }


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

    dat2 <- dat %>%
      mutate_if(grepl("*M$", names(.)), list(~(. * moe_factor)))

    if (!is.null(names(variables))) {
      for (i in 1:length(variables)) {
        names(dat2) <- str_replace(names(dat2), variables[i], names(variables)[i])
      }
    }

    dat2 <- dat2 %>%
      select(GEOID, NAME, everything())

  }

  # For ZCTAs, strip the state code from GEOID (issue #338 and #358)
  # Should only happen if the GEOID is 7 characters
  if (geography == "zip code tabulation area" && year > 2012 && unique(nchar(dat2$GEOID)) == 7) {
    dat2 <- dat2 %>%
      dplyr::mutate(
        GEOID = stringr::str_sub(GEOID, start = 3L)
      )
  }

  if (!is.null(summary_var)) {

    if (length(summary_var) > 1) {
      stop(paste0("Only one summary variable may be used per pull. ",
                  "Alternatively, place all variables in `variables` and ",
                  "use `output='wide'`"))
    }

    sumvar <- format_variables_acs(summary_var)

    sumdat <- suppressMessages(load_data_acs(geography, sumvar, key, year, state, county, zcta, survey))

    sumest <- paste0(summary_var, "E")

    summoe <- paste0(summary_var, "M")

    if (geography == "zip code tabulation area" && year > 2012 && unique(nchar(sumdat$GEOID)) == 7) {
      sumdat <- sumdat %>%
        dplyr::mutate(
          GEOID = stringr::str_sub(GEOID, start = 3L)
        )
    }

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


  # If multiple ZCTAs are requested for multiple states, handle the filtering here
  if (!is.null(zips_to_get)) {
    dat2 <- dat2 %>%
      dplyr::filter(GEOID %in% zips_to_get)
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

      # state and county need to be NULL for ZCTAs as these args aren't available in tigris
      # for `zctas()`
      if (geography == "zip code tabulation area") {
        geom <- try(suppressMessages(use_tigris(geography = geography, year = year,
                                                state = NULL, county = NULL, ...)))
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

    args <- list(...)

    if (shift_geo || geography == "zip code tabulation area" || "filter_by" %in% names(args)) {
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
