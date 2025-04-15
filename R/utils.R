# Called to check to see if "state" is a FIPS code, full name or abbreviation.
#
# returns NULL if input is NULL
# returns valid state FIPS code if input is even pseud-valid (i.e. single digit but w/in range)
# returns error if input is not a valid FIPS code
validate_state <- function(state, .msg=interactive()) {

  if (is.null(state)) return(NULL)

  state <- tolower(str_trim(state)) # forgive white space

  if (grepl("^[[:digit:]]+$", state)) { # we prbly have FIPS

    state <- sprintf("%02d", as.numeric(state)) # forgive 1-digit FIPS codes

    if (state %in% fips_state_table$fips) {
      return(state)
    } else {
      # perhaps they passed in a county FIPS by accident so forgive that, too,
      # but warn the caller
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% fips_state_table$fips) {
        message(sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                        state, state_sub,
                        fips_state_table[fips_state_table$fips == state_sub, "name"]),
                call.=FALSE)
        return(state_sub)
      } else {
        stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)

      }
    }

  } else if (grepl("^[[:alpha:]]+", state)) { # we might have state abbrev or name

    if (nchar(state) == 2 && state %in% fips_state_table$abb) { # yay, an abbrev!

      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$abb == state, "fips"],
                        toupper(state)))
      return(fips_state_table[fips_state_table$abb == state, "fips"])

    } else if (nchar(state) > 2 && state %in% fips_state_table$name) { # yay, a name!

      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$name == state, "fips"],
                        simpleCapSO(state)))
      return(fips_state_table[fips_state_table$name == state, "fips"])

    } else {
      stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)

    }

  } else {
    stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)

  }

}

# Some work on a validate_county function
#
#
validate_county <- function(state, county, .msg = interactive()) {

  if (is.null(state) || is.null(county)) return(NULL)

  state <- validate_state(state) # Get the state of the county

  county_table <- fips_codes[fips_codes$state_code == state, ] # Get a df for the requested state to work with

  if (grepl("^[[:digit:]]+$", county)) { # probably a FIPS code

    county <- sprintf("%03d", as.numeric(county)) # in case they passed in 1 or 2 digit county codes

    if (county %in% county_table$county_code) {
      return(county)

    } else {
      warning(sprintf("'%s' is not a current FIPS code for counties in %s", county, county_table$state_name[1]),
              call. = FALSE)
      return(county)

    }

  } else if ((grepl("^[[:alpha:]]+", county))) { # should be a county name

    county_index <- grepl(sprintf("^%s", county), county_table$county, ignore.case = TRUE)

    matching_counties <- county_table$county[county_index] # Get the counties that match

    if (length(matching_counties) == 0) {

      stop(sprintf("'%s' is not a valid name for counties in %s", county, county_table$state_name[1]),
              call. = FALSE)

    } else if (length(matching_counties) == 1) {

      if (.msg)
        message(sprintf("Using FIPS code '%s' for '%s'",
                        county_table[county_table$county == matching_counties, "county_code"],
                        matching_counties))

      return(county_table[county_table$county == matching_counties, "county_code"])

    } else if (length(matching_counties) > 1) {

      ctys <- format_vec(matching_counties)

      stop("Your county string matches ", ctys, " Please refine your selection.", call. = FALSE)


    }

  }

}


# Quick function to return formatted string for county codes

format_vec <- function(vec) {

  out <- paste0(vec, ', ')

  l <- length(out)

  out[l - 1] <- paste0(out[l - 1], 'and ')

  out[l] <- gsub(', ', '.', out[l])

  return(paste0(out, collapse = ''))

}

# Function from SO to do proper capitalization

simpleCapSO <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2),
         collapse=" ")
}

# print message with api call and decode special characters
print_api_call <- function(url) {
  url <- gsub("&key.*", "", url)
  message(paste("Census API call:", utils::URLdecode(url)))
}


# Internal function to clean up Connecticut (temporary until 2022 CB files are released)
clean_connecticut <- function() {
  ct_2022 <- suppressMessages(tigris::counties("CT", year = 2022))

  ct_cb_2021 <- suppressMessages(tigris::states(cb = TRUE, year = 2021) %>%
                                   dplyr::filter(GEOID == "09") %>%
                                   dplyr::select(geometry))

  ct_clean <- suppressWarnings(sf::st_intersection(ct_2022, ct_cb_2021))

  ct_clean %>%
    dplyr::select(STATEFP, COUNTYFP, COUNTYNS, GEOID, NAME, NAMELSAD,
                  LSAD, ALAND, AWATER) %>%
    dplyr::mutate(STUSPS = "CT", STATE_NAME = "Connecticut")

}


#' Convert polygon geometry to dots for dot-density mapping
#'
#' Dot-density maps are a compelling alternative to choropleth maps for cartographic visualization of demographic data as they allow for representation of the internal heterogeneity of geographic units.  This function helps users generate dots from an input polygon dataset intended for dot-density mapping.  Dots are placed randomly within polygons according to a given data:dots ratio; for example, a ratio of 100:1 for an input population value column will place approximately 1 dot in the polygon for every 100 people in the geographic unit.  Users can then map the dots using tools like \code{ggplot2::geom_sf()} or \code{tmap::tm_dots()}.
#'
#' \code{as_dot_density()} uses \code{terra::dots()} internally for fast creation of dots. As terra is not a hard dependency of the tidycensus package, users must first install terra before using this function.
#'
#' The \code{erase_water} parameter will internally call \code{tigris::erase_water()} to fetch water area for a given location in the United States and remove that water area from the polygons before placing dots in polygons. This will slow down performance of the function, but can improve cartographic accuracy in locations with significant water area. It is recommended that users transform their data into a projected coordinate reference system with \code{sf::st_transform()} prior to using this option in order to improve performance.
#'
#'
#' @param input_data An input sf object of geometry type \code{POLYGON} or \code{MULTIPOLYGON} that includes some information that can be converted to dots.  While the function is designed for use with data acquired with the tidycensus package, it will work for arbitrary polygon datasets.
#' @param value The value column to be used to determine the number of dots to generate. For tidycensus users, this will typically be the \code{"value"} column for decennial Census data or the \code{"estimate"} column for American Community Survey estimates.
#' @param values_per_dot The number of values per dot; used to determine the output data:dots ratio. A value of 100 means that each dot will represent approximately 100 values in the value column.
#' @param group A column in the dataset that identifies salient groups within which dots should be generated.  For a long-form tidycensus dataset, this will typically be the \code{"variable"} column or some derivative of it. The output dataset will be randomly shuffled to prevent "stacking" of groups in downstream dot-density maps.
#' @param erase_water If \code{TRUE}, calls \code{tigris::erase_water()} to remove water areas from the polygons prior to generating dots, allowing for dasymetric dot placement. This option is recommended if your location includes significant water area. If using this option, it is recommended that you first transform your data to a projected coordinate reference system using \code{sf::st_transform()} to improve performance. This argument only works for data in the United States.
#' @param area_threshold The area percentile threshold to be used when erasing water; ranges from 0 (all water area included) to 1 (no water area included)
#' @param water_year The year of the TIGER/Line water area shapefiles to use if erasing water. Defaults to 2020; ignore if not using the \code{erase_water} feature.
#'
#' @return The original dataset but of geometry type \code{POINT}, with the number of point features corresponding to the given value:dot ratio for a given group.
#' @export
#'
#' @examples \dontrun{
#'
#' library(tidycensus)
#' library(ggplot2)
#'
#' # Identify variables for mapping
#' race_vars <- c(
#'   Hispanic = "P2_002N",
#'   White = "P2_005N",
#'   Black = "P2_006N",
#'   Asian = "P2_008N"
#' )
#'
#' # Get data from tidycensus
#' baltimore_race <- get_decennial(
#'   geography = "tract",
#'   variables = race_vars,
#'   state = "MD",
#'   county = "Baltimore city",
#'   geometry = TRUE,
#'   year = 2020
#' )
#'
#' # Convert data to dots
#' baltimore_dots <- as_dot_density(
#'   baltimore_race,
#'   value = "value",
#'   values_per_dot = 100,
#'   group = "variable"
#' )
#'
#' # Use one set of polygon geometries as a base layer
#' baltimore_base <- baltimore_race[baltimore_race$variable == "Hispanic", ]
#'
#' # Map with ggplot2
#' ggplot() +
#'   geom_sf(data = baltimore_base,
#'           fill = "white",
#'           color = "grey") +
#'   geom_sf(data = baltimore_dots,
#'           aes(color = variable),
#'           size = 0.01) +
#'   theme_void()
#'
#' }
as_dot_density <- function(
    input_data,
    value,
    values_per_dot,
    group = NULL,
    erase_water = FALSE,
    area_threshold = NULL,
    water_year = 2020
) {

  # Ensure that terra package is installed
  if (!"terra" %in% installed.packages()) {
    stop("`as_dot_density()` uses the terra package to generate dots. Please install terra first then re-run.", call. = FALSE)
  }

  # If erase water is selected, erase water area from the polygons first
  if (erase_water) {

    if (is.null(area_threshold)) {
      area_threshold = 0.75
    }

    input_data <- tigris::erase_water(input_data,
                                      area_threshold = area_threshold,
                                      year = water_year)
  } else {
    if (!is.null(area_threshold)) {
      message("`area_threshold` is to be used when erasing water from input polygons; ignoring as `erase_water` is currently `FALSE`.")
    }
  }

  # If group is identified, iterate over the groups, shuffle, and combine
  if (!is.null(group)) {

    groups <- unique(input_data[[group]])

    output_dots <- purrr::map_dfr(groups, function(g) {

      group_data <- input_data[input_data[[group]] == g, ]

      group_data %>%
        terra::vect() %>%
        terra::dots(field = value,
                    size = values_per_dot) %>%
        sf::st_as_sf()
    }) %>%
      dplyr::slice_sample(prop = 1)

  } else {
    output_dots <- input_data %>%
      terra::vect() %>%
      terra::dots(field = value,
                  size = values_per_dot) %>%
      sf::st_as_sf()
  }

  # Sometimes, a strange dot gets thrown in.  Ensure this doesn't get returned.
  output_dots <- sf::st_filter(output_dots, input_data)

  return(output_dots)


}


#' Use population-weighted areal interpolation to transfer information from one set of shapes to another
#'
#' A common use-case when working with time-series small-area Census data is to transfer data from one set of shapes (e.g. 2010 Census tracts) to another set of shapes (e.g. 2020 Census tracts). Population-weighted interpolation is one such solution to this problem that takes into account the distribution of the population within a Census unit to intelligently transfer data between incongruent units.
#'
#' The approach implemented here is based on Esri's data apportionment algorithm, in which an "apportionment layer" of points (referred to here as the \code{weights}) is used to determine how to weight areas of overlap between origin and target zones.  Users must supply a "from" dataset as an sf object (the dataset from which numeric columns will be interpolated) and a "to" dataset, also of class sf, that contains the target zones. A third sf object, the "weights", may be an object of geometry type \code{POINT} or polygons from which points will be derived using \code{sf::st_point_on_surface()}.
#'
#' An intersection is computed between \code{from} and \code{to}, and a spatial join is computed between the intersection layer and the weights layer, represented as points.  A specified \code{weight_column} in \code{weights} will be used to determine the relative influence of each point on the allocation of values between \code{from} and \code{to}; if no weight column is specified, all points will be weighted equally.
#'
#' The \code{extensive} parameter (logical) should reflect the values being interpolated correctly.  If \code{TRUE}, the function returns a weighted sum for each zone.  If \code{FALSE}, a weighted mean will be returned.  For Census data, \code{extensive = TRUE} should be used for transferring counts / estimated counts between zones.  Derived metrics (e.g. population density, percentages, etc.) should use \code{extensive = FALSE}.  Margins of error in the ACS will not be transferred correctly with this function, so please use with caution.
#'
#' @param from The spatial dataset from which numeric attributes will be interpolated to target zones. By default, all numeric columns in this dataset will be interpolated.
#' @param to The target geometries (zones) to which numeric attributes will be interpolated.
#' @param to_id (optional) An ID column in the target dataset to be retained in the output. For data obtained with tidycensus, this will be \code{"GEOID"} by convention.  If \code{NULL}, the output dataset will include a column \code{id} that uniquely identifies each row.
#' @param extensive if \code{TRUE}, return weighted sums; if \code{FALSE}, return weighted means.
#' @param weights An input spatial dataset to be used as weights. If the dataset is not of geometry type \code{POINT}, it will be converted to points by the function with \code{sf::st_point_on_surface()}.  For US-based applications, this will commonly be a Census block dataset obtained with the tigris or tidycensus packages.
#' @param weight_column (optional) a column in \code{weights} used for weighting in the interpolation process.  Typically this will be a column representing the population (or other weighting metric, like housing units) of the input weights dataset.  If \code{NULL} (the default), each feature in \code{weights} is given an equal weight of 1.
#' @param weight_placement (optional) One of \code{"surface"}, where weight polygons are converted to points on polygon surfaces with \code{sf::st_point_on_surface()}, or \code{"centroid"}, where polygon centroids are used instead with \code{sf::st_centroid()}.  Defaults to \code{"surface"}.  This argument is not necessary if weights are already of geometry type \code{POINT}.
#' @param crs (optional) The EPSG code of the output projected coordinate reference system (CRS). Useful as all input layers (\code{from}, \code{to}, and \code{weights}) must share the same CRS for the function to run correctly.
#'
#' @return A dataset of class sf with the geometries and an ID column from \code{to} (the target shapes) but with numeric attributes of \code{from} interpolated to those shapes.
#' @export
#'
#' @examples \dontrun{
#' # Example: interpolating work-from-home from 2011-2015 ACS
#' # to 2020 shapes
#' library(tidycensus)
#' library(tidyverse)
#' library(tigris)
#' options(tigris_use_cache = TRUE)
#'
#' wfh_15 <- get_acs(
#'   geography = "tract",
#'   variables = "B08006_017",
#'   year = 2015,
#'   state = "AZ",
#'   county = "Maricopa",
#'   geometry = TRUE
#' ) %>%
#' select(estimate)
#'
#' wfh_20 <- get_acs(
#'   geography = "tract",
#'   variables = "B08006_017",
#'   year = 2020,
#'   state = "AZ",
#'   county = "Maricopa",
#'   geometry = TRUE
#'  )
#'
#' maricopa_blocks <- blocks(
#'   "AZ",
#'   "Maricopa",
#'   year = 2020
#' )
#'
#' wfh_15_to_20 <- interpolate_pw(
#'   from = wfh_15,
#'   to = wfh_20,
#'   to_id = "GEOID",
#'   weights = maricopa_blocks,
#'   weight_column = "POP20",
#'   crs = 26949,
#'   extensive = TRUE
#' )
#'
#' }
interpolate_pw <- function(from,
                           to,
                           to_id = NULL,
                           extensive,
                           weights,
                           weight_column = NULL,
                           weight_placement = c("surface", "centroid"),
                           crs = NULL) {

  # Check to make sure all inputs are valid
  if (!inherits(from, "sf") || !inherits(to, "sf") || !inherits(weights, "sf")) {
    stop("All inputs (from, to, and weights) must be sf objects.", call. = FALSE)
  }

  if (!all(unique(sf::st_geometry_type(from)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Input datasets `from` and `to` must both be of geometry type POLYGON or MULTIPOLYGON.",
         call. = FALSE)
  }

  if (!all(unique(sf::st_geometry_type(to)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Input datasets `from` and `to` must both be of geometry type POLYGON or MULTIPOLYGON.",
         call. = FALSE)
  }

  # Check for valid CRS processing
  # Ensure all inputs have a valid CRS defined
  if (any(sapply(list(sf::st_crs(from), sf::st_crs(to), sf::st_crs(weights)), is.na))) {
    stop("All inputs must have a valid CRS defined.")
  }

  # Check if all CRSs are identical
  if (!identical(sf::st_crs(from), sf::st_crs(to)) ||
      !identical(sf::st_crs(from), sf::st_crs(weights))) {
    # If CRS is provided, transform all the objects
    if (!is.null(crs)) {
      from <- sf::st_transform(from, crs)
      to <- sf::st_transform(to, crs)
      weights <- sf::st_transform(weights, crs)
    } else {
      # If no target CRS is provided, error out
      stop("All inputs must have the same CRS if CRS is not supplied")
    }
  }

  # Make a from and a to ID
  # Keep existing structure for logical purposes from original code
  if (is.null(to_id)) {
    to_id <- "id"
    to$id <- as.character(1:nrow(to))
  }

  # If the to_id is in the column names of from, it'll cause downstream problems
  # and this can happen with tidycensus data with GEOID
  # remove the column in this instance
  if (to_id %in% names(from)) {
    from[[to_id]] <- NULL
  }

  # If the weight column is NULL, give all input shapes the same weight
  # Also, if the weight column name is also in `from`, this will cause an error
  # (see issue #476).  Rename the weight column to something unlikely to be
  # in the input dataset.
  if (is.null(weight_column)) {

    weight_column <- "tidycensus_interpolation_weight"

    weights$tidycensus_interpolation_weight <- 1
  } else {

    # Generate the new weight column from the old weight column
    weights$tidycensus_interpolation_weight <- weights[[weight_column]]

    # Update the weight_column
    weight_column <- "tidycensus_interpolation_weight"

  }



  from_id <- "from_id"
  from$from_id <- as.character(1:nrow(from))

  # Convert strings to symbols for tidy evaluation
  weight_sym <- rlang::sym(weight_column)
  from_id_sym <- rlang::sym(from_id)
  to_id_sym <- rlang::sym(to_id)

  # Convert the input weights to points if input weights are not POINT
  is_point <- unique(sf::st_is(weights, "POINT"))

  # Accommodate input points if users want to bring their own
  if (is_point) {
    weight_points <- weights
  } else {

    # Use surface points or centroids for weight placement
    weight_loc <- rlang::arg_match(weight_placement)

    if (weight_loc == "surface") {

      weight_points <- suppressWarnings(weights %>%
                                          dplyr::select(!!weight_sym) %>%
                                          sf::st_point_on_surface())

    } else if (weight_loc == "centroid") {

      weight_points <- suppressWarnings(weights %>%
                                          dplyr::select(!!weight_sym) %>%
                                          sf::st_centroid())

    }

  }



  # Determine the denominator for the weights
  denominators <- from %>%
    sf::st_join(weight_points, left = FALSE) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(!!from_id_sym) %>%
    dplyr::summarize(tidycensus_weight_total = sum(!!weight_sym, na.rm = TRUE))

  # Calculate the intersections and intersection proportions
  intersections <- suppressWarnings(
    from %>%
      dplyr::left_join(denominators, by = from_id) %>%
      sf::st_intersection(to) %>%
      dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) %>%
      dplyr::mutate(intersection_id = dplyr::row_number()) %>%
      sf::st_join(weight_points, left = FALSE) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(intersection_id) %>%
      dplyr::mutate(intersection_value = sum(!!weight_sym, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(intersection_id, .keep_all = TRUE) %>%
      dplyr::mutate(weight_coef = intersection_value / tidycensus_weight_total) %>%
      dplyr::select(!!from_id_sym, !!to_id_sym, intersection_value, weight_coef)
    )

  # Merge the weights to the from data and interpolate any numeric columns in from to to
  if (extensive) {
    interpolated <- from %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(intersections, by = from_id) %>%
      dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                  .fns = ~(.x * weight_coef))) %>%
      dplyr::select(-weight_coef) %>%
      dplyr::group_by(!!to_id_sym) %>%
      dplyr::summarize(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                     .fns = ~sum(.x, na.rm = TRUE))) %>%
      dplyr::select(-intersection_value)
  } else {
    interpolated <- from %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(intersections, by = from_id) %>%
      dplyr::group_by(!!to_id_sym) %>%
      dplyr::summarize(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                     .fns = ~weighted.mean(.x,
                                                           w = intersection_value,
                                                           na.rm = TRUE))) %>%
      dplyr::select(-intersection_value, -weight_coef)
  }

  # Merge back to the original "to" shapes
  output_shapes <- to %>%
    dplyr::select(!!to_id_sym) %>%
    dplyr::left_join(interpolated, by = to_id)  # %>%
    # dplyr::rename_with(.cols = !!to_id_sym,
    #                    .fn = ~stringr::str_remove(.x, "_to"))

  return(output_shapes)


}
