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
    area_threshold = NULL
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
                                      area_threshold = area_threshold)
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
