#' Get available population groups for a given Decennial Census year and summary file
#'
#' @param year The decennial Census year; one of 2000, 2010, or 2020.
#' @param sumfile The summary file.  Available summary files are \code{"ddhca"}, \code{"sf2"}, and \code{"sf4"}.
#'
#' @return A tibble containing codes (to be used with the \code{pop_group} argument of \code{get_decennial()}) and descriptive names.
#' @export
get_pop_groups <- function(year, sumfile) {

  url <- sprintf("https://api.census.gov/data/%s/dec/%s/variables.json",
                 year, sumfile)

  j <- jsonlite::fromJSON(url)

  item_data <- j$variables$POPGROUP$values$item

  # Convert to a data frame
  df <- dplyr::tibble(pop_group = names(item_data), pop_group_label = unlist(item_data))

  # If the year is 2020, you need to get rid of anything that isn't 4 digits
  # as they left all the old codes in there
  if (year == 2020) {
    df <- dplyr::filter(df, nchar(pop_group) == 4)
  }

  df
}


#' Check to see if a given geography / population group combination is available in the Detailed DHC-A file.
#'
#' @param geography The requested geography.
#' @param pop_group The code representing the population group you'd like to check.
#' @param state The state (optional)
#' @param county The county (optional)
#'
#' @export
check_ddhca_groups <- function(
  geography,
  pop_group,
  state = NULL,
  county = NULL
) {
  rlang::inform("Checking table availability...")

  tables <- c("T01001", "T02001", "T02002", "T02003")
  names(tables) <- tables

  checks <- purrr::map(tables, function(tab) {
    try(
      suppressMessages(
        get_decennial(
          geography = geography,
          table = tab,
          year = 2020,
          sumfile = "ddhca",
          state = state,
          county = county,
          pop_group = pop_group
        )
      ), silent = TRUE
    )
  })

  has_data <- purrr::map_lgl(checks, function(x) {
    is.data.frame(x)
  })

  with_data <- names(has_data)[has_data]

  if (length(with_data) > 0) {
    rlang::inform(c("The following DDHC-A tables are available for your selection:", with_data))
  } else {
    rlang::inform("No DDHC-A tables are available for your selection. Please refine.")
  }


}