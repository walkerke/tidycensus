#' Load variables from a decennial Census or American Community Survey dataset to search in R
#'
#' @param year The year for which you are requesting variables.  Either the year of the decennial Census,
#'             or the endyear for a 5-year ACS sample.
#' @param dataset One of "sf1", "sf3", or "acs5".
#'
#' @return
#' @export
load_variables <- function(year, dataset) {

  set <- paste(as.character(year), dataset, sep = "/")

  url <- paste("http://api.census.gov/data",
               set,
               "variables.html", sep = "/")

  dat <- url %>%
    html() %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)

  dat[[1]]

}


#' Search for variables in a decennial Census or American Community Survey dataset
#'
#' @param year The year for which you are requesting variables.  Either the year of the decennial Census,
#'             or the endyear for a 5-year ACS sample.
#' @param dataset One of "sf1", "sf3", or "acs5".
#' @param string Character string to filter the variable list.
#'
#' @return
#' @export
search_variables <- function(year, dataset, string) {

  set <- paste(as.character(year), dataset, sep = "/")

  url <- paste("http://api.census.gov/data",
               set,
               "variables.html", sep = "/")

  dat <- url %>%
    html() %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)

  sub <- dat[[1]] %>%
    filter(grepl(string, Label, ignore.case = TRUE))

  sub

}