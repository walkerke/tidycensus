#' Title
#'
#' @param year
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param year
#' @param dataset
#' @param string
#'
#' @return
#' @export
#'
#' @examples
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