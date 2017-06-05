#' Load variables from a decennial Census or American Community Survey dataset to search in R
#'
#' @param year The year for which you are requesting variables.  Either the year of the decennial Census,
#'             or the endyear for a 5-year ACS sample.
#' @param dataset One of "sf1", "sf3", or "acs5".
#' @param cache Whether you would like to cache the dataset for future access, or load the dataset
#'              from an existing cache. Defaults to FALSE.
#'
#' @return A tibble of variables from the requested dataset.
#' @export
load_variables <- function(year, dataset, cache = FALSE) {

  rds <- paste0(dataset, "_", year, ".rds")

  get_dataset <- function() {
    set <- paste(as.character(year), dataset, sep = "/")

    url <- paste("http://api.census.gov/data",
                 set,
                 "variables.html", sep = "/")

    dat <- url %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)

    out <- dat[[1]]

    out <- out[-1,]

    out <- out[,1:3]

    return(tbl_df(out))
  }

  if (cache) {
    cache_dir <- user_cache_dir("tidycensus")
    if (!file.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }

    if (file.exists(cache_dir)) {
      file_loc <- file.path(cache_dir, rds)
      if (file.exists(file_loc)) {
        return(read_rds(file_loc))
      } else {
        df <- get_dataset()
        write_rds(df, file_loc)
        return(df)
      }
    }
  } else {
    return(get_dataset())
  }
}