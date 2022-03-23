#' Load variables from a decennial Census or American Community Survey dataset to search in R
#'
#' @param year The year for which you are requesting variables. Either the year
#'   or endyear of the decennial Census or ACS sample. 5-year ACS data is
#'   available from 2009 through 2018. 1-year ACS data is available from 2005
#'   through 2019.
#' @param dataset One of "sf1", "sf2", "sf3", "sf4", "pl",
#'   "as", "gu", "mp", "vi", "acs1", "acs3", "acs5", "acs1/profile",
#'   "acs3/profile, "acs5/profile", "acs1/subject", "acs3/subject", or
#'   "acs5/subject".
#' @param cache Whether you would like to cache the dataset for future access,
#'   or load the dataset from an existing cache. Defaults to FALSE.
#'
#' @return A tibble of variables from the requested dataset.
#' @examples \dontrun{
#' v15 <- load_variables(2015, "acs5", cache = TRUE)
#' View(v15)
#' }
#' @export
#'
load_variables <- function(year, dataset, cache = FALSE) {

  if (year == 2020 && stringr::str_detect(dataset, "acs1")) {
    stop("The 2020 1-year ACS was released as a set of experimental estimates that was not published to the Census API and is in turn not available in tidycensus.", call. = FALSE)
  }

  if (year == 1990) {
    stop("The 1990 decennial Census endpoint has been removed by the Census Bureau. We will support 1990 data again when the endpoint is updated; in the meantime, we recommend using NHGIS (https://nhgis.org) and the ipumsr R package.", call. = FALSE)
  }

  if (dataset == "sf3" && year > 2001) {
    stop("Summary File 3 was not released in 2010. Use tables from the American Community Survey via get_acs() instead.", call. = FALSE)
  }

   if (str_detect(dataset, "acs5") && year < 2009) {
    stop("5-year ACS support in tidycensus begins with the 2005-2009 5-year ACS. Consider using decennial Census data instead.", call. = FALSE)
  }

  if (str_detect(dataset, "acs1") && year < 2005) {
      stop("1-year ACS support in tidycensus begins with the 2005 1-year ACS. Consider using decennial Census data instead.", call. = FALSE)
  }

  if (str_detect(dataset, "acs3") && (year < 2007 || year > 2013)) {
      stop("3-year ACS support in tidycensus begins with the 2005-2007 3-year ACS and ends with the 2011-2013 3-year ACS. For newer data, use the 1-year or 5-year ACS.", call. = FALSE)
  }

  rds <- paste0(dataset, "_", year, ".rds")

  if (grepl("^acs[135]/(profile|subject|cprofile)$", dataset)) {
    rds <- gsub("/", "_", rds)
  }

  var_type <- NULL

  if (stringr::str_detect(dataset, "/")) {
    split <- stringr::str_split(dataset, "/")[[1]]
    dataset <- split[1]
    var_type <- split[2]
  }

  if (dataset %in% c("sf1", "sf2", "sf3", "sf4", "pl",
                     "as", "gu", "mp", "vi")) {
    dataset <- paste0("dec/", dataset)
  }

  if (dataset %in% c("acs1", "acs3", "acs5", "acsse")) {
    dataset <- paste0("acs/", dataset)
  }

  if (!is.null(var_type)) {
    dataset <- paste0(dataset, "/", var_type)
  }

  get_dataset <- function(d, year) {

    set <- paste(year, d, sep = "/")

    url <- paste("https://api.census.gov/data",
                 set,
                 "variables.json", sep = "/")

    dat <- httr::GET(url) %>%
      httr::content(as = "text") %>%
      jsonlite::fromJSON() %>%
      purrr::modify_depth(2, function(x) {
        x$validValues <- NULL
        x
      }) %>%
      purrr::flatten_df(.id = "name") %>%
      dplyr::arrange(name)

    out <- dat[,1:3]

    names(out) <- tolower(names(out))

    out1 <- out[grepl("^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P.*[0-9]|^H.*[0-9]|^K[0-9]|^CP[0-9]",
                      out$name), ]

    out1$name <- stringr::str_replace(out1$name, "E$|M$", "")

    out2 <- out1[!grepl("Margin Of Error|Margin of Error", out1$label), ]

    # Add geography information for acs5
    if (dataset == "acs/acs5" && year > 2010) {

      geo <- tidycensus::acs5_geography

      geo_lookup <- geo[geo$year == year,]

      out2 <- out2 %>%
        dplyr::mutate(table = stringr::str_remove(name, "_.*")) %>%
        dplyr::left_join(geo_lookup, by = "table") %>%
        dplyr::select(-year, -table)
    }

    return(as_tibble(out2))
  }

  if (cache) {
    cache_dir <- user_cache_dir("tidycensus")
    if (!file.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }

    if (file.exists(cache_dir)) {
      file_loc <- file.path(cache_dir, rds)
      if (file.exists(file_loc)) {

        out <- read_rds(file_loc)

        # For 5-year ACS without geography, refresh the table
        if (year > 2010 && dataset == "acs/acs5") {
          if (!"geography" %in% names(out)) {
            df <- get_dataset(dataset, year)
            readr::write_rds(df, file_loc)
            return(df)
          }
        }

        # For 2010 decennial Census, must get again if old
        if (year == 2010 && dataset == "dec/sf1") {

          # Check if an erroring variable is in the file
          if ("H00010001" %in% out$name) {
            df <- get_dataset(dataset, year)
            write_rds(df, file_loc)
            return(df)
          }

          # Update the cached file if missing PCT vars
          if (!"PCT001001" %in% out$name) {
            df <- get_dataset(dataset, year)
            write_rds(df, file_loc)
            return(df)
          }
        }

        out1 <- out[grepl("^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P.*[0-9]|^H.*[0-9]|^K[0-9]|^CP[0-9]",
                          out$name), ]

        out1$name <- str_replace(out1$name, "E$|M$", "")

        out2 <- out1[!grepl("Margin Of Error|Margin of Error", out1$label), ]
        return(out2)

      } else {
        df <- get_dataset(dataset, year)
        write_rds(df, file_loc)
        return(df)
      }
    }
  } else {
    get_dataset(dataset, year)
  }
}
