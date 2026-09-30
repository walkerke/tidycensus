#' Load variables from a decennial Census or American Community Survey dataset to search in R
#'
#' Finding the right variables to use with \code{get_decennial()} or \code{get_acs()} can be challenging; \code{load_variables()} attempts to make this easier for you.  Choose a year and a dataset to search for variables; those variables will be loaded from the Census website as an R data frame.  It is recommended that RStudio users use the \code{View()} function to interactively browse and filter these variables to find the right variables to use.
#'
#' \code{load_variables()} returns three columns by default: \code{name}, which is the Census ID code to be supplied to the \code{variables} parameter in \code{get_decennial()} or \code{get_acs()}; \code{label}, which is a detailed description of the variable; and \code{concept}, which provides information about the table that a given variable belongs to.  For 5-year ACS detailed tables datasets, a fourth column, \code{geography}, tells you the smallest geography at which a given variable is available.
#'
#' Datasets are named as they are in the Census API; see \url{https://api.census.gov/data.html} for a description of each dataset by year.
#'
#' For the American Community Survey, use \code{"acs1"}, \code{"acs3"}, or \code{"acs5"} for the Detailed Tables. Add \code{"/profile"} for the Data Profile (e.g. \code{"acs5/profile"}), \code{"/subject"} for the Subject Tables, or \code{"/cprofile"} for the Comparison Profile (\code{"acs1/cprofile"} and \code{"acs5/cprofile"}). \code{"acsse"} is the ACS 1-year Supplemental Estimates.
#'
#' For the decennial Census, use the name of a summary file, as supplied to the \code{sumfile} argument of \code{get_decennial()}. Run \code{summary_files(year)} to see the summary files available for 2000, 2010, or 2020. Common examples are \code{"pl"} (Redistricting Data), \code{"dhc"} (Demographic and Housing Characteristics, 2020), \code{"dp"} (Demographic Profile, 2020), and \code{"sf1"} (Summary File 1, 2000 and 2010).
#'
#' @param year The year for which you are requesting variables. Either the year
#'   of the decennial Census or the year / endyear of the ACS sample. 5-year ACS
#'   data are available from 2009, and 1-year ACS data from 2005, with the
#'   exception of 2020.
#' @param dataset The dataset name as used on the Census website.  See the Details in this documentation for a full list of dataset names.
#' @param cache Deprecated and ignored. Variable metadata is now loaded from
#'   the Census API without writing to a local cache.
#' @param key Your Census API key. Defaults to \code{NULL}, which uses your
#'   \code{CENSUS_API_KEY} environment variable.
#'
#' @return A tibble of variables from the requested dataset.
#' @examples \dontrun{
#' v15 <- load_variables(2015, "acs5")
#' View(v15)
#' }
#' @export
#'
load_variables <- function(
  year,
  dataset = c("sf1", "sf2", "sf3", "sf4", "pl", "dhc", "dp",
              "ddhca", "ddhcb", "sdhc", "as", "gu", "mp", "vi", "acsse",
              "dpas", "dpgu", "dpmp", "dpvi",
              "dhcvi", "dhcgu", "dhcvi", "dhcas",
              "acs1", "acs3", "acs5", "acs1/profile",
              "acs3/profile", "acs5/profile", "acs1/subject", "acs3/subject",
              "acs5/subject", "acs1/cprofile", "acs5/cprofile",
              "sf2profile", "sf3profile",
              "sf4profile", "aian", "aianprofile",
              "cd110h", "cd110s", "cd110hprofile", "cd110sprofile", "sldh",
              "slds", "sldhprofile", "sldsprofile", "cqr",
              "cd113", "cd113profile", "cd115", "cd115profile", "cd116",
              "plnat", "cd118"),
  cache = FALSE,
  key = NULL) {

  if (length(year) != 1 || !grepl('[0-9]{4}', year)){
    stop("Argument \"year\" must be a single year in format YYYY.")
  }

  dataset <- rlang::arg_match(dataset)

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

  var_type <- NULL

  if (stringr::str_detect(dataset, "/")) {
    split <- stringr::str_split(dataset, "/")[[1]]
    dataset <- split[1]
    var_type <- split[2]
  }

  if (dataset %in% c("sf1", "sf2", "sf3", "sf4", "pl", "ddhca", "ddhcb", "sdhc",
                     "as", "gu", "mp", "vi", "dhc", "dp",
                     "dpas", "dpgu", "dpmp", "dpvi",
                     "dhcvi", "dhcgu", "dhcvi", "dhcas",
                     "sf2profile", "sf3profile",
                     "sf4profile", "aian", "aianprofile",
                     "cd110h", "cd110s", "cd110hprofile", "cd110sprofile", "sldh",
                     "slds", "sldhprofile", "sldsprofile", "cqr",
                     "cd113", "cd113profile", "cd115", "cd115profile", "cd116",
                     "plnat", "cd118")) {
    dataset <- paste0("dec/", dataset)
  }

  if (dataset %in% c("acs1", "acs3", "acs5", "acsse")) {
    dataset <- paste0("acs/", dataset)
  }

  if (!is.null(var_type)) {
    dataset <- paste0(dataset, "/", var_type)
  }

  get_dataset <- function(d, year, key) {

    key <- get_census_api_key(key)

    set <- paste(year, d, sep = "/")

    url <- paste("https://api.census.gov/data",
                 set,
                 "variables.json", sep = "/")
    resp <- GET(url, query = list(key = key))
    if(httr::status_code(resp) == 404L){
      stop("API endpoint not found. Does this data set exist for the specified year? See https://api.census.gov/data.html for data availability.")
    }else if(httr::http_status(resp)$category != "Success"){
      stop(paste("API request failed. Reason:", httr::http_status(resp)$message))
    }
    dat <- resp %>%
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

    out1 <- out[grepl("^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P.*[0-9]|^H.*[0-9]|^K[0-9]|^CP[0-9]|^T[0-9]",
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

  if (isTRUE(cache)) {
    warning(
      "`cache` is deprecated and ignored. tidycensus no longer writes variable metadata to a local cache.",
      call. = FALSE
    )
  }

  get_dataset(dataset, year, key = key)
}
