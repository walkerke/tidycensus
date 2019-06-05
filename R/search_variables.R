#' Load variables from a decennial Census or American Community Survey dataset to search in R
#'
#' @param year The year for which you are requesting variables.  Either the year of the decennial Census,
#'             or the endyear for a 5-year ACS sample.
#' @param dataset One of "sf1", "sf3", "acs1", "acs3", "acs5", "acs1/profile", "acs3/profile, "acs5/profile",
#'                "acs1/subject", "acs3/subject", or "acs5/subject".
#' @param cache Whether you would like to cache the dataset for future access, or load the dataset
#'              from an existing cache. Defaults to FALSE.
#'
#' @return A tibble of variables from the requested dataset.
#' @examples \dontrun{
#' v15 <- load_variables(2015, "acs5", cache = TRUE)
#' View(v15)
#' }
#' @export
load_variables <- function(year, dataset, cache = FALSE) {

  if (dataset=="acs3") {
    if (year > 2013 || year < 2012)
      stop("The current acs3 survey contains data from 2012-2013. Please select a different year.")
  }

  rds <- paste0(dataset, "_", year, ".rds")

  if (grepl("^acs[135]/(profile|subject)$", dataset)) {
    rds <- gsub("/", "_", rds)
  }

  if (year > 2009 && (grepl("acs1", dataset) || grepl("acs5", dataset))) {
    dataset <- paste0("acs/", dataset)
  }

  get_dataset <- function(d) {

    # Account for URL change for 2010 decennial Census
    if (year == 2010 && dataset == "sf1") {
      d <- paste0("dec/", d)
    }

    set <- paste(year, d, sep = "/")

    # If ACS, use JSON parsing to speed things up
    if (grepl("acs[135]", d)) {

      url <- paste("https://api.census.gov/data",
                   set,
                   "variables.json", sep = "/")

      dat <- GET(url) %>%
        content(as = "text") %>%
        fromJSON() %>%
        modify_depth(2, function(x) {
          x$validValues <- NULL
          x
        }) %>%
        flatten_df(.id = "name") %>%
        arrange(name)

      out <- dat[,1:3]

      names(out) <- tolower(names(out))

      out1 <- out[grepl("^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P[0-9]|^H[0-9]", out$name), ]

      out1$name <- str_replace(out1$name, "E$|M$", "")

      out2 <- out1[!grepl("Margin Of Error|Margin of Error", out1$label), ]

      return(as_tibble(out2))
    # Otherwise use HTML scraping as JSON is not available for decennial Census
    } else {

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

      names(out) <- tolower(names(out))

      out1 <- out[grepl("^P.*|^H.*", out$name), ]

      out1$name <- str_replace(out1$name, "E$|M$", "")

      out2 <- out1[!grepl("Margin Of Error|Margin of Error", out1$label), ]

      return(as_tibble(out2))

    }

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

        # For 2010 decennial Census, must get again if old
        if (year == 2010 && dataset == "sf1") {

          # Check if an erroring variable is in the file
          if ("H00010001" %in% out$name) {
            df <- get_dataset(dataset)
            write_rds(df, file_loc)
            return(df)

          }

        }


        out1 <- out[grepl("^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P[0-9]|^H[0-9]", out$name), ]

        out1$name <- str_replace(out1$name, "E$|M$", "")

        out2 <- out1[!grepl("Margin Of Error|Margin of Error", out1$label), ]
        return(out2)
      } else {
        df <- get_dataset(dataset)
        write_rds(df, file_loc)
        return(df)
      }
    }
  } else {
    return(get_dataset(dataset))
  }
}

