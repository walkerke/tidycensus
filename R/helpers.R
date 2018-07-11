# Function to get the correct geom for a Census dataset
# if geometry = TRUE


use_tigris <- function(geography, year, cb = TRUE, resolution = "500k",
                       state = NULL, county = NULL, starts_with = NULL) {

  if (year %in% 2011:2012) {
    cb <- FALSE
  }

  if (geography == "state") {

    st <- states(cb = cb, resolution = resolution, year = year, class = "sf")

    if (year == 1990) {
      st <- mutate(st, GEOID = ST)
      st <- st %>%
        group_by(GEOID) %>%
        summarize() %>%
        st_cast("MULTIPOLYGON")

    } else if (year %in% c(2000, 2010)) {
      st <- mutate(st, GEOID = STATE)
      if (cb && year == 2000) {
        st <- st %>%
          group_by(GEOID) %>%
          summarize() %>%
          st_cast("MULTIPOLYGON")
      }
    }

    if (year == 2014) {
      st <- st_zm(st)
    }

    return(st)

  } else if (geography == "county") {

    ct <- counties(cb = cb, state = state, resolution = resolution, year = year,
             class = "sf")

    if (year == 1990) {
      ct <- mutate(ct, GEOID = paste0(ST, CO))
      ct <- ct %>%
        group_by(GEOID) %>%
        summarize() %>%
        st_cast("MULTIPOLYGON")
    } else if (year %in% c(2000, 2010)) {
      ct <- mutate(ct, GEOID = paste0(STATE, COUNTY))
      if (cb && year == 2000) {
        ct <- ct %>%
          group_by(GEOID) %>%
          summarize() %>%
          st_cast("MULTIPOLYGON")
      }
    }

    if (year == 2014) {
      ct <- st_zm(ct)
    }

    return(ct)

  } else if (geography == "tract") {

    tr <- tracts(cb = cb, state = state, county = county, year = year,
           class = "sf")

    if (year == 1990) {
      tr <- tr %>%
        mutate(TRACTSUF = ifelse(is.na(TRACTSUF), "00", TRACTSUF)) %>%
        mutate(GEOID = paste0(ST, CO, TRACTBASE, TRACTSUF))
    } else if (year %in% c(2000, 2010)) {
      if (year == 2000) {
        tr <- mutate(tr, TRACT = str_pad(TRACT, 6, "right", "0"))
      }
      tr <- mutate(tr, GEOID = paste0(STATE, COUNTY, TRACT))
    }
    if (any(duplicated(tr$GEOID))) {
      tr <- tr %>%
        group_by(GEOID) %>%
        summarize() %>%
        st_cast("MULTIPOLYGON")
    }

    if (year == 2014) {
      tr <- st_zm(tr)
    }

    return(tr)

  } else if (geography == "block group") {

    bg <- block_groups(cb = cb, state = state, county = county, year = year,
                 class = "sf")

    if (year == 2000) {
      bg <- bg %>%
        mutate(TRACT = str_pad(TRACT, 6, "right", "0")) %>%
        mutate(GEOID = paste0(STATE, COUNTY, TRACT, BLKGROUP))
    } else if (year == 2010) {
      bg <- mutate(bg, GEOID = paste0(STATE, COUNTY, TRACT, BLKGRP))
    }

    if (any(duplicated(bg$GEOID))) {
      bg <- bg %>%
        group_by(GEOID) %>%
        summarize() %>%
        st_cast("MULTIPOLYGON")
    }

    if (year == 2014) {
      bg <- st_zm(bg)
    }

    return(bg)

  } else if (geography %in% c("zcta", "zip code tabulation area")) {

    # For right now, to get it to work, it has to be cb = FALSE for 2010, 2011, and 2012
    # Re-visit this in the future.

    if (year %in% 2010:2012) cb <- FALSE

    # No ZCTA geometry for 2011, so use 2010 instead
    if (year == 2011) year <- 2010

    z <- zctas(cb = cb, starts_with = starts_with, year = year,
               class = "sf", state = state)

    if (year == 2000) {
      z <- rename(z, GEOID = GEOID00)
    } else {
      z <- rename(z, GEOID = GEOID10)
    }

    return(z)

  } else if (geography == "block") {

    bl <- blocks(state = state, county = county, year = year, class = "sf")

    if (year > 2000) {
      bl <- rename(bl, GEOID = GEOID10)
    } else if (year == 2000) {
      bl <- rename(bl, GEOID = BLKIDFP00)
    }

    return(bl)

  } else {

    stop(sprintf("Geometry for %s is not yet supported.  Use the tigris package and join as normal instead.",
                 geography), call. = FALSE)

  }
}

#' Install a CENSUS API Key in Your \code{.Renviron} File for Repeated Use
#' @description This function will add your CENSUS API key to your \code{.Renviron} file so it can be called securely without being stored
#' in your code. After you have installed your key, it can be called any time by typing \code{Sys.getenv("CENSUS_API_KEY")} and can be
#' used in package functions by simply typing CENSUS_API_KEY If you do not have an \code{.Renviron} file, the function will create on for you.
#' If you already have an \code{.Renviron} file, the function will append the key to your existing file, while making a backup of your
#' original file for disaster recovery purposes.
#' @param key The API key provided to you from the Census formated in quotes. A key can be acquired at \url{http://api.census.gov/data/key_signup.html}
#' @param install if TRUE, will install the key in your \code{.Renviron} file for use in future sessions.  Defaults to FALSE.
#' @param overwrite If this is set to TRUE, it will overwrite an existing CENSUS_API_KEY that you already have in your \code{.Renviron} file.
#' @importFrom utils write.table read.table
#' @examples
#'
#' \dontrun{
#' census_api_key("111111abc", install = TRUE)
#' # First time, reload your environment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("CENSUS_API_KEY")
#' }
#'
#' \dontrun{
#' # If you need to overwrite an existing key:
#' census_api_key("111111abc", overwrite = TRUE, install = TRUE)
#' # First time, relead your environment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("CENSUS_API_KEY")
#' }
#' @export

census_api_key <- function(key, overwrite = FALSE, install = FALSE){

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(file.exists(renv)){
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if(!file.exists(renv)){
      file.create(renv)
    }
    else{
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("CENSUS_API_KEY", oldenv),]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("CENSUS_API_KEY",tv))){
          stop("A CENSUS_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("CENSUS_API_KEY='", key, "'")
    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message('Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("CENSUS_API_KEY"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return(key)
  } else {
    message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(CENSUS_API_KEY = key)
  }

}


# Function to generate a vector of variables from an ACS table
variables_from_table_acs <- function(table, year, survey, cache_table) {

  if (grepl("^((DP)|(S[0-9].))", table)) {
    stop("The `table` parameter is only available for ACS detailed tables.", call. = FALSE)
  }

  # Look to see if table exists in cache dir
  cache_dir <- user_cache_dir("tidycensus")

  dset <- paste0(survey, "_", year, ".rds")



  if (cache_table) {
    message(sprintf("Loading %s variables for %s from table %s and caching the dataset for faster future access.", toupper(survey), year, table))
    df <- load_variables(year, survey, cache = TRUE)
  } else {
    if (file.exists(file.path(cache_dir, dset))) {
      df <- load_variables(year, survey, cache = TRUE)
    } else {
      message(sprintf("Loading %s variables for %s from table %s. To cache this dataset for faster access to ACS tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per ACS dataset.", toupper(survey), year, table))
      df <- load_variables(year, survey, cache = FALSE)
    }
  }

  # For backwards compatibility
  names(df) <- tolower(names(df))

  specific <- paste0(table, "_")

  # Find all variables that match the table
  sub <- df[grepl(specific, df$name), ]

  vars <- sub$name

  return(vars)

}


# Function to generate a vector of variables from an Census table
variables_from_table_decennial <- function(table, year, sumfile, cache_table) {

  if (grepl("^((DP)|(S[0-9].))", table)) {
    stop("The `table` parameter is only available for ACS detailed tables.", call. = FALSE)
  }

  # Look to see if table exists in cache dir
  cache_dir <- user_cache_dir("tidycensus")

  dset <- paste0(sumfile, "_", year, ".rds")

  if (cache_table) {

    df <- load_variables(year, sumfile, cache = TRUE)
    names(df) <- tolower(names(df))

    # Check to see if we need to look in sf3
    if (!any(grepl(table, df$name))) {
      df <- load_variables(year, dataset = "sf3", cache = TRUE)
      names(df) <- tolower(names(df))
    }

    message(sprintf("Loading %s variables for %s from table %s and caching the dataset for faster future access.", toupper(sumfile), year, table))

  } else {
    if (file.exists(file.path(cache_dir, dset))) {
      df <- load_variables(year, sumfile, cache = TRUE)
      names(df) <- tolower(names(df))

      # Check to see if we need to look in sf3
      if (!any(grepl(table, df$name))) {
        df <- load_variables(year, dataset = "sf3", cache = TRUE)
        names(df) <- tolower(names(df))
      }

    } else {
      message(sprintf("Loading %s variables for %s from table %s. To cache this dataset for faster access to Census tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per Census dataset.", toupper(sumfile), year, table))
      df <- load_variables(year, sumfile, cache = FALSE)
      names(df) <- tolower(names(df))

      # Check to see if we need to look in sf3
      if (!any(grepl(table, df$name))) {
        df <- load_variables(year, dataset = "sf3", cache = FALSE)
        names(df) <- tolower(names(df))
      }
    }
  }

  # Find all variables that match the table
  vars <- df %>%
    filter(grepl(paste0(table, "[0-9]+"), name)) %>%
    pull(name)

  return(vars)

}



