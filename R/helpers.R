# Function to get the correct geom for a Census dataset
# if geometry = TRUE


use_tigris <- function(geography, year, cb = TRUE, resolution = "500k",
                       state = NULL, county = NULL, starts_with = NULL) {

  if (geography == "state") {

    st <- states(cb = cb, resolution = resolution, year = year, class = "sf")

    if (year == 1990) {
      st <- mutate(st, GEOID = ST)
    } else if (year %in% c(2000, 2010)) {
      st <- mutate(st, GEOID = STATE)
    }

    return(st)

  } else if (geography == "county") {

    ct <- counties(cb = cb, state = state, resolution = resolution, year = year,
             class = "sf")

    if (year == 1990) {
      ct <- mutate(ct, GEOID = paste0(ST, CO))
    } else if (year %in% c(2000, 2010)) {
      ct <- mutate(ct, GEOID = paste0(STATE, COUNTY))
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

    return(bg)

  } else if (geography == "zcta" | geography == "zip code tabulation area") {

    # For right now, to get it to work, it has to be cb = FALSE for 2010
    # Re-visit this in the future.

    if (year == 2010) cb <- FALSE

    z <- zctas(cb = cb, starts_with = starts_with, year = year,
               class = "sf", state = state)

    if (year %in% c(2000, 2010)) {
      z <- mutate(z, GEOID = NAME)
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

  } else {

    stop(sprintf("Geometry for %s is not yet supported.  Use the tigris package and join as normal instead.",
                 geography), call. = FALSE)

  }
}

#' Install a CENSUS API Key in Your \code{.Renviron} File for Repeated Use
#' @description This function will add your CENSUS API key to your \code{.Renviron} file so it can be called securely without being stored
#' in your code. After you have installed your key, it can be called any time by typing \code{Sys.getenv("CENSUS_KEY")} and can be
#' used in package functions by simply typing CENSUS_KEY If you do not have an \code{.Renviron} file, the function will create on for you.
#' If you already have an \code{.Renviron} file, the function will append the key to your existing file, while making a backup of your
#' original file for disaster recovery purposes.
#' @param key The API key provided to you from the Census formated in quotes. A key can be acquired at \url{http://api.census.gov/data/key_signup.html}
#' @param overwrite If this is set to TRUE, it will overwrite an existing CENSUS_KEY that you already have in your \code{.Renviron} file.
#' @importFrom utils write.table
#' @examples
#'
#' \dontrun{
#' census_api_key("111111abc")
#' # First time, relead your enviornment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("CENSUS_KEY")
#' }
#'
#' \dontrun{
#' # If you need to overwrite an existing key:
#' census_api_key("111111abc", overwrite = TRUE)
#' # First time, relead your enviornment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("CENSUS_KEY")
#' }
#' @export

census_api_key <- function(key=NA, overwrite=NA){
  # go to the home dir. and look for an .Renviron file. If not, create one.
  setwd(Sys.getenv("HOME"))
  if(file.exists(".Renviron")){
    # Backup original .Renviron before doing anything else here.
    file.copy(".Renviron", ".Renviron_backup")
  }
  if(!file.exists(".Renviron")){
    file.create(".Renviron")
  }
  else{
    if(isTRUE(overwrite)){
      message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
      oldenv=read.table(".Renviron", stringsAsFactors = FALSE)
      newenv <- oldenv[-grep("CENSUS_KEY", oldenv),]
      write.table(newenv, ".Renviron", quote = FALSE, sep = "\n",
                  col.names = FALSE, row.names = FALSE)
    }
    else{
      tv <- readLines(".Renviron")
      if(isTRUE(any(grepl("CENSUS_KEY",tv)))){
        stop("A CENSUS_KEY already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
      }
    }
  }

  keyconcat <- paste("CENSUS_KEY=","'",key,"'", sep = "")
  # Append API key to .Renviron file
  write(keyconcat, ".Renviron", sep = "\n", append = TRUE)
  message('Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("CENSUS_KEY")')
  return(key)
}