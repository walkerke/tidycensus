# via: http://www.epa.gov/envirofw/html/codes/state.html
#
# this somewhat duplicates "state_codes" but it's primarily intended
# to validate_state. A TODO might be to refactor the code to eliminate this
# but put "state_codes" to all lowercase for utility functions and then
# use string transformations when presenting messages to the user

fips_state_table <- structure(list(abb = c("ak", "al", "ar", "as", "az", "ca", "co",
"ct", "dc", "de", "fl", "ga", "gu", "hi", "ia", "id", "il", "in",
"ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt",
"nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or",
"pa", "pr", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vi", "vt",
"wa", "wi", "wv", "wy", "mp"), fips = c("02", "01", "05", "60", "04",
"06", "08", "09", "11", "10", "12", "13", "66", "15", "19", "16",
"17", "18", "20", "21", "22", "25", "24", "23", "26", "27", "29",
"28", "30", "37", "38", "31", "33", "34", "35", "32", "36", "39",
"40", "41", "42", "72", "44", "45", "46", "47", "48", "49", "51",
"78", "50", "53", "55", "54", "56", "69"), name = c("alaska", "alabama",
"arkansas", "american samoa", "arizona", "california", "colorado",
"connecticut", "district of columbia", "delaware", "florida",
"georgia", "guam", "hawaii", "iowa", "idaho", "illinois", "indiana",
"kansas", "kentucky", "louisiana", "massachusetts", "maryland",
"maine", "michigan", "minnesota", "missouri", "mississippi",
"montana", "north carolina", "north dakota", "nebraska", "new hampshire",
"new jersey", "new mexico", "nevada", "new york", "ohio", "oklahoma",
"oregon", "pennsylvania", "puerto rico", "rhode island", "south carolina",
"south dakota", "tennessee", "texas", "utah", "virginia", "virgin islands",
"vermont", "washington", "wisconsin", "west virginia", "wyoming", "northern mariana islands"
)), .Names = c("abb", "fips", "name"), row.names = c(NA, -56L
), class = "data.frame")

population_estimates_variables <- c("POP", "DENSITY")
components_estimates_variables <- c("BIRTHS", "DEATHS","DOMESTICMIG","INTERNATIONALMIG","NATURALINC","NETMIG","RBIRTH","RDEATH","RDOMESTICMIG","RINTERNATIONALMIG","RNATURALINC","RNETMIG")
housing_estimates_variables <- "HUEST"


.onLoad <- function(libname, pkgname) {
  utils::data("fips_codes", package=pkgname, envir=parent.env(environment()))
}

# Place-holder until I update with the tidyeval framework
utils::globalVariables(c("variable", "value", "GEOID", "NAME", "type", "moe",
                         ".", "NAME.y", "summary_moe", "TRACTBASE", "TRACT",
                         "ANPSADPI", "BLKGROUP", "BLKGRP", "BLKIDFP00", "CO", "COUNTY",
                         "GEOID10", "ST", "STATE", "TRACTSUF", "name", ".data", "GEONAME",
                         "GEOID00", "POP", "PERIOD", "DATE", "PERIOD_CODE", "DATE_CODE",
                         "DATE_", "STATEFP00", "STATEFP10", "CNTYIDFP00", "CTIDFP00",
                         "BKGPIDFP00", "var_code", "val_label"))

#' @importFrom rlang .data
NULL

# Housing and person replicate weight variable vectors for get_pums()
housing_weight_variables <- c("WGTP1", "WGTP2", "WGTP3", "WGTP4", "WGTP5",
                              "WGTP6", "WGTP7", "WGTP8", "WGTP9", "WGTP10",
                              "WGTP11", "WGTP12", "WGTP13", "WGTP14", "WGTP15",
                              "WGTP16", "WGTP17", "WGTP18", "WGTP19", "WGTP20",
                              "WGTP21", "WGTP22", "WGTP23", "WGTP24", "WGTP25",
                              "WGTP26", "WGTP27", "WGTP28", "WGTP29", "WGTP30",
                              "WGTP31", "WGTP32", "WGTP33", "WGTP34", "WGTP35",
                              "WGTP36", "WGTP37", "WGTP38", "WGTP39", "WGTP40",
                              "WGTP41", "WGTP42", "WGTP43", "WGTP44", "WGTP45",
                              "WGTP46", "WGTP47", "WGTP48", "WGTP49", "WGTP50",
                              "WGTP51", "WGTP52", "WGTP53", "WGTP54", "WGTP55",
                              "WGTP56", "WGTP57", "WGTP58", "WGTP59", "WGTP60",
                              "WGTP61", "WGTP62", "WGTP63", "WGTP64", "WGTP65",
                              "WGTP66", "WGTP67", "WGTP68", "WGTP69", "WGTP70",
                              "WGTP71", "WGTP72", "WGTP73", "WGTP74", "WGTP75",
                              "WGTP76", "WGTP77", "WGTP78", "WGTP79", "WGTP80")


person_weight_variables <-  c("PWGTP1", "PWGTP2", "PWGTP3", "PWGTP4", "PWGTP5",
                              "PWGTP6", "PWGTP7", "PWGTP8", "PWGTP9", "PWGTP10",
                              "PWGTP11", "PWGTP12", "PWGTP13", "PWGTP14", "PWGTP15",
                              "PWGTP16", "PWGTP17", "PWGTP18", "PWGTP19", "PWGTP20",
                              "PWGTP21", "PWGTP22", "PWGTP23", "PWGTP24", "PWGTP25",
                              "PWGTP26", "PWGTP27", "PWGTP28", "PWGTP29", "PWGTP30",
                              "PWGTP31", "PWGTP32", "PWGTP33", "PWGTP34", "PWGTP35",
                              "PWGTP36", "PWGTP37", "PWGTP38", "PWGTP39", "PWGTP40",
                              "PWGTP41", "PWGTP42", "PWGTP43", "PWGTP44", "PWGTP45",
                              "PWGTP46", "PWGTP47", "PWGTP48", "PWGTP49", "PWGTP50",
                              "PWGTP51", "PWGTP52", "PWGTP53", "PWGTP54", "PWGTP55",
                              "PWGTP56", "PWGTP57", "PWGTP58", "PWGTP59", "PWGTP60",
                              "PWGTP61", "PWGTP62", "PWGTP63", "PWGTP64", "PWGTP65",
                              "PWGTP66", "PWGTP67", "PWGTP68", "PWGTP69", "PWGTP70",
                              "PWGTP71", "PWGTP72", "PWGTP73", "PWGTP74", "PWGTP75",
                              "PWGTP76", "PWGTP77", "PWGTP78", "PWGTP79", "PWGTP80")
