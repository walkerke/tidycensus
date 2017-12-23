format_variables_acs <- function(variables) {

  # First, remove E or M if user has put it in
  variables1 <- map_chr(variables, function(x) {
    if (str_sub(x, -1) %in% c("E", "M")) {
      x <- str_sub(x, 1, -2)
    } else {
      x <- x
    }
  })

  # Now, make unique
  variables2 <- unique(variables1)

  # Now, expand with both E and M
  variables3 <- map_chr(variables2, function(y) paste0(y, c("E", "M"), collapse = ","))

  # Now, put together all these strings if need be
  var <- paste0(variables3, collapse = ",")

  return(var)

}

load_data_acs <- function(geography, formatted_variables, key, year, state = NULL, county = NULL, survey) {

  if (survey == "acs1") {
    if (year > 2014) {
      survey <- "acs/acs1"
    }
  }

  if (survey == "acs5" && year > 2014) {
    survey <- "acs/acs5"
  }

  base <- paste("https://api.census.gov/data",
                 as.character(year),
                 survey, sep = "/")

  if (grepl("^DP", formatted_variables)) {
    message("Using the ACS Data Profile")
    base <- paste0(base, "/profile")
  }

  if (grepl("^S[0-9].", formatted_variables)) {
    message("Using the ACS Subject Tables")
    if (survey == "acs1") {
      base <- paste("https://api.census.gov/data",
                    as.character(year),
                    "subject",
                    sep = "/")
    } else {
      base <- paste0(base, "/subject")

    }
  }

  if (!is.null(state)) {

    state <- map_chr(state, function(x) {
      validate_state(x)
    })

    if (length(state) > 1) {
      state <- paste0(state, collapse = ",")
    }

    if (!is.null(county)) {

      county <- map_chr(county, function(x) {
        validate_county(state, x)
      })

      if (length(county) > 1) {
        county <- paste0(county, collapse = ",")
      }

      in_area <- paste0("state:", state,
                        "+county:", county)

    } else {

      in_area <- paste0("state:", state)

    }

    vars_to_get <- paste0(formatted_variables, ",NAME")

    call <- GET(base, query = list(get = vars_to_get,
                                   "for" = paste0(geography, ":*"),
                                   "in" = in_area,
                                   key = key))
  }

  else {

    vars_to_get <- paste0(formatted_variables, ",NAME")

    call <- GET(base, query = list(get = vars_to_get,
                                   "for" = paste0(geography, ":*"),
                                   key = key))
  }

  # Make sure call status returns 200, else, print the error message for the user.
  callStatus <- http_status(call)
  if (callStatus$reason != "OK") {
    message(callStatus$category, " ", callStatus$reason, " ", callStatus$message)
  } else {
    content <- content(call, as = "text")
  }

  validate_call(content = content, geography = geography, year = year,
                dataset = survey)

  dat <- tbl_df(fromJSON(content))

  colnames(dat) <- dat[1,]

  dat <- dat[-1,]

  var_vector <- unlist(strsplit(formatted_variables, split = ","))

  dat[var_vector] <- lapply(dat[var_vector], as.numeric)

  v2 <- c(var_vector, "NAME")

  # Get the geography ID variables
  id_vars <- names(dat)[! names(dat) %in% v2]

  # Paste into a GEOID column
  dat$GEOID <- do.call(paste0, dat[id_vars])

  # Now, remove them
  dat <- dat[, !(names(dat) %in% id_vars)]

  return(dat)

}


load_data_decennial <- function(geography, variables, key, year,
                                sumfile, state = NULL, county = NULL) {


  var <- paste0(variables, collapse = ",")

  if (year == 1990) {
    vars_to_get <- paste0(var, ",ANPSADPI")
  } else {
    vars_to_get <- paste0(var, ",NAME")
  }


  base <- paste0("https://api.census.gov/data/",
                 year,
                 "/",
                 sumfile)

  if (!is.null(state)) {

    state <- map_chr(state, function(x) {
      validate_state(x)
    })

    if (length(state) > 1) {
      state <- paste0(state, collapse = ",")
    }

    if (!is.null(county)) {

      county <- map_chr(county, function(x) {
        validate_county(state, x)
      })

      if (length(county) > 1) {
        county <- paste0(county, collapse = ",")
      }

      in_area <- paste0("state:", state,
                        "+county:", county)

    } else {

      in_area <- paste0("state:", state)

    }

    call <- GET(base, query = list(get = vars_to_get,
                                   "for" = paste0(geography, ":*"),
                                   "in" = in_area,
                                   key = key))
  }

  else {

    call <- GET(base, query = list(get = vars_to_get,
                                   "for" = paste0(geography, ":*"),
                                   key = key))
  }

  # Make sure call status returns 200, else, print the error message for the user.

  callStatus <- http_status(call)
  if (callStatus$reason != "OK") {
    if (sumfile == "sf1") {
      message("Checking SF3 API for data...")
    } else {
      message(callStatus$category, " ", callStatus$reason, " ", callStatus$message)
    }
  } else {
    content <- content(call, as = "text")
  }

  # Fix issue in SF3 2000 API - https://github.com/walkerke/tidycensus/issues/22
  if (year == 2000 && sumfile == "sf3") {
    content <- str_replace(content, 'O"Brien', "O'Brien")
    content <- str_replace(content, 'Prince George"s', "Prince George's")
    content <- str_replace(content, 'Queen Anne"s', "Queen Anne's")
    content <- str_replace(content, 'St. Mary"s', "St. Mary's")

  }

  validate_call(content = content, geography = geography, year = year,
                dataset = sumfile)

  dat <- tbl_df(fromJSON(content))

  colnames(dat) <- dat[1,]

  dat <- dat[-1,]

  if (year == 1990) {
    dat <- rename(dat, NAME = ANPSADPI)
  }

  dat[variables] <- lapply(dat[variables], as.numeric)

  v2 <- c(variables, "NAME")

  # Get the geography ID variables
  id_vars <- names(dat)[! names(dat) %in% v2]

  # Paste into a GEOID column
  dat$GEOID <- do.call(paste0, dat[id_vars])


  # Fix issues with ZCTAs if necessary
  if (geography == "zip code tabulation area") {
    l <- unique(nchar(dat$GEOID))
    if (l == 7) {
      dat$GEOID <- str_sub(dat$GEOID, 3, 7)
    }
  }

  # Now, remove them
  dat <- dat[, !(names(dat) %in% id_vars)]

  return(dat)

}

