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
  var <- paste(variables3, sep = "", collapse = ",")

  return(var)

}

load_data_acs <- function(geography, formatted_variables, key, endyear, state = NULL, county = NULL, survey = "acs5") {

  base <- paste0("https://api.census.gov/data/",
                 as.character(endyear),
                 paste0("/",as.character(survey)))

  if (grepl("^DP", formatted_variables)) {
    message("Using the ACS Data Profile")
    base <- paste0(base, "/profile")
  }

  if (grepl("^S[0-9].", formatted_variables)) {
    message("Using the ACS Subject Tables")
    base <- paste0(base, "/subject")
  }

  if (!is.null(state)) {

    state <- map_chr(state, function(x) {
      validate_state(x)
    })

    if (length(state) > 1) {
      state <- paste(state, sep = "", collapse = ",")
    }

    if (!is.null(county)) {

      county <- map_chr(county, function(x) {
        validate_county(state, x)
      })

      if (length(county) > 1) {
        county <- paste(county, sep = "", collapse = ",")
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



  content <- content(call, as = "text")

  validate_call(content = content, geography = geography, year = endyear,
                dataset = survey)

  dat <- tbl_df(fromJSON(content))

  colnames(dat) <- dat[1,]

  dat <- dat[-1,]

  var_vector <- unlist(strsplit(formatted_variables, split = ","))

  l <- length(var_vector)

  if (length(var_vector) > 1) {

    dat[,var_vector] <- apply(dat[,var_vector], 2, function(x) as.numeric(x))

  } else if (length(var_vector) == 1) {

    dat[[var_vector]] <- as.numeric(dat[[var_vector]])

  }

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


  var <- paste(variables, sep = "", collapse = ",")

  if (year == 1990) {
    vars_to_get <- paste0(var, ",ANPSADPI")
  } else {
    vars_to_get <- paste0(var, ",NAME")
  }


  base <- paste0("https://api.census.gov/data/",
                 as.character(year),
                 "/",
                 sumfile)

  if (!is.null(state)) {

    state <- map_chr(state, function(x) {
      validate_state(x)
    })

    if (length(state) > 1) {
      state <- paste(state, sep = "", collapse = ",")
    }

    if (!is.null(county)) {

      county <- map_chr(county, function(x) {
        validate_county(state, x)
      })

      if (length(county) > 1) {
        county <- paste(county, sep = "", collapse = ",")
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



  content <- content(call, as = "text")

  validate_call(content = content, geography = geography, year = year,
                dataset = sumfile)

  dat <- tbl_df(fromJSON(content))

  colnames(dat) <- dat[1,]

  dat <- dat[-1,]

  if (year == 1990) {
    dat <- rename(dat, NAME = ANPSADPI)
  }

  l <- length(variables)

  if (length(variables) > 1) {

    dat[,variables] <- apply(dat[,variables], 2, function(x) as.numeric(x))

  } else if (length(variables) == 1) {

    dat[[variables]] <- as.numeric(dat[[variables]])

  }

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

