# Called to check to see if "state" is a FIPS code, full name or abbreviation.
#
# returns NULL if input is NULL
# returns valid state FIPS code if input is even pseud-valid (i.e. single digit but w/in range)
# returns error if input is not a valid FIPS code
validate_state <- function(state, .msg=interactive()) {

  if (is.null(state)) return(NULL)

  state <- tolower(str_trim(state)) # forgive white space

  if (grepl("^[[:digit:]]+$", state)) { # we prbly have FIPS

    state <- sprintf("%02d", as.numeric(state)) # forgive 1-digit FIPS codes

    if (state %in% fips_state_table$fips) {
      return(state)
    } else {
      # perhaps they passed in a county FIPS by accident so forgive that, too,
      # but warn the caller
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% fips_state_table$fips) {
        message(sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                        state, state_sub,
                        fips_state_table[fips_state_table$fips == state_sub, "name"]),
                call.=FALSE)
        return(state_sub)
      } else {
        stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)

      }
    }

  } else if (grepl("^[[:alpha:]]+", state)) { # we might have state abbrev or name

    if (nchar(state) == 2 && state %in% fips_state_table$abb) { # yay, an abbrev!

      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$abb == state, "fips"],
                        toupper(state)))
      return(fips_state_table[fips_state_table$abb == state, "fips"])

    } else if (nchar(state) > 2 && state %in% fips_state_table$name) { # yay, a name!

      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$name == state, "fips"],
                        simpleCapSO(state)))
      return(fips_state_table[fips_state_table$name == state, "fips"])

    } else {
      stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)

    }

  } else {
    stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)

  }

}

# Some work on a validate_county function
#
#
validate_county <- function(state, county, .msg = interactive()) {

  if (is.null(state) || is.null(county)) return(NULL)

  state <- validate_state(state) # Get the state of the county

  county_table <- fips_codes[fips_codes$state_code == state, ] # Get a df for the requested state to work with

  if (grepl("^[[:digit:]]+$", county)) { # probably a FIPS code

    county <- sprintf("%03d", as.numeric(county)) # in case they passed in 1 or 2 digit county codes

    if (county %in% county_table$county_code) {
      return(county)

    } else {
      warning(sprintf("'%s' is not a current FIPS code for counties in %s", county, county_table$state_name[1]),
              call. = FALSE)
      return(county)

    }

  } else if ((grepl("^[[:alpha:]]+", county))) { # should be a county name

    county_index <- grepl(sprintf("^%s", county), county_table$county, ignore.case = TRUE)

    matching_counties <- county_table$county[county_index] # Get the counties that match

    if (length(matching_counties) == 0) {

      stop(sprintf("'%s' is not a valid name for counties in %s", county, county_table$state_name[1]),
              call. = FALSE)

    } else if (length(matching_counties) == 1) {

      if (.msg)
        message(sprintf("Using FIPS code '%s' for '%s'",
                        county_table[county_table$county == matching_counties, "county_code"],
                        matching_counties))

      return(county_table[county_table$county == matching_counties, "county_code"])

    } else if (length(matching_counties) > 1) {

      ctys <- format_vec(matching_counties)

      stop("Your county string matches ", ctys, " Please refine your selection.", call. = FALSE)


    }

  }

}


# Quick function to return formatted string for county codes

format_vec <- function(vec) {

  out <- paste0(vec, ', ')

  l <- length(out)

  out[l - 1] <- paste0(out[l - 1], 'and ')

  out[l] <- gsub(', ', '.', out[l])

  return(paste0(out, collapse = ''))

}

# Function from SO to do proper capitalization

simpleCapSO <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2),
         collapse=" ")
}