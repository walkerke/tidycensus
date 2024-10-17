format_variables_acs <- function(variables) {

  # find code in 'data-raw/no_moe_vars.R' to pull these vars from api
  no_moes <- c("B00001_001", "B00002_001", "B98001_001", "B98001_002", "B98002_001",
               "B98002_002", "B98002_003", "B98003_001", "B98011_001", "B98012_001",
               "B98012_002", "B98012_003", "B98013_001", "B98013_002", "B98013_003",
               "B98013_004", "B98013_005", "B98013_006", "B98013_007", "B98014_001",
               "B98021_001", "B98021_002", "B98021_003", "B98021_004", "B98021_005",
               "B98021_006", "B98021_007", "B98021_008", "B98021_009", "B98021_010",
               "B98022_001", "B98022_002", "B98022_003", "B98022_004", "B98022_005",
               "B98022_006", "B98022_007", "B98022_008", "B98022_009", "B98022_010",
               "B98031_001", "B98032_001", "B99011_001", "B99011_002", "B99011_003",
               "B99012_001", "B99012_002", "B99012_003", "B99021_001", "B99021_002",
               "B99021_003", "B99031_001", "B99031_002", "B99031_003", "B99051_001",
               "B99051_002", "B99051_003", "B99051_004", "B99051_005", "B99051_006",
               "B99051_007", "B99052_001", "B99052_002", "B99052_003", "B99052_004",
               "B99052_005", "B99052_006", "B99052_007", "B99052PR_001", "B99052PR_002",
               "B99052PR_003", "B99052PR_004", "B99052PR_005", "B99052PR_006",
               "B99052PR_007", "B99053_001", "B99053_002", "B99053_003", "B99061_001",
               "B99061_002", "B99061_003", "B99071_001", "B99071_002", "B99071_003",
               "B99072_001", "B99072_002", "B99072_003", "B99072_004", "B99072_005",
               "B99072_006", "B99072_007", "B99080_001", "B99080_002", "B99080_003",
               "B99081_001", "B99081_002", "B99081_003", "B99081_004", "B99081_005",
               "B99082_001", "B99082_002", "B99082_003", "B99082_004", "B99082_005",
               "B99083_001", "B99083_002", "B99083_003", "B99083_004", "B99083_005",
               "B99084_001", "B99084_002", "B99084_003", "B99084_004", "B99084_005",
               "B99085_001", "B99085_002", "B99085_003", "B99086_001", "B99086_002",
               "B99086_003", "B99087_001", "B99087_002", "B99087_003", "B99087_004",
               "B99087_005", "B99088_001", "B99088_002", "B99088_003", "B99088_004",
               "B99088_005", "B99089_001", "B99089_002", "B99089_003", "B99092_001",
               "B99092_002", "B99092_003", "B99102_001", "B99102_002", "B99102_003",
               "B99103_001", "B99103_002", "B99103_003", "B99104_001", "B99104_002",
               "B99104_003", "B99104_004", "B99104_005", "B99104_006", "B99104_007",
               "B99121_001", "B99121_002", "B99121_003", "B99122_001", "B99122_002",
               "B99122_003", "B99123_001", "B99123_002", "B99123_003", "B99124_001",
               "B99124_002", "B99124_003", "B99125_001", "B99125_002", "B99125_003",
               "B99126_001", "B99126_002", "B99126_003", "B99131_001", "B99131_002",
               "B99131_003", "B99132_001", "B99132_002", "B99132_003", "B99141_001",
               "B99141_002", "B99141_003", "B99142_001", "B99142_002", "B99142_003",
               "B99151_001", "B99151_002", "B99151_003", "B99152_001", "B99152_002",
               "B99152_003", "B99161_001", "B99161_002", "B99161_003", "B99162_001",
               "B99162_002", "B99162_003", "B99162_004", "B99162_005", "B99162_006",
               "B99162_007", "B99163_001", "B99163_002", "B99163_003", "B99163_004",
               "B99163_005", "B99171_001", "B99171_002", "B99171_003", "B99171_004",
               "B99171_005", "B99171_006", "B99171_007", "B99171_008", "B99171_009",
               "B99171_010", "B99171_011", "B99171_012", "B99171_013", "B99171_014",
               "B99171_015", "B99172_001", "B99172_002", "B99172_003", "B99172_004",
               "B99172_005", "B99172_006", "B99172_007", "B99172_008", "B99172_009",
               "B99172_010", "B99172_011", "B99172_012", "B99172_013", "B99172_014",
               "B99172_015", "B99181_001", "B99181_002", "B99181_003", "B99182_001",
               "B99182_002", "B99182_003", "B99183_001", "B99183_002", "B99183_003",
               "B99184_001", "B99184_002", "B99184_003", "B99185_001", "B99185_002",
               "B99185_003", "B99186_001", "B99186_002", "B99186_003", "B99187_001",
               "B99187_002", "B99187_003", "B99187_004", "B99187_005", "B99187_006",
               "B99187_007", "B99191_001", "B99191_002", "B99191_003", "B99191_004",
               "B99191_005", "B99191_006", "B99191_007", "B99191_008", "B99192_001",
               "B99192_002", "B99192_003", "B99192_004", "B99192_005", "B99192_006",
               "B99192_007", "B99192_008", "B99193_001", "B99193_002", "B99193_003",
               "B99193_004", "B99193_005", "B99193_006", "B99193_007", "B99193_008",
               "B99194_001", "B99194_002", "B99194_003", "B99194_004", "B99194_005",
               "B99194_006", "B99194_007", "B99194_008", "B99201_001", "B99201_002",
               "B99201_003", "B99201_004", "B99201_005", "B99201_006", "B99201_007",
               "B99201_008", "B99211_001", "B99211_002", "B99211_003", "B99212_001",
               "B99212_002", "B99212_003", "B99221_001", "B99221_002", "B99221_003",
               "B99231_001", "B99231_002", "B99231_003", "B99232_001", "B99232_002",
               "B99232_003", "B99233_001", "B99233_002", "B99233_003", "B99233_004",
               "B99233_005", "B99234_001", "B99234_002", "B99234_003", "B99234_004",
               "B99234_005", "B99241_001", "B99241_002", "B99241_003", "B99242_001",
               "B99242_002", "B99242_003", "B99243_001", "B99243_002", "B99243_003",
               "B99244_001", "B99244_002", "B99244_003", "B99245_001", "B99245_002",
               "B99245_003", "B99246_001", "B99246_002", "B99246_003", "B992510_001",
               "B992510_002", "B992510_003", "B992511_001", "B992511_002", "B992511_003",
               "B992512_001", "B992512_002", "B992512_003", "B992513_001", "B992513_002",
               "B992513_003", "B992514_001", "B992514_002", "B992514_003", "B992515_001",
               "B992515_002", "B992515_003", "B992516_001", "B992516_002", "B992516_003",
               "B992518_001", "B992518_002", "B992518_003", "B992519_001", "B992519_002",
               "B992519_003", "B99252_001", "B99252_002", "B99252_003", "B992520_001",
               "B992520_002", "B992520_003", "B992521_001", "B992521_002", "B992521_003",
               "B992522_001", "B992522_002", "B992522_003", "B992522_004", "B992522_005",
               "B992522_006", "B992522_007", "B992523_001", "B992523_002", "B992523_003",
               "B99253_001", "B99253_002", "B99253_003", "B99254_001", "B99254_002",
               "B99254_003", "B99255_001", "B99255_002", "B99255_003", "B99256_001",
               "B99256_002", "B99256_003", "B99257_001", "B99257_002", "B99257_003",
               "B99258_001", "B99258_002", "B99258_003", "B99259_001", "B99259_002",
               "B99259_003", "B992701_001", "B992701_002", "B992701_003", "B992702_001",
               "B992702_002", "B992702_003", "B992703_001", "B992703_002", "B992703_003",
               "B992704_001", "B992704_002", "B992704_003", "B992705_001", "B992705_002",
               "B992705_003", "B992706_001", "B992706_002", "B992706_003", "B992707_001",
               "B992707_002", "B992707_003", "B992708_001", "B992708_002", "B992708_003",
               "B992709_001", "B992709_002", "B992709_003", "B99281_001", "B99281_002",
               "B99281_003", "B99282_001", "B99282_002", "B99282_003", "B99282_004",
               "B99282_005", "B99282_006", "B99282_007", "B99282_008", "B99282_009",
               "B99283_001", "B99283_002", "B99283_003", "B99283_004", "B99283_005")

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

  # The comparison profile doesn't have MOEs, so account for that
  if (!any(grepl("^CP[0-9].", variables2))) {

    # Next, separate into vars with and without MOEs
    variables2a <- variables2[!variables2 %in% no_moes]

    variables2_nomoe <- variables2[variables2 %in% no_moes]

    # Now, expand with both E and M if MOE is applicable
    variables3 <- map_chr(variables2a, function(y) paste0(y, c("E", "M"), collapse = ","))

    if (length(variables2_nomoe)) {
      variables3_nomoe <- paste0(variables2_nomoe, "E")

      variables3 <- c(variables3, variables3_nomoe)
    }
  } else {
    variables3 <- paste0(variables2, "E")
  }

  # Now, put together all these strings if need be
  var <- paste0(variables3, collapse = ",")

  return(var)

}

load_data_acs <- function(geography, formatted_variables, key, year, state = NULL,
                          county = NULL, zcta = NULL, survey, show_call = FALSE) {

  base <- paste("https://api.census.gov/data",
                  as.character(year), "acs",
                  survey, sep = "/")

  if (grepl("^DP", formatted_variables)) {
    message("Using the ACS Data Profile")
    base <- paste0(base, "/profile")
  }

  if (grepl("^S[0-9].", formatted_variables)) {
    message("Using the ACS Subject Tables")
    base <- paste0(base, "/subject")
  }

  if (grepl("^CP[0-9].", formatted_variables)) {
    message("Using the ACS Comparison Profile")
    base <- paste0(base, "/cprofile")
  }

  for_area <- paste0(geography, ":*")

  if (!is.null(zcta))  {

    # if (is.null(state)) {
    #   stop("The `zcta` argument requires specifying a state.", call. = FALSE)
    # }

    if (!is.null(county)) {
      stop("ZCTAs do not nest within counties, so `county` should not be specified.",
           call. = FALSE)
    }

    if (!is.null(state)) {
      state <- map_chr(state, function(x) {
        validate_state(x)
      })

      in_area <- paste0("state:", state)

    } else {
      in_area <- NULL
    }

    for_area <- paste0(zcta, collapse = ",")

    vars_to_get <- paste0(formatted_variables, ",NAME")

    call <- GET(base, query = list(get = vars_to_get,
                                   "for" = paste0(geography, ":", for_area),
                                   "in" = in_area,
                                   key = key))

  } else if (!is.null(state) && is.null(zcta)) {

    state <- map_chr(state, function(x) {
      validate_state(x)
    })

    if (length(state) > 1) {
      state <- paste0(state, collapse = ",")
    }

    if (geography == "state") {
      for_area <- paste0("state:", state)
    }

    if (!is.null(county)) {

      county <- map_chr(county, function(x) {
        validate_county(state, x)
      })

      if (length(county) > 1) {
        county <- paste0(county, collapse = ",")
      }

      if (geography == "county") {

        for_area <- paste0("county:", county)
        in_area <- paste0("state:", state)

      } else {

        in_area <- paste0("state:", state,
                          "+county:", county)

      }

    } else {

      if (geography == "block group" && is.null(county)) {
          in_area <- paste0("state:", state,
                            "&in=county:*")
      } else {
        in_area <- paste0("state:", state)
      }

    }

    vars_to_get <- paste0(formatted_variables, ",NAME")

    if (geography == "state" && !is.null(state)) {

      call <- GET(base, query = list(get = vars_to_get,
                                     "for" = for_area,
                                     key = key))
    } else {

      call <- GET(base, query = list(get = vars_to_get,
                                     "for" = for_area,
                                     "in" = in_area,
                                     key = key))
    }


  } else {

    vars_to_get <- paste0(formatted_variables, ",NAME")

    call <- GET(base, query = list(get = vars_to_get,
                                   "for" = paste0(geography, ":*"),
                                   key = key))
  }

  if (show_call) {
    call_url <- gsub("&key.*", "", call$url)
    message(paste("Census API call:", call_url))
  }

  # Make sure call status returns 200, else, print the error message for the user.
  if (call$status_code != 200) {
    msg <- content(call, as = "text")

    if (grepl("The requested resource is not available", msg)) {
      stop("One or more of your requested variables is likely not available at the requested geography.  Please refine your selection.", call. = FALSE)
    } else {
      stop(sprintf("Your API call has errors.  The API message returned is %s.", msg), call. = FALSE)
    }

  }

  # callStatus <- http_status(call)
  # if (callStatus$reason != "OK") {
  #   message(callStatus$category, " ", callStatus$reason, " ", callStatus$message)
  # } else {
  #   content <- content(call, as = "text")
  # }
  #
  # validate_call(content = content, geography = geography, year = year,
  #               dataset = survey)

  content <- content(call, as = "text")

  if (grepl("You included a key with this request", content)) {
    stop("You have supplied an invalid or inactive API key. To obtain a valid API key, visit https://api.census.gov/data/key_signup.html. To activate your key, be sure to click the link provided to you in the email from the Census Bureau that contained your key.", call. = FALSE)
  }

  dat <- fromJSON(content)

  colnames(dat) <- dat[1,]

  dat <- as_tibble(dat)

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


load_data_decennial <- function(geography, variables, key, year, sumfile, pop_group,
                                state = NULL, county = NULL, show_call = FALSE) {


  var <- paste0(variables, collapse = ",")

  if (year == 1990) {
    vars_to_get <- paste0(var, ",ANPSADPI")
  } else {
    vars_to_get <- paste0(var, ",NAME")
  }

  if (!is.null(pop_group)) {
    if (pop_group == "all") {
      vars_to_get <- paste0(vars_to_get, ",POPGROUP")
      pop_group <- NULL
    }

  }

  base <- paste0("https://api.census.gov/data/",
                                  year,
                                  "/dec/",
                                  sumfile)

  # if (year == 2010) {
  #   base <- paste0("https://api.census.gov/data/",
  #                  year,
  #                  "/dec/",
  #                  sumfile)
  # } else {
  #   base <- paste0("https://api.census.gov/data/",
  #                  year,
  #                  "/",
  #                  sumfile)
  # }




  for_area <- paste0(geography, ":*")

  if (!is.null(state)) {

    state <- map_chr(state, function(x) {
      validate_state(x)
    })

    if (length(state) > 1) {
      state <- paste0(state, collapse = ",")
    }

    if (geography == "state") {
      for_area <- paste0("state:", state)
    }

    if (!is.null(county)) {

      county <- map_chr(county, function(x) {
        validate_county(state, x)
      })

      if (length(county) > 1) {
        county <- paste0(county, collapse = ",")
      }

      if (geography == "county") {

        for_area <- paste0("county:", county)
        in_area <- paste0("state:", state)

      } else {

        in_area <- paste0("state:", state,
                          "+county:", county)

      }

    } else {

      if ((geography == "block group" || geography == "block") && is.null(county)) {
        in_area <- paste0("state:", state,
                          "&in=county:*")
      } else {
        in_area <- paste0("state:", state)
      }

    }

    if (geography == "state" && !is.null(state)) {

      call <- GET(base, query = list(get = vars_to_get,
                                     "for" = for_area,
                                     key = key,
                                     "POPGROUP" = pop_group))
    } else {

      call <- GET(base, query = list(get = vars_to_get,
                                     "for" = for_area,
                                     "in" = in_area,
                                     key = key,
                                     "POPGROUP" = pop_group))
    }
  }

  else {

    call <- GET(base, query = list(get = vars_to_get,
                                   "for" = paste0(geography, ":*"),
                                   key = key,
                                   "POPGROUP" = pop_group))
  }

  if (show_call) {
    call_url <- gsub("&key.*", "", call$url)
    message(paste("Census API call:", call_url))
  }

  # Make sure call status returns 200, else, print the error message for the user.
  # Try to handle 204's here
  if (call$status_code == 204) {

    if (sumfile == "ddhca") {
      rlang::abort(c("Your DDHC-A request returned No Content from the API.",
                     "i" = "The DDHC-A file uses an 'adaptive design' where data availability varies by geography and by population group.",
                     "i" = "Read Section 3-1 at https://www2.census.gov/programs-surveys/decennial/2020/technical-documentation/complete-tech-docs/detailed-demographic-and-housing-characteristics-file-a/2020census-detailed-dhc-a-techdoc.pdf for more information.",
                     "i" = "In tidycensus, use the function `check_ddhca_groups()` to see if your data is available."))

    } else {
      rlang::abort("No content was returned from the API.  Please refine your selection.")

    }

  }

  if (call$status_code != 200) {
    msg <- content(call, as = "text")

    if (grepl("The requested resource is not available", msg)) {
      stop("One or more of your requested variables is likely not available at the requested geography.  Please refine your selection.", call. = FALSE)
    } else if (grepl("unknown variable", msg) && length(variables) > 1 && year < 2010) {
      stop(sprintf("The Census API has returned the error message %s.\nThis may be due to mixing SF1 and SF3 variables. If so, separate your requests to SF1 and SF3 when using `get_decennial()` by using the `sumfile` argument in separate calls.", msg))
    } else {
      stop(sprintf("Your API call has errors.  The API message returned is %s.", msg), call. = FALSE)
    }

  }


  # callStatus <- http_status(call)
  # if (callStatus$reason != "OK") {
  #   if (sumfile == "sf1") {
  #     message("Checking SF3 API for data...")
  #   } else {
  #     message(callStatus$category, " ", callStatus$reason, " ", callStatus$message)
  #   }
  # } else {
  #   content <- content(call, as = "text")
  # }

  content <- content(call, as = "text")

  if (grepl("You included a key with this request", content)) {
    stop("You have supplied an invalid or inactive API key. To obtain a valid API key, visit https://api.census.gov/data/key_signup.html. To activate your key, be sure to click the link provided to you in the email from the Census Bureau that contained your key.", call. = FALSE)
  }

  # Fix issue in SF3 2000 API - https://github.com/walkerke/tidycensus/issues/22
  if (year == 2000 && sumfile == "sf3") {
    content <- str_replace(content, 'O"Brien', "O'Brien")
    content <- str_replace(content, 'Prince George"s', "Prince George's")
    content <- str_replace(content, 'Queen Anne"s', "Queen Anne's")
    content <- str_replace(content, 'St. Mary"s', "St. Mary's")

  }

  dat <- fromJSON(content)

  colnames(dat) <- dat[1,]

  dat <- as_tibble(dat)

  dat <- dat[-1,]

  if (year == 1990) {
    dat <- rename(dat, NAME = ANPSADPI)
  }

  vnum <- variables[variables != "POPGROUP"]

  dat[vnum] <- lapply(dat[vnum], as.numeric)

  v2 <- c(variables, "NAME", "POPGROUP")

  # Get the geography ID variables
  id_vars <- names(dat)[! names(dat) %in% v2]

  # Paste into a GEOID column
  # Apply tract band-aid (GH issue #317)
  if ("tract" %in% id_vars && year == 2000) {
    dat$tract <- stringr::str_pad(dat$tract, 6, "right", "0")
  }

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


load_data_estimates <- function(geography, product = NULL, variables = NULL, key, year,
                                time_series, state = NULL, county = NULL, show_call = FALSE) {

  if (!is.null(product)) {
    if (!product %in% c("population", "components",
                        "charagegroups", "housing")) {
      stop("You have selected an invalid product.  Valid requests are 'population', 'components', 'characteristics', and 'housing'.", call. = FALSE)
    }
  }

  for_area <- paste0(geography, ":*")

  if (year >= 2019) {
    geo_name <- "NAME"
  } else {
    geo_name <- "GEONAME"
  }

  if (is.null(variables)) {
    if (is.null(product)) {
      stop("Either a product or a vector of variables (from a single product) is required.", call. = FALSE)
    } else {
      if (product == "population") {
        vars_to_get <- paste0(geo_name, ",POP,DENSITY")
      } else if (product == "components") {
        vars_to_get <- paste0(geo_name, ",BIRTHS,DEATHS,DOMESTICMIG,INTERNATIONALMIG,NATURALINC,NETMIG,RBIRTH,RDEATH,RDOMESTICMIG,RINTERNATIONALMIG,RNATURALINC,RNETMIG")
      } else if (product == "housing") {
        vars_to_get <- paste0(geo_name, ",HUEST")
      } else {
        stop("Other products are not yet supported.", call. = FALSE)
      }
    }
  } else {
    if (!is.null(product)) {
      if (product != "charagegroups") {
        stop("Please specify either a product or a vector of variables, but not both.", call. = FALSE)
      }
    } else {
      if (all(variables %in% c("POP", "DENSITY"))) {
        product <- "population"
      } else if (all(variables %in% c("BIRTHS", "DEATHS", "DOMESTICMIG", "INTERNATIONALMIG", "NATURALINC", "NETMIG", "RBIRTH", "RDEATH", "RDOMESTICMIG", "RINTERNATIONALMIG", "RNATURALINC", "RNETMIG"))) {
        product <- "components"
      } else if (all(variables %in% c("HUEST"))) {
        product <- "housing"
      } else {
        stop(
          paste('Variables must be a subset of only one of the following sets of valid variables:',
                'for population estimates, c("POP", "DENSITY");',
                'for housing unit estimates, c("HUEST");',
                'and for components of change estimates, c("BIRTHS", "DEATHS", "DOMESTICMIG", "INTERNATIONALMIG", "NATURALINC", "NETMIG", "RBIRTH", "RDEATH", "RDOMESTICMIG", "RINTERNATIONALMIG", "RNATURALINC", "RNETMIG").'),
          call. = FALSE)
      }
    }
    vars_to_get <- paste0(geo_name, ",", paste0(variables, collapse = ","))
  }

  if (time_series) {
    if (product == "components") {
      if (year >= 2018) {
        vars_to_get <- paste0(vars_to_get, ",PERIOD_CODE")
      } else {
        vars_to_get <- paste0(vars_to_get, ",PERIOD")
      }
    } else {
      if (year >= 2018) {
        vars_to_get <- paste0(vars_to_get, ",DATE_CODE")
      } else {
        vars_to_get <- paste0(vars_to_get, ",DATE_")
      }
    }
  }

  base <- sprintf("https://api.census.gov/data/%s/pep/%s", year, product)

  if (!is.null(state)) {

    state <- map_chr(state, function(x) {
      validate_state(x)
    })

    if (length(state) > 1) {
      state <- paste0(state, collapse = ",")
    }

    if (geography == "state") {
      for_area <- paste0("state:", state)
    }

    if (!is.null(county)) {

      county <- map_chr(county, function(x) {
        validate_county(state, x)
      })

      if (length(county) > 1) {
        county <- paste0(county, collapse = ",")
      }

      if (geography == "county") {

        for_area <- paste0("county:", county)
        in_area <- paste0("state:", state)

      } else {

        in_area <- paste0("state:", state,
                          "+county:", county)
      }

    } else {
      in_area <- paste0("state:", state)
    }




    if (geography == "state" && !is.null(state)) {

      call <- GET(base, query = list(get = vars_to_get,
                                     "for" = for_area,
                                     key = key))
    } else {

      call <- GET(base, query = list(get = vars_to_get,
                                     "for" = for_area,
                                     "in" = in_area,
                                     key = key))
    }


  }

  else {

    call <- GET(base, query = list(get = vars_to_get,
                                   "for" = paste0(geography, ":*"),
                                   key = key))
  }

  if (show_call) {
    call_url <- gsub("&key.*", "", call$url)
    message(paste("Census API call:", call_url))
  }

  # Make sure call status returns 200, else, print the error message for the user.
  if (call$status_code != 200) {
    msg <- content(call, as = "text")

    if (grepl("The requested resource is not available", msg)) {
      stop("One or more of your requested variables is likely not available at the requested geography.  Please refine your selection.", call. = FALSE)
    } else {
      stop(sprintf("Your API call has errors.  The API message returned is %s.", msg), call. = FALSE)
    }

  }


  content <- content(call, as = "text")

  if (grepl("You included a key with this request", content)) {
    stop("You have supplied an invalid or inactive API key. To obtain a valid API key, visit https://api.census.gov/data/key_signup.html. To activate your key, be sure to click the link provided to you in the email from the Census Bureau that contained your key.", call. = FALSE)
  }

  dat <- fromJSON(content)

  colnames(dat) <- dat[1,]

  dat <- as_tibble(dat)

  dat <- dat[-1,]

  var_vector <- unlist(strsplit(vars_to_get, split = ","))

  var_vector <- var_vector[var_vector != geo_name]

  dat[var_vector] <- lapply(dat[var_vector], as.numeric)

  v2 <- c(var_vector, geo_name)

  if ("county" %in% names(dat)) {
    dat$county <- stringr::str_pad(dat$county, 3, "left", "0")
  }

  # Get the geography ID variables
  id_vars <- names(dat)[! names(dat) %in% v2]

  # Paste into a GEOID column
  dat$GEOID <- do.call(paste0, dat[id_vars])

  # Now, remove them
  dat <- dat[, !(names(dat) %in% id_vars)]

  return(dat)
}

load_data_pums <- function(variables, state, puma, key, year, survey,
                           variables_filter, recode, show_call) {

  # for which years is data dictionary available in pums_variables?
  # we'll use this a couple times later on
  recode_years <- 2017:2023

  base <- sprintf("https://api.census.gov/data/%s/acs/%s/pums",
                  year, survey)


  if (!is.null(puma)) {

    if (length(state) > 1) {
      stop('When requesting PUMAs for more than one state, you must set state to "multiple" and set puma to a named vector of state/PUMA pairs.', call. = FALSE)
    }

    # pumas in multiple states can be requested with a named vector of
    # state / puma pairs in the puma argument of get_pums()
    if (state == "multiple") {

      # print FIPS code of states used just once
      purrr::walk(unique(names(puma)), validate_state)

      geo <- purrr::map2_chr(names(puma), unname(puma), function(x, y) {
        paste0("7950000US", suppressMessages(validate_state(x)), y)
      })

      geo <- paste0(geo, collapse = ",")

    } else {
      # if PUMAs requested are in one state only
      state <- validate_state(state)
      geo <- purrr::map_chr(puma, function(x) {
        paste0("7950000US", state, x)
      })

      if (length(puma) > 1) {
        geo <- paste0(geo, collapse = ",")
      }
    }
  } else {
    # if no PUMAs specified, get all PUMAs in each state requested
    if (!is.null(state)) {
      geo <- purrr::map_chr(state, function(x) {
        paste0("0400000US", validate_state(x))
      })

    } else {
      geo <- NULL
    }

    if (length(state) > 1) {
      geo <- paste0(geo, collapse = ",")

    }
  }

  # Handle variables & variable filter
  # If a variables filter is supplied, those variables should be removed from
  # variables (if applicable)
  if (!is.null(variables_filter)) {
    filter_names <- names(variables_filter)

    variables <- variables[!variables %in% filter_names]

    if (length(variables) > 0) {
      var <- paste0(variables, collapse = ",")

      # Handle housing-only query
      if ("SPORDER" %in% filter_names) {
        vars_to_get <- paste0("SERIALNO,WGTP,PWGTP,", var)
      } else {
        vars_to_get <- paste0("SERIALNO,SPORDER,WGTP,PWGTP,", var)
      }
    } else {
      if ("SPORDER" %in% filter_names) {
        vars_to_get <- "SERIALNO,WGTP,PWGTP"
      } else {
        vars_to_get <- "SERIALNO,SPORDER,WGTP,PWGTP"
      }
    }

    # If geo is NULL, state should be added back in here
    if (is.null(geo)) {
      vars_to_get <- paste0(vars_to_get, ",ST")
    }

    # Combine the default query with the variables filter query
    query_default <- list(get = vars_to_get,
                          ucgid = geo,
                          key = key)

    # Collapse vectors if supplied
    variables_filter_collapsed <- purrr::map(variables_filter,
                                             ~{paste0(.x, collapse = ",")})

    query <- c(
      list(get = vars_to_get),
      variables_filter_collapsed,
      list(ucgid = geo, key = key)
    )

  } else {

    var <- paste0(variables, collapse = ",")

    vars_to_get <- paste0("SERIALNO,SPORDER,WGTP,PWGTP,", var)

    # If geo is NULL, state should be added back in here
    if (is.null(geo)) {
      if (year > 2022) {
        vars_to_get <- paste0(vars_to_get, ",STATE")
      } else {
        vars_to_get <- paste0(vars_to_get, ",ST")
      }
    }

    query <- list(get = vars_to_get,
                  ucgid = geo,
                  key = key)

  }

  call <- httr::GET(base,
                    query = query,
                    httr::progress())

  if (show_call) {
    call_url <- gsub("&key.*", "", call$url)
    message(paste("Census API call:", call_url))
  }

  # Make sure call status returns 200, else, print the error message for the user.
  if (call$status_code != 200) {
    msg <- content(call, as = "text")

    if (grepl("The requested resource is not available", msg)) {
      stop("One or more of your requested variables is likely not available at the requested geography.  Please refine your selection.", call. = FALSE)
    } else {
      stop(sprintf("Your API call has errors.  The API message returned is %s.", msg), call. = FALSE)
    }

  }


  content <- httr::content(call, as = "text")

  if (grepl("You included a key with this request", content)) {
    stop("You have supplied an invalid or inactive API key. To obtain a valid API key, visit https://api.census.gov/data/key_signup.html. To activate your key, be sure to click the link provided to you in the email from the Census Bureau that contained your key.", call. = FALSE)
  }

  dat <- jsonlite::fromJSON(content)

  colnames(dat) <- dat[1,]

  dat <- dplyr::as_tibble(dat, .name_repair = "minimal")

  dat <- dat[-1,]

  # Convert the weights columns to numeric
  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::contains("WGTP"),
        as.numeric
      )
    )
  # dat$WGTP <- as.numeric(dat$WGTP)
  # dat$PWGTP <- as.numeric(dat$PWGTP)

  # Filter the pums lookup table for the selected year and survey
  pums_variables_filter <- tidycensus::pums_variables %>%
    filter(year == !!year, survey == !!survey)

  # Do some clean up of the API response: pad returned values with 0s when
  # necessary to match data dictionary codes and
  # convert variables to numeric according to data dictionary

  # Only works for years included in pums_variables data dictionary
  if (year %in% recode_years) {
    var_val_length <- pums_variables_filter %>%
      filter(!is.na(.data$val_length)) %>%
      distinct(.data$var_code, .data$val_length, .data$val_na)

    num_vars <- pums_variables_filter %>%
      filter(.data$data_type == "num") %>%
      distinct(.data$var_code) %>%
      pull()

    # For all variables in which we know what the length should be, pad with 0s
    dat_padded <- suppressWarnings(
      dat %>%
        select(.data$SERIALNO, .data$SPORDER, any_of(var_val_length$var_code)) %>%
        pivot_longer(
          cols = -c(.data$SERIALNO, .data$SPORDER),
          names_to = "var_code",
          values_to = "val"
        ) %>%
        left_join(var_val_length, by = "var_code") %>%
        mutate(
          val = ifelse(!is.na(.data$val_na) & .data$val_na == .data$val, strrep("b", .data$val_length), .data$val),
          val = ifelse(.data$var_code != "NAICSP", str_pad(.data$val, .data$val_length, pad = "0"), .data$val),
          val = ifelse(.data$var_code == "NAICSP" & .data$val == "*", "bbbbbbbb", .data$val),  # special NULL value returned by API for this var
        ) %>%
        select(-.data$val_length, -.data$val_na) %>%
        pivot_wider(
          names_from = .data$var_code,
          values_from = .data$val
        )
    )

    # Rejoin padded variables to API return
    dat <- dat %>%
      select(.data$SERIALNO, .data$SPORDER, !any_of(var_val_length$var_code)) %>%
      left_join(dat_padded, by = c("SERIALNO", "SPORDER")) %>%
      mutate_at(vars(any_of(num_vars)), as.double)
    }

  # Do you want to return value labels also?
  if (recode) {

    # Only works for years included in pums_variables data dictionary
    if (year %in% recode_years) {
      var_lookup <- pums_variables_filter %>%
        select(.data$var_code, val = .data$val_min, .data$val_label)

      # Vector of variables that are possible to recode
      vars_to_recode <- pums_variables_filter %>%
        filter(.data$recode) %>%
        distinct(.data$var_code) %>%
        pull()

      # Pivot to long format and join variable codes to lookup table with labels
      recoded_long <- suppressWarnings(
        dat %>%
        select(.data$SERIALNO, .data$SPORDER, any_of(vars_to_recode)) %>%
        pivot_longer(
          cols = -c(.data$SERIALNO, .data$SPORDER),
          names_to = "var_code",
          values_to = "val"
        ) %>%
        left_join(var_lookup, by = c("var_code", "val")) %>%
        select(-.data$val)
      )

      # Create a "pivot spec" with nicer names for the labeled columns
      # https://tidyr.tidyverse.org/articles/pivot.html#wider-1
      spec <- recoded_long %>%
        build_wider_spec(
          names_from = .data$var_code,
          values_from = .data$val_label
        ) %>%
        mutate(.name = paste0(.data$var_code, "_label", ""))

      recoded_wide <- recoded_long %>%
        pivot_wider_spec(spec)

      recode_v <- recoded_long %>% pull(var_code) %>% unique()

      for (var in recode_v){

        order_v <- var_lookup %>%
          filter(var_code==var) %>%
          pull(val_label)

        var_label <- str_c(var, "_label")

        recoded_wide[[var_label]] <-
          readr::parse_factor(recoded_wide[[var_label]], ordered=TRUE, levels=order_v)

      }


      # Join recoded columns to API return
      dat <- dat %>%
        left_join(recoded_wide, by = c("SERIALNO", "SPORDER"))
    } else {
      message(paste("Recoding is currently supported for",
                    min(recode_years), "-", max(recode_years),
                    "data. Returning original data only."))
      }
    }
  return(dat)
}

load_data_pums_vacant <- function(variables, state, puma, key, year, survey,
                           variables_filter, recode, show_call) {

  # for which years is data dictionary available in pums_variables?
  # we'll use this a couple times later on
  recode_years <- 2017:2023

  base <- sprintf("https://api.census.gov/data/%s/acs/%s/pums",
                  year, survey)

  # Parse the variables for housing vs. person variables
  housing_catalog <- pums_variables %>%
    dplyr::filter(level != "person" | is.na(level)) %>%
    dplyr::pull(var_code) %>%
    unique()

  housing_variables <- variables[variables %in% housing_catalog]

  if (!is.null(puma)) {

    if (length(state) > 1) {
      stop('When requesting PUMAs for more than one state, you must set state to "multiple" and set puma to a named vector of state/PUMA pairs.', call. = FALSE)
    }

    # pumas in multiple states can be requested with a named vector of
    # state / puma pairs in the puma argument of get_pums()
    if (state == "multiple") {

      # print FIPS code of states used just once
      purrr::walk(unique(names(puma)), validate_state)

      geo <- purrr::map2_chr(names(puma), unname(puma), function(x, y) {
        paste0("7950000US", suppressMessages(validate_state(x)), y)
      })

      geo <- paste0(geo, collapse = ",")

    } else {
      # if PUMAs requested are in one state only
      state <- validate_state(state)
      geo <- purrr::map_chr(puma, function(x) {
        paste0("7950000US", state, x)
      })

      if (length(puma) > 1) {
        geo <- paste0(geo, collapse = ",")
      }
    }
  } else {
    # if no PUMAs specified, get all PUMAs in each state requested
    if (!is.null(state)) {
      geo <- purrr::map_chr(state, function(x) {
        paste0("0400000US", validate_state(x))
      })

    } else {
      geo <- NULL
    }

    if (length(state) > 1) {
      geo <- paste0(geo, collapse = ",")

    }
  }

  # Handle variables & variable filter
  # In this scenario, the "filter" is for vacant households
  # This will be specified on the `get_pums()` side
  if (!is.null(variables_filter)) {
    filter_names <- names(variables_filter)

    housing_variables <- housing_variables[!housing_variables %in% filter_names]

    var <- paste0(housing_variables, collapse = ",")

    vars_to_get <- paste0("SERIALNO,WGTP,", var)

    # If geo is NULL, state should be added back in here
    if (is.null(geo)) {
      if (year > 2022) {
        vars_to_get <- paste0(vars_to_get, ",STATE")
      } else {
        vars_to_get <- paste0(vars_to_get, ",ST")
      }
    }

    # Combine the default query with the variables filter query
    query_default <- list(get = vars_to_get,
                          ucgid = geo,
                          key = key)

    # Collapse vectors if supplied
    variables_filter_collapsed <- purrr::map(variables_filter,
                                             ~{paste0(.x, collapse = ",")})

    query <- c(
      list(get = vars_to_get),
      variables_filter_collapsed,
      list(ucgid = geo, key = key)
    )

  } else {

    # Not implemented here
    # var <- paste0(variables, collapse = ",")
    #
    # vars_to_get <- paste0("SERIALNO,SPORDER,WGTP,PWGTP,", var)
    #
    # # If geo is NULL, state should be added back in here
    # if (is.null(geo)) {
    #   vars_to_get <- paste0(vars_to_get, ",ST")
    # }
    #
    # query <- list(get = vars_to_get,
    #               ucgid = geo,
    #               key = key)

  }

  call <- httr::GET(base,
                    query = query,
                    httr::progress())

  if (show_call) {
    call_url <- gsub("&key.*", "", call$url)
    message(paste("Census API call:", call_url))
  }

  # Make sure call status returns 200, else, print the error message for the user.
  if (call$status_code != 200) {
    msg <- content(call, as = "text")

    if (grepl("The requested resource is not available", msg)) {
      stop("One or more of your requested variables is likely not available at the requested geography.  Please refine your selection.", call. = FALSE)
    } else {
      stop(sprintf("Your API call has errors.  The API message returned is %s.", msg), call. = FALSE)
    }

  }


  content <- httr::content(call, as = "text")

  if (grepl("You included a key with this request", content)) {
    stop("You have supplied an invalid or inactive API key. To obtain a valid API key, visit https://api.census.gov/data/key_signup.html. To activate your key, be sure to click the link provided to you in the email from the Census Bureau that contained your key.", call. = FALSE)
  }

  dat <- jsonlite::fromJSON(content)

  colnames(dat) <- dat[1,]

  dat <- dplyr::as_tibble(dat, .name_repair = "minimal")

  dat <- dat[-1,]

  # Convert the weights columns to numeric
  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::contains("WGTP"),
        as.numeric
      )
    )
  # dat$WGTP <- as.numeric(dat$WGTP)
  # dat$PWGTP <- as.numeric(dat$PWGTP)

  # Filter the pums lookup table for the selected year and survey
  pums_variables_filter <- tidycensus::pums_variables %>%
    filter(year == !!year, survey == !!survey)

  # Do some clean up of the API response: pad returned values with 0s when
  # necessary to match data dictionary codes and
  # convert variables to numeric according to data dictionary

  # Only works for years included in pums_variables data dictionary
  if (year %in% recode_years) {
    var_val_length <- pums_variables_filter %>%
      dplyr::filter(!is.na(.data$val_length)) %>%
      dplyr::distinct(.data$var_code, .data$val_length, .data$val_na)

    num_vars <- pums_variables_filter %>%
      dplyr::filter(.data$data_type == "num") %>%
      dplyr::distinct(.data$var_code) %>%
      dplyr::pull()

    # For all variables in which we know what the length should be, pad with 0s
    dat_padded <- suppressWarnings(
      dat %>%
        dplyr::select(.data$SERIALNO, dplyr::any_of(var_val_length$var_code)) %>%
        tidyr::pivot_longer(
          cols = -c(.data$SERIALNO),
          names_to = "var_code",
          values_to = "val"
        ) %>%
        dplyr::left_join(var_val_length, by = "var_code") %>%
        dplyr::mutate(
          val = ifelse(!is.na(.data$val_na) & .data$val_na == .data$val, strrep("b", .data$val_length), .data$val),
          val = ifelse(.data$var_code != "NAICSP", str_pad(.data$val, .data$val_length, pad = "0"), .data$val),
          val = ifelse(.data$var_code == "NAICSP" & .data$val == "*", "bbbbbbbb", .data$val),  # special NULL value returned by API for this var
        ) %>%
        dplyr::select(-.data$val_length, -.data$val_na) %>%
        tidyr::pivot_wider(
          names_from = .data$var_code,
          values_from = .data$val
        )
    )

    # Rejoin padded variables to API return
    dat <- dat %>%
      dplyr::select(.data$SERIALNO, !dplyr::any_of(var_val_length$var_code)) %>%
      dplyr::left_join(dat_padded, by = c("SERIALNO")) %>%
      dplyr::mutate_at(vars(dplyr::any_of(num_vars)), as.double)
  }

  # Do you want to return value labels also?
  if (recode) {

    # Only works for years included in pums_variables data dictionary
    if (year %in% recode_years) {
      var_lookup <- pums_variables_filter %>%
        dplyr::select(.data$var_code, val = .data$val_min, .data$val_label)

      # Vector of variables that are possible to recode
      vars_to_recode <- pums_variables_filter %>%
        dplyr::filter(.data$recode) %>%
        dplyr::distinct(.data$var_code) %>%
        dplyr::pull()

      # Pivot to long format and join variable codes to lookup table with labels
      recoded_long <- suppressWarnings(
        dat %>%
          dplyr::select(.data$SERIALNO, dplyr::any_of(vars_to_recode)) %>%
          tidyr::pivot_longer(
            cols = -c(.data$SERIALNO),
            names_to = "var_code",
            values_to = "val"
          ) %>%
          dplyr::left_join(var_lookup, by = c("var_code", "val")) %>%
          dplyr::select(-.data$val)
      )

      # Create a "pivot spec" with nicer names for the labeled columns
      # https://tidyr.tidyverse.org/articles/pivot.html#wider-1
      spec <- recoded_long %>%
        tidyr::build_wider_spec(
          names_from = .data$var_code,
          values_from = .data$val_label
        ) %>%
        dplyr::mutate(.name = paste0(.data$var_code, "_label", ""))

      recoded_wide <- recoded_long %>%
        tidyr::pivot_wider_spec(spec)

      recode_v <- recoded_long %>% dplyr::pull(var_code) %>% unique()

      for (var in recode_v){

        order_v <- var_lookup %>%
          dplyr::filter(var_code==var) %>%
          dplyr::pull(val_label)

        var_label <- stringr::str_c(var, "_label")

        recoded_wide[[var_label]] <-
          readr::parse_factor(recoded_wide[[var_label]], ordered=TRUE, levels=order_v)

      }


      # Join recoded columns to API return
      dat <- dat %>%
        dplyr::left_join(recoded_wide, by = c("SERIALNO"))
    } else {
      message(paste("Recoding is currently supported for",
                    min(recode_years), "-", max(recode_years),
                    "data. Returning original data only."))
    }
  }
  return(dat)
}

load_data_flows <- function(geography, variables, key, year, state = NULL,
                            county = NULL, msa = NULL, show_call = FALSE) {

  base <- paste("https://api.census.gov/data", year, "acs", "flows", sep = "/")

  # collapse variables in a single string for api call
  vars_to_get <- paste0(variables, collapse = ",")

  # get all in a given geography unless specified
  for_area <- paste0(geography, ":*")
  in_area <- NULL

  # turn text states into fips if specified, else get all states
  # collapse into string for api call
  if (!is.null(state)) {
    state <- paste(map_chr(state, ~ validate_state(.x)), collapse = ",")
  } else {
    state <- "*"
  }

  # turn text counties into fips if specified, validate with specified state
  # collapse into string for api call
  # can only filter counties for one state
  if (!is.null(county)) {
    county <- paste(map_chr(county, ~ validate_county(state, .x)), collapse = ",")
  }

  # if msa codes specified, collapse into string for api call
  if (!is.null(msa)) {
    msa <- paste0(msa, collapse = ",")
  }

  # define counties (for) within states (in) for filtering in api call
  if (geography == "county") {
    if (!is.null(county)) {
    for_area <- paste0("county:", county)
    }
    if (!is.null(state)) {
      in_area <- paste0("state:", state)
    }
  }

  # if county is defined, construct for call using state and county
  # otherwise filter only at state level
  if (geography == "county subdivision") {
    if (!is.null(county)) {
      in_area <- paste0("state:", state, " ", "county:", county)
    } else {
      in_area <- paste0("state:", state)
    }
  }

  # if msas are specified construct for call to filter by msa
  if (geography == "metropolitan statistical area/micropolitan statistical area") {

    # before 2016 msa geography name in api is different
    if (year <= 2015) {
      geography <- "metropolitan statistical areas"
    }

    if (!is.null(msa)) {
      for_area <- paste0(geography, ":", msa)
    } else {
      for_area <- paste0(geography, ":*")
    }
  }

  # build query for api call
  # for calls where in_area is not defined, "in" will be NULL and not included in GET()
  query <- list(get = vars_to_get,
                "for" = for_area,
                "in" = in_area,
                key = key)

  # execute api call
  call <- GET(base, query = query)

  # print nicely formatted api call if requested
  if (show_call) {
    call_url <- gsub("&key.*", "", call$url)
    message(paste("Census API call:", utils::URLdecode(call_url)))
  }

  # Make sure call status returns 200, else, print the error message for the user.
  if (call$status_code != 200) {
    msg <- content(call, as = "text")

    if (grepl("The requested resource is not available", msg)) {
      stop("One or more of your requested variables is likely not available at the requested geography.  Please refine your selection.", call. = FALSE)
    } else {
      stop(sprintf("Your API call has errors.  The API message returned is %s.", msg), call. = FALSE)
    }
  }

  # convert json response to text
  content <- content(call, as = "text")

  if (grepl("You included a key with this request", content)) {
    stop("You have supplied an invalid or inactive API key. To obtain a valid API key, visit https://api.census.gov/data/key_signup.html. To activate your key, be sure to click the link provided to you in the email from the Census Bureau that contained your key.", call. = FALSE)
  }

  # convert response to json
  dat <- fromJSON(content)

  # first row of response are colnames
  colnames(dat) <- dat[1, ]
  dat <- dat[-1, ]

  # convert to tibble
  dat <- as_tibble(dat)

  # remove columns that contains geographies automatically returned by api - we already have this as GEOID1
  dat <- dat[, !(names(dat) %in% c("state", "county", "county subdivision",
                                   "metropolitan statistical area/micropolitan statistical area",
                                   "metropolitan statistical areas"))]

  # convert vars to numeric, skip geoids and names
  str_vars <- variables[grepl("_NAME|GEOID|^STATE|^COUNTY|^MCD|^METRO", variables)]
  num_vars <- variables[!variables %in% str_vars]
  dat[num_vars] <- lapply(dat[num_vars], as.numeric)

  return(dat)

}
