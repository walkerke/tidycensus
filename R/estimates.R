#' Title
#'
#' @param geography geography
#' @param product product
#' @param variables variables
#' @param year year
#' @param state state
#' @param county county
#' @param output output
#' @param geometry geometry
#' @param keep_geo_vars TRUE or FALSE
#' @param shift_geo TRUE or FALSE
#' @param key The Census API key
#'
#' @return population estimates
#' @export
get_estimates <- function(geography, product = NULL, variables = NULL,
                          year = 2017, state = NULL, county = NULL,
                          output = "tidy", geometry = FALSE, keep_geo_vars = FALSE,
                          shift_geo = FALSE, key = NULL) {

  if (!product %in% c("population", "components", "monthly",
                      "age groups","housing")) {
    stop("You have selected an invalid product.  Valid requests are 'population', 'components', 'monthly', 'age groups', and 'housing'.", call. = FALSE)
  }

  if (Sys.getenv('CENSUS_API_KEY') != '') {

    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.')

  }

  if (product == "monthly") product <- "natmonthly"

  if (product == "age groups") product <- "charagegroups"

  base <- sprintf("https://api.census.gov/data/%s/pep/%s", year, product)

  for_area <- paste0(geography, ":*")

  vars_to_get <- function(product) {

    # Add variables functionality after the function is working
    if (product == "population") {
      return("GEONAME,POP,DENSITY")
    } else if (product == "components") {
      return("GEONAME,BIRTHS,DEATHS,DOMESTICMIG,INTERNATIONALMIG,NATURALINC,NETMIG,RBIRTH,RDEATH,RDOMESTICMIG,RINTERNATIONALMIG,RNATURALINC,RNETMIG")
    } else if (product == "housing") {
      return("GEONAME,HUEST")
    } else {
      stop("Other products are not yet supported.", call. = FALSE)
    }

  }

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

      call <- GET(base, query = list(get = vars_to_get(product),
                                     "for" = for_area,
                                     key = key))
    } else {

      call <- GET(base, query = list(get = vars_to_get(product),
                                     "for" = for_area,
                                     "in" = in_area,
                                     key = key))
    }


  }

  else {

    call <- GET(base, query = list(get = vars_to_get(product),
                                   "for" = paste0(geography, ":*"),
                                   key = key))
  }

  content <- content(call, as = "text")

  dat <- tbl_df(fromJSON(content))

  colnames(dat) <- dat[1,]

  dat <- dat[-1,]

  var_vector <- unlist(strsplit(vars_to_get(product), split = ","))

  var_vector <- var_vector[var_vector != "GEONAME"]

  dat[var_vector] <- lapply(dat[var_vector], as.numeric)

  v2 <- c(var_vector, "GEONAME")

  # Get the geography ID variables
  id_vars <- names(dat)[! names(dat) %in% v2]

  # Paste into a GEOID column
  dat$GEOID <- do.call(paste0, dat[id_vars])

  # Now, remove them
  dat <- dat[, !(names(dat) %in% id_vars)]

  return(dat)


}