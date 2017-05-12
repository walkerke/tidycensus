#' Title
#'
#' @param endyear
#' @param state
#' @param geography
#' @param variables
#' @param output
#' @param key
#' @param county
#' @param geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_acs <- function(geography, variables, key, endyear = 2015, output = "tidy",
                    state = NULL, county = NULL, geometry = FALSE, keep_geo_vars = FALSE,
                    summary_var = NULL, summary_var_name = "summary", ...) {

  if (length(variables) > 50) {
    stop("The maximum number of variables supported by the Census API at one time is 50. Consider splitting your variables into multiple calls and using cbind/rbind to combine them.", call. = FALSE)
  }

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

  base <- paste0("http://api.census.gov/data/",
                 as.character(endyear),
                 "/acs5")

  if (!is.null(state)) {

    state <- validate_state(state)

    if (!is.null(county)) {

      county <- validate_county(state, county)

      in_area <- paste0("state:", state,
                        "+county:", county)

    } else {

      in_area <- paste0("state:", state)

    }

    call <- GET(base, query = list(get = var,
                                   "for" = paste0(geography, ":*"),
                                   "in" = in_area,
                                   key = api_key))
  }

   else {

     call <- GET(base, query = list(get = var,
                                    "for" = paste0(geography, ":*"),
                                    key = api_key))
  }



  content <- content(call, as = "text")

  dat <- tbl_df(fromJSON(content))

  colnames(dat) <- dat[1,]

  dat <- dat[-1,]

  var_vector <- unlist(strsplit(variables3, split = ","))

  l <- length(var_vector)

  if (length(var_vector) > 1) {

    dat[,var_vector] <- apply(dat[,var_vector], 2, function(x) as.numeric(x))

  } else if (length(var_vector) == 1) {

    dat[[var_vector]] <- as.numeric(dat[[var_vector]])

  }

  # Get the geography ID variables
  id_vars <- names(dat)[! names(dat) %in% var_vector]

  # Paste into a GEOID column
  dat$GEOID <- do.call(paste0, dat[id_vars])

  if (output == "tidy") {

    sub <- dat[c("GEOID", var_vector)]

    dat2 <- sub %>%
      gather(key = variable, value = value, -GEOID) %>%
      separate(variable, into = c("variable", "type"), sep = -2) %>%
      mutate(type = ifelse(type == "E", "estimate", "moe")) %>%
      spread(type, value)


  } else if (output == "wide") {

    dat2 <- dat

  }

  if (!is.null(summary_var)) {



  }

  if (geometry == TRUE) {

    geom <- use_tigris(geography = geography, year = endyear,
                       state = state, county = county, ...)

    if (keep_geo_vars == FALSE) {

      geom <- select(geom, GEOID, geometry)

    }

    # Merge and return the output
    out <- left_join(geom, dat2, by = "GEOID")

    return(out)

  } else {

    return(dat2)

  }

}