#' Title
#'
#' @param geography
#' @param variables
#' @param key
#' @param year
#' @param sumfile
#' @param state
#' @param county
#' @param geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_decennial <- function(geography, variables, key, year = 2010, sumfile = "sf1",
                   state = NULL, county = NULL, geometry = FALSE, output = "tidy",
                   keep_geo_vars = FALSE, ...) {

  if (geography %in% c("tract", "block group") & year == 1990 & is.null(county)) {
    stop("At the moment, tracts and block groups for 1990 require specifying a county.",
         call. = FALSE)
  }

  if (length(variables) > 50) {
    stop("The maximum number of variables supported by the Census API at one time is 50. Consider splitting your variables into multiple calls and using cbind/rbind to combine them.", call. = FALSE)
  }

  var <- paste(variables, sep = "", collapse = ",")

  base <- paste0("http://api.census.gov/data/",
                 as.character(year),
                 "/",
                 sumfile)

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

  l <- length(variables)

  if (length(variables) > 1) {

    dat[,variables] <- apply(dat[,variables], 2, function(x) as.numeric(x))

  } else if (length(variables) == 1) {

    dat[[variables]] <- as.numeric(dat[[variables]])

  }

  # Get the geography ID variables
  id_vars <- names(dat)[! names(dat) %in% variables]

  # Paste into a GEOID column
  dat$GEOID <- do.call(paste0, dat[id_vars])

  if (output == "tidy") {

    sub <- dat[c("GEOID", variables)]

    dat2 <- sub %>%
      gather(key = variable, value = value, -GEOID)

  } else if (output == "wide") {

    dat2 <- dat

  }

  if (geometry == TRUE) {

    geom <- use_tigris(geography = geography, year = year,
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