# Validate an API call and return an appropriate error message

validate_call <- function(content, geography, year, dataset) {

  # Invalid API key
  if (grepl("Invalid Key", content)) {
    stop("You have entered an invalid Census API key. Check your API key string and try again. If you need an API key, you can get one at http://api.census.gov/data/key_signup.html.",
         call. = FALSE)
  } else if (grepl("error: unknown variable", content)) {
    stop(sprintf('One or more of your requested variables is invalid. Use `load_variables(%s, "%s")` to make sure that you entered the variable IDs correctly.', year, dataset), call. = FALSE)
  } else if (grepl("error: CSV not allowed", content)) {
    if (geography == "tract") {
      stop('Multiple states are not allowed when geography == "tract". Please see https://walkerke.github.io/2017/05/tidycensus-every-tract/ for how to combine tracts for multiple states with map_df or map/reduce/rbind.', call. = FALSE)
    } else if (geography == "block group") {
      stop('Multiple states and/or counties are not allowed when geography == "block group". Please see https://walkerke.github.io/2017/05/tidycensus-every-tract/ for how to combine datasets with map_df or map/reduce/rbind.', call. = FALSE)
    }
  } else if (grepl("unknown/unsupported geography", content)) {
    stop(sprintf("You have specified an invalid geography or a geography hierarchy that is currently unavailable in tidycensus. Please visit http://api.census.gov/data/%s/%s/geography.html for available geographies. tidycensus currently supports geographies nested within states and/or counties.", year, dataset), call. = FALSE)
  }

}