# Helpers for post-processing Census API responses

census_missing_values <- -seq.int(111111111, 999999999, by = 111111111)

replace_census_missing <- function(data) {
  missing_character <- as.character(census_missing_values)

  data[] <- lapply(data, function(column) {
    if (is.numeric(column)) {
      column[column %in% census_missing_values] <- NA
    } else if (is.character(column)) {
      column[column %in% missing_character] <- NA_character_
    }

    column
  })

  data
}

recode_named_variables <- function(data, variables) {
  if (is.null(names(variables))) {
    return(data)
  }

  original <- unname(variables)
  replacement <- original

  # Preserve sequential replacement when an alias is another variable code.
  for (i in seq_along(original)) {
    replacement[replacement == original[i]] <- names(variables)[i]
  }

  character_columns <- which(vapply(data, is.character, logical(1)))

  for (j in character_columns) {
    index <- match(data[[j]], original)
    matched <- !is.na(index)
    data[[j]][matched] <- replacement[index[matched]]
  }

  data
}

recode_wide_variable_names <- function(data, variables) {
  if (is.null(names(variables))) {
    return(data)
  }

  new_names <- names(data)

  for (i in seq_along(variables)) {
    new_names <- stringr::str_replace(
      new_names,
      variables[i],
      names(variables)[i]
    )
  }

  names(data) <- new_names
  data
}
