library(tidyverse)

clean_data_dict <- function(path, survey, year) {

  data_dict <- read.csv(
    path,
    col.names = c("type", "var_code", "data_type", "length",
                  "var_label", "val_max", "val_label"),
    colClasses = "character") %>%
    as_tibble() %>%
    mutate(data_type = if_else(data_type == "C", "chr", "num")) %>%
    distinct()

  var_name <- data_dict %>%
    filter(type == "NAME") %>%
    mutate(
      level = case_when(
        var_code == "ADJHSG"      ~ "housing",
        var_code == "SPORDER" ~ "person"
      )
    ) %>%
    fill(level) %>%
    select(var_code, var_label, data_type, level)

  num_vars <- var_name %>%
    filter(data_type == "num") %>%
    pull(var_code)

  b_vars <- data_dict %>%
    filter(data_type == "chr" & str_detect(val_max, "b"))

  b_vars_0 <- data_dict %>%
    filter(var_code %in% b_vars$var_code, var_label == "0") %>%
    distinct(var_code) %>%
    pull(var_code)

  b_vars_no_0 <- b_vars %>%
    filter(!var_code %in% b_vars_0) %>%
    distinct(var_code) %>%
    pull(var_code)

  var_value <- data_dict %>%
    filter(type == "VAL") %>%
    select(var_code, val_min = var_label, val_max, val_label)

  # Fix missing YRBLT value that pops up in 2021 and 2022, but is missing from the dict
  if (year >= 2021) {
    addl_yrblt <- tibble::tibble(
      var_code = "YRBLT",
      val_min = "1938",
      val_max = "1938",
      val_label = "N/A (GQ)"
    )

    var_value <- bind_rows(var_value, addl_yrblt)
  }

  pums_variables <- var_name %>%
    left_join(var_value, by = "var_code") %>%
    group_by(var_code) %>%
    mutate(recode = if_else(data_type == "chr" & n() > 1, TRUE, FALSE)) %>%
    group_by(var_code) %>%
    mutate(val_length = if_else(data_type == "chr", max(nchar(val_max)), NA_integer_)) %>%
    ungroup() %>%
    mutate(
      val_na = case_when(
        var_code %in% b_vars_0    ~ -1,
        var_code %in% b_vars_no_0 ~ 0
        ),
      survey = survey,
      year = year
      ) %>%

    select(survey, year, everything())

  # If the year is 2020, we need to pad the character values
  if (year == 2020) {
    pums_variables <- pums_variables %>%
      mutate(val_min = if_else(data_type == "chr", str_pad(val_min, val_length, "left", "0"), val_min),
             val_max = if_else(data_type == "chr", str_pad(val_max, val_length, "left", "0"), val_max))
  }

  # Correct problem field with en-dash
  pums_variables <- pums_variables %>%
    mutate(val_label = str_replace_all(val_label, "Field of degree science and engineering related flag – National Science Foundation definition", "Field of degree science and engineering related flag - NSF definition"))

  print(glue::glue("{survey}{year}"))
  pums_variables

}

files <- list.files("data-raw", pattern = "^pums-dict.*\\.csv$", full.names = TRUE)
survey <- str_extract(files, "acs[0-9]")
year <- str_extract(files, "20[0-9]{2}")

pums_variables <- pmap_dfr(list(files, survey, year), ~ clean_data_dict(..1, ..2, ..3))

usethis::use_data(pums_variables, overwrite = TRUE)
