# find all variables in acs that don't have margins of error associated with
# them then paste use this vector of variables in format_variables_acs() as no
# moe variables

library(dplyr)
library(purrr)
library(stringr)
library(httr)
library(jsonlite)

get_no_moe_vars <- function(year){
  GET(str_c("https://api.census.gov/data/", year,"/acs/acs5/variables.json")) %>%
    content(as = "text") %>%
    fromJSON() %>%
    flatten_dfr(.id = "name") %>%
    mutate(moe = if_else(str_detect(attributes, "M"), TRUE, FALSE)) %>%
    filter(moe == FALSE) %>%
    mutate(name = str_sub(name, 1, -2)) %>%
    arrange(name) %>%
    select(name)
}

no_moe_vars <- 2017:2019 %>%
  map_df(get_no_moe_vars) %>%
  distinct(name) %>%
  arrange(name) %>%
  pull(name)


# copy output vector to clipboard
clipr::write_clip(no_moe_vars)

# then use datapasta::vector_paste() to paste into format_variables_acs()
# in 'R/load_data.R'