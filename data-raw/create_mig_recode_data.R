# create migration recode look up table as dataset from csv
# if new breakdown vars get added to future years of migration api
# can add variable names and codes to the csv and recoding in get_flows()
# should still work with these new vars!

library(readr)
library(dplyr)
library(stringr)
library(usethis)

mig_recodes <- read_csv(
  "data-raw/mig-flow-recode.csv",
  col_types = cols(
    characteristic = col_character(),
    code = col_double(),
    desc = col_character(),
    ordered = col_logical()
    )
  ) %>%
  mutate(code = as.character(code)) %>%
  mutate(code = str_pad(code, 2, pad = "0"))

use_data(mig_recodes, overwrite = TRUE)
