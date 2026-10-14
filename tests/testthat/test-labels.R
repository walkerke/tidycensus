test_that("variable labels are cleaned across ACS and decennial styles", {
  expect_equal(
    clean_variable_label(c(
      "Estimate!!Total:!!Male:!!Under 5 years",
      "Estimate!!Total!!Male!!Under 5 years",
      " !!Total:!!Population of one race:!!White alone",
      "Total!!Male!!Under 1 year",
      "Percent!!SEX AND AGE!!Total population!!Male",
      "2019 Estimate!!EMPLOYMENT STATUS!!Population 16 years and over"
    )),
    c(
      "Total > Male > Under 5 years",
      "Total > Male > Under 5 years",
      "Total > Population of one race > White alone",
      "Total > Male > Under 1 year",
      "Percent: SEX AND AGE > Total population > Male",
      "2019: EMPLOYMENT STATUS > Population 16 years and over"
    )
  )
  expect_equal(clean_concept(c("SEX BY AGE [79]", "Sex by Age (White Alone)")),
               c("SEX BY AGE", "Sex by Age (White Alone)"))
})

test_that("label-based names add concepts only where labels collide, or always for 'concept'", {
  code <- c("B01001_003", "B01001A_003", "B19013_001")
  label <- c("Total > Male > Under 5 years", "Total > Male > Under 5 years",
             "Median household income in the past 12 months (in 2023 inflation-adjusted dollars)")
  concept <- c("Sex by Age", "Sex by Age (White Alone)",
               "Median Household Income in the Past 12 Months (in 2023 Inflation-Adjusted Dollars)")

  expect_message(
    nm <- label_names(code, label, concept, "label"),
    "share a label"
  )
  expect_equal(nm, c("total_male_under_5_years", "total_male_under_5_years_white_alone",
                     "median_household_income_in_the_past_12_months_in_2023_inflation_adjusted_dollars"))

  expect_equal(
    label_names(code, label, concept, "concept"),
    c("total_male_under_5_years_sex_by_age", "total_male_under_5_years_white_alone",
      "median_household_income_in_the_past_12_months_in_2023_inflation_adjusted_dollars")
  )

  # Totals from different tables get their concepts
  expect_equal(
    suppressMessages(label_names(c("B01001_001", "B19001_001"), c("Total", "Total"),
                                 c("Sex by Age", "Household Income"), "label")),
    c("total_sex_by_age", "total_household_income")
  )

  expect_error(label_names(c("A_001", "B_001"), c("Total", "Total"), c("Same", "Same"), "label"),
               "unique names")
})

test_that("get_acs() labels output and keeps names the user supplied", {
  rm(list = ls(tidycensus:::.label_cache), envir = tidycensus:::.label_cache)

  local_mocked_bindings(
    load_data_acs = function(...) {
      dplyr::tibble(GEOID = "44", NAME = "Rhode Island",
                    B01001_003E = 30000, B01001_003M = 100,
                    B01001A_003E = 20000, B01001A_003M = 90)
    },
    load_variables = function(year, dataset, ...) {
      dplyr::tibble(name = c("B01001_003", "B01001A_003"),
                    label = c("Estimate!!Total:!!Male:!!Under 5 years", "Estimate!!Total:!!Male:!!Under 5 years"),
                    concept = c("Sex by Age", "Sex by Age (White Alone)"))
    }
  )
  vars <- c("B01001_003", "B01001A_003")

  tidy <- suppressMessages(get_acs("state", vars, state = "RI", year = 2023, labels = TRUE, key = "test-key"))
  expect_setequal(unique(tidy$variable), c("total_male_under_5_years", "total_male_under_5_years_white_alone"))

  named <- suppressMessages(get_acs("state", vars, state = "RI", year = 2023, labels = TRUE,
                                    output = "wide", key = "test-key"))
  expect_equal(names(named)[3:6], c("total_male_under_5_yearsE", "total_male_under_5_yearsM",
                                    "total_male_under_5_years_white_aloneE", "total_male_under_5_years_white_aloneM"))

  concept <- suppressMessages(get_acs("state", vars, state = "RI", year = 2023, labels = "concept",
                                      key = "test-key"))
  expect_setequal(unique(concept$variable), c("total_male_under_5_years_sex_by_age",
                                           "total_male_under_5_years_white_alone"))

  wide <- suppressMessages(get_acs("state", vars, state = "RI", year = 2023, labels = "attributes",
                                   output = "wide", key = "test-key"))
  expect_equal(names(wide)[3:4], c("B01001_003E", "B01001_003M"))
  expect_equal(attr(wide$B01001_003E, "label"), "Total > Male > Under 5 years")
  expect_equal(attr(wide$B01001A_003E, "concept"), "Sex by Age (White Alone)")

  # attributes have no effect on tidy output
  expect_message(
    tidy_attr <- get_acs("state", vars, state = "RI", year = 2023, labels = "attributes", key = "test-key"),
    "only applies to wide output"
  )
  expect_identical(tidy_attr, suppressMessages(get_acs("state", vars, state = "RI", year = 2023, key = "test-key")))

  mine <- suppressMessages(get_acs("state", c(boys = "B01001_003", "B01001A_003"), state = "RI", year = 2023,
                                   labels = TRUE, key = "test-key"))
  expect_setequal(unique(mine$variable), c("boys", "total_male_under_5_years"))

  expect_error(get_acs("state", vars, state = "RI", year = 2023, labels = "yes", key = "test-key"),
               "`labels` must be")
})

test_that("unnamed elements of partly named variables keep their IDs", {
  expect_equal(fill_variable_names(c(income = "B19013_001", "B25064_001")),
               c(income = "B19013_001", B25064_001 = "B25064_001"))
  expect_equal(fill_variable_names(c("B19013_001", "B25064_001")), c("B19013_001", "B25064_001"))
})

test_that("load_variables() cleans labels only when asked", {
  variables_json <- '{"variables":{"B01001_003E":{"label":"Estimate!!Total:!!Male:!!Under 5 years","concept":"Sex by Age","predicateType":"int"}}}'
  local_mocked_bindings(census_api_get = function(url, query = list(), ...) {
    if (grepl("variables[.]json$", url)) {
      httr2::response(200, url = url, body = charToRaw(variables_json))
    } else {
      httr2::response(404, url = url, body = charToRaw("Not Found"))
    }
  })

  expect_equal(load_variables(2023, "acs1", key = "test-key")$label, "Estimate!!Total:!!Male:!!Under 5 years")
  v <- load_variables(2023, "acs1", key = "test-key", clean_labels = TRUE)
  expect_equal(names(v), c("name", "label", "concept"))
  expect_equal(v$label, "Total > Male > Under 5 years")
})
