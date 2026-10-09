test_that("missing-value replacement preserves Census response semantics", {
  input <- dplyr::tibble(
    GEOID = c("001", "-111111111", "003"),
    NAME = c("First", "Second", "-999999999"),
    estimate = c(-111111111, 10, -999999999),
    integer_value = c(-222222222L, 20L, 30L),
    flag = c(TRUE, FALSE, TRUE)
  )

  expected <- dplyr::tibble(
    GEOID = c("001", NA, "003"),
    NAME = c("First", "Second", NA),
    estimate = c(NA, 10, NA),
    integer_value = c(NA_integer_, 20L, 30L),
    flag = c(TRUE, FALSE, TRUE)
  )

  expect_identical(tidycensus:::replace_census_missing(input), expected)
})

test_that("named-variable recoding preserves sequential aliases", {
  variables <- c(first = "A", second = "first", final = "B")
  input <- dplyr::tibble(
    GEOID = c("001", "A", "003"),
    NAME = c("Area A", "B", "Area C"),
    variable = c("A", "first", "B"),
    estimate = c(1, 2, 3)
  )

  expected <- dplyr::tibble(
    GEOID = c("001", "second", "003"),
    NAME = c("Area A", "final", "Area C"),
    variable = c("second", "second", "final"),
    estimate = c(1, 2, 3)
  )

  expect_identical(
    tidycensus:::recode_named_variables(input, variables),
    expected
  )
})

test_that("unnamed variables leave results unchanged", {
  input <- dplyr::tibble(variable = c("A", "B"), estimate = c(1, 2))

  expect_identical(
    tidycensus:::recode_named_variables(input, c("A", "B")),
    input
  )
  expect_identical(
    tidycensus:::recode_wide_variable_names(input, c("A", "B")),
    input
  )
})

test_that("wide-output recoding preserves existing name replacement", {
  variables <- c(first = "B01001_001", second = "B01001_002")
  input <- dplyr::tibble(
    GEOID = "001",
    B01001_001E = 1,
    B01001_001M = 2,
    B01001_002E = 3,
    B01001_002M = 4
  )

  expected <- input
  names(expected) <- c(
    "GEOID", "firstE", "firstM", "secondE", "secondM"
  )

  expect_identical(
    tidycensus:::recode_wide_variable_names(input, variables),
    expected
  )
})
