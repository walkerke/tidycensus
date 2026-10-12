test_that("validate_county() prefers a complete county name", {
  expect_equal(validate_county("TX", "Collin"), "085")
  expect_equal(validate_county("TX", "Collingsworth"), "087")
  expect_equal(validate_county("IA", "clay"), "041")
  expect_equal(validate_county("MD", "Baltimore County"), "005")
  expect_equal(validate_county("MD", "Baltimore city"), "510")
  expect_equal(validate_county("MD", "Anne"), "003")
})

test_that("validate_county() matches names as text, not regular expressions", {
  expect_error(validate_county("MD", "Anne (x"), "not a valid name")
})

test_that("validate_county() errors on ambiguous names outside interactive sessions", {
  withr::local_options(rlang_interactive = FALSE)

  expect_error(
    validate_county("MD", "Baltimore"),
    "Baltimore County \\(005\\), Baltimore city \\(510\\)"
  )
  expect_error(validate_county("TX", "Coll"), "Collingsworth")
})

test_that("validate_county() asks about ambiguous names in interactive sessions", {
  withr::local_options(rlang_interactive = TRUE)

  local_mocked_bindings(county_menu = function(choices, title) {
    expect_match(title, "Which county are you requesting")
    expect_equal(choices, c("Baltimore County (005)", "Baltimore city (510)"))
    2L
  })
  expect_equal(validate_county("MD", "Baltimore", .msg = FALSE), "510")

  local_mocked_bindings(county_menu = function(choices, title) 0L)
  expect_error(validate_county("MD", "Baltimore", .msg = FALSE), "No county selected")
})

test_that("get_acs() asks once and uses the same county for data and geometry", {
  withr::local_options(rlang_interactive = TRUE)
  asked <- 0
  seen <- list()

  local_mocked_bindings(
    county_menu = function(choices, title) {
      asked <<- asked + 1
      2L
    },
    load_data_acs = function(geography, formatted_variables, key, year,
                             state = NULL, county = NULL, ...) {
      seen$data <<- county
      data.frame(
        GEOID = "24510010100", NAME = "Tract 101",
        B01001_001E = 100, B01001_001M = 10
      )
    },
    use_tigris = function(geography, year, cb, resolution, state, county, ...) {
      seen$geometry <<- county
      sf::st_sf(
        GEOID = "24510010100",
        geometry = sf::st_sfc(sf::st_point(c(-76.6, 39.3)), crs = 4269)
      )
    }
  )

  suppressMessages(get_acs(
    "tract", variables = "B01001_001", state = "MD", county = "Baltimore",
    year = 2023, geometry = TRUE, key = "fake"
  ))

  expect_equal(asked, 1)
  expect_equal(seen$data, "510")
  expect_equal(seen$geometry, "510")
})
