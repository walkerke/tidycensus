test_that("variables_from_table_decennial matches underscore tables for 2020 special files", {
  skip_on_cran()
  local_mocked_bindings(
    group_variables = function(...) NULL,
    load_variables = function(year, dataset, cache, key = NULL) {
      data.frame(
        name = c("T03001_001N", "T03001_002N", "T03001001"),
        label = NA_character_,
        concept = NA_character_
      )
    }
  )

  expect_equal(
    variables_from_table_decennial("T03001", 2020, "ddhcb", FALSE),
    c("T03001_001N", "T03001_002N")
  )

  expect_equal(
    variables_from_table_decennial("T03001", 2020, "sdhc", FALSE),
    c("T03001_001N", "T03001_002N")
  )
})

test_that("get_decennial normalizes state-prefixed ZCTA GEOIDs", {
  skip_on_cran()
  local_mocked_bindings(
    load_data_decennial = function(...) {
      data.frame(
        GEOID = c("5682001", "5682007"),
        NAME = c("ZCTA5 82001, Wyoming", "ZCTA5 82007, Wyoming"),
        P010001 = c(35855, 16460)
      )
    }
  )

  out <- suppressMessages(
    get_decennial(
      geography = "zcta",
      variables = "P010001",
      year = 2000,
      state = "WY",
      key = "test-key",
      output = "wide"
    )
  )

  expect_equal(out$GEOID, c("82001", "82007"))
})

test_that("variables_from_table_acs drops comparison profile significance columns", {
  skip_on_cran()
  local_mocked_bindings(
    group_variables = function(...) {
      c("CP02_2023_001E", "CP02_2023_001EA", "CP02_2023_001PE", "CP02_2023_001PEA", "CP02_2023to2018_001SS")
    }
  )

  vars <- variables_from_table_acs("CP02", 2023, "acs5/cprofile", FALSE, key = "test-key")

  expect_equal(as.vector(vars), c("CP02_2023_001", "CP02_2023_001P"))
  expect_equal(attr(vars, "census_group"), "CP02")
})

test_that("empty or missing variable IDs error clearly (#590)", {
  expect_error(
    get_acs(geography = "county", variables = c(income = "B19013_001", poverty = ""),
            state = "VT", key = "test-key"),
    "empty or missing"
  )

  expect_error(
    get_decennial(geography = "county", variables = c("P1_001N", NA),
                  state = "VT", year = 2020, key = "test-key"),
    "empty or missing"
  )
})

test_that("lowercase variable IDs are converted to uppercase with a message", {
  local_mocked_bindings(
    load_data_acs = function(geography, formatted_variables, ...) {
      stop(formatted_variables)
    }
  )

  expect_message(
    expect_error(
      get_acs(geography = "county", variables = c(income = "b19013_001"),
              state = "VT", year = 2024, key = "test-key"),
      "B19013_001E"
    ),
    "Converting variable IDs to uppercase: b19013_001"
  )
})

test_that("VACS is requested once when return_vacant = TRUE", {
  local_mocked_bindings(
    load_data_pums = function(variables, ...) {
      stop(paste(variables, collapse = ","))
    }
  )

  expect_error(
    suppressMessages(
      get_pums(variables = c("VACS", "HHLANP"), state = "RI", year = 2024,
               return_vacant = TRUE, key = "test-key")
    ),
    "^VACS,HHLANP$"
  )
})

test_that("drop_empty removes rows missing from the boundary file (#650)", {
  skip_on_cran()
  local_mocked_bindings(
    load_data_decennial = function(...) {
      data.frame(
        GEOID = c("22071001701", "22071990000"),
        NAME = c("Census Tract 17.01", "Census Tract 9900 (water)"),
        P1_001N = c(1200, 0)
      )
    },
    use_tigris = function(...) {
      sf::st_sf(
        GEOID = "22071001701",
        geometry = sf::st_sfc(sf::st_point(c(0, 0)))
      )
    }
  )

  args <- list(
    geography = "tract", variables = "P1_001N", year = 2020,
    state = "LA", key = "test-key", output = "wide", geometry = TRUE
  )

  kept <- suppressMessages(do.call(get_decennial, args))
  expect_equal(nrow(kept), 2)
  expect_equal(sum(sf::st_is_empty(kept)), 1)

  dropped <- suppressMessages(do.call(get_decennial, c(args, drop_empty = TRUE)))
  expect_equal(dropped$GEOID, "22071001701")
})
