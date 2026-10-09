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

  ##############################################################################
  # Year 2000

  # Anticipate the warning for year-2000 ZCTAs including separate polygons
  expect_warning(
    out <- suppressMessages(
      get_decennial(
        geography = "zcta",
        variables = "P010001",
        year = 2000,
        state = "WY",
        key = "test-key",
        output = "wide",
        geometry = TRUE
      )
    ),
    regexp = "ZCTAs for 2000 include separate polygons for discontiguous parts"
  )

  # Ensure GEOIDs for ZCTAs got the two-digit state prefix removed
  expect_equal(out$GEOID, c("82001", "82007"))

  # Geometry column has data
  expect_false(any(sf::st_is_empty(out)))

  ##############################################################################
  # Year 2010
  out <- suppressMessages(
    get_decennial(
      geography = "zcta",
      variables = "P010001",
      year = 2010,
      state = "WY",
      key = "test-key",
      output = "wide",
      geometry = TRUE
    )
  )

  # Ensure GEOIDs for ZCTAs got the two-digit state prefix removed
  expect_equal(out$GEOID, c("82001", "82007"))

  # Geometry column has data
  expect_false(any(sf::st_is_empty(out)))
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

test_that("cb is passed to use_tigris() (#604)", {
  skip_on_cran()
  captured_cb <- NULL
  local_mocked_bindings(
    load_data_decennial = function(...) {
      data.frame(GEOID = "44001", NAME = "Bristol County, Rhode Island", P1_001N = 1)
    },
    use_tigris = function(..., cb) {
      captured_cb <<- cb
      sf::st_sf(GEOID = "44001", geometry = sf::st_sfc(sf::st_point(c(0, 0))))
    }
  )

  args <- list(geography = "county", variables = "P1_001N", year = 2020,
               state = "RI", key = "test-key", geometry = TRUE)

  suppressMessages(do.call(get_decennial, args))
  expect_true(captured_cb)

  suppressMessages(do.call(get_decennial, c(args, cb = FALSE)))
  expect_false(captured_cb)
})

test_that("ZCTAs by state error clearly for 2020 and later (#564)", {
  expect_error(
    suppressMessages(get_acs(geography = "zcta", variables = "B01003_001",
                             state = "VT", year = 2024, key = "test-key")),
    "does not support requesting ZCTAs by state"
  )
})

test_that("cartographic boundary ZCTAs use 2020 shapes for later years", {
  captured_year <- NULL
  local_mocked_bindings(
    zctas = function(..., year) {
      captured_year <<- year
      sf::st_sf(GEOID20 = "02809", geometry = sf::st_sfc(sf::st_point(c(0, 0))))
    }
  )

  use_tigris(geography = "zcta", year = 2024)
  expect_equal(captured_year, 2020)

  use_tigris(geography = "zcta", year = 2024, cb = FALSE)
  expect_equal(captured_year, 2024)
})

test_that("coded median year built values give a warning (#526)", {
  mock_acs <- function(values) {
    function(...) {
      data.frame(
        GEOID = c("48001950100", "48001950200"),
        NAME = c("Tract 9501", "Tract 9502"),
        B25035_001E = values,
        B25035_001M = c(5, 5)
      )
    }
  }

  local_mocked_bindings(load_data_acs = mock_acs(c(0, 1985)))
  expect_warning(
    suppressMessages(get_acs(geography = "tract", variables = c(yr_built = "B25035_001"),
                             state = "TX", year = 2020, key = "test-key")),
    "0 means \"1939 or earlier\""
  )

  local_mocked_bindings(load_data_acs = mock_acs(c(1938, 1985)))
  expect_no_warning(
    suppressMessages(get_acs(geography = "tract", variables = "B25035_001",
                             state = "TX", year = 2024, key = "test-key"))
  )
})

test_that("get_acs() wide output uses the requested estimate and MOE suffixes (#600)", {
  local_mocked_bindings(
    load_data_acs = function(...) {
      dplyr::tibble(GEOID = c("44001", "44003"), NAME = c("Bristol County, Rhode Island", "Kent County, Rhode Island"),
                    B19013_001E = c(100000, 90000), B19013_001M = c(5000, 4000))
    }
  )

  default <- suppressMessages(get_acs("county", c(income = "B19013_001"), state = "RI", year = 2023,
                                      output = "wide", key = "test-key"))
  expect_equal(names(default), c("GEOID", "NAME", "incomeE", "incomeM"))

  custom <- suppressMessages(get_acs("county", c(income = "B19013_001"), state = "RI", year = 2023,
                                     output = "wide", suffix = c("_est", "_MOE"), key = "test-key"))
  expect_equal(names(custom), c("GEOID", "NAME", "income_est", "income_MOE"))
  expect_equal(unname(as.list(custom)), unname(as.list(default)))

  expect_error(get_acs("county", "B19013_001", state = "RI", year = 2023, output = "wide",
                       suffix = c("E", "E"), key = "test-key"), "two different strings")
})
