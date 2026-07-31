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
  expect_all_false(sf::st_is_empty(out))

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
  expect_all_false(sf::st_is_empty(out))
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
