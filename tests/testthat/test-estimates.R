test_that("2025 place population estimates parse from the city totals file", {
  skip_on_cran()

  captured_url <- NULL

  local_mocked_bindings(
    read_estimates_csv = function(https_url, ftp_url, required_col) {
      captured_url <<- https_url
      expect_match(https_url, "2020-2025/cities/totals/sub-est2025[.]csv")
      expect_match(ftp_url, "2020-2025/cities/totals/sub-est2025[.]csv")
      expect_equal(required_col, "SUMLEV")

      data.frame(
        SUMLEV = c("040", "162", "162"),
        STATE = c("48", "48", "06"),
        COUNTY = c("000", "000", "000"),
        PLACE = c("00000", "05000", "44000"),
        COUSUB = "00000",
        CONCIT = "00000",
        PRIMGEO_FLAG = 0,
        FUNCSTAT = "A",
        NAME = c("Texas", "Austin city", "Los Angeles city"),
        STNAME = c("Texas", "Texas", "California"),
        ESTIMATESBASE2020 = c(29145505, 961855, 3898747),
        POPESTIMATE2020 = c(29234361, 965827, 3895848),
        POPESTIMATE2021 = c(29561286, 974447, 3847114),
        POPESTIMATE2022 = c(30029848, 974013, 3820914),
        POPESTIMATE2023 = c(30503301, 979882, 3821576),
        POPESTIMATE2024 = c(30976754, 993588, 3822808),
        POPESTIMATE2025 = c(31450000, 1001000, 3825000),
        stringsAsFactors = FALSE
      )
    }
  )

  estimates <- suppressMessages(
    get_estimates(
      geography = "place",
      product = "population",
      vintage = 2025,
      year = 2025,
      state = "TX",
      output = "wide"
    )
  )

  expect_match(captured_url, "sub-est2025[.]csv")
  expect_equal(nrow(estimates), 1)
  expect_equal(estimates$GEOID, "4805000")
  expect_equal(estimates$NAME, "Austin city, Texas")
  expect_equal(estimates$POPESTIMATE, 1001000)
})

test_that("Puerto Rico municipio characteristics parse from the single-year file (#581)", {
  skip_on_cran()

  local_mocked_bindings(
    read_estimates_csv = function(https_url, ftp_url, required_col) {
      expect_match(https_url, "2020-2025/counties/asrh/cc-est2025-syasex-72[.]csv")
      expect_match(ftp_url, "2020-2025/counties/asrh/cc-est2025-syasex-72[.]csv")

      expand.grid(YEAR = 1:7, AGE = c(0, 85)) |>
        transform(
          SUMLEV = "050",
          STATE = "72",
          COUNTY = "001",
          STNAME = "Puerto Rico Commonwealth",
          CTYNAME = "Adjuntas Municipio",
          TOT_POP = 30,
          TOT_MALE = 10,
          TOT_FEMALE = 20
        )
    }
  )

  out <- suppressMessages(get_estimates(
    geography = "county",
    product = "characteristics",
    breakdown = c("AGEGROUP", "SEX"),
    state = "PR",
    vintage = 2025,
    time_series = TRUE
  ))

  expect_equal(unique(out$GEOID), "72001")
  expect_equal(unique(out$NAME), "Adjuntas Municipio, Puerto Rico")
  expect_equal(sort(unique(out$year)), 2020:2025)
  expect_equal(sort(unique(out$AGEGROUP)), c(1, 18))
  expect_equal(out$value[out$year == 2025 & out$AGEGROUP == 18 & out$SEX == 2], 20)
})

test_that("Puerto Rico municipios error clearly for unpublished data (#581)", {
  expect_error(
    suppressMessages(get_estimates(
      geography = "county", product = "characteristics", breakdown = "RACE",
      state = "PR", vintage = 2025
    )),
    "Race and Hispanic origin breakdowns are not available"
  )

  expect_error(
    suppressMessages(get_estimates(
      geography = "county", product = "characteristics", breakdown = "SEX",
      state = "PR", vintage = 2024
    )),
    "Vintage 2025 and later"
  )
})

test_that("PEP names convert Latin-1 to UTF-8 and leave UTF-8 alone", {
  latin1 <- iconv("Doña Ana County", from = "UTF-8", to = "latin1")
  Encoding(latin1) <- "unknown"
  utf8 <- "Mayagüez, PR"

  expect_equal(
    tidycensus:::fix_pep_encoding(c(latin1, utf8, "Travis County")),
    c("Doña Ana County", "Mayagüez, PR", "Travis County")
  )
})
