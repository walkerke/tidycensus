test_that("2025 place population estimates parse from the city totals file", {
  skip_on_cran()

  captured_url <- NULL

  local_mocked_bindings(
    census_read_csv = function(https_url, ftp_url, required_col) {
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
    census_read_csv = function(https_url, ftp_url, required_col) {
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

test_that("intercensal population totals parse from the city totals file (#629)", {
  skip_on_cran()

  local_mocked_bindings(
    census_read_csv = function(https_url, ftp_url, required_col) {
      expect_match(https_url, "2010-2020/intercensal/cities/sub-est2020int[.]csv")

      data.frame(
        SUMLEV = c("040", "050", "162"),
        STATE = "26",
        COUNTY = c("000", "001", "000"),
        PLACE = c("00000", "00000", "22000"),
        NAME = c("Michigan", "Alcona County", "Detroit city"),
        STNAME = "Michigan",
        ESTIMATESBASE2010 = c(9883640, 10942, 713777),
        POPESTIMATE2010 = c(9877510, 10894, 711210),
        POPESTIMATE2015 = c(9987448, 10433, 679402),
        CENSUS2020POP = c(10077331, 10167, 639111)
      )
    }
  )

  out <- suppressMessages(get_estimates(
    geography = "county",
    product = "intercensal",
    vintage = 2020,
    variables = "all",
    state = "MI"
  ))

  expect_equal(unique(out$GEOID), "26001")
  expect_equal(unique(out$NAME), "Alcona County, Michigan")
  expect_setequal(unique(out$variable), c("ESTIMATESBASE", "POPESTIMATE", "CENSUSPOP"))
  expect_equal(out$value[out$variable == "CENSUSPOP"], 10167)
  expect_equal(out$year[out$variable == "CENSUSPOP"], 2020L)
})

test_that("2000-2010 intercensal characteristics map year and age codes (#629)", {
  skip_on_cran()

  local_mocked_bindings(
    census_read_csv = function(https_url, ftp_url, required_col) {
      expect_match(https_url, "2000-2010/intercensal/county/co-est00int-alldata-44[.]csv")

      # YEAR 1 = 2000 base, 2 = July 2000, 12 = 2010 Census, 13 = July 2010;
      # AGEGRP 99 = all ages, 0 = under 1, 1 = ages 1-4
      expand.grid(YEAR = c(1, 2, 12, 13), AGEGRP = c(99, 0, 1)) |>
        transform(
          SUMLEV = "050", STATE = 44, COUNTY = 1,
          STNAME = "Rhode Island", CTYNAME = "Bristol County",
          TOT_POP = 0, TOT_MALE = ifelse(AGEGRP == 99, 30, 15), TOT_FEMALE = 0,
          NHWA_MALE = ifelse(AGEGRP == 99, 30, 15), NHWA_FEMALE = 0,
          HWA_MALE = 0, HWA_FEMALE = 0
        )
    }
  )

  out <- suppressMessages(get_estimates(
    geography = "county",
    product = "intercensal",
    vintage = 2010,
    breakdown = c("AGEGROUP", "SEX"),
    state = "RI"
  ))

  expect_equal(unique(out$GEOID), "44001")
  expect_equal(sort(unique(out$year)), c(2000L, 2010L))
  # Under 1 and 1-4 combine into the 0-4 group
  expect_equal(out$value[out$year == 2000 & out$AGEGROUP == 1 & out$SEX == 1], 30)
  expect_equal(out$value[out$year == 2000 & out$AGEGROUP == 0 & out$SEX == 1], 30)

  # time_series stops at `year`, as it does for population totals
  ts <- suppressMessages(get_estimates(
    geography = "county",
    product = "intercensal",
    vintage = 2010,
    year = 2000,
    time_series = TRUE,
    breakdown = "SEX",
    state = "RI"
  ))
  expect_equal(unique(ts$year), 2000L)
})

test_that("intercensal estimates error clearly for unsupported requests (#629)", {
  expect_error(
    suppressMessages(get_estimates(geography = "county", product = "intercensal", state = "RI")),
    "vintage = 2020"
  )
  expect_error(
    suppressMessages(get_estimates(geography = "county", product = "intercensal",
                                   vintage = 2020, year = 2005, state = "RI")),
    "available for 2010 through 2019"
  )
  expect_error(
    suppressMessages(get_estimates(geography = "county", product = "intercensal",
                                   vintage = 2020, year = 2020, state = "RI",
                                   breakdown = "SEX")),
    "available for 2010 through 2019"
  )
})

test_that("intercensal characteristics support multiple states and geometry (#629)", {
  skip_on_cran()

  local_mocked_bindings(
    census_read_csv = function(https_url, ftp_url, required_col) {
      st <- sub(".*alldata-([0-9]{2})[.]csv$", "\\1", https_url)

      expand.grid(YEAR = 2:11, AGEGRP = 0:1) |>
        transform(
          SUMLEV = "050", STATE = st, COUNTY = "001",
          STNAME = ifelse(st == "44", "Rhode Island", "Massachusetts"),
          CTYNAME = "First County",
          TOT_POP = 20, TOT_MALE = 10, TOT_FEMALE = 10,
          NHWA_MALE = 10, NHWA_FEMALE = 10, HWA_MALE = 0, HWA_FEMALE = 0
        )
    },
    use_tigris = function(...) {
      sf::st_sf(
        GEOID = c("44001", "25001"),
        geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
      )
    }
  )

  out <- suppressMessages(get_estimates(
    geography = "county",
    product = "intercensal",
    vintage = 2020,
    state = c("RI", "MA"),
    breakdown = "SEX",
    year = 2015,
    time_series = TRUE,
    geometry = TRUE
  ))

  expect_s3_class(out, "sf")
  expect_setequal(unique(out$GEOID), c("44001", "25001"))
  expect_equal(range(out$year), c(2010L, 2015L))
  expect_false(any(sf::st_is_empty(out)))
})

test_that("intercensal requests for Puerto Rico error clearly (#629)", {
  expect_error(
    suppressMessages(get_estimates(geography = "county", product = "intercensal",
                                   vintage = 2020, state = "PR")),
    "Puerto Rico are not currently available"
  )
})

# A housing unit estimates sheet as readxl reads it (col_names = FALSE): title
# rows, the header with years, indented areas, then footnotes
housing_sheet <- function(names, values) {
  years <- as.character(2020:2022)
  rows <- lapply(seq_along(names), function(i) {
    c(names[i], as.character(c(100, values[[i]])))
  })
  sheet <- rbind(
    c("Annual Estimates of Housing Units", NA, NA, NA, NA),
    c("Geographic Area", "April 1, 2020 Estimates Base", "Housing Unit Estimate (as of July 1)", NA, NA),
    c(NA, NA, years),
    do.call(rbind, rows),
    c(NA, NA, NA, NA, NA),
    c("Note: The estimates are based on the 2020 Census.", NA, NA, NA, NA)
  )
  dplyr::as_tibble(as.data.frame(sheet, stringsAsFactors = FALSE), .name_repair = "minimal")
}

test_that("2020s housing unit estimates parse with GEOIDs for states, regions, and the US", {
  sheet <- housing_sheet(
    c("United States", "Northeast Region", ".Rhode Island", ".Vermont"),
    list(c(1000, 1010, 1020), c(500, 505, 510), c(463, 465, 468), c(334, 335, 336))
  )

  states <- parse_housing_table(sheet, "state")
  expect_equal(unique(states$GEOID), c("44", "50"))
  expect_equal(unique(states$variable), "HUEST")
  expect_equal(states$year, rep(c("2020", "2021", "2022"), 2))
  expect_equal(states$value[states$GEOID == "44"], c(463, 465, 468))

  expect_equal(unique(parse_housing_table(sheet, "region")$GEOID), "1")
  expect_equal(unique(parse_housing_table(sheet, "us")$GEOID), "1")
})

test_that("2020s housing county names match the population file, including truncated names", {
  counties <- data.frame(
    STATE = c("09", "09"),
    COUNTY = c("110", "130"),
    STNAME = "Connecticut",
    CTYNAME = c("Capitol Planning Region", "Lower Connecticut River Valley Planning Regio"),
    stringsAsFactors = FALSE
  )
  sheet <- housing_sheet(
    c("United States", ".Capitol Planning Region, Connecticut", ".Lower Connecticut River Valley Planning Region, Connecticut"),
    list(c(1, 2, 3), c(410, 412, 414), c(81, 82, 83))
  )

  hu <- parse_housing_table(sheet, "county", counties)
  expect_equal(unique(hu$GEOID), c("09110", "09130"))
  expect_equal(unique(hu$NAME)[2], "Lower Connecticut River Valley Planning Region, Connecticut")
})

test_that("2020s housing names matching more than one county error", {
  counties <- data.frame(
    STATE = c("09", "09"),
    COUNTY = c("110", "120"),
    STNAME = "Connecticut",
    CTYNAME = c("Capitol Planning Region", "Capitol Planning Region"),
    stringsAsFactors = FALSE
  )
  sheet <- housing_sheet(
    c("United States", ".Capitol Planning Region, Connecticut"),
    list(c(1, 2, 3), c(410, 412, 414))
  )

  expect_error(parse_housing_table(sheet, "county", counties), "Capitol Planning Region, Connecticut")
})

test_that("2020s housing rows with missing values error instead of disappearing", {
  sheet <- housing_sheet(
    c("United States", ".Rhode Island", ".Vermont"),
    list(c(1000, 1010, 1020), c(NA, 465, 468), c(334, 335, 336))
  )

  expect_error(parse_housing_table(sheet, "state"), "Rhode Island")
})

test_that("2020s housing geometry uses the vintage's boundaries", {
  skip_on_cran()

  captured_year <- NULL

  local_mocked_bindings(
    read_housing_estimates = function(geography, vintage) {
      dplyr::tibble(GEOID = "09110", NAME = "Capitol Planning Region, Connecticut",
                    variable = "HUEST", year = c("2020", "2025"), value = c(410, 420))
    },
    use_tigris = function(geography, year, ...) {
      captured_year <<- year
      stop("captured geometry", call. = FALSE)
    }
  )

  expect_error(
    suppressMessages(get_estimates("county", product = "housing", state = "CT",
                                   vintage = 2025, year = 2020, geometry = TRUE)),
    "geometry data download failed"
  )
  expect_equal(captured_year, 2025)
})

test_that("2020s population geometry uses the vintage's boundaries", {
  skip_on_cran()

  captured_year <- NULL

  local_mocked_bindings(
    census_read_csv = function(https_url, ftp_url, required_col) {
      data.frame(
        SUMLEV = "050", REGION = "1", DIVISION = "1", STATE = "09", COUNTY = "110",
        STNAME = "Connecticut", CTYNAME = "Capitol Planning Region",
        POPESTIMATE2020 = 975000, POPESTIMATE2025 = 980000,
        stringsAsFactors = FALSE
      )
    },
    use_tigris = function(geography, year, ...) {
      captured_year <<- year
      stop("captured geometry", call. = FALSE)
    }
  )

  expect_error(
    suppressMessages(get_estimates("county", variables = "POPESTIMATE", state = "CT",
                                   vintage = 2025, year = 2020, geometry = TRUE)),
    "geometry data download failed"
  )
  expect_equal(captured_year, 2025)
})
