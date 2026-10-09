test_that("metropolitan divisions are requested within every metro area (#519)", {
  queries <- list()

  local_mocked_bindings(
    census_api_get = function(url, query = list(), ...) {
      queries[[length(queries) + 1]] <<- query
      body <- if (identical(query$get, "NAME")) {
        '[["NAME","metropolitan statistical area/micropolitan statistical area"],["Boston-Cambridge-Newton, MA-NH Metro Area","14460"],["Aberdeen, SD Micro Area","10100"],["Atlanta-Sandy Springs-Roswell, GA Metro Area","12060"]]'
      } else {
        '[["B01001_001E","B01001_001M","NAME","metropolitan statistical area/micropolitan statistical area","metropolitan division"],["2000000","1000","Boston, MA Metro Division; Boston-Cambridge-Newton, MA-NH Metro Area","14460","14454"]]'
      }
      httr2::response(200, url = url, body = charToRaw(body))
    }
  )

  x <- suppressMessages(get_acs("metropolitan division", "B01001_001", year = 2023, key = "test-key"))

  # micropolitan areas (which have no divisions) are left out of the parent list
  expect_equal(queries[[2]][["in"]], "metropolitan statistical area/micropolitan statistical area:14460,12060")
  expect_equal(queries[[2]][["for"]], "metropolitan division:*")
  expect_equal(x$GEOID, "1446014454")
  expect_equal(x$estimate, 2000000)
})

test_that("tribal census tracts are requested within every American Indian area (#621)", {
  queries <- list()

  local_mocked_bindings(
    census_api_get = function(url, query = list(), ...) {
      queries[[length(queries) + 1]] <<- query
      body <- if (identical(query$get, "NAME")) {
        '[["NAME","american indian area/alaska native area/hawaiian home land"],["Acoma Pueblo and Off-Reservation Trust Land","0010"],["Tohono O\'odham Nation Reservation and Off-Reservation Trust Land","4200"]]'
      } else {
        '[["P1_001N","NAME","american indian area/alaska native area/hawaiian home land","tribal census tract"],["3094","Tribal Census Tract T001; Acoma Pueblo and Off-Reservation Trust Land, NM","0010","T00100"]]'
      }
      httr2::response(200, url = url, body = charToRaw(body))
    }
  )

  x <- suppressMessages(get_decennial("tribal census tract", "P1_001N", year = 2020, sumfile = "dhc", key = "test-key"))

  expect_equal(queries[[2]][["in"]], "american indian area/alaska native area/hawaiian home land:0010,4200")
  expect_equal(x$GEOID, "0010T00100")
})

test_that("national-only geographies reject state and county, and tribal tracts reject the 1-year ACS", {
  expect_error(get_acs("metropolitan division", "B01001_001", state = "NY", year = 2023, key = "test-key"),
               "returned for the entire US")
  expect_error(get_decennial("tribal census tract", "P1_001N", state = "AZ", year = 2020, sumfile = "dhc", key = "test-key"),
               "returned for the entire US")
  expect_error(get_acs("tribal census tract", "B01001_001", year = 2023, survey = "acs1", key = "test-key"),
               "not available in the 1-year ACS")
})
