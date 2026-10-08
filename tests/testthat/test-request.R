test_that("show_call output never includes the API key", {
  base <- "https://api.census.gov/data/2024/acs/acs5"
  expected <- paste0(base, "?get=NAME&for=state%3A%2A")

  expect_equal(remove_api_key(paste0(base, "?get=NAME&for=state%3A%2A&key=abc")), expected)
  expect_equal(remove_api_key(paste0(base, "?key=abc&get=NAME&for=state%3A%2A")), expected)
  expect_equal(remove_api_key(paste0(base, "?get=NAME&key=abc&for=state%3A%2A")), expected)
  expect_equal(remove_api_key(expected), expected)
})

test_that("status descriptions keep httr's wording", {
  expect_equal(http_status_message(httr2::response(400)), "Client error: (400) Bad Request")
  expect_equal(http_status_message(httr2::response(404)), "Client error: (404) Not Found")
  expect_equal(http_status_message(httr2::response(503)), "Server error: (503) Service Unavailable")
})

test_that("Census API responses map to tidycensus error messages", {
  expect_error(
    census_api_content(httr2::response(400, body = charToRaw("error: The requested resource is not available."))),
    "likely not available at the requested geography"
  )
  expect_error(
    census_api_content(httr2::response(400, body = charToRaw("error: unknown variable 'XYZ'"))),
    "The API message returned is error: unknown variable 'XYZ'"
  )
  expect_error(
    census_api_content(httr2::response(200, body = charToRaw("<html>You included a key with this request</html>"))),
    "invalid or inactive API key"
  )
  expect_equal(
    census_api_content(httr2::response(200, body = charToRaw('[["NAME"],["Vermont"]]'))),
    '[["NAME"],["Vermont"]]'
  )
  expect_equal(resp_text(httr2::response(204)), "")
})

test_that("requests are built the same way as before, with a user agent and no timeout", {
  query <- list(get = "NAME", "for" = "state:*", "in" = NULL, key = "k")
  req <- census_request("https://api.census.gov/data/2024/acs/acs5", query)

  expect_equal(
    req$url,
    "https://api.census.gov/data/2024/acs/acs5?get=NAME&for=state%3A%2A&key=k"
  )
  expect_match(req$options$useragent, "^tidycensus/")
  expect_null(req$options$timeout_ms)
})

test_that("repeated key parameters are all removed", {
  expect_equal(
    remove_api_key("https://api.census.gov/data?key=A&key=B&get=NAME"),
    "https://api.census.gov/data?get=NAME"
  )
})

test_that("the API key never appears in displayed error text", {
  url <- "https://api.census.gov/data/2024/acs/acs5?get=NAME&key=SECRETKEY123"
  resp <- httr2::response(
    400, url = url,
    body = charToRaw(paste("error: unknown variable in", url))
  )

  err <- tryCatch(census_api_content(resp), error = conditionMessage)
  expect_false(grepl("SECRETKEY123", err))
  expect_match(err, "<REDACTED>")

  expect_equal(url_api_keys("https://x/?key=A&get=NAME&key=B"), c("A", "B"))
  expect_equal(redact_api_key("key=A and A again", "A"), "key=<REDACTED> and <REDACTED> again")
})

test_that("status descriptions match httr for codes httr2 words differently", {
  expect_equal(http_status_message(httr2::response(414)), "Client error: (414) Request-URI Too Long")
  expect_equal(http_status_message(httr2::response(431)), "Client error: (431) Request Header Fields Too Large (RFC 6585)")
  expect_equal(http_status_message(httr2::response(429)), "Client error: (429) Too Many Requests (RFC 6585)")
})

test_that("connection errors show only the root cause", {
  inner <- simpleError("Could not resolve host: api.census.gov")
  outer <- rlang::error_cnd(message = "Failed to perform HTTP request.", parent = inner)
  expect_equal(root_cause_message(outer), "Could not resolve host: api.census.gov")
})
