expect_api_key_arg <- function(args, expected, position = NULL) {
  key <- args$key

  if (is.null(key) && !is.null(position)) {
    key <- args[[position]]
  }

  expect_equal(key, expected)
}

test_that("get_census_api_key requires an API key", {
  skip_on_cran()

  withr::local_envvar(CENSUS_API_KEY = NA)

  expect_error(
    tidycensus:::get_census_api_key(NULL),
    "A Census API key is required"
  )

  expect_equal(tidycensus:::get_census_api_key("direct-key"), "direct-key")

  withr::local_envvar(CENSUS_API_KEY = "env-key")
  expect_equal(tidycensus:::get_census_api_key(NULL), "env-key")
})

test_that("metadata requests include the Census API key", {
  skip_on_cran()

  captured <- list()

  local_mocked_bindings(
    census_api_get = function(url, query = list(), ...) {
      captured[[length(captured) + 1]] <<- list(url = url, query = query)
      stop("captured request", call. = FALSE)
    }
  )

  expect_error(load_variables(2022, "acs5", key = "metadata-key"), "captured request")
  expect_equal(captured[[1]]$query$key, "metadata-key")
  expect_match(captured[[1]]$url, "/2022/acs/acs5/variables\\.json$")

  expect_error(get_pop_groups(2020, "ddhca", key = "metadata-key"), "captured request")
  expect_equal(captured[[2]]$query$key, "metadata-key")
  expect_match(captured[[2]]$url, "/2020/dec/ddhca/variables\\.json$")
})

test_that("load_variables deprecates cache without writing to a cache directory", {
  skip_on_cran()

  captured <- list()

  local_mocked_bindings(
    census_api_get = function(url, query = list(), ...) {
      captured[[length(captured) + 1]] <<- list(url = url, query = query)
      stop("captured request", call. = FALSE)
    }
  )

  expect_warning(
    expect_error(load_variables(2022, "acs5", cache = TRUE, key = "metadata-key"), "captured request"),
    "`cache` is deprecated"
  )
  expect_equal(captured[[1]]$query$key, "metadata-key")
})

test_that("table expansion helpers pass direct keys to Census group metadata", {
  skip_on_cran()

  seen <- list()

  local_mocked_bindings(
    census_api_get = function(url, query = list(), ...) {
      seen[[length(seen) + 1]] <<- list(
        url = url,
        query = query
      )
      httr2::response(200, url = url, body = charToRaw(if (grepl("B01001", url)) {
        '{"variables":{"B01001_001E":{},"B01001_001EA":{},"B01001_001M":{},"B01001_002E":{},"B01001_002M":{},"GEO_ID":{},"NAME":{}}}'
      } else {
        '{"variables":{"P1_001N":{},"P1_001NA":{},"P1_002N":{},"GEO_ID":{},"NAME":{}}}'
      }))
    }
  )

  expect_equal(
    as.vector(tidycensus:::variables_from_table_acs(
      "B01001",
      2022,
      "acs5",
      cache_table = FALSE,
      key = "table-key"
    )),
    c("B01001_001", "B01001_002")
  )
  expect_equal(seen[[1]]$query$key, "table-key")
  expect_match(seen[[1]]$url, "/groups/B01001\\.json$")

  expect_equal(
    as.vector(tidycensus:::variables_from_table_decennial(
      "P1",
      2020,
      "pl",
      cache_table = FALSE,
      key = "table-key"
    )),
    c("P1_001N", "P1_002N")
  )
  expect_equal(seen[[2]]$query$key, "table-key")
  expect_match(seen[[2]]$url, "/groups/P1\\.json$")
})

test_that("public table calls thread direct keys into table expansion", {
  skip_on_cran()

  local_mocked_bindings(
    variables_from_table_acs = function(table, year, survey, cache_table, key = NULL) {
      expect_equal(key, "public-key")
      vars <- "B01001_001"
      attr(vars, "census_group") <- table
      vars
    },
    load_data_acs = function(...) {
      args <- list(...)
      expect_equal(args$group, "B01001")
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        B01001_001E = 1,
        B01001_001M = 2
      )
    }
  )

  acs <- suppressMessages(
    get_acs(
      geography = "state",
      table = "B01001",
      year = 2022,
      state = "TX",
      key = "public-key",
      output = "wide"
    )
  )

  expect_equal(acs$GEOID, "48")

  local_mocked_bindings(
    variables_from_table_decennial = function(table, year, sumfile, cache_table, key = NULL) {
      expect_equal(key, "public-key")
      vars <- "P1_001N"
      attr(vars, "census_group") <- table
      vars
    },
    load_data_decennial = function(...) {
      args <- list(...)
      expect_equal(args$group, "P1")
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        P1_001N = 1
      )
    }
  )

  decennial <- suppressMessages(
    get_decennial(
      geography = "state",
      table = "P1",
      year = 2020,
      sumfile = "pl",
      state = "TX",
      key = "public-key",
      output = "wide"
    )
  )

  expect_equal(decennial$GEOID, "48")
})

test_that("public variable calls pass direct keys to API data loaders", {
  skip_on_cran()

  local_mocked_bindings(
    load_data_acs = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key", position = 3)
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        B01001_001E = 1,
        B01001_001M = 2
      )
    }
  )

  acs <- suppressMessages(
    get_acs(
      geography = "state",
      variables = "B01001_001",
      year = 2022,
      state = "TX",
      key = "entry-key",
      output = "wide"
    )
  )
  expect_equal(acs$GEOID, "48")

  local_mocked_bindings(
    load_data_decennial = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key", position = 3)
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        P1_001N = 1
      )
    }
  )

  decennial <- suppressMessages(
    get_decennial(
      geography = "state",
      variables = "P1_001N",
      year = 2020,
      sumfile = "pl",
      state = "TX",
      key = "entry-key",
      output = "wide"
    )
  )
  expect_equal(decennial$GEOID, "48")

  local_mocked_bindings(
    load_data_estimates = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key")
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        POP = 1,
        DENSITY = 2
      )
    }
  )

  estimates <- suppressMessages(
    get_estimates(
      geography = "state",
      product = "population",
      year = 2019,
      state = "TX",
      key = "entry-key",
      output = "wide"
    )
  )
  expect_equal(estimates$GEOID, "48")

  local_mocked_bindings(
    load_data_flows = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key")
      data.frame(
        GEOID1 = "48001",
        GEOID2 = "48003",
        FULL1_NAME = "Anderson County, Texas",
        FULL2_NAME = "Andrews County, Texas",
        MOVEDIN = 1,
        MOVEDIN_M = 2,
        MOVEDOUT = 3,
        MOVEDOUT_M = 4,
        MOVEDNET = -2,
        MOVEDNET_M = 5
      )
    }
  )

  flows <- suppressMessages(
    get_flows(
      geography = "county",
      year = 2018,
      state = "TX",
      key = "entry-key",
      output = "wide"
    )
  )
  expect_equal(flows$GEOID1, "48001")

  local_mocked_bindings(
    load_data_pums = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key")
      data.frame(
        SERIALNO = "1",
        SPORDER = "1",
        WGTP = 1,
        PWGTP = 1,
        ST = "48",
        AGEP = "35"
      )
    }
  )

  pums <- suppressMessages(
    get_pums(
      variables = "AGEP",
      state = "TX",
      year = 2022,
      survey = "acs1",
      key = "entry-key"
    )
  )
  expect_equal(pums$SERIALNO, "1")
})

test_that("population group labels use the resolved Census API key", {
  skip_on_cran()

  local_mocked_bindings(
    load_data_decennial = function(...) {
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        POPGROUP = "0001",
        T01001_001N = 1
      )
    },
    get_pop_groups = function(year, sumfile, key = NULL) {
      expect_equal(key, "label-key")
      data.frame(
        pop_group = "0001",
        pop_group_label = "Total population"
      )
    }
  )

  decennial <- suppressMessages(
    get_decennial(
      geography = "state",
      variables = "T01001_001N",
      year = 2020,
      sumfile = "ddhca",
      state = "TX",
      pop_group = "0001",
      pop_group_label = TRUE,
      key = "label-key",
      output = "wide"
    )
  )

  expect_equal(decennial$pop_group_label, "Total population")
})

test_that("low-level Census API data requests include the API key", {
  skip_on_cran()

  captured <- list()

  local_mocked_bindings(
    census_api_get = function(url, query = list(), ...) {
      captured[[length(captured) + 1]] <<- list(url = url, query = query)
      stop("captured request", call. = FALSE)
    }
  )

  expect_error(
    tidycensus:::load_data_acs(
      geography = "state",
      formatted_variables = "B01001_001E,B01001_001M",
      key = "data-key",
      year = 2022,
      survey = "acs5"
    ),
    "captured request"
  )

  expect_error(
    tidycensus:::load_data_decennial(
      geography = "state",
      variables = "P1_001N",
      key = "data-key",
      year = 2020,
      sumfile = "pl",
      pop_group = NULL
    ),
    "captured request"
  )

  expect_error(
    tidycensus:::load_data_estimates(
      geography = "state",
      product = "population",
      variables = NULL,
      key = "data-key",
      year = 2019,
      time_series = FALSE
    ),
    "captured request"
  )

  expect_error(
    tidycensus:::load_data_pums(
      variables = "AGEP",
      state = "TX",
      puma = NULL,
      key = "data-key",
      year = 2022,
      survey = "acs1",
      variables_filter = NULL,
      recode = FALSE,
      show_call = FALSE
    ),
    "captured request"
  )

  expect_error(
    tidycensus:::load_data_pums_vacant(
      variables = "HINCP",
      state = "TX",
      puma = NULL,
      key = "data-key",
      year = 2022,
      survey = "acs1",
      variables_filter = list(TYPEHUGQ = "3"),
      recode = FALSE,
      show_call = FALSE
    ),
    "captured request"
  )

  expect_error(
    tidycensus:::load_data_flows(
      geography = "county",
      variables = c("GEOID1", "GEOID2", "MOVEDIN"),
      key = "data-key",
      year = 2018,
      state = "TX"
    ),
    "captured request"
  )

  expect_true(all(vapply(captured, function(x) identical(x$query$key, "data-key"), logical(1))))
})

test_that("low-level table data requests use Census API groups", {
  skip_on_cran()

  captured <- list()

  local_mocked_bindings(
    census_api_get = function(url, query = list(), ...) {
      captured[[length(captured) + 1]] <<- list(url = url, query = query)
      stop("captured request", call. = FALSE)
    }
  )

  expect_error(
    tidycensus:::load_data_acs(
      geography = "state",
      formatted_variables = "B01001_001E,B01001_001M",
      key = "data-key",
      year = 2022,
      survey = "acs5",
      group = "B01001"
    ),
    "captured request"
  )

  expect_error(
    tidycensus:::load_data_decennial(
      geography = "state",
      variables = "P1_001N",
      key = "data-key",
      year = 2020,
      sumfile = "pl",
      pop_group = NULL,
      group = "P1"
    ),
    "captured request"
  )

  expect_equal(captured[[1]]$query$get, "group(B01001)")
  expect_equal(captured[[2]]$query$get, "group(P1)")
  expect_true(all(vapply(captured, function(x) identical(x$query$key, "data-key"), logical(1))))
})

test_that("load_variables adds table universes when the API publishes them (#596)", {
  variables_json <- '{"variables":{"B01001_001E":{"label":"Estimate!!Total:","concept":"Sex by Age","predicateType":"int"},"B19013_001E":{"label":"Estimate!!Median household income","concept":"Median Household Income","predicateType":"int"}}}'
  groups_json <- function(universe) {
    if (universe) {
      '{"groups":[{"name":"B01001","description":"Sex by Age","variables":"","universe ":"Total population"},{"name":"B19013","description":"Median Household Income","variables":"","universe ":"Households "}]}'
    } else {
      '{"groups":[{"name":"B01001","description":"Sex by Age","variables":""}]}'
    }
  }
  mock_api <- function(groups) {
    function(url, query = list(), ...) {
      if (grepl("variables[.]json$", url)) {
        httr2::response(200, url = url, body = charToRaw(variables_json))
      } else if (is.null(groups)) {
        httr2::response(404, url = url, body = charToRaw("Not Found"))
      } else {
        httr2::response(200, url = url, body = charToRaw(groups))
      }
    }
  }

  local_mocked_bindings(census_api_get = mock_api(groups_json(TRUE)))
  v <- load_variables(2023, "acs1", key = "test-key")
  expect_equal(v$universe, c("Total population", "Households"))

  local_mocked_bindings(census_api_get = mock_api(groups_json(FALSE)))
  expect_false("universe" %in% names(load_variables(2023, "acs1", key = "test-key")))

  local_mocked_bindings(census_api_get = mock_api(NULL))
  expect_equal(names(load_variables(2023, "acs1", key = "test-key")), c("name", "label", "concept"))
})
