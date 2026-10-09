test_that("spatial row binding preserves normalized sf output", {
  pieces <- suppressMessages(split(
    tidycensus::county_laea,
    substr(tidycensus::county_laea$GEOID, 1L, 2L)
  ))

  expected <- suppressMessages(suppressWarnings(
    purrr::reduce(pieces, rbind) %>%
      dplyr::as_tibble() %>%
      sf::st_as_sf()
  ))
  actual <- suppressMessages(suppressWarnings(
    tidycensus:::bind_spatial_rows(pieces) %>%
      dplyr::as_tibble() %>%
      sf::st_as_sf()
  ))

  expect_identical(actual, expected)
})
