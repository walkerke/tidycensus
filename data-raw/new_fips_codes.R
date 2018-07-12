library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

co90 <- counties(year = 1990, cb = TRUE)


wh <- get_acs(geography = "tract", state = "AK", county = "Kusilvak",
              variables = "B01003_001", year = 2015)