# Benchmark multi-state sf binding on real Census geometry and ACS data.
#
# Environment variables:
#   CENSUS_API_KEY                 Census API key (needed only for a cold cache)
#   TIDYCENSUS_BENCH_CACHE         ACS fixture directory (defaults under tempdir())
#   TIDYCENSUS_GEOMETRY_FIXTURE    national tract fixture RDS path
#   TIDYCENSUS_BENCH_RESULTS       optional path for the results CSV

options(stringsAsFactors = FALSE)
set.seed(20260816)

pkgload::load_all(".", quiet = TRUE)

variables <- c(
  "B01001_020", "B01001_021", "B01001_022",
  "B01001_044", "B01001_045", "B01001_046"
)
formatted_variables <- tidycensus:::format_variables_acs(variables)
state_codes <- unique(tidycensus::fips_codes$state_code)
state_codes <- state_codes[
  !is.na(state_codes) & (as.integer(state_codes) <= 56L | state_codes == "72")
]

cache_dir <- Sys.getenv(
  "TIDYCENSUS_BENCH_CACHE",
  file.path(tempdir(), "tidycensus-spatial-bind")
)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
geometry_path <- Sys.getenv(
  "TIDYCENSUS_GEOMETRY_FIXTURE",
  file.path(cache_dir, "national-tracts-2023.rds")
)

if (file.exists(geometry_path)) {
  geometry_fixture <- readRDS(geometry_path)
} else {
  message("Fetching national 2023 tract geometry")
  timing <- system.time({
    tracts <- suppressMessages(tigris::tracts(
      year = 2023,
      cb = TRUE,
      class = "sf"
    ))
  })
  geometry_fixture <- list(
    data = tracts,
    fetch_elapsed_s = unname(timing[["elapsed"]])
  )
  saveRDS(geometry_fixture, geometry_path)
}
tracts <- geometry_fixture$data

fetch_acs_fixture <- function(path, state) {
  if (file.exists(path)) {
    return(readRDS(path))
  }

  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key)) {
    stop("CENSUS_API_KEY is required when an ACS fixture is not cached.", call. = FALSE)
  }

  message("Fetching ACS data for state ", state)
  timing <- system.time({
    data <- suppressMessages(tidycensus:::load_data_acs(
      geography = "tract",
      formatted_variables = formatted_variables,
      key = key,
      year = 2023,
      state = state,
      county = NULL,
      zcta = NULL,
      survey = "acs5",
      show_call = FALSE,
      group = "B01001"
    ))
  })
  fixture <- list(
    data = data,
    api_elapsed_s = unname(timing[["elapsed"]]),
    captured_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  saveRDS(fixture, path)
  fixture
}

acs_fixtures <- setNames(vector("list", length(state_codes)), state_codes)
for (state_code in state_codes) {
  acs_fixtures[[state_code]] <- fetch_acs_fixture(
    file.path(cache_dir, paste0("state-", state_code, ".rds")),
    state_code
  )
}

normalize_sf <- function(data) {
  data %>%
    dplyr::as_tibble() %>%
    sf::st_as_sf()
}

baseline_bind <- function(pieces) {
  suppressMessages(suppressWarnings(
    normalize_sf(purrr::reduce(pieces, rbind))
  ))
}

candidate_bind <- function(pieces) {
  suppressMessages(suppressWarnings(
    normalize_sf(tidycensus:::bind_spatial_rows(pieces))
  ))
}

tract_pieces <- split(tracts, tracts$STATEFP)
county_pieces <- suppressMessages(split(
  tidycensus::county_laea,
  substr(tidycensus::county_laea$GEOID, 1L, 2L)
))

to_tidy_geometry <- function(state_code) {
  geometry <- tract_pieces[[state_code]]
  values <- acs_fixtures[[state_code]]$data
  estimates <- as.matrix(values[paste0(variables, "E")])
  margins <- as.matrix(values[paste0(variables, "M")])
  tidy <- dplyr::tibble(
    GEOID = rep(values$GEOID, each = length(variables)),
    NAME = rep(values$NAME, each = length(variables)),
    variable = rep(variables, times = nrow(values)),
    estimate = as.vector(t(estimates)),
    moe = as.vector(t(margins))
  )
  index <- match(tidy$GEOID, geometry$GEOID)
  matched <- !is.na(index)

  output <- geometry[index[matched], c("GEOID", "geometry")]
  output$NAME <- tidy$NAME[matched]
  output$variable <- tidy$variable[matched]
  output$estimate <- tidy$estimate[matched]
  output$moe <- tidy$moe[matched]
  output[c("GEOID", "NAME", "variable", "estimate", "moe", "geometry")]
}

tidy_tract_pieces <- lapply(state_codes, to_tidy_geometry)
names(tidy_tract_pieces) <- state_codes

measure_pair <- function(baseline, candidate, repetitions) {
  baseline()
  candidate()

  baseline_times <- candidate_times <- numeric(repetitions)
  schedule <- sample(rep(c("baseline", "candidate"), repetitions))
  baseline_index <- candidate_index <- 0L

  for (implementation in schedule) {
    gc(FALSE)
    if (implementation == "baseline") {
      baseline_index <- baseline_index + 1L
      baseline_times[baseline_index] <- system.time(baseline())[["elapsed"]]
    } else {
      candidate_index <- candidate_index + 1L
      candidate_times[candidate_index] <- system.time(candidate())[["elapsed"]]
    }
  }

  c(
    baseline_median_s = median(baseline_times),
    candidate_median_s = median(candidate_times),
    saving_s = median(baseline_times) - median(candidate_times),
    speedup = median(baseline_times) / median(candidate_times),
    baseline_min_s = min(baseline_times),
    baseline_max_s = max(baseline_times),
    candidate_min_s = min(candidate_times),
    candidate_max_s = max(candidate_times)
  )
}

allocated_bytes <- function(operation) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  gc(FALSE)
  Rprofmem(path)
  on.exit(Rprofmem(NULL), add = TRUE)
  operation()
  Rprofmem(NULL)
  allocations <- suppressWarnings(as.numeric(
    sub(" .*", "", readLines(path, warn = FALSE))
  ))
  sum(allocations, na.rm = TRUE)
}

scales <- list(
  list(
    scale = "National county geometry",
    pieces = county_pieces,
    features = sum(vapply(county_pieces, nrow, integer(1))),
    state_pieces = length(county_pieces),
    repetitions = 9L
  ),
  list(
    scale = "National tract geometry",
    pieces = tract_pieces,
    features = sum(vapply(tract_pieces, nrow, integer(1))),
    state_pieces = length(tract_pieces),
    repetitions = 7L
  ),
  list(
    scale = "National 6-variable tidy ACS geometry",
    pieces = tidy_tract_pieces,
    features = sum(vapply(tidy_tract_pieces, nrow, integer(1))),
    state_pieces = length(tidy_tract_pieces),
    repetitions = 3L
  )
)

results <- vector("list", length(scales))
for (i in seq_along(scales)) {
  scale <- scales[[i]]
  baseline <- function() baseline_bind(scale$pieces)
  candidate <- function() candidate_bind(scale$pieces)

  message("Benchmarking ", scale$scale)
  baseline_output <- baseline()
  candidate_output <- candidate()
  stopifnot(identical(baseline_output, candidate_output))

  timings <- measure_pair(baseline, candidate, scale$repetitions)
  allocations <- c(
    baseline_alloc_bytes = allocated_bytes(baseline),
    candidate_alloc_bytes = allocated_bytes(candidate)
  )

  results[[i]] <- data.frame(
    scale = scale$scale,
    features = scale$features,
    state_pieces = scale$state_pieces,
    output_mb = as.numeric(object.size(baseline_output)) / 1024^2,
    repetitions = scale$repetitions,
    as.list(c(timings, allocations)),
    check.names = FALSE
  )
}

results <- do.call(rbind, results)
results$baseline_alloc_mb <- results$baseline_alloc_bytes / 1024^2
results$candidate_alloc_mb <- results$candidate_alloc_bytes / 1024^2

print(results, row.names = FALSE)
cat("\nGeometry fetch elapsed: ", geometry_fixture$fetch_elapsed_s, " s\n", sep = "")
cat(
  "ACS fixture fetch elapsed: ",
  sum(vapply(acs_fixtures, `[[`, numeric(1), "api_elapsed_s")),
  " s\n",
  sep = ""
)
cat(R.version.string, "\n")
versions <- vapply(
  c("tidycensus", "dplyr", "purrr", "sf", "tigris", "testthat"),
  function(package) as.character(utils::packageVersion(package)),
  character(1)
)
print(versions)

results_path <- Sys.getenv("TIDYCENSUS_BENCH_RESULTS")
if (nzchar(results_path)) {
  utils::write.csv(results, results_path, row.names = FALSE)
}
