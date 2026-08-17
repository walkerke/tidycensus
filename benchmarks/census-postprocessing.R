# Benchmark Census response post-processing on real ACS extracts.
#
# The API is used only to capture fixtures. Timed runs replay the same responses
# through the baseline and candidate functions so network variation cannot be
# mistaken for an implementation speedup.
#
# Environment variables:
#   CENSUS_API_KEY                Census API key (needed only for a cold cache)
#   TIDYCENSUS_BENCH_CACHE        fixture directory (defaults under tempdir())
#   TIDYCENSUS_BENCH_BASELINE     git ref to compare (defaults to origin/master)
#   TIDYCENSUS_BENCH_RESULTS      optional path for the results CSV

options(stringsAsFactors = FALSE)
set.seed(20260816)

pkgload::load_all(".", quiet = TRUE)

variables <- sprintf("B01001_%03d", seq_len(48L))
names(variables) <- sprintf("sex_age_%02d", seq_along(variables))
formatted_variables <- tidycensus:::format_variables_acs(unname(variables))

state_codes <- unique(tidycensus::fips_codes$state_code)
state_codes <- state_codes[
  !is.na(state_codes) & (as.integer(state_codes) <= 56L | state_codes == "72")
]

cache_dir <- Sys.getenv(
  "TIDYCENSUS_BENCH_CACHE",
  file.path(tempdir(), "tidycensus-census-postprocessing")
)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

fetch_fixture <- function(path, state, county = NULL) {
  if (file.exists(path)) {
    return(readRDS(path))
  }

  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key)) {
    stop("CENSUS_API_KEY is required when a fixture is not cached.", call. = FALSE)
  }

  message(
    "Fetching ", state,
    if (!is.null(county)) paste0(" / ", county) else "",
    " from the Census API"
  )
  timing <- system.time({
    data <- suppressMessages(tidycensus:::load_data_acs(
      geography = "tract",
      formatted_variables = formatted_variables,
      key = key,
      year = 2023,
      state = state,
      county = county,
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

king <- fetch_fixture(
  file.path(cache_dir, "king-county-wa.rds"),
  state = "WA",
  county = "King"
)

states <- setNames(vector("list", length(state_codes)), state_codes)
for (state_code in state_codes) {
  states[[state_code]] <- fetch_fixture(
    file.path(cache_dir, paste0("state-", state_code, ".rds")),
    state = state_code
  )
}

baseline_ref <- Sys.getenv("TIDYCENSUS_BENCH_BASELINE", "origin/master")
baseline_file <- tempfile(fileext = ".R")
on.exit(unlink(baseline_file), add = TRUE)
status <- system2(
  "git",
  c("show", paste0(baseline_ref, ":R/acs.R")),
  stdout = baseline_file
)
if (!identical(status, 0L)) {
  stop("Could not load R/acs.R from ", baseline_ref, call. = FALSE)
}

baseline_env <- new.env(parent = asNamespace("tidycensus"))
sys.source(baseline_file, envir = baseline_env)
baseline_get_acs <- baseline_env$get_acs
candidate_get_acs <- tidycensus::get_acs

active_scale <- NULL
mock_load_data_acs <- function(geography, formatted_variables, key, year,
                               state = NULL, county = NULL, zcta = NULL,
                               survey, show_call = FALSE, group = NULL) {
  requested <- strsplit(formatted_variables, ",", fixed = TRUE)[[1L]]
  fixture <- if (identical(active_scale, "county")) {
    king
  } else {
    code <- suppressMessages(tidycensus:::validate_state(state))
    states[[code]]
  }
  fixture$data[c("GEOID", "NAME", requested)]
}

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
    scale = "King County, WA tracts",
    kind = "county",
    state = "WA",
    county = "King",
    tracts = nrow(king$data),
    api_requests = 1L,
    api_elapsed_s = king$api_elapsed_s,
    repetitions = 7L
  ),
  list(
    scale = "California tracts",
    kind = "state",
    state = "06",
    county = NULL,
    tracts = nrow(states[["06"]]$data),
    api_requests = 1L,
    api_elapsed_s = states[["06"]]$api_elapsed_s,
    repetitions = 5L
  ),
  list(
    scale = "United States and Puerto Rico tracts",
    kind = "national",
    state = state_codes,
    county = NULL,
    tracts = sum(vapply(states, function(x) nrow(x$data), integer(1))),
    api_requests = length(states),
    api_elapsed_s = sum(vapply(states, `[[`, numeric(1), "api_elapsed_s")),
    repetitions = 3L
  )
)

results <- vector("list", length(scales))
for (i in seq_along(scales)) {
  scale <- scales[[i]]
  active_scale <- scale$kind

  baseline <- function() suppressMessages(baseline_get_acs(
    geography = "tract",
    variables = variables,
    year = 2023,
    survey = "acs5",
    output = "tidy",
    state = scale$state,
    county = scale$county,
    key = "replayed-real-response"
  ))
  candidate <- function() suppressMessages(candidate_get_acs(
    geography = "tract",
    variables = variables,
    year = 2023,
    survey = "acs5",
    output = "tidy",
    state = scale$state,
    county = scale$county,
    key = "replayed-real-response"
  ))

  message("Benchmarking ", scale$scale)
  testthat::with_mocked_bindings({
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
      tracts = scale$tracts,
      variables = length(variables),
      output_rows = nrow(baseline_output),
      output_mb = as.numeric(object.size(baseline_output)) / 1024^2,
      api_requests = scale$api_requests,
      api_elapsed_s = scale$api_elapsed_s,
      repetitions = scale$repetitions,
      as.list(c(timings, allocations)),
      check.names = FALSE
    )
  }, load_data_acs = mock_load_data_acs, .package = "tidycensus")
}

results <- do.call(rbind, results)
results$baseline_alloc_mb <- results$baseline_alloc_bytes / 1024^2
results$candidate_alloc_mb <- results$candidate_alloc_bytes / 1024^2

print(results, row.names = FALSE)
cat("\n", R.version.string, "\n", sep = "")
versions <- vapply(
  c("tidycensus", "dplyr", "tidyr", "purrr", "testthat"),
  function(package) as.character(utils::packageVersion(package)),
  character(1)
)
print(versions)

results_path <- Sys.getenv("TIDYCENSUS_BENCH_RESULTS")
if (nzchar(results_path)) {
  utils::write.csv(results, results_path, row.names = FALSE)
}
