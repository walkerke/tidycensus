# Multi-state spatial binding benchmark

This benchmark compares the sequential `reduce(rbind)` behavior on
`origin/master` at `5461f03` with the single-pass spatial row binding on this
branch. It uses real Census geometry at three output scales, including a
national tidy ACS result built from actual 2023 ACS estimates and margins of
error.

## Results

| Workload | Features | State pieces | Output size | Repetitions | Baseline | Candidate | Time saved | Speedup | Baseline allocation | Candidate allocation |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| National county geometry | 3,143 | 51 | 3.9 MB | 9 | 0.157 s | 0.015 s | 0.142 s | 10.5x | 11.2 MB | 0.3 MB |
| National tract geometry | 85,186 | 56 | 157.3 MB | 7 | 6.829 s | 0.122 s | 6.707 s | 56.0x | 1,124.9 MB | 13.1 MB |
| National 6-variable tidy ACS geometry | 510,270 | 52 | 821.6 MB | 3 | 27.390 s | 0.485 s | 26.905 s | 56.5x | 3,298.8 MB | 54.7 MB |

Elapsed values are medians from randomized baseline/candidate run order after
one warm-up of each implementation. The observed ranges were 0.156--0.159 vs.
0.014--0.016 seconds for counties, 6.763--6.939 vs. 0.119--0.127 seconds for
tracts, and 27.340--27.525 vs. 0.482--0.485 seconds for the tidy ACS result.
Cumulative allocations come from one `Rprofmem()` run and are not peak memory.

Every result is normalized through the same existing `as_tibble()` / `st_as_sf()`
steps and checked with `identical()` before timing. This verifies row and column
order, values, geometry list columns, CRS, and final `sf` attributes.

## Data and timing boundary

The national tract fixture is the official 2023 cartographic-boundary geometry
downloaded by `tigris::tracts(year = 2023, cb = TRUE)`. The largest workload
joins that geometry shape with real 2023 ACS 5-year B01001 values for six
variables, matching the tidy spatial output returned by a multi-state
`get_acs(..., geometry = TRUE)` call.

The timed boundary is only the multi-state row bind and the existing final
normalization. Geometry download, API transfer, and data/geometry joins are
excluded. Cold national geometry download took 4.250 seconds; capture of the 52
state/DC/Puerto Rico ACS responses took 147.426 seconds. Those values are
descriptive and are not counted as code speedup because the package's live
network path and tigris caching can vary independently.

The small county case saves only 0.142 seconds. The change is motivated by the
6.7--26.9 second absolute savings on national tract-scale spatial results.

## Reproduction

From the package root:

```sh
export CENSUS_API_KEY=your_key
export TIDYCENSUS_BENCH_CACHE=/path/to/a/persistent/cache
export TIDYCENSUS_GEOMETRY_FIXTURE=/path/to/national-tracts-2023.rds
Rscript benchmarks/spatial-bind.R
```

If `TIDYCENSUS_GEOMETRY_FIXTURE` does not exist, the script downloads and saves
the official geometry there. Set `TIDYCENSUS_BENCH_RESULTS` to write the raw
results as CSV.

The reported run used R 4.5.0 on an Apple M4 Pro with 48 GB RAM, with dplyr
1.1.4, purrr 1.2.1, sf 1.0-23, tigris 2.2.1, and testthat 3.2.3.
