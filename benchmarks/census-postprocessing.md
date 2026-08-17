# Census response post-processing benchmark

This benchmark compares `origin/master` at `5461f03` with the response
post-processing changes on this branch. It uses real 2023 ACS 5-year tract
extracts for all 48 estimates and margins of error in table B01001. The
variables are supplied as a named vector, matching the workflow affected by
this change.

## Results

| Scale | Tracts | Output rows | Repetitions | Baseline | Candidate | Time saved | Speedup | Baseline allocation | Candidate allocation |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| King County, WA | 495 | 23,760 | 7 | 0.558 s | 0.024 s | 0.534 s | 23.3x | 233.0 MB | 30.7 MB |
| California | 9,129 | 438,192 | 5 | 9.898 s | 0.671 s | 9.227 s | 14.8x | 4,258.7 MB | 529.4 MB |
| United States and Puerto Rico | 85,381 | 4,098,288 | 3 | 90.646 s | 4.719 s | 85.927 s | 19.2x | 40,340.7 MB | 5,459.6 MB |

Elapsed values are medians from randomized baseline/candidate run order after
one warm-up of each implementation. The observed ranges were 0.551--0.593 vs.
0.023--0.024 seconds for King County, 9.765--10.193 vs. 0.641--0.760 seconds
for California, and 90.553--91.077 vs. 4.713--4.724 seconds nationally.
Cumulative allocations come from one `Rprofmem()` run and are not peak memory.

Every scale is checked with `identical()` before it is timed. The optimization
therefore produced byte-for-byte identical public `get_acs()` output for each
fixture.

## Data and timing boundary

The script first downloads actual Census API responses and caches each response
outside the repository. It then replays the exact same response through the
baseline and candidate `get_acs()` functions. This isolates local response
transformation from network latency and prevents Census API variance from being
reported as a code speedup.

Fixture capture used the Census group endpoint to reduce API traffic. The cold
capture took 2.289 seconds for King County, 14.212 seconds for California, and
147.426 seconds for the 52 state/DC/Puerto Rico responses used in the national
fixture. Those network measurements are descriptive only and are not combined
with the post-processing timings above. A normal 48-variable `get_acs()` call
uses two API requests per state because tidycensus chunks named variables into
groups of 24.

## Reproduction

From the package root:

```sh
export CENSUS_API_KEY=your_key
export TIDYCENSUS_BENCH_CACHE=/path/to/a/persistent/cache
Rscript benchmarks/census-postprocessing.R
```

Set `TIDYCENSUS_BENCH_RESULTS` to write the raw results as CSV, or
`TIDYCENSUS_BENCH_BASELINE` to compare against a git ref other than
`origin/master`.

The reported run used R 4.5.0 on an Apple M4 Pro with 48 GB RAM, with dplyr
1.1.4, tidyr 1.3.2, purrr 1.2.1, and testthat 3.2.3.
