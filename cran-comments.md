## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS Tahoe 26.6 (aarch64), R 4.6.1
* win-builder (R-devel)

## Reverse dependencies

We checked 25 reverse dependencies, comparing R CMD check results across the CRAN and development versions of this package. We saw 0 new problems, and failed to check 0 packages.

## Notes

* This version requires tigris (>= 2.3.0), which was accepted on CRAN on October 16.
* Requests now use httr2 in place of httr.
* Tests that call the Census API are skipped on CRAN.
