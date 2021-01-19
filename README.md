## tidycensus

[![Build Status](https://travis-ci.org/walkerke/tidycensus.svg?branch=master)](https://travis-ci.org/walkerke/tidycensus) ![CRAN Badge](http://www.r-pkg.org/badges/version/tidycensus)  ![CRAN Downloads](http://cranlogs.r-pkg.org/badges/tidycensus)

__tidycensus__ is an R package that allows users to interface with the US Census Bureau's decennial Census and five-year American Community APIs and return tidyverse-ready data frames, optionally with simple feature geometry included.  Install from CRAN with the following command: 

```r
install.packages("tidycensus")
```

To learn more about the package, please visit the package documentation at https://walker-data.com/tidycensus. 

Note: This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.

Update logs:

## In version 0.11: 

* `get_acs()` now defaults to `year = 2019`, meaning that 2015-2019 5-year estimates and 2019 1-year estimates are retrieved if no year is specified.
* `get_estimates()` now also defaults to `year = 2019`. 
* tidycensus can once again access the 2000 Summary File 3 in `get_decennial()` as the Census Bureau has restored its API endpoint.
* Various bug fixes and internal improvements; browse [the GitHub issues](https://github.com/walkerke/tidycensus/issues) for a complete listing.

## In version 0.10.2:

* tidycensus now includes functionality to download and analyze data from the [American Community Survey Public Use Microdata Series (PUMS) datasets](https://www.census.gov/programs-surveys/acs/microdata.html).  [Read through the corresponding documentation to learn how to use these features.](https://walker-data.com/tidycensus/articles/pums-data.html)
* tidycensus cannot access 1990 data or 2000 Summary File 3 data due to the removal of these API endpoints by the Census Bureau.  When new endpoints for these years are added, tidycensus will support them again in a future release. 


## In version 0.9.9.2: 

* `geometry = TRUE` now works for all geographies currently available in tidycensus.  
* `get_acs()` can now support the ACS Supplemental Estimates API. 


## In version 0.9.5: 

* `get_acs()` now defaults to the 2014-2018 five-year American Community Survey estimates, or the 2018 1-year estimates if users set `survey = "acs1"`.
* Various improvements and bug fixes (largely thanks to Matt Herman's contributions).  

## In version 0.8.1: 

* tidycensus now includes support for the Census Bureau Population Estimates API.  Please see https://walkerke.github.io/tidycensus/articles/other-datasets.html for examples of how to use these new features.  

* Important internal changes to ensure that tidycensus continues to work with upcoming changes to Census API endpoints.  

* Several internal improvements and bug fixes.  Please see the closed issues at https://github.com/walkerke/tidycensus/issues for a list.  


