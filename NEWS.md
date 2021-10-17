# tidycensus 1.1

* tidycensus now supports the 2020 decennial Census PL-94171 redistricting data.  Use `year = 2020` in a call to `get_decennial()` to access the data, and `load_variables(2020, "pl")` to look up variable IDs.
* A new geography, `"voting district"` is now available for 2020 to assist with redistricting analyses.

# tidycensus 1.0

* Support for the ACS Migration Flows API is available in a new function, `get_flows()`. [Browse the supporting documentation](https://walker-data.com/tidycensus/articles/other-datasets.html#migration-flows-1) for information on how to analyze and map migration flows data in tidycensus. 
* The `shift_geo` argument is deprecated in favor of using the new `tigris::shift_geometry()` function. This allows for shifting of geometries for any geography in national mapping projects. 
* Various bug fixes and performance improvements; browse [the GitHub issues](https://github.com/walkerke/tidycensus/issues) for a complete listing.

# tidycensus 0.11.4

* Added support for the 2015-2019 5-year ACS PUMS data in `get_pums()`.  Updated variable definitions are found in the data object `pums_variables`. 
* The default year in `get_pums()` is updated to 2019.
* For 2015-2019 ACS aggregate data, `get_acs()` now supports the `state` parameter for zip code tabulation areas (ZCTAs) to allow subsetting by state.
* Various bug fixes and internal improvements; browse [the GitHub issues](https://github.com/walkerke/tidycensus/issues) for a complete listing.
* Added a `NEWS.md` file to track changes to the package.

# tidycensus 0.11 

* `get_acs()` now defaults to `year = 2019`, meaning that 2015-2019 5-year estimates and 2019 1-year estimates are retrieved if no year is specified.
* `get_estimates()` now also defaults to `year = 2019`. 
* tidycensus can once again access the 2000 Summary File 3 in `get_decennial()` as the Census Bureau has restored its API endpoint.
* Various bug fixes and internal improvements; browse [the GitHub issues](https://github.com/walkerke/tidycensus/issues) for a complete listing.

# tidycensus 0.10.2

* tidycensus now includes functionality to download and analyze data from the [American Community Survey Public Use Microdata Series (PUMS) datasets](https://www.census.gov/programs-surveys/acs/microdata.html).  [Read through the corresponding documentation to learn how to use these features.](https://walker-data.com/tidycensus/articles/pums-data.html)
* tidycensus cannot access 1990 data or 2000 Summary File 3 data due to the removal of these API endpoints by the Census Bureau.  When new endpoints for these years are added, tidycensus will support them again in a future release. 


# tidycensus 0.9.9.2

* `geometry = TRUE` now works for all geographies currently available in tidycensus.  
* `get_acs()` can now support the ACS Supplemental Estimates API. 


# tidycensus 0.9.5

* `get_acs()` now defaults to the 2014-2018 five-year American Community Survey estimates, or the 2018 1-year estimates if users set `survey = "acs1"`.
* Various improvements and bug fixes (largely thanks to Matt Herman's contributions).  

# tidycensus 0.8.1

* tidycensus now includes support for the Census Bureau Population Estimates API.  Please see https://walkerke.github.io/tidycensus/articles/other-datasets.html for examples of how to use these new features.  

* Important internal changes to ensure that tidycensus continues to work with upcoming changes to Census API endpoints.  

* Several internal improvements and bug fixes.  Please see the closed issues at https://github.com/walkerke/tidycensus/issues for a list.  

# tidycensus 0.4.6

* Bug fixed that was causing GEOIDs for some states to be converted to NA on certain Linux platforms

* A new parameter, `shift_geo`, allows tidycensus users to get US state and county geometry originally obtained with the __albersusa__ R package with Alaska and Hawaii shifted and re-scaled for better cartographic display of the entire US.  

```r
library(tidycensus)
library(tidyverse)
library(viridis)

us_county_income <- get_acs(geography = "county", variables = "B19013_001", 
                            shift_geo = TRUE, geometry = TRUE)

ggplot(us_county_income) + 
  geom_sf(aes(fill = estimate), color = NA) + 
  coord_sf(datum = NA) + 
  theme_minimal() + 
  scale_fill_viridis_c()

```