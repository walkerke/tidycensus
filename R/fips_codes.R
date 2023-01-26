#' Dataset with FIPS codes for US states and counties
#'
#' Built-in dataset for use with the \code{lookup_code} function.
#' To access the data directly, issue the command \code{data(fips_codes)}.
#'
#' Note: this dataset includes FIPS codes for all counties that have appeared in
#' the decennial Census or American Community Survey from 2010 to the present.
#' This means that counties that have been renamed or absorbed into other
#' geographic entities since 2010 remain in this dataset along with newly added
#' or renamed counties.
#'
#' If you need the FIPS codes and names for counties for a particular Census
#' year, you can use the \link[tigris]{counties} function from the tigris
#' package and set the year parameter as required.
#'
#'
#' @title Dataset with FIPS codes for US states and counties
#' @description Built-in dataset for smart state and county lookup.
#'              To access the data directly, issue the command \code{data(fips_codes)}.
#'
#' \itemize{
#'   \item \code{county}: County name, title-case
#'   \item \code{county_code}: County code. (3-digit, 0-padded, character)
#'   \item \code{state}: Upper-case abbreviation of state
#'   \item \code{state_code}: State FIPS code (2-digit, 0-padded, character)
#'   \item \code{state_name}: Title-case name of state
#' }
#'
#' @docType data
#' @keywords datasets
#' @name fips_codes
#'
#' @usage data(fips_codes)
"fips_codes"


#' Dataset of US states with Alaska and Hawaii shifted and re-scaled
#'
#' Built-in dataset for use with the \code{shift_geo} parameter, with the continental United States in a
#' Lambert azimuthal equal area projection and Alaska and Hawaii shifted and re-scaled.  The data were originally
#' obtained from the albersusa R package (\url{https://github.com/hrbrmstr/albersusa}).
#'
#' @title Dataset of US states with Alaska and Hawaii shifted and re-scaled
#'
#' @docType data
#' @keywords datasets
#' @name state_laea
#'
#' @usage data(state_laea)
"state_laea"

#' Dataset of US counties with Alaska and Hawaii shifted and re-scaled
#'
#' Built-in dataset for use with the \code{shift_geo} parameter, with the continental United States in a
#' Lambert azimuthal equal area projection and Alaska and Hawaii counties and Census areas shifted and re-scaled.
#' The data were originally obtained from the albersusa R package (\url{https://github.com/hrbrmstr/albersusa}).
#'
#' @title Dataset of US counties with Alaska and Hawaii shifted and re-scaled
#'
#' @docType data
#' @keywords datasets
#' @name county_laea
#'
#' @usage data(county_laea)
"county_laea"


#' Dataset with PUMS variables and codes
#'
#' Built-in dataset that is created from the
#' \href{https://www.census.gov/programs-surveys/acs/microdata/documentation.html}{Census
#' PUMS data dictionaries}. Use this dataset to lookup the names of variables to
#' use in \code{\link{get_pums}}. This dataset also contains labels for the
#' coded values returned by the Census API and is used when \code{recode = TRUE}
#' in \code{\link{get_pums}}.
#'
#' Because variable names and codes change from year to year, you should filter
#' this dataset for the survey and year of interest. NOTE: 2017 - 2019 and 2021 acs1 and
#' 2017 - 2021 acs5 variables are available.
#'
#' @title Dataset with PUMS variables and codes
#' @description Built-in dataset for variable name and code label lookup.
#'              To access the data directly, issue the command \code{data(pums_variables)}.
#'
#' \itemize{
#'   \item \code{survey}: acs1 or acs5
#'   \item \code{year}: Year of data. For 5-year data, last year in range.
#'   \item \code{var_code}: Variable name
#'   \item \code{var_label}: Variable label
#'   \item \code{data_type}: chr or num
#'   \item \code{level}: housing or person
#'   \item \code{val_min}: For numeric variables, the minimum value
#'   \item \code{val_max}: For numeric variables, the maximum value
#'   \item \code{val_label}: Value label
#'   \item \code{recode}: Use labels to recode values
#'   \item \code{val_length}: Length of value returned
#'   \item \code{val_na}: Value of NA value returned by API (if known)

#' }
#'
#' @docType data
#' @keywords datasets
#' @name pums_variables
#'
#' @usage data(pums_variables)
"pums_variables"


#' Dataset with Migration Flows characteristic recodes
#'
#' Built-in dataset that is created from the
#' \href{https://www.census.gov/data/developers/data-sets/acs-migration-flows.html}{Migration
#' Flows API documentation}. This dataset contains labels for the coded values
#' returned by the Census API and is used when \code{breakdown_labels = TRUE} in
#' \code{\link{get_flows}}.
#'
#' @title Dataset with Migration Flows characteristic recodes
#' @description Built-in dataset for Migration Flows code label lookup.
#'
#' \itemize{
#'   \item \code{characteristic}: Characteristic variable name
#'   \item \code{code}: Characteristic calue code
#'   \item \code{desc}: Characteristic calue label
#'   \item \code{ordered}: Whether or not recoded value should be ordered factor
#' }
#'
#' @docType data
#' @keywords datasets
#' @name mig_recodes
#'
#' @usage data(mig_recodes)
"mig_recodes"

#' Dataset used to identify geography availability in the 5-year ACS Detailed Tables
#'
#' Built-in dataset that includes information on the smallest geography at which
#' 5-year ACS Detailed Tables data are available, by table, since 2011. This dataset
#' is used internally by \code{load_variables()} to add a \code{geography} column
#' when variables are retrieved for a 5-year ACS Detailed Tables dataset.
#'
#' @title Dataset used to identify geography availability in the 5-year ACS Detailed Tables
#' @description Built-in dataset for use by \code{load_variables()} to identify the smallest
#' geography at which 5-year ACS data are available
#'
#' \itemize{
#'   \item \code{table}: The ACS Table ID
#'   \item \code{geography}: The smallest geography at which a given table is available
#'   for a given year
#'   \item \code{year}: The endyear of the 5-year ACS dataset
#' }
#'
#' @docType data
#' @keywords datasets
#' @name acs5_geography
#'
#' @usage data(acs5_geography)
"acs5_geography"
