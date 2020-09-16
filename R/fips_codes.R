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
#' \href{https://www.census.gov/programs-surveys/acs/technical-documentation/pums/documentation.2018.html}{Census
#' PUMS data dictionaries}. Use this dataset to lookup the names of variables to
#' use in \code{\link{get_pums}}. This dataset also contains labels for the
#' coded values returned by the Census API and is used when \code{recode = TRUE}
#' in \code{\link{get_pums}}.
#'
#' Because variable names and codes change from year to year, you should filter
#' this dataset for the survey and year of interest. NOTE: only 2017 and 2018 (acs1 and
#' acs5) variables are available.
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


