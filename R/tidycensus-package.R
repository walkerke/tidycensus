#' Return tidy data frames from the US Census Bureau API
#'
#' This packages uses US Census Bureau data but is neither endorsed nor supported by the US Census Bureau.
#'
#' @author Kyle Walker
#' @name tidycensus
#' @docType package
#' @import httr
#' @import sf
#' @import dplyr
#' @importFrom purrr map_dbl map_chr map reduce map_df flatten_df modify_depth map_dfc
#' @import tidyr
#' @importFrom jsonlite fromJSON
#' @import tigris
#' @import stringr
#' @import rvest
#' @import rappdirs
#' @importFrom readr read_rds write_rds parse_factor read_csv
#' @importFrom xml2 read_html
#' @import units
#' @importFrom utils packageVersion installed.packages
#' @importFrom rlang inform abort arg_match sym
#' @importFrom crayon cyan silver red green
#' @importFrom tidyselect vars_select_helpers
NULL