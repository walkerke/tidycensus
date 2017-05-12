library(xml2)
library(jsonlite)
library(rvest)
library(tidyverse)
library(stringr)

fetch_sfvars <- function(year) {

  url <- paste0("http://api.census.gov/data/",
                as.character(year),
                "/acs5/variables.html")

  dat <- url %>%
    html() %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)

  out <- paste0("data/sf_",
                as.character(year),
                ".rds")

  write_rds(dat, out)

}

walk(2009:2015, fetch_sfvars)


fetch_censusvars <- function(dataset) {

  url <- paste0("http://api.census.gov/data/",
                dataset,
                "/variables.html")

  dat <- url %>%
    html() %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)

  year <- str_sub(dataset, 1, 4)

  sf <- str_sub(dataset, -3)

  out <- paste0("data/",
                sf, "_", year,
                ".rds")

  write_rds(dat, out)

}

sets <- c("2010/sf1", "2000/sf1", "2000/sf3",
          "1990/sf1", "1990/sf3")


walk(sets, fetch_censusvars)





j <- fromJSON("http://api.census.gov/data/2015/acs5/variables.json")


url <- "http://api.census.gov/data/2009/acs5/variables.html"

f15 <- url %>%
  html() %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

b <- f15[[1]]


x <- read_xml("http://api.census.gov/data/2015/acs5/variables.xml")