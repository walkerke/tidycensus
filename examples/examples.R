# api_key <- "5ed58a5745802102fb83d4eec5d1f7326f65ffab"

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(forcats)
library(viridis)
library(stringr)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
# census_api_key("5ed58a5745802102fb83d4eec5d1f7326f65ffab", install = TRUE)

# All block groups in the US - let's try
# Need state, then all counties for that state, in a given year
ctys <- counties(cb = TRUE)

state_codes <- unique(fips_codes$state_code)[1:51]

bgs <- map_df(state_codes, function(state_code) {
  state <- filter(ctys, STATEFP == state_code)
  county_codes <- state$COUNTYFP
  get_acs(geography = "block group", variables = "B25038_001",
          state = state_code, county = county_codes)
})


poverty <- get_acs(geography = "county", state = "KY",
                   variables = "S1701_C02_001", summary_var = "S1701_C01_001")


age <- paste0("B01001_0", 10:49)

ashland <- get_acs(geography = "place", variables = age,
                   state = "WI") %>%
  filter(grepl("Ashland", NAME))



us <- unique(fips_codes$state)[1:51]
totalpop <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B01003_001",
          state = x)
})

totalpop_sf <- get_acs(geography = "tract", variables = "B01003_001",
                       state = us, geometry = TRUE)

z00 <- get_decennial(geography = "zcta", variables = "PCT0120001", state = "MN",
                     geometry = TRUE)

c15 <- get_acs(geography = "county", variables = "B19013_001", survey = "acs1")

zc <- get_acs(geography = "zip code tabulation area", variables = "B19013_001",
              summary_var = "B01001_001", geometry = TRUE)

tarr <- get_acs(geography = "tract", variables = c("B19013_001", "B01001_001"),
                state = "TX", county = "Tarrant", geometry = TRUE, output = "wide",
                moe_level = 99)

tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = c("Tarrant", "Dallas"), geometry = TRUE)

tarr <- get_acs(geography = "tract", variables = "DP04_0127P",
                state = "TX", county = c("Tarrant", "Dallas"), geometry = TRUE)

tarr <- get_acs(geography = "tract", variables = c("DP04_0127P", "DP04_0127"),
                state = c("TX", "OK"), geometry = TRUE)

tarr <- get_acs(geography = "tract", variables = "DP")

age <- paste0("B01001_0", 10:49)

lotsofvars <- get_acs(geography = "tract", variables = age,
                      state = "TX", county = c("Tarrant", "Dallas"), geometry = TRUE)

lotsofvars <- get_acs(geography = "tract", variables = age,
                      state = c("TX", "OK"),  geometry = FALSE)

st <- c("TX", "OK")

lotsofvars <- get_acs(geography = "tract", variables = age,
                      state = st,  geometry = TRUE)


lotsofvars <- get_acs(geography = "tract", variables = age, output = "wide",
                      state = "TX", county = c("Tarrant", "Dallas"), geometry = FALSE)


tarr <- get_acs(geography = "block group", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)

us <- unique(fips_codes$state)[1:51]

txok <- get_acs(geography = "tract", variables = "B19013_001",
                state = c("TX", "OK", "AR", "LA"))

txok <- get_acs(geography = "tract", variables = "B19013_001",
                state = us)

# Testing failures
tarr <- get_acs(geography = "blok group", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)

tarr <- get_acs(geography = "block group", variables = "B19013_001",
                state = "TX", county = c("Tarrant", "Dallas"), geometry = TRUE)

tarr <- get_acs(geography = "county", variables = "B19013_001",
                state = c("TX", "OK"), geometry = TRUE)

tarr <- get_acs(geography = "county", variables = "B19013_1",
                state = c("TX", "OK"), geometry = TRUE)

census_api_key("blooblah")

tarr <- get_acs(geography = "county", variables = "B19013_001",
                state = c("TX", "OK"), geometry = TRUE)


den <- get_acs(geography = "block group", variables = "B19013_001",
                state = "CO", county = "Arapahoe", geometry = TRUE)

racevars <- c("B03002_003", "B03002_004", "B03002_006", "B03001_003")
harris <- get_acs(geography = "tract", variables = racevars, key = api_key,
                  state = "TX", county = "Harris County", geometry = TRUE,
                  summary_var = "B01003_001") %>%
  mutate(pct = 100 * (estimate / summary_est),
         variable = fct_recode(variable,
                               White = "B03002_003",
                               Black = "B03002_004",
                               Asian = "B03002_006",
                               Hispanic = "B03001_003")
         ) %>%
  st_transform(26915)

ggplot(harris, aes(fill = pct, color = pct)) +
  facet_wrap(~variable) +
  geom_sf() +
  scale_fill_viridis() +
  scale_color_viridis()


vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT")
vt %>%
  mutate(NAME = str_replace(NAME, " County, Vermont", "")) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2011-2015 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")





# Try with normalizing var


# Decennial Census

black00 <- get_decennial(geography = "tract", variables = "P007003", year = 2000,
                  key = api_key, state = "MN", county = "Hennepin", geometry = TRUE)

lots <- paste0("PCT0120", 10:80)

lots00 <- get_decennial(geography = "tract", variables = lots, year = 2000,
                         key = api_key, state = "MN", county = "Hennepin", geometry = TRUE)

lots00 <- get_decennial(geography = "tract", variables = lots, year = 2000, output = "wide",
                        key = api_key, state = "MN", county = "Hennepin", geometry = TRUE)

l3 <- paste0("PCT0200", 10:97)

lots3 <- get_decennial(geography = "tract", variables = l3, year = 2000,
                       key = api_key, state = "MN", county = "Hennepin", geometry = TRUE)

armed <- get_decennial(geography = "block group", variables = "P038002", year = 2000,
                       state = "MN", county = c("Hennepin", "Ramsey"), geometry = TRUE)

black90 <- get_decennial(geography = "tract", variables = "P0100002", year = 1990,
                         state = "IL", county = "Cook", geometry = TRUE)

ny <- get_decennial(geography = "county", variables = "P0100002", year = 1990,
                    state = "NY", geometry = TRUE)

vars <- c("P0100001", "P0100002", "P0100004", "P0080001")
race10 <- get_decennial(geography = "tract", variables = vars10, year = 2010,
                        key = api_key, state = "IL",
                        geometry = TRUE, summary_var = "P0010001")

vars10 <- c("P0050003", "P0050004", "P0050006", "P0040003")

il <- get_decennial(geography = "county", variables = vars10, year = 2010,
                    summary_var = "P0010001", state = "IL", geometry = TRUE) %>%
  mutate(pct = 100 * (value / summary_value))

ggplot(il, aes(fill = pct, color = pct)) +
  geom_sf() +
  facet_wrap(~variable)


tidy <- tarr %>%
  mutate(GEOID = paste0(state, county, tract)) %>%
  select(GEOID, B19013_001E, B19013_001M) %>%
  gather(key = variable, value = value, -GEOID) %>%
  separate(variable, into = c("variable", "type"), sep = -2) %>%
  mutate(type = ifelse(type == "E", "estimate", "moe")) %>%
  spread(type, value)

tr <- tracts("TX", "Tarrant", cb = TRUE, class = "sf")

tidy2 <- left_join(tidy, tr, by = "GEOID")

library(tidycensus)
library(tidyverse)
library(sf)
library(viridis)

get_acs(geography = "tract", variables = "B19013_001E",
        key = api_key, state = "IL", county = "Cook",
        geometry = TRUE) %>%
  rename(hhincome = B19013_001E) %>%
  ggplot() +
  geom_sf(aes(fill = hhincome,
              color = hhincome)) +
  coord_sf(crs = 26916) +
  scale_fill_viridis() +
  scale_color_viridis()




tarr %>%
  st_transform(26914) %>%
  ggplot() +
  geom_sf(aes(fill = B19013_001E)) +
  scale_fill_viridis()


df <- census(geography = "tract", variables = c("P0010001", "P0030001"),
             key = api_key, state = "OR", county = "Benton")

black00 <- census(geography = "tract", variables = "P007003", year = 2000,
                  key = api_key, state = "MN", county = "Hennepin")

slovak90 <- census(geography = "tract", variables = "P0350025", year = 1990,
                  sumfile = "sf3", key = api_key, state = "NY", county = "Queens")
