api_key <- "5ed58a5745802102fb83d4eec5d1f7326f65ffab"

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(rlang)

options(tigris_use_cache = TRUE)

tarr <- get_acs(geography = "tract", variables = c("B19013_001", "B01001_001"), key = api_key,
                state = "TX", county = "Tarrant", geometry = TRUE, output = "wide")

# Try with normalizing var


# Decennial Census

black00 <- get_decennial(geography = "tract", variables = "P007003", year = 2000,
                  key = api_key, state = "MN", county = "Hennepin", geometry = TRUE)

black90 <- get_decennial(geography = "tract", variables = "P0100002", year = 1990,
                         key = api_key, state = "IL", county = "Cook", geometry = TRUE)

vars <- c("P0100001", "P0100002", "P0100004", "P0080001")
vars10 <- c("P0050003", "P0050004", "P0050006", "P0040003")
race10 <- get_decennial(geography = "tract", variables = vars10, year = 2010,
                        key = api_key, state = "IL",
                        geometry = TRUE)

ggplot(race90, aes(fill = value, color = value)) +
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