rm(list = ls())

setwd("~/Documents/Classes/PPHA 30560/ppha-30560/final-project/")

library("tidyverse")

all_boundary_files <- bind_rows(tibble(filename = list.files("data/congressional-district-boundaries", pattern = "*.geojson", full.names = TRUE)) %>%
                                  extract(filename,
                                          into = c("state_name", "from_session", "to_session"),
                                          regex = "([A-Za-z_]+)_(\\d+)_to_(\\d+)",
                                          remove = FALSE,
                                          convert = TRUE) %>%
                                  mutate(from_election_year = from_session * 2 + 1786,
                                         to_election_year = to_session * 2 + 1786) %>%
                                  select(filename, state_name, from_election_year, to_election_year),
                                tibble(filename = list.files("data/congressional-district-boundaries", pattern = "*.shp$", full.names = TRUE)) %>%
                                  mutate(session_number = as.integer(str_remove(str_extract(filename, "\\d{3}\\.shp"), ".shp")),
                                         from_election_year = session_number * 2 + 1786,
                                         to_election_year = from_election_year) %>%
                                  select(filename, from_election_year, to_election_year)) %>%
  mutate(census_year = floor(from_election_year / 10) * 10,
         census_year = if_else(census_year < 1790, NA_real_, census_year),
         state_name = str_replace_all(state_name, "_", " "))

write_csv(all_boundary_files, "congressional-district-files.csv")
