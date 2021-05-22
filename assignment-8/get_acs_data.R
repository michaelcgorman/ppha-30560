rm(list = ls())

setwd("~/Documents/Classes/PPHA 30560/ppha-30560/assignment-8/")

library("tidyverse")
library("tidycensus")
library("sf")

options(tigris_use_cache = TRUE)

# acs5_2019_vars <- load_variables(2019, "acs5", cache = TRUE)

county_commute_times <- get_acs(geography = "county",
                                table = "B08303",
                                cache_table = TRUE,
                                year = 2019,
                                survey = "acs5",
                                output = "wide",
                                geometry = TRUE) %>%
  select(-ends_with("M")) %>%
  rename(total_commuters = B08303_001E,
         num_under_05_mins = B08303_002E,
         num_05_to_09_mins = B08303_003E,
         num_10_to_14_mins = B08303_004E,
         num_15_to_19_mins = B08303_005E,
         num_20_to_24_mins = B08303_006E,
         num_25_to_29_mins = B08303_007E,
         num_30_to_34_mins = B08303_008E,
         num_35_to_39_mins = B08303_009E,
         num_40_to_44_mins = B08303_010E,
         num_45_to_59_mins = B08303_011E,
         num_60_to_89_mins = B08303_012E,
         num_90_plus_mins = B08303_013E) %>%
  mutate(share_under_05_mins = num_under_05_mins / total_commuters,
         share_05_to_09_mins = num_05_to_09_mins / total_commuters,
         share_10_to_14_mins = num_10_to_14_mins / total_commuters,
         share_15_to_19_mins = num_15_to_19_mins / total_commuters,
         share_20_to_24_mins = num_20_to_24_mins / total_commuters,
         share_25_to_29_mins = num_25_to_29_mins / total_commuters,
         share_30_to_34_mins = num_30_to_34_mins / total_commuters,
         share_35_to_39_mins = num_35_to_39_mins / total_commuters,
         share_40_to_44_mins = num_40_to_44_mins / total_commuters,
         share_45_to_59_mins = num_45_to_59_mins / total_commuters,
         share_60_to_89_mins = num_60_to_89_mins / total_commuters,
         share_90_plus_mins = num_90_plus_mins / total_commuters,
         total_under_05_mins = num_under_05_mins,
         total_under_10_mins = total_under_05_mins + num_05_to_09_mins,
         total_under_15_mins = total_under_10_mins + num_10_to_14_mins,
         total_under_20_mins = total_under_15_mins + num_15_to_19_mins,
         total_under_25_mins = total_under_20_mins + num_20_to_24_mins,
         total_under_30_mins = total_under_25_mins + num_25_to_29_mins,
         total_under_35_mins = total_under_30_mins + num_30_to_34_mins,
         total_under_40_mins = total_under_35_mins + num_35_to_39_mins,
         total_under_45_mins = total_under_40_mins + num_40_to_44_mins,
         total_under_60_mins = total_under_45_mins + num_45_to_59_mins,
         total_under_90_mins = total_under_60_mins + num_60_to_89_mins,
         median_commuter = total_commuters / 2,
         median_length_detailed = case_when(total_under_05_mins > median_commuter ~ "under 5 minutes",
                                            total_under_10_mins > median_commuter ~ "5 to 10 minutes",
                                            total_under_15_mins > median_commuter ~ "10 to 15 minutes",
                                            total_under_20_mins > median_commuter ~ "15 to 20 minutes",
                                            total_under_25_mins > median_commuter ~ "20 to 25 minutes",
                                            total_under_30_mins > median_commuter ~ "25 to 30 minutes",
                                            total_under_35_mins > median_commuter ~ "30 to 35 minutes",
                                            total_under_40_mins > median_commuter ~ "35 to 40 minutes",
                                            total_under_45_mins > median_commuter ~ "40 to 45 minutes",
                                            total_under_60_mins > median_commuter ~ "45 to 60 minutes",
                                            total_under_90_mins > median_commuter ~ "60 to 90 minutes",
                                            TRUE ~ "over 90 minutes"),
         median_length = case_when(total_under_10_mins > median_commuter ~ "under 10 minutes",
                                   total_under_20_mins > median_commuter ~ "10 to 20 minutes",
                                   total_under_30_mins > median_commuter ~ "20 to 30 minutes",
                                   total_under_40_mins > median_commuter ~ "30 to 40 minutes",
                                   TRUE ~ "over 40 minutes"),
         total_length = num_under_05_mins * 2.5 +
           num_05_to_09_mins * 7.5 +
           num_10_to_14_mins * 12.5 +
           num_15_to_19_mins * 17.5 +
           num_20_to_24_mins * 22.5 +
           num_25_to_29_mins * 27.5 +
           num_30_to_34_mins * 32.5 +
           num_35_to_39_mins * 37.5 +
           num_40_to_44_mins * 42.5 +
           num_45_to_59_mins * 52.5 +
           num_60_to_89_mins * 75 +
           num_90_plus_mins * 100, # very rough guesstimate
         mean_length = total_length / total_commuters)

county_commute_times %>%
  select(GEOID, NAME, median_length, median_length_detailed, starts_with("share"), geometry) %>%
  write_sf("county_commute_times.geojson")


state_borders <- get_acs(geography = "state",
                         variables = "B01001_001",
                         geometry = TRUE) %>%
  select(GEOID, NAME, geometry)

state_borders %>%
  write_sf("state_borders.geojson")
