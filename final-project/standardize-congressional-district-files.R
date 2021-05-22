rm(list = ls())

setwd("~/Documents/Classes/PPHA 30560/ppha-30560/final-project/")

library("tidyverse")
library("sf")

load_boundary_file_pre_2011 <- function(filename) {
  st_read(filename) %>%
    mutate(state_abbr = case_when(statename == "District Of Columbia" ~ "DC",
                                  TRUE ~ state.abb[match(statename,state.name)]),
           district_number = as.integer(district),
           district_number = if_else(district_number == 0L, 1L, district_number),
           district_number = if_else(district_number < 10L, paste0("0", district_number), as.character(district_number)),
           district_name = paste0(state_abbr, district_number)) %>%
    select(-district) %>%
    st_transform("NAD83")
}

load_boundary_file_post_2011 <- function(filename) {
  st_read(filename) %>%
    rename(district_number = 2) %>%
    filter(district_number != "ZZ") %>%
    mutate(state_abbr = case_when(STATEFP == "01" ~ "AL",
                                  STATEFP == "02" ~ "AK",
                                  STATEFP == "04" ~ "AZ",
                                  STATEFP == "05" ~ "AR",
                                  STATEFP == "06" ~ "CA",
                                  STATEFP == "08" ~ "CO",
                                  STATEFP == "09" ~ "CT",
                                  STATEFP == "10" ~ "DE",
                                  STATEFP == "11" ~ "DC",
                                  STATEFP == "12" ~ "FL",
                                  STATEFP == "13" ~ "GA",
                                  STATEFP == "15" ~ "HI",
                                  STATEFP == "16" ~ "ID",
                                  STATEFP == "17" ~ "IL",
                                  STATEFP == "18" ~ "IN",
                                  STATEFP == "19" ~ "IA",
                                  STATEFP == "20" ~ "KS",
                                  STATEFP == "21" ~ "KY",
                                  STATEFP == "22" ~ "LA",
                                  STATEFP == "23" ~ "ME",
                                  STATEFP == "24" ~ "MD",
                                  STATEFP == "25" ~ "MA",
                                  STATEFP == "26" ~ "MI",
                                  STATEFP == "27" ~ "MN",
                                  STATEFP == "28" ~ "MS",
                                  STATEFP == "29" ~ "MO",
                                  STATEFP == "30" ~ "MT",
                                  STATEFP == "31" ~ "NE",
                                  STATEFP == "32" ~ "NV",
                                  STATEFP == "33" ~ "NH",
                                  STATEFP == "34" ~ "NJ",
                                  STATEFP == "35" ~ "NM",
                                  STATEFP == "36" ~ "NY",
                                  STATEFP == "37" ~ "NC",
                                  STATEFP == "38" ~ "ND",
                                  STATEFP == "39" ~ "OH",
                                  STATEFP == "40" ~ "OK",
                                  STATEFP == "41" ~ "OR",
                                  STATEFP == "42" ~ "PA",
                                  STATEFP == "44" ~ "RI",
                                  STATEFP == "45" ~ "SC",
                                  STATEFP == "46" ~ "SD",
                                  STATEFP == "47" ~ "TN",
                                  STATEFP == "48" ~ "TX",
                                  STATEFP == "49" ~ "UT",
                                  STATEFP == "50" ~ "VT",
                                  STATEFP == "51" ~ "VA",
                                  STATEFP == "53" ~ "WA",
                                  STATEFP == "54" ~ "WV",
                                  STATEFP == "55" ~ "WI",
                                  STATEFP == "56" ~ "WY",
                                  STATEFP == "60" ~ "AS",
                                  STATEFP == "66" ~ "GU",
                                  STATEFP == "69" ~ "MP",
                                  STATEFP == "72" ~ "PR",
                                  STATEFP == "78" ~ "VI",
                                  TRUE ~ STATEFP),
           district_number = as.integer(district_number),
           district_number = if_else(district_number == 0L, 1L, district_number),
           district_number = if_else(district_number == 98L, 1L, district_number),
           district_number = if_else(district_number < 10L, paste0("0", district_number), as.character(district_number)),
           district_name = paste0(state_abbr, district_number)) %>%
    st_transform("NAD83")
}

load_boundary_file <- function(filename, census_year) {
  if(is.na(census_year) || census_year < 2010) {
    load_boundary_file_pre_2011(filename)
  } else {
    load_boundary_file_post_2011(filename)
  }
}

congressional_district_files <- read_csv("congressional-district-files.csv")

congressional_districts <- map2_dfr(congressional_district_files$filename, congressional_district_files$census_year, load_boundary_file) %>%
  select(statename, state_abbr, district_number, district_name, startcong, endcong, member, geometry)
