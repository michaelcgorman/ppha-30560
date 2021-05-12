rm(list = ls())

setwd("~/Documents/Classes/PPHA 30560/ppha-30560/assignment-7/")

library("tidyverse")

read_csv("pop_18_poverty.csv", col_types = cols(Geo_FIPS = col_character(),
                                                                Geo_NAME = col_character(),
                                                                Geo_QName = col_character(),
                                                                pop_18_in_poverty = col_double())) %>%
  mutate(Geo_FIPS = case_when(str_length(Geo_FIPS) == 4 ~ paste0("0", Geo_FIPS),
                              TRUE ~ Geo_FIPS)) %>%
  write_csv("poverty_data.csv")

