rm(list = ls())

setwd("~/Documents/Classes/PPHA 30560/ppha-30560/final-project/")

library("tidyverse")

data_files <- tibble(
  codebook = list.files("data/nhgis0004_csv", "*.txt", full.names = TRUE),
  data_file = list.files("data/nhgis0004_csv", "*.csv", full.names = TRUE)
) %>%
  mutate(year_geo = str_remove(str_extract(data_file, "\\d{4}_[a-z]*.csv"), ".csv"),
         year = str_extract(year_geo, "\\d{4}"),
         geo = str_extract(year_geo, "[a-z]+")) %>%
  select(-year_geo) %>%
  filter(year != "0125")

shapefiles <- tibble(
  shapefile = list.files("data/nhgis0002_shape", full.names = TRUE)
) %>%
  mutate(geo_year = str_remove(str_extract(shapefile, "[a-z]*_\\d{4}.zip"), ".zip"),
         year = str_extract(geo_year, "\\d{4}"),
         geo = str_extract(geo_year, "[a-z]+")) %>%
  select(-geo_year)

files <- left_join(data_files, shapefiles, by = c("year", "geo"))

write_csv(files, "ipums-files.csv")
