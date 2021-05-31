rm(list = ls())

library("tidyverse")
library("sf")

ridership <- read_csv("data/ridership-trends.csv")
routes <- read_sf("data/CTA_BusRoutes/CTA_BusRoutes.shp") %>%
  st_transform(crs = "EPSG:4326")

merged <- routes %>%
  left_join(ridership, by = c("ROUTE" = "route")) %>%
  select(ROUTE, NAME, starts_with("ridership_"), geometry) %>%
  rename(route_number = ROUTE,
         route_name = NAME)

merged %>%
  write_sf("to-mapshaper/cta_routes.geojson")

read_sf("data/Boundaries - City/geo_export_a07929fb-0956-46fb-ac4c-cd3f1368cc32.shp") %>%
  st_transform(crs = "EPSG:4326") %>%
  #st_crop(st_bbox(merged)) %>%
  write_sf("to-mapshaper/city_boundaries.geojson")

read_sf("data/Waterways/geo_export_c9113f38-dfcb-47a3-a0e5-88aff3e52cf8.shp") %>%
  st_transform(crs = "EPSG:4326") %>%
  #st_crop(st_bbox(merged)) %>%
  st_union() %>%
  write_sf("to-mapshaper/water.geojson")

read_sf("data/Boundaries - City/geo_export_a07929fb-0956-46fb-ac4c-cd3f1368cc32.shp") %>%
  st_transform(crs = "EPSG:4326") %>%
  #st_crop(st_bbox(merged)) %>%
  write_sf("to-mapshaper/nearby_municipalities.geojson")
