rm(list = ls())

library("tidyverse")
library("sf")

ridership <- read_csv("data/ridership-trends.csv")
routes <- read_sf("data/CTA_BusRoutes/CTA_BusRoutes.shp")

merged <- routes %>%
  left_join(ridership, by = c("ROUTE" = "route")) %>%
  select(ROUTE, NAME, starts_with("rank_"), geometry) %>%
  rename(route_number = ROUTE,
         route_name = NAME)

merged %>%
  write_sf("cta-routes.geojson")
