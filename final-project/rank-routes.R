rm(list = ls())

library("tidyverse")
library("lubridate")

ridership_by_route <- read_csv("data/CTA_-_Ridership_-_Bus_Routes_-_Monthly_Day-Type_Averages___Totals.csv") %>%
  filter(!route %in% c("992", "1001")) %>%
  mutate(month_start_date = mdy(Month_Beginning),
         year_num = year(month_start_date),
         month_num = month(month_start_date)) %>%
  select(route, month_start_date, year_num, month_num, MonthTotal) %>%
  rename(ridership = MonthTotal) %>%
  arrange(year_num) %>%
  group_by(route, month_num) %>%
  mutate(rolling_avg_ridership = (lag(ridership, 1L) +
                                    lag(ridership, 2L) +
                                    lag(ridership, 3L) +
                                    lag(ridership, 4L)) / 4,
         ridership_change = (ridership - rolling_avg_ridership) / rolling_avg_ridership) %>%
  ungroup()

route_ranks <- ridership_by_route %>%
  filter(year_num == 2020, month_num == 4) %>%
  mutate(ridership_drop_group = case_when(ridership_change <= -0.9 ~ 1,
                                          ridership_change <= -0.8 ~ 2,
                                          ridership_change <= -0.7 ~ 3,
                                          ridership_change <= -0.6 ~ 4,
                                          ridership_change <= 0 ~ 5,
                                          TRUE ~ NA_real_)) %>%
  select(route, ridership_drop_group)

recent_trends <- ridership_by_route %>%
  filter(year_num >= 2020) %>%
  rename(monthly_ridership = ridership) %>%
  group_by(year_num, month_num) %>%
  mutate(current_ridership = ntile(monthly_ridership, 10),
         year_month = paste0("ridership_", year_num, "_", if_else(month_num < 10, "0", ""), month_num)) %>%
  ungroup() %>%
  select(route, year_month, current_ridership) %>%
  pivot_wider(names_from = year_month,
              values_from = current_ridership) %>%
  drop_na() %>%
  left_join(route_ranks, by = "route")

# recent_trends %>%
#   ggplot(aes(x = month_start_date, y = ridership_change, group = route, color = as.factor(rank))) +
#   geom_line(alpha = 0.25)

recent_trends %>%
  write_csv("data/ridership-trends.csv")
