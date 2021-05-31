rm(list = ls())

library("tidyverse")
library("lubridate")

ridership_by_route <- read_csv("data/CTA_-_Ridership_-_Bus_Routes_-_Monthly_Day-Type_Averages___Totals.csv") %>%
  filter(route != "1001") %>%
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
         ridership_change = (ridership - rolling_avg_ridership) / rolling_avg_ridership)

route_ranks <- ridership_by_route %>%
  filter(year_num >= 2020) %>%
  group_by(month_start_date) %>%
  mutate(rank = ntile(ridership_change, 5)) %>%
  ungroup() %>%
  group_by(route) %>%
  summarize(rank = floor(median(rank))) %>%
  filter(!is.na(rank))

recent_trends <- ridership_by_route %>%
  left_join(route_ranks, by = "route") %>%
  ungroup() %>%
  filter(!is.na(rank),
         month_start_date %in% c(ymd("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"))) %>%
  rename(monthly_ridership = ridership,
         rank_group = rank) %>%
  mutate(quarter = case_when(month_start_date == ymd("2020-01-01") ~ "2020Q1",
                             month_start_date == ymd("2020-04-01") ~ "2020Q2",
                             month_start_date == ymd("2020-07-01") ~ "2020Q3",
                             month_start_date == ymd("2020-10-01") ~ "2020Q4",
                             month_start_date == ymd("2021-01-01") ~ "2021Q1"),
         current_ridership_rank = ntile(monthly_ridership, 5)) %>%
  select(route, quarter, rank_group, current_ridership_rank) %>%
  pivot_wider(names_from = quarter,
              values_from = c(rank_group, current_ridership_rank)) %>%
  drop_na()

# recent_trends %>%
#   ggplot(aes(x = month_start_date, y = ridership_change, group = route, color = as.factor(rank))) +
#   geom_line(alpha = 0.25)

recent_trends %>%
  write_csv("data/ridership-trends.csv")
