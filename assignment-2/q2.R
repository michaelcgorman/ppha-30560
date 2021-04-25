library("tidyverse")
library("lubridate")
library("shadowtext")

rm(list = ls())
setwd("~/Documents/Classes/PPHA 30560/HW2/")

data <- read_csv("time_series_covid19_confirmed_global.csv") %>%
  filter(`Country/Region` %in% c("US",
                                 "United Kingdom",
                                 "India",
                                 "Brazil",
                                 "Australia",
                                 "Sweden",
                                 "Belgium",
                                 "Mexico")) %>%
  filter(!(!is.na(`Province/State`) & `Country/Region` == "United Kingdom")) %>% # Drop UK territories
  select(-`Province/State`, -Lat, -Long) %>%
  group_by(`Country/Region`) %>%
  summarise(across(where(is.numeric), sum)) %>% # Collapse Australian provinces into one national row
  mutate(`Country/Region` = if_else(`Country/Region` == "US", "United States", `Country/Region`)) %>%
  rename(country = `Country/Region`) %>%
  pivot_longer(-country,
               names_to = "report_date",
               values_to = "cases") %>%
  mutate(report_date = mdy(report_date))

hundredth_case <- data %>%
  filter(cases >= 100) %>%
  group_by(country) %>%
  summarise(hundredth_case = min(report_date))

data <- data %>%
  left_join(hundredth_case, by = "country") %>%
  mutate(days_since_hundredth_case = as.numeric(report_date - hundredth_case)) %>%
  filter(days_since_hundredth_case >= 0)

daily_33_percent_increase <- tibble(
  days_since_hundredth_case = 0:45,
  cases = 100 * (1.33^days_since_hundredth_case),
  is_final_point = days_since_hundredth_case == max(days_since_hundredth_case),
  label = if_else(is_final_point, "33% DAILY INCREASE", NA_character_)
)

max_date <- ymd("2020-03-01")

while(max_date < max(data$report_date + weeks(1))) {
  filename <- paste0("q2_charts/", strptime(max_date, "%Y-%Om-%d"), ".pdf")
  
  data %>%
    filter(report_date < max_date) %>%
    group_by(country) %>%
    mutate(is_final_point = days_since_hundredth_case == max(days_since_hundredth_case),
           label = if_else(is_final_point, country, NA_character_)) %>%
    ungroup() %>%
    ggplot(aes(x = days_since_hundredth_case, y = cases, color = country)) +
    geom_line(data = daily_33_percent_increase, color = "#999999", linetype = "dashed") +
    geom_text(data = daily_33_percent_increase, aes(label = label), color = "#999999", hjust = 0, size = 2.5) +
    geom_line() +
    geom_point(shape = 20, show.legend = FALSE) +
    geom_shadowtext(aes(label = label), hjust = 0, nudge_x = 0.75, show.legend = FALSE, bg.color = "white", alpha = 0.75) +
    scale_y_log10(labels = scales::comma) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.margin = unit(c(1,4.5,1,1), "lines"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(title = "Cumulative number of cases, by number of days since 100th case",
         x = "Number of days since 100th case",
         y = "")
  
  ggsave(filename)
  max_date <- max_date + weeks(1)
}

