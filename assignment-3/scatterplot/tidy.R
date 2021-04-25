library("tidyverse")

d <- read_csv("../scatterplot/GLB.Ts+dSST.csv", skip = 1) %>%
  select(1:13) %>%
  gather("month", "value", 2:13) %>%
  mutate(month = case_when(
    month == "Jan" ~ "01",
    month == "Feb" ~ "02",
    month == "Mar" ~ "03",
    month == "Apr" ~ "04",
    month == "May" ~ "05",
    month == "Jun" ~ "06",
    month == "Jul" ~ "07",
    month == "Aug" ~ "08",
    month == "Sep" ~ "09",
    month == "Oct" ~ "10",
    month == "Nov" ~ "11",
    month == "Dec" ~ "12",
  ),
  date = as.Date(str_c(Year, month, "01", sep = "-"))) %>%
  select(date, value)

d %>%
  write_csv("../scatterplot/temp.csv")



