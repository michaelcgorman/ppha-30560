
dates <- data_frame(dates = seq(as.Date("2016-04-11"), as.Date("2021-04-09"), by = "day"))

d <- read_csv("SP500.csv") %>%
  mutate(dates = DATE) %>%
  mutate(SP500 = as.numeric(SP500))
tail(d)

dates.f <- dates %>%
  left_join(d, by = "dates" )

dates.f %>%
  select(date = dates, SP500) %>%
  write_csv("../line-chart/sp500.csv")
