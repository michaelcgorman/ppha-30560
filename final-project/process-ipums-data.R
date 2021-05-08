rm(list = ls())

setwd("~/Documents/Classes/PPHA 30560/ppha-30560/final-project/")

library("tidyverse")
library("ipumsr")

ddi <- read_ipums_ddi("data/ahtus_00001.xml")
data <- read_ipums_micro(ddi)