## libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

## load data
us_counties_2020 <- read_csv("hw-2/us-counties-2020.csv")
larimer_2020 <- us_counties_2020 |>
  filter(county == "Larimer" & state == "Colorado") |>
  mutate(new_cases = c(0, diff(cases)))

## look at cases
ggplot(larimer_2020) +
  geom_point(aes(date, new_cases))

## save
write_csv(larimer_2020, file = "hw-2/larimer-2020.csv")

