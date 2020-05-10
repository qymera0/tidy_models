library(tidymodels)
library(nycflights13)
library(skimr)


# 01 LOAD THE DATA --------------------------------------------------------

flight_data <- 
  flights %>%
  mutate(arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
         arr_delay = factor(arr_delay),
         date = as.Date(time_hour)) %>%
  inner_join(weather, by = c("origin", "time_hour")) %>%
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>%
  na.omit() %>%
  mutate_if(is.character, as.factor)

flight_data %>%
  count(arr_delay) %>%
  mutate(prop = n/sum(n))

glimpse(flight_data)

flight_data %>%
  skim(dest, carrier)

