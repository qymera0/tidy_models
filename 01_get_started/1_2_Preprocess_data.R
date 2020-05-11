library(tidymodels)
library(nycflights13)
library(skimr)
library(timeDate)

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

# 02 DATA SPLITING --------------------------------------------------------

set.seed(555)

data_split <- initial_split(flight_data, prop = 3/4) # possible to do stratification

train_data <- training(data_split)

test_data <- testing(data_split)

# 03 RECEPIES AND ROLES ---------------------------------------------------

flights_rec <-
  recipe(arr_delay ~., data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID")  # maintain the data but no use in the models 

summary(flights_rec)  

# Create features

flight_data %>%
  distinct(date) %>%
  mutate(numeric_date = as.numeric(date))

flights_rec <-
  recipe(arr_delay ~., data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID") %>%
  step_date(date, features = c("dow", "month")) %>%
  step_holiday(date, holidays = listHolidays("US")) %>%
  step_rm(date) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())

# 04 FIT MODEL WITH RECIPIE -----------------------------------------------

lr_mod <-
  logistic_reg() %>%
  set_engine("glm")

flights_wflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(flights_rec)

flights_wflow

flights_fit <-
  flights_wflow %>%
  fit(data = train_data)

flights_fit %>%
  pull_workflow_fit() %>%
  tidy()


# 05 PREDICT --------------------------------------------------------------

predict(flights_fit, test_data)

flights_pred <-
  predict(flights_fit, test_data, type = "prob") %>%
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))

flights_pred

# 06 ROC CURVE ------------------------------------------------------------

flights_pred %>%
  roc_curve(truth = arr_delay, .pred_late) %>%
  autoplot()

flights_pred %>% 
  roc_auc(truth = arr_delay, .pred_late)
