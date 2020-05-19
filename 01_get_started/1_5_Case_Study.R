
# 0 LOAD PACKAGES ---------------------------------------------------------

library(tidymodels)
library(readr)
library(vip)

# 01 GETTING DATA ---------------------------------------------------------

fileLink <- "https://tidymodels.org/start/case-study/hotels.csv"

hotels <- read_csv(fileLink) %>%
  mutate_if(is.character, as.factor)

hotels %>%
  count(children) %>%
  mutate(prob = n/sum(n))


# 02 DATA SPLITTING AND RESAMPLING ----------------------------------------

set.seed(123)

splits <- initial_split(hotels, strata = children)

hotel_other <- training(splits)

hotel_test <- testing(splits)

# Creation of validation test

val_set <- validation_split(hotel_other,
                            strata = children,
                            prop = 0.8)


# 03 1ST MODEL PENALIZED LOGISTIC REGRESSION ------------------------------

# Model specification

lr_mod <-
  logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") # Mixture = 1 ensures "lasso"

# Recipe

holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <-
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Create Workflow

lr_workflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(lr_recipe)

# Create grio for tunning

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

# Trainning and tunning the model

lr_res <-
  lr_workflow %>%
  tune_grid(val_set, 
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

# Select optimized parameter

lr_plot <- 
  lr_res %>%
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot

top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 

top_models

lr_best <- 
  lr_res %>%
  collect_metrics() %>%
  arrange(penalty) %>%
  slice(12)

lr_best

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)


# 04 2ND MODEL TREE -------------------------------------------------------

# Parallelization

cores <- parallel::detectCores()

rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_engine("ranger", num.threads = cores) %>%
  set_mode("classification")

# Recipie and Workflow

rf_recipe <-
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date) 

# Create Workflow

rf_workflow <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)

set.seed(345)

rf_res <-
  rf_workflow %>%
  tune_grid(val_set, 
            grid = 25,
            control = control_grid(save_pred = T),
            mestrics = metric_set(roc_auc))

rf_res %>%
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best

rf_res %>% 
  collect_predictions()

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Random Forest")

bind_rows(rf_auc, lr_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)

# # 05 LAST FIT -----------------------------------------------------------

# the last model

last_rf_mod <- 
  rand_forest(mtry = 8, min_n = 7, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# the last workflow

last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit

set.seed(345)

last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit %>% 
  collect_metrics()

last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)

last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(children, .pred_children) %>% 
  autoplot()