library(tidymodels)
library(modeldata)


# 01 GET DATA -------------------------------------------------------------

data(cells, package = "modeldata")


# 02 SPLIT DATA -----------------------------------------------------------

cells %>%
  count(class) %>%
  mutate(prop = n/sum(n)) # Unbalanced class

cell_split <- initial_split(cells %>% select(-case), 
                            strata = class) #Stratified Split

cell_train <- training(cell_split)

cell_test <- testing(cell_split)

cell_train %>%
  count(class) %>%
  mutate(prop = n/sum(n))

cell_test %>%
  count(class) %>%
  mutate(prop = n/sum(n))

# 03 MODELING -------------------------------------------------------------

# Create an object that has thas the model characteristics

rf_mod <-
  rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

set.seed(234)

rf_fit <-
  rf_mod %>%
  fit(class ~., data = cell_train)

rf_fit

# 04 MODEL PERFORMANCE ----------------------------------------------------

rf_training_pred <-
  predict(rf_fit, cell_train) %>%
  bind_cols(predict(rf_fit, cell_train, type = "prob")) %>%
  bind_cols(cell_train %>% select(class))

rf_training_pred %>%
  roc_auc(truth = class, .pred_PS)

rf_training_pred %>%
  accuracy(truth = class, .pred_class)

rf_testing_pred <-
  predict(rf_fit, cell_test) %>%
  bind_cols(predict(rf_fit, cell_test, type = "prob")) %>%
  bind_cols(cell_test %>% select(class))

rf_testing_pred %>%
  roc_auc(truth = class, .pred_PS)

rf_testing_pred %>%
  accuracy(truth = class, .pred_class)

# 05 RESAMPLING RESCUE ----------------------------------------------------

set.seed(345)

folds <- vfold_cv(cell_train, v = 10)

folds

rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(class ~ .)

set.seed(456)

rf_fit_rs <- 
  rf_wf %>%
  fit_resamples(folds)

collect_metrics(rf_fit_rs)

# 06 TEST FINAL MODEL -----------------------------------------------------

rf_testing_pred %>%                   
  roc_auc(truth = class, .pred_PS)

rf_testing_pred %>%                   
  accuracy(truth = class, .pred_class)
