library(tidymodels)

data(ames)

set.seed(4595)

data_split <- initial_split(ames, strata = "Sale_Price", p = 0.75)

ames_train <- training(data_split)

ames_test <- testing(data_split)


# RANDOM FOREST -----------------------------------------------------------

rf_defaults <- rand_forest(mode = "regression")

rf_defaults

# Formula interface

preds <- c("Longitude", "Latitude", "Lot_Area", "Neighborhood", "Year_Sold") # use only some predictors from whole data setr

rf_xy_fit <-
  rf_defaults %>% 
  set_engine("ranger") %>% 
  fit_xy(
    x = ames_train[ ,preds],
    y = log10(ames_train$Sale_Price)
  )

rf_xy_fit

test_results <-
  ames_test %>% 
  select(Sale_Price) %>% 
  mutate(Sale_Price = log10(Sale_Price)) %>% 
  bind_cols(
    predict(rf_xy_fit, new_data = ames_test[ ,preds])
  )

test_results %>% slice(1:5)

test_results %>% metrics(truth = Sale_Price, estimate = .pred)

# Formula method with parameters

rand_forest(mode = "regression", mtry = 3, trees = 1000) %>% 
  set_engine("ranger") %>% 
  fit(
    log10(Sale_Price) ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
    data = ames_train
  )

rand_forest(mode = "regression", mtry = 3, trees = 1000) %>%
  set_engine("randomForest") %>%
  fit(
    log10(Sale_Price) ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
    data = ames_train
  )

rand_forest(mode = "regression", mtry = .preds(), trees = 1000) %>%
  set_engine("ranger") %>%
  fit(
    log10(Sale_Price) ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
    data = ames_train
  )

# REGULARIZED REGRESSIONS -------------------------------------------------

# Recipe

norm_recipe <-
  recipe(
    Sale_Price ~ Latitude + Lot_Area + Neighborhood + Year_Sold, 
    data = ames_train
  ) %>% 
  step_other(Neighborhood) %>% 
  step_dummy(all_nominal()) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) %>% 
  step_log(Sale_Price, base = 10) %>% 
  # estimate means and standard deviations
  prep(training = ames_train, retain = TRUE)

# Spefify model

glmn_fit <-
  linear_reg(penalty = .001, mixture = 0.5) %>% 
  set_engine("glmnet") %>% 
  fit(Sale_Price ~., data = juice(norm_recipe))

glmn_fit

test_normalized <- bake(norm_recipe, new_data = ames_test, all_predictors())

test_results <-
  test_results %>% 
  rename('random forest' = .pred) %>% 
  bind_cols(
    predict(glmn_fit, new_data = test_normalized) %>% 
      rename(glmnet = .pred)
  )

test_results

test_results %>% metrics(truth = Sale_Price, estimate = glmnet)

test_results %>% 
  gather(model, prediction, -Sale_Price) %>% 
  ggplot(aes(x = prediction, y = Sale_Price)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  facet_wrap(~model) + 
  coord_fixed()
