
# 0 MODEL PACKAGE ---------------------------------------------------------

library(tidymodels)

library(modeldata)

library(vip)

# 1 PREDICTING IMAGE SEGMENTATION -----------------------------------------

set.seed(123)

data(cells, package = "modeldata")

cell_split <- initial_split(cells %>% select(-case),
                            strata = class)

cell_train <- training(cell_split)

cell_test <- testing(cell_split)


# 02 TUNNING MODEL HYPERPARAMETERS ----------------------------------------

tune_spec <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tree_grid

set.seed(234)

cell_folds <- vfold_cv(cell_train)


# 03 MODEL FITTI WITH GRID ------------------------------------------------

set.seed(345)

tree_wf <-
  workflow() %>%
  add_model(tune_spec) %>%
  add_formula(class ~ .)

tree_res <- 
  tree_wf %>%
  tune_grid(
    resamples = cell_folds,
    grid = tree_grid
  )

tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

tree_res %>%
  show_best("roc_auc")

best_tree <- tree_res %>% select_best("roc_auc")

best_tree
  

# 04 FITTING FINAL MODEL --------------------------------------------------

final_wf <-
  tree_wf %>%
  finalize_workflow(best_tree)

final_wf
