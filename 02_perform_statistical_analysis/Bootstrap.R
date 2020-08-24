
# INTRODUCTION ------------------------------------------------------------

library(tidymodels)
library(furrr)

ggplot(mtcars, aes(mpg, wt)) +
  geom_point()

# non-linear fit

nlsfit <- nls(mpg ~ k / wt + b, 
              mtcars,
              start = list(k = 1,
                           b = 0))

summary(nlsfit)

# plot the model

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() + 
  geom_line(aes(y = predict(nlsfit)))


# BOOTSTRAPING MODELS -----------------------------------------------------

set.seed(27)

# Make 2000 bootstraping data

boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)

boots

# helper funtion to fit nls to all datasets

fit_nls_on_bootstrap <- function(split){
  
  nls(mpg ~ k / wt + b, analysis(split), start = list(k = 1, b = 0))
  
}

plan(multiprocess)

boot_models <-
  boots %>% 
  mutate(model = future_map(splits, fit_nls_on_bootstrap),
         coef_info = map(model, tidy))

boot_coefs <-
  boot_models %>%
  unnest(coef_info)


# CONFIDENCE INTERVALS ----------------------------------------------------

percentile_intervals <- int_pctl(boot_models, coef_info)  

percentile_intervals

ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")

# POSSIBLE MODEL FITS -----------------------------------------------------

boot_aug <-
  boot_models %>% 
  sample_n(200) %>% 
  mutate(augmented = map(model, augment)) %>% 
  unnest(augmented)

ggplot(boot_aug, aes(wt, mpg)) +
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "blue") +
  geom_point()

fit_spline_on_bootstrap <- function(split){
  
  data <- analysis(split)
  
  smooth.spline(data$wt, data$mpg, df = 4)
  
}

boot_splines <-
  boots %>%
  sample_n(200) %>% 
  mutate(spline = map(splits, fit_spline_on_bootstrap), 
         aug_train = map(spline, augment))

splines_aug <- 
  boot_splines %>% 
  unnest(aug_train)

ggplot(splines_aug, aes(x, y)) +
  geom_line(aes(y = .fitted, group = id), alpha = 0.2, col = "blue") +
  geom_point()
