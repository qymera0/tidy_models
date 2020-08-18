
# INTRODUCTION ------------------------------------------------------------

library(tidymodels)

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

boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)

boots

# helper funtion to fit nls to all datasets

fit_nls_on_bootstrap <- function(spli){
  
  nls(mpg ~ k / wt + b, analysis(split), start = list(k = 1, b = 0))
  
}

boot_models <-
  