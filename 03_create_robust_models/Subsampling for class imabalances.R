
# SIMULATED DATA ----------------------------------------------------------

imbal_data <-
  readr::read_csv("https://bit.ly/imbal_data") %>% 
  mutate(Class = factor(Class))

dim(imbal_data)

table(imbal_data$Class)


# SUBSAMPLING DATA --------------------------------------------------------

library(tidymodels)

library(themis)

imbal_rec <-
  recipe(Class ~., data = imbal_data) %>% 
  step_rose(class)
