
# CORRELATION ANALYSIS ----------------------------------------------------

library(tidymodels)

data("Orange")

Orange <- as_tibble(Orange)

Orange

cor(Orange$age, Orange$circumference)

ggplot(Orange, aes(age, circumference, color = Tree)) +
  geom_line()

# Test correlation individually within trees

Orange %>% 
  group_by(Tree) %>% 
  summarise(correlation = cor(age, circumference))

# Hipotesis teste on correlation

ct <- cor.test(Orange$age, Orange$circumference)

ct

# organize all values of the test in a tibble

tidy(ct)

# Nest - map - unest models

nested <-
  Orange %>% 
  nest(data = c(age, circumference))

nested %>% 
  mutate(test = map(data, ~ cor.test(.x$age, .x$circumference)))

# tidy all objects

nested %>% 
  mutate(
    test = map(data, ~cor.test(.x$age, .x$circumference)),
    tidied = map(test, tidy)
  )

# Unnest results from the tests

Orange %>% 
  nest(data = c(age, circumference)) %>% 
  mutate(
    test = map(data, ~cor.test(.x$age, .x$circumference)),
    tidied = map(test, tidy)
  ) %>% 
  unnest(cols = tidied) %>% 
  select(-data, -test)


# REGRESSION MODELS -------------------------------------------------------


lm_fit <- lm(age ~ circumference, data = Orange)

summary(lm_fit)

tidy(lm_fit)

# Multiple regressions at one

Orange %>% 
  nest(data = c(-Tree)) %>% 
  mutate(
    fit = map(data, ~lm(age ~ circumference, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  select(-data, -fit)

# Example on mtcars

data("mtcars")

mtcars <- as_tibble(mtcars)

mtcars

mtcars %>%
  nest(data = c(-am)) %>% 
  mutate(
    fit = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)),  # S3 list-col
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  select(-data, -fit)

regressions <-
  mtcars %>%
  nest(data = c(-am)) %>% 
  mutate(
    fit = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)),  # S3 list-col
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

# Model fit

regressions %>% 
  select(tidied) %>% 
  unnest(tidied)

# Model qualit of fit

regressions %>% 
  select(glanced) %>% 
  unnest(glanced)

# All info

regressions %>% 
  select(augmented) %>% 
  unnest(augmented)
