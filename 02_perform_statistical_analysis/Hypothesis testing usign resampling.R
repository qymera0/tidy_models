library(tidymodels)

data(gss)

dplyr::glimpse(gss)


# SPECIFY VARIABLES -------------------------------------------------------

gss %>% 
  specify(response = age) %>% 
  class()

gss %>% 
  specify(age ~ partyid)

gss %>% 
  specify(response = age, explanatory = partyid)

# For proportion testing

gss %>% 
  specify(response = college, success = "degree")


# DECLARE THE HYPOTESIS ---------------------------------------------------

gss %>% 
  specify(college ~ partyid, success = "degree") %>% 
  hypothesise(null = "independence")


# Point estimation for null hypothesis

gss %>% 
  specify(response = hours) %>% 
  hypothesise(null = "point", mu = 40)


# GENERATE THE DISTRIBUTION -----------------------------------------------

gss %>% 
  specify(response = hours) %>% 
  hypothesise(null = "point", mu = 40) %>% 
  generate(reps = 5000, type = "bootstrap")

gss %>% 
  specify(partyid ~ age) %>% 
  hypothesise(null = "independence") %>% 
  generate(reps = 5000, type = "permute")

# CALCULATE STATISTICS ----------------------------------------------------

gss %>% 
  specify(response = hours) %>% 
  hypothesise(null = "point", mu = 40) %>% 
  generate(reps = 5000, type = "bootstrap") %>% 
  calculate(stat = "mean")

gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate("diff in means", order = c("degree", "no degree"))

# OTHER UTILITIES ---------------------------------------------------------

# find the point estimate

point_estimate <-
  gss %>% 
  specify(response = hours) %>% 
  calculate(stat = "mean")

null_dist <-
  gss %>% 
  specify(response = hours) %>% 
  hypothesise(null = "point", mu = 40) %>% 
  generate(reps = 5000, type = "bootstrap") %>% 
  calculate(stat = "mean")

null_dist %>% 
  visualise()

null_dist %>% 
  visualise() +
  shade_p_value(obs_stat = point_estimate, direction = "two_sided")

p_value <-
  null_dist %>% 
  get_p_value(obs_stat = point_estimate, direction = "two_sided")

p_value


# THEORETICAL METHODS -----------------------------------------------------

# Skip the generation part

null_f_distn <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate(stat = "F")

null_f_distn_theoretical <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "F")

F_hat <-
  gss %>% 
  specify(age ~ partyid) %>% 
  calculate(stat = "F")

visualize(null_f_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

visualize(null_f_distn, method = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")
