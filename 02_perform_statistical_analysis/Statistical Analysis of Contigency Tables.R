library(tidymodels)

data(ad_data, package = "modeldata")

ad_data %>% 
  select(Genotype, Class)


# TEST OF INDEPENDENCE ----------------------------------------------------

observed_indep_statistic <-
  ad_data %>% 
  specify(Genotype ~ Class) %>% 
  calculate(stat = "Chisq")

# generate null distribution using randomization

null_distribution_simlated <-
  ad_data %>%
  specify(Genotype ~ Class) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 5000, type = "permute") %>%
  calculate(stat = "Chisq")


# generate null distribution using theory

null_distribution_theoretical <-
  ad_data %>%
  specify(Genotype ~ Class) %>% 
  hypothesize(null = "independence") %>% 
  calculate(stat = "Chisq")

null_distribution_simlated %>% 
  visualise() + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")

ad_data %>%
  specify(Genotype ~ Class) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")

# visualize both null distributions and the test statistic!
null_distribution_simlated %>%
  visualize(method = "both") + 
  shade_p_value(observed_indep_statistic,
                direction = "greater")

# calculate the p value from the observed statistic and null distribution
p_value_independence <- null_distribution_simlated %>%
  get_p_value(obs_stat = observed_indep_statistic,
              direction = "greater")

p_value_independence


# GOODNESS OF FIT TEST ----------------------------------------------------

meta_rates <- c("E2E2" = 0.71, "E2E3" = 11.4, "E2E4" = 2.32,
                "E3E3" = 61.0, "E3E4" = 22.6, "E4E4" = 2.22)

meta_rates <- meta_rates/sum(meta_rates)

obs_rates <- table(ad_data$Genotype)/nrow(ad_data)

round(cbind(obs_rates, meta_rates) * 100, 2)

# Calculate the null distribution

observed_gof_statistic <-
  ad_data %>% 
  specify(response = Genotype) %>% 
  hypothesise(null = "point", p = meta_rates) %>% 
  calculate(stat = "Chisq")

null_distribution_gof <-
  ad_data %>% 
  specify(response = Genotype) %>% 
  hypothesise(null = "point", p = meta_rates) %>% 
  generate(reps = 5000, type = "simulate") %>% 
  calculate(stat = "Chisq")

null_distribution_gof %>%
  visualize() + 
  shade_p_value(observed_gof_statistic,
                direction = "greater")

p_value_gof <- null_distribution_gof %>%
  get_p_value(observed_gof_statistic,
              direction = "greater")

p_value_gof
