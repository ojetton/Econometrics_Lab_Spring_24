# base R, random variables

rbinom(10, 100, 0.9)

rnorm(10, mean = 5, sd = 2)

runif(10, min = -100, max = 100)


# packages
library(pacman)
p_load(tidyverse, ggplot2)

ex_data_1 = tibble(
  variable = rnorm(1000, mean = 3, sd = 2) + runif(1000, min = -1, max = 1)
)

ggplot(ex_data_1) +
  geom_density(aes(variable), 
               fill = "blue", alpha = 0.5) +
  # true expected value
  geom_vline(xintercept = 3) +
  # data mean
  geom_vline(xintercept = mean(ex_data_1$variable),
             color = "red")


# SEEDS
set.seed(3551579)
rnorm(10)


# SIMULATIONS
data_gen = function(N, alpha, beta, delta){
  
  # generate data
  data = tibble(
    ID = 1:N,
    X = rnorm(N, mean = 10, sd = 2),
    Z = runif(N, min = -3, max = 3) - 0.2*X,
    e = rnorm(N),
    Y = alpha + beta*X + delta*Z + e
  )
  # return the data
  return(data)
}


# test data
test_data = data_gen(1000, alpha = 4, beta = 1/2, delta = 2)

tidy(feols(test_data, Y ~ X + Z)) %>% 
  filter(term == "X") %>%
  select(2)


# simulate regressions
p_load(fixest, broom)

# function
  # input: number of iterations
reg_sim = function(iter){
  
  data_i = data_gen(1000, alpha = 4, beta = 1/2, delta = 2)

  reg = feols(data_i, Y ~ X + Z)
  
  bind_rows(tidy(reg)) %>%
    filter(term == "X") %>%
    select(2)
}

# iteration number
iter = 100

# map function
results_1 = bind_rows(map(1:iter, reg_sim))

ggplot(results_1) +
  geom_density(aes(estimate))


# OVB simulation
reg_sim_2 = function(iter){
  
  data_i = data_gen(1000, alpha = 4, beta = 1/2, delta = 2)
  
  reg = feols(data_i, Y ~ X)
  
  bind_rows(tidy(reg)) %>%
    filter(term == "X") %>%
    select(2)
}

# Run each simulation 1000 times and graph
  # both densities on same graph

iter = 1000

results_full = bind_rows(map(1:iter, reg_sim))
results_ovb = bind_rows(map(1:iter, reg_sim_2))

ggplot() +
  geom_density(aes(results_full$estimate), fill = "blue") +
  geom_density(aes(results_ovb$estimate), fill = "red")


# t-statistic simulation
reg_sim_t = function(iter){
  
  data_i = data_gen(1000, alpha = 4, beta = 1/2, delta = 2)
  
  # regressions
  reg_1 = feols(data_i, Y ~ X + Z) # full regression 
  reg_2 = feols(data_i, Y ~ X)
  
  bind_rows(tidy(reg_1), tidy(reg_2)) %>%
    filter(term == "X") %>%
    # coefficient was in 2nd spot
    # t-statistic in 4th place
    select(4) %>%
    # label
    mutate(OVB = c("No", "Yes"))
}

results_t = bind_rows(map(1:iter, reg_sim_t))

ggplot(results_t) +
  geom_density(aes(statistic, fill = OVB))



