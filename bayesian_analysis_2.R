# # Number of samples
# n = 10
# set.seed(4)
# 
# # Whether temperature is > 10 degrees
# predatorObserved <- sample(c(0,1), size = n, replace = TRUE)
# 
# # Temperature
# temperature <- rgamma(n,10)
# 
# # Generate the observations
# y_sim <- rpois(n = n, lambda = exp(2 +  0.2 * temperature - 0.5 * (predatorObserved == 1)))
# hist(y_sim)

# Bayesian analysis example
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(shinystan)

observations <- read.csv("c:/users/derek/work/teaching/biol 4062/example data/species_richness_observations.csv")

# Run the GLM
glm1 <- glm(numSpecies ~ temperature + predatorObserved, data = observations, family = poisson())
summary(glm1)

# Run the GLM with Stan
stan_glm1 <- stan_glm(numSpecies ~ temperature + predatorObserved, data = observations, family = poisson,
                      prior = normal(location = c(10,2.5), scale = c(2,2)),
                      prior_intercept = normal(0,5),
                      seed = 12345)

prior_summary(stan_glm1)
summary(stan_glm1)

# Compare the results
round(rbind(glm = coef(glm1), stan_glm = coef(stan_glm1)), digits = 2)

# View the analytics in ShinyStan
launch_shinystan(stan_glm1)

# Plot the posterior with bayesplot and ggplot2
posterior <- as.matrix(stan_glm1)
plot_title <- ggtitle("Posterior distributions with medians and 99% credible intervals")

mcmc_areas(posterior, pars = c("(Intercept)", "temperature", "predatorObserved"),
           prob = 0.99) + plot_title











