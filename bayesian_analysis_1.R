# Simple MCMC example
# Example of an MCMC sampling approach


# EXAMPLE 1: Artificial example, one 'data point'
#-------------------------------------------------

# Number of samples to take
num_samples = 5000
MCMC_samples = vector(length = num_samples)
start_point = 0
MCMC_samples[1] = start_point

# True mean that we are aiming to find
true_mean = 3
true_sd = 4


for (ii in 2:num_samples)
{
  # Propose a new sample by adding a random value to the existing sample
  mu_proposal = MCMC_samples[ii-1] + rnorm(1,0,1)

  # Keep the sample if the ratios of the probability density functions are > a random number
  # between zero and one. Note that this isn't quite how it would be done - you would be
  # estimating the log likelihood of data with an unknown mean (and possibly sd), with the proposal
  # mean (and sd) replacing the true mean and sd, but this is just for the purposes of 
  # demonstration
  llik_current = dnorm(MCMC_samples[ii-1], true_mean, true_sd)
  llik_proposal = dnorm(mu_proposal, true_mean, true_sd)
  if ((llik_proposal / llik_current) > runif(1))
  {
    MCMC_samples[ii] = mu_proposal
  }  else
    MCMC_samples[ii] = MCMC_samples[ii - 1]
}



# Plot the chain
plot(MCMC_samples, type = "l", xlab = "Step", ylab = "Location")

# Show where it spends the most time sampling
hist(MCMC_samples, breaks = seq(-25.5,25.5))
plot(density(MCMC_samples), xlab = "Parameter estimate", main = "", ylim = c(0,0.2))
lines(seq(-25.5,25.5, 0.01), dnorm(seq(-25.5,25.5, 0.01), true_mean, true_sd), lty = 2)
print(mean(MCMC_samples))








# EXAMPLE 2: Artificial example, multiple 'data points'
#-------------------------------------------------

# Number of samples to take
num_samples = 50
MCMC_samples_mean = vector(length = num_samples)
MCMC_samples_sd = vector(length = num_samples)
MCMC_samples_mean[1] = 0
MCMC_samples_sd[1] = 1

# True mean that we are aiming to find
true_mean = 3
true_sd = 4

data = rnorm(500, true_mean, true_sd)


for (ii in 2:num_samples)
{
  #print("ii")
  #print(ii)
  # Propose a new sample by adding a random value to the existing sample
  mu_proposal = MCMC_samples_mean[ii-1] + rnorm(1,0,1)
  sd_proposal = exp(log(MCMC_samples_sd[ii-1]) + rnorm(1,0,1))
  
  #print(mu_proposal)
  #print(sd_proposal)
  
  # Keep the sample if the ratios of the probability density functions are > a random number
  # between zero and one
  lik_current = sum(dnorm(data, MCMC_samples_mean[ii-1], MCMC_samples_sd[ii-1], log = T))
  lik_proposal = sum(dnorm(data, mu_proposal, sd_proposal, log = T))
  if (exp(lik_proposal - lik_current) > runif(1))
  {
    MCMC_samples_mean[ii] = mu_proposal
    MCMC_samples_sd[ii] = sd_proposal
  }  else
  {
    MCMC_samples_mean[ii] = MCMC_samples_mean[ii - 1]
    MCMC_samples_sd[ii] = MCMC_samples_sd[ii - 1]
  }
}



# Plot the chain
plot(MCMC_samples_mean, type = "l", xlab = "Step", ylab = "Location")
dev.new()
plot(MCMC_samples_sd, type = "l", xlab = "Step", ylab = "Location")

# Plot the posteriors
plot(density(MCMC_samples_mean), xlab = "Parameter estimate (mean)", main = "", xlim = c(2,6), ylim = c(0,3))
plot(density(MCMC_samples_sd), xlab = "Parameter estimate (sd)", main = "", xlim = c(2,6), ylim = c(0,4))

# Plot the 'true' distribution versus the estimate
plot(seq(-25.5,25.5, 0.01), dnorm(seq(-25.5,25.5, 0.01), true_mean, true_sd), lty = 2, type = "l")
lines(seq(-25.5,25.5, 0.01), dnorm(seq(-25.5,25.5, 0.01), median(MCMC_samples_mean), median(MCMC_samples_sd)), col = "red", xlab = "Value", ylab = "Probability density")