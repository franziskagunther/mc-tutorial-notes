## This script is supposed to go along with Chris' tutorial on monte carlo simulations and  likelihood. 

# Q: Why is it 10* √2π and not 4* √2π?

# Goal: Estimating integral 1 up to 3 of N ~ (1, 2) without calculus but with the Monte Carlo simulation method.

# How we will do it: Generate random samples from our normal distribution of which we want to know some integral
# (some probability), then count the numbers falling into a specific range (that defines the integral), divide them
# by the total number of samples we draw and there we have the integral.

n <- 100 # number of samples to take
sims <- rnorm(n, mean = 1, sd = 2) # simulated normally distributed numbers
mc_integral <- sum(sims >= 1 & sims <= 3)/n # find proportion of values between 1-3 

print(mc_integral) 

# pnorm gives the integral under the Normal distribution (in this case with mean 1 and standard deviation 2) from 
# negative infinity up to the value specified by q, it replaces the table of probabilites and Z-scores
mc_exact = pnorm(q=3, mean=1, sd=2) - pnorm(q=1, mean=1, sd=2) 
print(mc_exact)

# exercise

accuracy_n <- matrix( 
  integer(10),         # the data elements 
  nrow=5,              # number of rows 
  ncol=2,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

n_samples <- c(100, 1000, 10000, 100000, 1000000)

for (r in 1:nrow(accuracy_n)) {
  for (c in 1:ncol(accuracy_n)) { 
    if (c == 1) {
      accuracy_n[r,c] <- n_samples[r]
    } else if (c == 2) {
      sims <- rnorm(n_samples[r], mean = 1, sd = 2) 
      mc_integral <- sum(sims >= 1 & sims <= 3)/n_samples[r]
      accuracy_n[r,c] <- mc_integral
    }
  }
}

plot(accuracy_n[, 1], accuracy_n[, 2],
     xlab = "Number of Samples", ylab = "Accuracy",
     pch = 19, frame = FALSE)

# Could be improved by running the simulation of a sample of 100/1000/.. multiple times and 
# plotting their mean and standard deviation

# sample() is used to sample from a binomial (can be used with a vector of probabilities for each
# of the options), rnorm() is used to sample from a normal

# It turns out that we are getting the probabilities reasonably approximated for Binomial distributions
# well.

# exercise

runs <- 100 # number of simulations to run
flips_per_experiment <- c(10, 50, 100, 1000)
mc_exact <- pbinom(3, 10, 0.5, lower.tail=FALSE)

sample_sizes <- integer(length(flips_per_experiment)) 
accuracy <- integer(length(flips_per_experiment))

for (i in 1:length(flips_per_experiment)) {
  sample_sizes[i] <- flips_per_experiment[i]
  greater_than_three <- integer(runs)
  for ( j in 1:runs ) {
    coin_flips <- sample(c(0,1), sample_sizes[i], replace=T, prob=c(0.5, 0.5))
    compare_value <- 3/10 * sample_sizes[i]
    greater_than_three[j] <- ( sum(coin_flips) > compare_value )
  }
  accuracy[i] <- ( sum(greater_than_three)/runs ) - mc_exact
}

library(ggplot2)

df = data.frame(sample_sizes, accuracy)

ggplot(df, aes(x=sample_sizes, y=accuracy)) + 
  geom_line() +
  geom_point() +
  ylab("Estimate-Exact") +
  xlab("Run")

# I am unsure if this is correct. With increasing size of the single samples, I get an increasing 
# number of samples with a number of heads greater than 3/10 of their size, which is why accuracy 
# decreases! This can't be correct!