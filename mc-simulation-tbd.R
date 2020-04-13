## This script is supposed to go along with a tutorial on monte carlo simulations and maximum likelihood estimators. 

# Q: Why is it 10 * √2π and not 4 * √2π?

# Goal: Estimating integral 1 up to 4 of Gamma ~ (1, 2) without calculus but with the Monte Carlo simulation method.

# How we will do it: Generate random samples from our gamma distribution of which we want to know some integral
# (some probability), then count the number of cases within a sample falling into the specified range 
# (that defines the integral) and divide them by the respective sample size. Compute the mean difference of the
# repeats from the previously computed accurate integral under the Gamma distribution (mc_exact) and its 
# standard deviation.

library(ggplot2)

# EX(modified)
reps <- 100 # number of repeats 
mc_exact = pgamma(q=4, shape=1, scale=2) - pgamma(q=1, shape=1, scale=2) 

accuracy_n <- matrix( 
  integer(10),         # the data elements 
  nrow=5,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

n_samples <- c(100, 1000, 10000, 100000, 1000000) # sample sizes

for (r in 1:nrow(accuracy_n)) {
  mc_integral <- integer(reps) # vector with 100 entries
  
  for (rep in 1:reps) {
    sims <- rgamma(n_samples[r], shape = 1, scale = 2) # simulate n_samples samples from a Gamma distribution 
                                                       # with above specified properties
    
    mc_integral[rep] <- sum(sims >= 1 & sims <= 3)/n_samples[r] # compute proportion of samples in the specified range
  }
  
  for (c in 1:ncol(accuracy_n)) { 
    if (c == 1) {
      # enter sample size into first column of result matrix
      accuracy_n[r,c] <- n_samples[r]
    } else if (c == 2) {
      # enter mean accuracy into second column of result matrix
      accuracy_n[r,c] <- mean(mc_integral - mc_exact)
    } else if (c == 3) {
      # enter sd accuracy into second column of result matrix
      accuracy_n[r,c] <- sd(mc_integral - mc_exact)
    }
  }
}

# make a dataframe out of the columns of the matrix for convenience
df = data.frame(ssize = accuracy_n[, 1], mean = accuracy_n[, 2], sd = accuracy_n[, 3]) 

ggplot(df, aes(x=ssize, y=mean)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05)) + 
  ylab("Estimate-Exact") +
  xlab("Run")

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