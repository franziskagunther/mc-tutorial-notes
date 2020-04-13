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
    
    mc_integral[rep] <- sum(sims >= 1 & sims <= 4)/n_samples[r] # compute proportion of samples in the specified range
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

# EX(revisited)

reps_b <- c(100, 1000, 10000, 100000) 
n_samples_b <- 10
mc_exact_b <- pbinom(3, 10, lower.tail=FALSE, prob=0.5)
accuracy <- integer(length(reps_b)) # vector to record accuracy values
accuracy_sd <- integer(length(reps_b)) # vector to record accuracy sd values

for (i in 1:length(reps_b)) {
  greater_than_three <- integer(reps_b[i])
  for ( j in 1:reps_b[i] ) {
    coin_flips <- sample(c(0,1), n_samples_b, replace=T)
    greater_than_three[j] <- ( sum(coin_flips) > 3 )
  }
  accuracy[i] <- mean(greater_than_three - mc_exact_b) 
  accuracy_sd[i] <- sd(greater_than_three - mc_exact_b) 
}

df = data.frame(reps_b, accuracy, accuracy_sd)

ggplot(df, aes(x=reps_b, y=accuracy)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=accuracy-accuracy_sd, ymax=accuracy+accuracy_sd), width=.2,
                position=position_dodge(0.05)) + 
  ylab("Estimate-Exact") +
  xlab("Run")

# EX(spinner modified with 5 sections and possibility to go bust)

play_game <- function(){
  results <- sample( c(-2, -1, 0, 1, 2), 20, replace=TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)) 
  print(length(results))
  count <- 0
  for (i in results) {
    print(i)
    if (i < 0) {
      count <- 0
    } else {
      count <- count + i
    }
  }
  return(sum(count)) # function returns the sum of all the spins 
}

return_expscore <- function(reps) {
  score_per_game = integer(reps)
  
  for ( it in 1:reps ) {
    score_per_game[it] <- play_game() # play the game by calling the function
  }
  
  expected_score <- mean(score_per_game)
  return(expected_score)
}

# Maximum Likelihood Estimators

# Goal: Finding the estimator with the greatest likelihood of describing the 
# population distribution that x have been drawn from

# Known: Data comes from a Normal distribution with unknown mean

# neglogLikelihood returns the negative log of the pdf of the Normal 
# distribution

neglogLikelihood <- function(mu, x) {
  logF = dnorm(x, mean = mu, sd = 1, log = TRUE)
  return(-sum(logF)) 
}

x = c(-0.5, 1.0, 0.2, -0.3, 0.5, 0.89, -0.11, -0.71, 1.0, -1.3, 0.84)
n = length(x)

mu_init = 1.0

out  = optim(mu_init, neglogLikelihood, gr = NULL, x, method = "L-BFGS-B", lower = -Inf, upper = Inf)

print(out$par)
print(mean(x))

mu <- seq(-0.1, 0.3, length = 101)

plot(mu, neglogL, pch="-")
points(out$par, out$value, col="red", pch=0)
neglogL <- apply( matrix(mu), 1, neglogLikelihood, x)

# The algorithm finds the same value that is found when visualising the 
# likelihood function with some values plugged in for parameter mu.

# EX
# Searched: p, probability of obtaining a head

neglogLikelihood <- function(probh, n, x) {
  logF = dbinom(x, n, prob = c(probh, 1-probh), log = TRUE)
  return(-sum(logF)) 
}

x = c(3, 2, 4, 5, 2)
n = 10

p_init = 0.5
out  = optim(p_init, neglogLikelihood, gr = NULL, x, n, method = "L-BFGS-B", lower = 0.001, upper = 1-0.001)

print(out$par)