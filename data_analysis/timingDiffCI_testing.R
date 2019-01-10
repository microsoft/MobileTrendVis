library(truncnorm)

# Set to true if you only want to test coverage under the null hypothesis
same_mu <- F


# Set to true if your CI method assumes homogeneity of variance
same_sigma <- F


# Set to true if your CI method assumes equal sample sizes
same_n <- T


# Minimum sample size above which your CI method is assumed to be reasonably accurate
min_N <- 6


# Number of different experiments to test
n_exp = 20


# Number of replications for each experiment (the more the more precise the coverage
# value but if you use a slow CI function, you may need to lower this value)
n_replications = 500


# Build a list of experiments, each defined by two populations with randomly
# chosen mu, sigma, and sample size
mu1 <- runif(n_exp, min = 15, max = 30)
if (same_mu) {mu2 <- mu1} else {mu2 <- runif(n_exp, min = 15, max = 30)}
sigma1 <- runif(n_exp, min = 1, max = 10)
if (same_sigma) {sigma2 <- sigma1} else {sigma2 <- runif(n_exp, min = 1, max = 10)}
n1 <- floor(exp(runif(n_exp, min = log(min_N), max = log(500))))
if (same_n) {n2 <- n1} else {n2 <- floor(exp(runif(n_exp, min = log(min_N), max = log(500))))}


# Go through each experiment and look at how often the CI captures the true mean (=coverage)
# If the coverage is consistently close to your nominal coverage (e.g., 95%) then your
# function should be working. Coverage won't always be exactly 95%, especially if
# n_replications is low.
for (e in 1:n_exp) {
  n_captures <- 0
  message("n = ", n1[e])
  for (r in 1: n_replications) {
    sample1.df <- data.frame(rlnorm(n = n1[e], mean = log(mu1[e]), sd= log(sigma1[e])))
    sample1.df$condition <- 'animation'
    sample1.df$task_index <- 0
    colnames(sample1.df) <- c('completion_time','condition','task_index')
    sample1.df$completion_time <- sample1.df$completion_time + 1
    sample2.df <- data.frame(rlnorm(n= n2[e], mean = log(mu2[e]), sd = log(sigma2[e])))
    sample2.df$condition <- 'multiples'
    sample2.df$task_index <- 0
    colnames(sample2.df) <- c('completion_time','condition','task_index')
    sample2.df$completion_time <- sample2.df$completion_time + 1
    sample.df <- rbind(sample1.df,sample2.df)
    # message("min: ", min(sample.df$completion_time))
    ci <- timing_diffCI(sample.df, 0)
    if (mu1[e] > mu2[e]) {true_diff <- exp(log(mu1[e]) - log(mu2[e]))} else {true_diff <- exp(log(mu2[e]) - log(mu1[e]))}
    # message("CI1 = ", ci$lowerBound_CI, ", mu1 = ", mu1[e], ", mu2 = ", mu2[e], ", true_diff = ", true_diff, ", CI2 = ", ci$upperBound_CI)
    if (true_diff >= ci$lowerBound_CI && true_diff <= ci$upperBound_CI) {
      n_captures <- n_captures + 1;
    }
  }
  capture_rate <- 100*n_captures/n_replications
  message("Coverage: ", round(capture_rate), "%")
}
