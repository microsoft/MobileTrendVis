library(truncnorm)


# Set to true if your CI method assumes homogeneity of variance
same_sigma <- F


# Set to true if your CI method assumes equal sample sizes
same_n <- T


# Minimum sample size above which your CI method is assumed to be reasonably accurate
min_N <- 10


# Number of different experiments to test
n_exp = 20


# Number of replications for each experiment (the more the more precise the coverage
# value but if you use a slow CI function, you may need to lower this value)
n_replications = 500


# Build a list of experiments, each defined by two populations with randomly
# prob and sample size
sigma1 <- runif(n_exp, min = 1, max = 10)
if (same_sigma) {sigma2 <- sigma1} else {sigma2 <- runif(n_exp, min = 1, max = 10)}
sigma1 <- sigma1 / 10
sigma2 <- sigma2 / 10
n1 <- floor(exp(runif(n_exp, min = log(min_N), max = log(100))))
if (same_n) {n2 <- n1} else {n2 <- floor(exp(runif(n_exp, min = log(min_N), max = log(100))))}


# Go through each experiment and look at how often the CI captures the true mean (=coverage)
# If the coverage is consistently close to your nominal coverage (e.g., 95%) then your
# function should be working. Coverage won't always be exactly 95%, especially if
# n_replications is low.
for (e in 1:n_exp) {
  n_captures <- 0
  message("n = ", n1[e])
  for (r in 1: n_replications) {
    sample1.df <- data.frame(rbinom(n = n1[e], size = 1, prob = sigma1[e]))
    sample1.df$condition <- 'animation'
    sample1.df$task_index <- 0
    colnames(sample1.df) <- c('accuracy','condition','task_index')
    sample2.df <- data.frame(rbinom(n= n2[e], size = 1, prob = sigma2[e]))
    sample2.df$condition <- 'multiples'
    sample2.df$task_index <- 0
    colnames(sample2.df) <- c('accuracy','condition','task_index')
    prob_1 <- sigma1[e]
    prob_2 <- sigma2[e]
    sample.df <- rbind(sample1.df,sample2.df)
    ci <- propDiffCI(sample.df, 0)
    if (prob_1 / prob_2) {true_diff <- prob_1 - prob_2} else {true_diff <- prob_2 - prob_1}
    # message("CI1 = ", ci$lowerBound_CI, ", prob_1 = ", prob_1, ", prob_2 = ", prob_2, ", true_diff = ", true_diff, ", CI2 = ", ci$upperBound_CI)
    if (true_diff >= ci$lowerBound_CI && true_diff <= ci$upperBound_CI) {
      n_captures <- n_captures + 1;
    }
  }
  capture_rate <- 100*n_captures/n_replications
  message("Coverage: ", round(capture_rate), "%")
}
