library(boot)
library(PropCIs)
library(Hmisc)

deterministic <- TRUE
replicates <- 10000
intervalMethod <- "bca"
significantDigits <- 3

samplemean <- function(x, d) {
  return(mean(x[d]))
}

# Returns the point estimate and confidence interval in an array of length 3
bootstrapCI <- function(statistic, datapoints,conf) {
  # Compute the point estimate
  pointEstimate <- statistic(datapoints)
  # Make the rest of the code deterministic
  if (deterministic) set.seed(0)
  # Generate bootstrap replicates
  b <- boot(datapoints, statistic, R = replicates, parallel="multicore")
  # Compute interval
  ci <- boot.ci(b, type = intervalMethod,conf=conf)
  # Return the point estimate and CI bounds
  # You can print the ci object for more info and debug
  lowerBound <- ci$bca[4]
  upperBound <- ci$bca[5]
  return(c(pointEstimate, lowerBound, upperBound))
}

# Returns the mean and its confidence interval in an array of length 3
bootstrapMeanCI <- function(datapoints,conf) {
  return(bootstrapCI(samplemean, datapoints,conf))
}

#boot dif
bootdif <- function(df) {
  m <- attr(smean.cl.boot(df[df$condition=='multiples',]$points, B=10000, reps=TRUE), 'reps')
  a <- attr(smean.cl.boot(df[df$condition=='animation',]$points, B=10000, reps=TRUE), 'reps')
  meandif <- diff(tapply(df$points, df$condition, mean, na.rm=TRUE)) %>% as.numeric() * -1 #bootstrapped mean diff
  
  mean_m <- mean(m, na.rm = T) %>% as.numeric()
  mean_a <- mean(a, na.rm = T) %>% as.numeric()
  
  m.a <- quantile(a-m, c(.05,.95)) #95% CI
  bm.a <- quantile(a-m, c((1-0.9875),0.9875)) #98.75% CI
  
  result.df <- data.frame('A-M',meandif, m.a[1], m.a[2], bm.a[1], bm.a[2], mean_m, mean_a)
  colnames(result.df) <- c('condition','mean','lowerBound_CI','upperBound_CI','lowerBound_BCI','upperBound_BCI','mean_m','mean_a')
  
  return(result.df)
}

bootdif_familiarity <- function(df) {
  a <- attr(smean.cl.boot(df[df$condition=='multiples',]$familiarity, B=10000, reps=TRUE), 'reps')
  b <- attr(smean.cl.boot(df[df$condition=='animation',]$familiarity, B=10000, reps=TRUE), 'reps')
  
  meandif <- diff(tapply(df$familiarity, df$condition, mean, na.rm=TRUE)) %>% as.numeric() #bootstrapped mean diff
  a.b <- quantile(b-a, c(.05,.95)) #95% CI
  
  mean_a <- mean(a, na.rm = T) %>% as.numeric()
  mean_b <- mean(b, na.rm = T) %>% as.numeric()
  
  result.df <- data.frame('A-M',meandif, a.b[1], a.b[2], mean_a, mean_b)
  colnames(result.df) <- c('condition','mean','lowerBound_CI','upperBound_CI','mean_a','mean_b')
  
  return(result.df)
}

bootdif_confidence <- function(df) {
  a <- attr(smean.cl.boot(df[df$condition=='multiples',]$confidence, B=10000, reps=TRUE), 'reps')
  b <- attr(smean.cl.boot(df[df$condition=='animation',]$confidence, B=10000, reps=TRUE), 'reps')
  
  meandif <- diff(tapply(df$confidence, df$condition, mean, na.rm=TRUE)) %>% as.numeric() #bootstrapped mean diff
  a.b <- quantile(b-a, c(.05,.95)) #95% CI
  
  mean_a <- mean(a, na.rm = T) %>% as.numeric()
  mean_b <- mean(b, na.rm = T) %>% as.numeric()
  
  result.df <- data.frame('A-M',meandif, a.b[1], a.b[2], mean_a, mean_b)
  colnames(result.df) <- c('condition','mean','lowerBound_CI','upperBound_CI','mean_a','mean_b')
  
  return(result.df)
}

bootdif_ease <- function(df) {
  a <- attr(smean.cl.boot(df[df$condition=='multiples',]$ease, B=10000, reps=TRUE), 'reps')
  b <- attr(smean.cl.boot(df[df$condition=='animation',]$ease, B=10000, reps=TRUE), 'reps')
  
  meandif <- diff(tapply(df$ease, df$condition, mean, na.rm=TRUE)) %>% as.numeric() #bootstrapped mean diff
  a.b <- quantile(b-a, c(.05,.95)) #95% CI
  
  mean_a <- mean(a, na.rm = T) %>% as.numeric()
  mean_b <- mean(b, na.rm = T) %>% as.numeric()
  
  result.df <- data.frame('A-M',meandif, a.b[1], a.b[2], mean_a, mean_b)
  colnames(result.df) <- c('condition','mean','lowerBound_CI','upperBound_CI','mean_a','mean_b')
  
  return(result.df)
}

bootdif_byOrdering <- function(df) {
  a <- attr(smean.cl.boot(df[df$ordering==0,]$points, B=10000, reps=TRUE), 'reps')
  b <- attr(smean.cl.boot(df[df$ordering==1,]$points, B=10000, reps=TRUE), 'reps')
  
  meandif <- diff(tapply(df$points, df$ordering, mean, na.rm=TRUE)) %>% as.numeric() #bootstrapped mean diff
  a.b <- quantile(b-a, c(.05,.95)) #95% CI
  
  mean_a <- mean(a, na.rm = T) %>% as.numeric()
  mean_b <- mean(b, na.rm = T) %>% as.numeric()
  
  result.df <- data.frame('diff',meandif, a.b[1], a.b[2], mean_a, mean_b)
  colnames(result.df) <- c('diff','mean_diff','lowerBound_CI','upperBound_CI','mean_a','mean_b')
  
  return(result.df)
}