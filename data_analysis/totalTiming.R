source('bootCIs.R')

timing_by_condition <- function(result.df) {
  
  result.df <- result.df[log(result.df$completion_time) < mean(log(result.df$completion_time)) + 3 * sd(log(result.df$completion_time)),]
  
  result.df$log_completion_time <- log(result.df$completion_time)
  
  #compute mean for each user_id and condition
  CT_aggregate.df <- aggregate(abs(result.df$log_completion_time), list(result.df$user_id,result.df$condition), mean)
  
  names(CT_aggregate.df) <- c("user_id","condition","log_completion_time")
  CT_aggregate.df$condition <- revalue(CT_aggregate.df$condition, c("animation" = "A",
                                                                    "multiples" = "M"
  ))
  CT_aggregate.df$condition <- ordered(CT_aggregate.df$condition, levels = c('M','A'))
  
  CTlog_multiples <- subset(CT_aggregate.df, condition=="M")$log_completion_time
  CTlog_animation <- subset(CT_aggregate.df, condition=="A")$log_completion_time
  
  n_CT_multiples <- length(CTlog_multiples)
  n_CT_animation <- length(CTlog_animation)
  
  ttest_CT_multiples <- t.test(CTlog_multiples, conf.level = 0.95)
  ttest_CT_animation <- t.test(CTlog_animation, conf.level = 0.95)
  
  mean_CT_multiples = mean(CTlog_multiples)
  mean_CT_animation = mean(CTlog_animation)
  
  cimin_CT_multiples = exp(ttest_CT_multiples$conf.int[1])
  cimin_CT_animation = exp(ttest_CT_animation$conf.int[1])
  
  mean_CT_multiples = exp(mean_CT_multiples)
  mean_CT_animation = exp(mean_CT_animation)
  
  cimax_CT_multiples = exp(ttest_CT_multiples$conf.int[2])
  cimax_CT_animation = exp(ttest_CT_animation$conf.int[2])
  
  CT_analysis = c()
  CT_analysis$ratio = c("M","A")
  CT_analysis$pointEstimate = c(mean_CT_multiples,mean_CT_animation)
  CT_analysis$ci.min = c(cimin_CT_multiples,cimin_CT_animation)
  CT_analysis$ci.max = c(cimax_CT_multiples,cimax_CT_animation)
  CT_analysis$n = c(n_CT_multiples,n_CT_animation)
  
  CT_analysis.df <- data.frame('Mean Completion Time (s)',factor(CT_analysis$ratio),CT_analysis$pointEstimate, CT_analysis$ci.min, CT_analysis$ci.max,CT_analysis$n)
  colnames(CT_analysis.df) <- c("task","condition", "mean", "lowerBound_CI", "upperBound_CI","n")
  
  return(CT_analysis.df)
}

mean_timing_CIs.df <- timing_by_condition(trials_test.df)
mean_timing_CIs.df$condition <- ordered(mean_timing_CIs.df$condition, levels = c('M','A'))

plot_mean_timing_CI <- dualChart(mean_timing_CIs.df,ymin = -1,ymax = 26,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = -30,percentScale=F)

ggsave(plot = plot_mean_timing_CI, filename = "plot_mean_timing_CI.pdf", device="pdf", width = 3.75, height = 0.35, units = "in", dpi = 300)
ggsave(plot = plot_mean_timing_CI, filename = "plot_mean_timing_CI.png", device="png", width = 3.75, height = 0.35, units = "in", dpi = 300)

timing_diffCI <- function(result.df) {
  
  result_subset.df <- result.df[log(result.df$completion_time) < mean(log(result.df$completion_time)) + 3 * sd(log(result.df$completion_time)),]
  result_subset.df$condition <- revalue(result_subset.df$condition, c("animation" = "A",
                                                                      "multiples" = "M"
  ))
  result_subset.df$condition <- ordered(result_subset.df$condition, levels = c('A','M'))
  
  ttest <- t.test(log(completion_time) ~ condition, data = result_subset.df)
  lowerBound_CI <- exp(ttest$conf.int[1]) %>% as.numeric()
  upperBound_CI <- exp(ttest$conf.int[2]) %>% as.numeric()
  
  mean_mult <- ttest$estimate[2] %>% as.numeric()
  mean_anim <- ttest$estimate[1] %>% as.numeric()
  
  mean <- exp(mean_anim - mean_mult)
  
  timing_diff.df <- data.frame('A/M','A/M',mean,lowerBound_CI, upperBound_CI, exp(mean_anim), exp(mean_mult))
  colnames(timing_diff.df) <- c('task','condition','mean','lowerBound_CI','upperBound_CI','mean_anim','mean_mult')
  
  return(timing_diff.df)
  
}

mean_timing_diff_CIs.df <- timing_diffCI(trials_test.df)

plot_mean_timing_diff_CI <- dualChart(mean_timing_diff_CIs.df,ymin = 0.95,ymax = 1.55,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = F,vLineVal = 1,percentScale=F)

ggsave(plot = plot_mean_timing_diff_CI, filename = "plot_mean_timing_diff_CI.pdf", device="pdf", width = 3.75, height = 0.5, units = "in", dpi = 300)
ggsave(plot = plot_mean_timing_diff_CI, filename = "plot_mean_timing_diff_CI.png", device="png", width = 3.75, height = 0.5, units = "in", dpi = 300)
