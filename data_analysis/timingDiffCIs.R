timing_diffCI <- function(result.df,task_num) {
  
  result_subset.df <- subset(result.df,task_index==task_num)
  result_subset.df <- result_subset.df[log(result_subset.df$completion_time) < mean(log(result_subset.df$completion_time)) + 3 * sd(log(result_subset.df$completion_time)),]
  result_subset.df$condition <- ordered(result_subset.df$condition, levels = c('animation','multiples'))
  
  ttest <- t.test(log(completion_time) ~ condition, data = result_subset.df)
  lowerBound_CI <- exp(ttest$conf.int[1]) %>% as.numeric()
  upperBound_CI <- exp(ttest$conf.int[2]) %>% as.numeric()
  
  mean_anim <- ttest$estimate[1] %>% as.numeric()
  mean_mult <- ttest$estimate[2] %>% as.numeric()
  
  mean <- exp(mean_anim - mean_mult)

  timing_diff.df <- data.frame(task_num,'Anim / Mult',mean,lowerBound_CI, upperBound_CI, exp(mean_anim), exp(mean_mult))
  colnames(timing_diff.df) <- c('task','condition','mean','lowerBound_CI','upperBound_CI','mean_anim','mean_mult')
  
  return(timing_diff.df)
  
}

#task 1
timing_diff_CIs.df <- timing_diffCI(trials_test.df,1)

#task 2
timing_diff_CIs.df <- rbind(timing_diff_CIs.df,timing_diffCI(trials_test.df,2))

#task 3
timing_diff_CIs.df <- rbind(timing_diff_CIs.df,timing_diffCI(trials_test.df,3))

#task 4
timing_diff_CIs.df <- rbind(timing_diff_CIs.df,timing_diffCI(trials_test.df,4))

#task 5
timing_diff_CIs.df <- rbind(timing_diff_CIs.df,timing_diffCI(trials_test.df,5))

#task 6
timing_diff_CIs.df <- rbind(timing_diff_CIs.df,timing_diffCI(trials_test.df,6))

#task 7
timing_diff_CIs.df <- rbind(timing_diff_CIs.df,timing_diffCI(trials_test.df,7))

#task 8
timing_diff_CIs.df <- rbind(timing_diff_CIs.df,timing_diffCI(trials_test.df,8))

#task 9
timing_diff_CIs.df <- rbind(timing_diff_CIs.df,timing_diffCI(trials_test.df,9))

timing_diff_CIs.df$task <- timing_diff_CIs.df$task %>% as.factor()
timing_diff_CIs.df$task <- revalue(timing_diff_CIs.df$task, c("1" = "Task 1",
                                                              "2" = "Task 2",
                                                              "3" = "Task 3",
                                                              "4" = "Task 4",
                                                              "5" = "Task 5",
                                                              "6" = "Task 6",
                                                              "7" = "Task 7",
                                                              "8" = "Task 8",
                                                              "9" = "Task 9"
))

plot_timing_diff_CI <- dualChart(timing_diff_CIs.df,ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = 1,percentScale=F)

ggsave(plot = plot_timing_diff_CI, filename = "plot_timing_diff_CI.pdf", device="pdf", width = 3.75, height = 1.75, units = "in", dpi = 300)
ggsave(plot = plot_timing_diff_CI, filename = "plot_timing_diff_CI.png", device="png", width = 3.75, height = 1.75, units = "in", dpi = 300)

timing_diff_CIs.df$condition <- revalue(timing_diff_CIs.df$condition, c("Anim / Mult" = "A/M"
))

plot_timing_diff_CI_axis <- gridChart(timing_diff_CIs.df,'Task 1',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_axis, filename = "plot_timing_diff_CI_axis.png", device="png", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_1 <- gridChart(timing_diff_CIs.df,'Task 1',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_1, filename = "plot_timing_diff_CI_1.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_2 <- gridChart(timing_diff_CIs.df,'Task 2',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_2, filename = "plot_timing_diff_CI_2.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_3 <- gridChart(timing_diff_CIs.df,'Task 3',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_3, filename = "plot_timing_diff_CI_3.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_4 <- gridChart(timing_diff_CIs.df,'Task 4',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_4, filename = "plot_timing_diff_CI_4.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_5 <- gridChart(timing_diff_CIs.df,'Task 5',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_5, filename = "plot_timing_diff_CI_5.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_6 <- gridChart(timing_diff_CIs.df,'Task 6',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_6, filename = "plot_timing_diff_CI_6.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_7 <- gridChart(timing_diff_CIs.df,'Task 7',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_7, filename = "plot_timing_diff_CI_7.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_8 <- gridChart(timing_diff_CIs.df,'Task 8',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_8, filename = "plot_timing_diff_CI_8.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_diff_CI_9 <- gridChart(timing_diff_CIs.df,'Task 9',ymin = 0.7,ymax = 2.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 1,percentScale=F)
ggsave(plot = plot_timing_diff_CI_9, filename = "plot_timing_diff_CI_9.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

