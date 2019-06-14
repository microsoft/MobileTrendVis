timingCI <- function(result.df,task_num,exp_condition) {
  
  result_subset.df <- subset(result.df,task_index==task_num)
  result_subset.df <- result_subset.df[log(result_subset.df$completion_time) < mean(log(result_subset.df$completion_time)) + 3 * sd(log(result_subset.df$completion_time)) & result_subset.df$condition==exp_condition,]
  result_subset.df$log_response_time <- log(result_subset.df$completion_time)
  result_skewness <- skewness(result_subset.df$completion_time)
  result_log_skewness <- skewness(result_subset.df$log_response_time)
  
  ttest <- t.test(result_subset.df$log_response_time, conf.level = 0.95)
  bttest <- t.test(result_subset.df$log_response_time, conf.level = 0.994)
  
  lowerBound_CI <- exp(ttest$conf.int[1])
  upperBound_CI <- exp(ttest$conf.int[2])
  lowerBound_BCI <- exp(bttest$conf.int[1])
  upperBound_BCI <- exp(bttest$conf.int[2])
  mean_completion_time <- exp(mean(result_subset.df$log_response_time))
  
  timing.df <- data.frame(task_num,exp_condition,mean_completion_time,lowerBound_CI,upperBound_CI,lowerBound_BCI,upperBound_BCI,length(result_subset.df$completion_time),result_skewness,result_log_skewness)
  colnames(timing.df) <- c('task','condition','mean','lowerBound_CI','upperBound_CI','lowerBound_BCI','upperBound_BCI','n','skewness','log_skewness')
  
  return(timing.df)
  
}

#task 1
timing_CIs.df <- timingCI(trials_test.df,1,'animation')
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,1,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,1,'stepper'))

#task 2
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,2,'animation'))
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,2,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,2,'stepper'))

#task 3
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,3,'animation'))
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,3,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,3,'stepper'))

#task 4
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,4,'animation'))
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,4,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,4,'stepper'))

#task 5
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,5,'animation'))
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,5,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,5,'stepper'))

#task 6
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,6,'animation'))
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,6,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,6,'stepper'))

#task 7
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,7,'animation'))
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,7,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,7,'stepper'))

#task 8
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,8,'animation'))
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,8,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,8,'stepper'))

#task 9
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,9,'animation'))
timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,9,'multiples'))
# timing_CIs.df <- rbind(timing_CIs.df,timingCI(trials_test.df,9,'stepper'))

timing_CIs.df$condition <- revalue(timing_CIs.df$condition, c("animation" = "Animation",
                                                              "multiples" = "Multiples"
))

timing_CIs.df$task <- timing_CIs.df$task %>% as.factor()
timing_CIs.df$task <- revalue(timing_CIs.df$task, c("1" = "Task 1",
                                                    "2" = "Task 2",
                                                    "3" = "Task 3",
                                                    "4" = "Task 4",
                                                    "5" = "Task 5",
                                                    "6" = "Task 6",
                                                    "7" = "Task 7",
                                                    "8" = "Task 8",
                                                    "9" = "Task 9"
))

timing_CIs.df$condition <- ordered(timing_CIs.df$condition, levels = c('Multiples','Animation'))

plot_timing_CI <- dualChart(timing_CIs.df,ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = -30,percentScale=F)

ggsave(plot = plot_timing_CI, filename = "plot_timing_CI.pdf", device="pdf", width = 3.75, height = 2.25, units = "in", dpi = 300)
ggsave(plot = plot_timing_CI, filename = "plot_timing_CI.png", device="png", width = 3.75, height = 2.25, units = "in", dpi = 300)

timing_CIs.df$condition <- revalue(timing_CIs.df$condition, c("Animation" = "A",
                                                              "Multiples" = "M"
))

plot_timing_CI_axis <- gridChart(timing_CIs.df,'Task 1',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_axis, filename = "plot_timing_CI_axis.png", device="png", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_1 <- gridChart(timing_CIs.df,'Task 1',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_1, filename = "plot_timing_CI_1.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_2 <- gridChart(timing_CIs.df,'Task 2',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_2, filename = "plot_timing_CI_2.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_3 <- gridChart(timing_CIs.df,'Task 3',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_3, filename = "plot_timing_CI_3.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_4 <- gridChart(timing_CIs.df,'Task 4',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_4, filename = "plot_timing_CI_4.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_5 <- gridChart(timing_CIs.df,'Task 5',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_5, filename = "plot_timing_CI_5.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_6 <- gridChart(timing_CIs.df,'Task 6',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_6, filename = "plot_timing_CI_6.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_7 <- gridChart(timing_CIs.df,'Task 7',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_7, filename = "plot_timing_CI_7.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_8 <- gridChart(timing_CIs.df,'Task 8',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_8, filename = "plot_timing_CI_8.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_timing_CI_9 <- gridChart(timing_CIs.df,'Task 9',ymin = -2.5,ymax = 32.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -30,percentScale=F)
ggsave(plot = plot_timing_CI_9, filename = "plot_timing_CI_9.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)
