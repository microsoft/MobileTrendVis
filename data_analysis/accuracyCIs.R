library(PropCIs)
library(plyr)

accuracyCI <- function(result.df,task_num,exp_condition) {
  
  result_subset.df <- subset(result.df,task_index==task_num)
  result_subset.df <- result_subset.df[log(result_subset.df$completion_time) < mean(log(result_subset.df$completion_time)) + 3 * sd(log(result_subset.df$completion_time)) & result_subset.df$condition==exp_condition,]
  
  CI <- PropCIs::exactci(sum(result_subset.df$accuracy), length(result_subset.df$accuracy), 0.95)
  BCI <- PropCIs::exactci(sum(result_subset.df$accuracy), length(result_subset.df$accuracy), 0.994)
  
  exactCI.df <- data.frame(task_num,exp_condition,mean(result_subset.df$accuracy),CI$conf.int[1],CI$conf.int[2],BCI$conf.int[1],BCI$conf.int[2],length(result_subset.df$accuracy))
  colnames(exactCI.df) <- c('task','condition','mean','lowerBound_CI','upperBound_CI','lowerBound_BCI','upperBound_BCI','n')
  
  return(exactCI.df)
  
}


#task 1
accuracy_CIs.df <- accuracyCI(trials_test.df,1,'animation')
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,1,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,1,'stepper'))

#task 2
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,2,'animation'))
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,2,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,2,'stepper'))

#task 3
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,3,'animation'))
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,3,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,3,'stepper'))

#task 4
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,4,'animation'))
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,4,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,4,'stepper'))

#task 5
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,5,'animation'))
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,5,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,5,'stepper'))

#task 6
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,6,'animation'))
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,6,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,6,'stepper'))

#task 7
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,7,'animation'))
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,7,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,7,'stepper'))

#task 8
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,8,'animation'))
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,8,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,8,'stepper'))

#task 9
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,9,'animation'))
accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,9,'multiples'))
# accuracy_CIs.df <- rbind(accuracy_CIs.df,accuracyCI(trials_test.df,9,'stepper'))

accuracy_CIs.df$condition <- revalue(accuracy_CIs.df$condition, c("animation" = "Animation",
                                                                  "multiples" = "Multiples"
))

accuracy_CIs.df$task <- accuracy_CIs.df$task %>% as.factor()
accuracy_CIs.df$task <- revalue(accuracy_CIs.df$task, c("1" = "Task 1",
                                                        "2" = "Task 2",
                                                        "3" = "Task 3",
                                                        "4" = "Task 4",
                                                        "5" = "Task 5",
                                                        "6" = "Task 6",
                                                        "7" = "Task 7",
                                                        "8" = "Task 8",
                                                        "9" = "Task 9"
))

accuracy_CIs.df$condition <- ordered(accuracy_CIs.df$condition, levels = c('Multiples','Animation'))

plot_accuracy_CI <- dualChart(accuracy_CIs.df,ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = -1,percentScale=T)

ggsave(plot = plot_accuracy_CI, filename = "plot_accuracy_CI.pdf", device="pdf", width = 3.75, height = 2.25, units = "in", dpi = 300)
ggsave(plot = plot_accuracy_CI, filename = "plot_accuracy_CI.png", device="png", width = 3.75, height = 2.25, units = "in", dpi = 300)

accuracy_CIs.df$condition <- revalue(accuracy_CIs.df$condition, c("Animation" = "A",
                                                                  "Multiples" = "M"
))

plot_accuracy_CI_axis <- gridChart(accuracy_CIs.df,'Task 1',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_axis, filename = "plot_accuracy_CI_axis.png", device="png", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_1 <- gridChart(accuracy_CIs.df,'Task 1',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_1, filename = "plot_accuracy_CI_1.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_2 <- gridChart(accuracy_CIs.df,'Task 2',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_2, filename = "plot_accuracy_CI_2.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_3 <- gridChart(accuracy_CIs.df,'Task 3',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_3, filename = "plot_accuracy_CI_3.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_4 <- gridChart(accuracy_CIs.df,'Task 4',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_4, filename = "plot_accuracy_CI_4.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_5 <- gridChart(accuracy_CIs.df,'Task 5',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_5, filename = "plot_accuracy_CI_5.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_6 <- gridChart(accuracy_CIs.df,'Task 6',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_6, filename = "plot_accuracy_CI_6.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_7 <- gridChart(accuracy_CIs.df,'Task 7',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_7, filename = "plot_accuracy_CI_7.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_8 <- gridChart(accuracy_CIs.df,'Task 8',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_8, filename = "plot_accuracy_CI_8.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_accuracy_CI_9 <- gridChart(accuracy_CIs.df,'Task 9',ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = -1,percentScale=T)
ggsave(plot = plot_accuracy_CI_9, filename = "plot_accuracy_CI_9.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

