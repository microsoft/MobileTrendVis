propDiffCI <- function(result.df,task_num) {
  
  result_subset.df <- subset(result.df,task_index==task_num)
  result_subset.df <- result_subset.df[log(result_subset.df$completion_time) < mean(log(result_subset.df$completion_time)) + 3 * sd(log(result_subset.df$completion_time)),]
  
  count_animation <- nrow(result_subset.df[result_subset.df$condition=='animation',])
  count_multiples <- nrow(result_subset.df[result_subset.df$condition=='multiples',])
  
  correct_animation <- nrow(result_subset.df[result_subset.df$condition=='animation' & result_subset.df$accuracy==1,])
  correct_multiples <- nrow(result_subset.df[result_subset.df$condition=='multiples' & result_subset.df$accuracy==1,])
  
  prop_animation <- correct_animation / count_animation
  prop_multiples <- correct_multiples / count_multiples
  
  prop_diff <- prop_animation - prop_multiples
  
  prop_test <- prop.test(x=c(correct_animation,correct_multiples), n=c(count_animation,count_multiples), correct=T)
  
  lowerBound_CI <- prop_test$conf.int[1]
  upperBound_CI <- prop_test$conf.int[2]
  
  diffCI.df <- data.frame('Anim - Mult',task_num,correct_animation,count_animation,prop_animation,correct_multiples,count_multiples,prop_multiples,prop_diff,lowerBound_CI,upperBound_CI)
  colnames(diffCI.df) <- c('condition','task','correct_animation','count_animation','prop_animation','correct_multiples','count_multiples','prop_multiples','mean','lowerBound_CI','upperBound_CI')
  
  return(diffCI.df)
  
}

#task 1
propDiff_CIs.df <- propDiffCI(trials_test.df,1)

#task 2
propDiff_CIs.df <- rbind(propDiff_CIs.df,propDiffCI(trials_test.df,2))

#task 3
propDiff_CIs.df <- rbind(propDiff_CIs.df,propDiffCI(trials_test.df,3))

#task 4
propDiff_CIs.df <- rbind(propDiff_CIs.df,propDiffCI(trials_test.df,4))

#task 5
propDiff_CIs.df <- rbind(propDiff_CIs.df,propDiffCI(trials_test.df,5))

#task 6
propDiff_CIs.df <- rbind(propDiff_CIs.df,propDiffCI(trials_test.df,6))

#task 7
propDiff_CIs.df <- rbind(propDiff_CIs.df,propDiffCI(trials_test.df,7))

#task 8
propDiff_CIs.df <- rbind(propDiff_CIs.df,propDiffCI(trials_test.df,8))

#task 9
propDiff_CIs.df <- rbind(propDiff_CIs.df,propDiffCI(trials_test.df,9))

propDiff_CIs.df$task <- propDiff_CIs.df$task %>% as.factor()
propDiff_CIs.df$task <- revalue(propDiff_CIs.df$task, c("1" = "Task 1",
                                                        "2" = "Task 2",
                                                        "3" = "Task 3",
                                                        "4" = "Task 4",
                                                        "5" = "Task 5",
                                                        "6" = "Task 6",
                                                        "7" = "Task 7",
                                                        "8" = "Task 8",
                                                        "9" = "Task 9"
))

plot_propDiff_CI <- dualChart(propDiff_CIs.df,ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = 0,percentScale=T)

ggsave(plot = plot_propDiff_CI, filename = "plot_propDiff_CI.pdf", device="pdf", width = 3.75, height = 1.75, units = "in", dpi = 300)
ggsave(plot = plot_propDiff_CI, filename = "plot_propDiff_CI.png", device="png", width = 3.75, height = 1.75, units = "in", dpi = 300)

propDiff_CIs.df$condition <- revalue(propDiff_CIs.df$condition, c("Anim - Mult" = "A-M"
))

plot_propDiff_CI_axis <- gridChart(propDiff_CIs.df,'Task 1',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_axis, filename = "plot_propDiff_CI_axis.png", device="png", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_1 <- gridChart(propDiff_CIs.df,'Task 1',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_1, filename = "plot_propDiff_CI_1.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_2 <- gridChart(propDiff_CIs.df,'Task 2',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_2, filename = "plot_propDiff_CI_2.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_3 <- gridChart(propDiff_CIs.df,'Task 3',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_3, filename = "plot_propDiff_CI_3.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_4 <- gridChart(propDiff_CIs.df,'Task 4',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_4, filename = "plot_propDiff_CI_4.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_5 <- gridChart(propDiff_CIs.df,'Task 5',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_5, filename = "plot_propDiff_CI_5.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_6 <- gridChart(propDiff_CIs.df,'Task 6',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_6, filename = "plot_propDiff_CI_6.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_7 <- gridChart(propDiff_CIs.df,'Task 7',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_7, filename = "plot_propDiff_CI_7.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_8 <- gridChart(propDiff_CIs.df,'Task 8',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_8, filename = "plot_propDiff_CI_8.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)

plot_propDiff_CI_9 <- gridChart(propDiff_CIs.df,'Task 9',ymin = -0.65,ymax = 0.65,yAxisLabel = "",xAxisLabel = "",displayXLabels = F,displayYLabels = F,vLineVal = 0,percentScale=T)
ggsave(plot = plot_propDiff_CI_9, filename = "plot_propDiff_CI_9.pdf", device="pdf", width = 1, height = 1, units = "in", dpi = 300)


