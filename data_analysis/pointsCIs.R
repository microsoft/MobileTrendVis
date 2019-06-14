library(plyr)

#error magnitude
pointsCI <- function(result.df,task_num,exp_condition) {
  
  result_subset.df <- subset(result.df,task_index==task_num)
  result_subset.df <- result_subset.df[log(result_subset.df$completion_time) < mean(log(result_subset.df$completion_time)) + 3 * sd(log(result_subset.df$completion_time)) & result_subset.df$condition==exp_condition,]
  
  CI <- bootstrapMeanCI((result_subset.df$points / result_subset.df$num_responses),0.95)
  BCI <- bootstrapMeanCI((result_subset.df$points / result_subset.df$num_responses),0.99)
  
  exactCI.df <- data.frame(task_num,exp_condition,CI[1],CI[2],CI[3],BCI[2],BCI[3],length(result_subset.df$points))
  colnames(exactCI.df) <- c('task','condition','mean','lowerBound_CI','upperBound_CI','lowerBound_BCI','upperBound_BCI','n')
  
  return(exactCI.df)
}

#task 1
points_CIs.df <- pointsCI(trials_test.df,1,'animation')
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,1,'multiples'))

#task 2
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,2,'animation'))
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,2,'multiples'))

#task 4
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,4,'animation'))
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,4,'multiples'))

#task 5
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,5,'animation'))
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,5,'multiples'))

#task 6
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,6,'animation'))
points_CIs.df <- rbind(points_CIs.df,pointsCI(trials_test.df,6,'multiples'))

points_CIs.df$condition <- revalue(points_CIs.df$condition, c("animation" = "A",
                                                              "multiples" = "M"
))

points_CIs.df$task <- points_CIs.df$task %>% as.factor()
points_CIs.df$task <- revalue(points_CIs.df$task, c("1" = "Task 1",
                                                        "2" = "Task 2",
                                                        "4" = "Task 4",
                                                        "5" = "Task 5",
                                                        "6" = "Task 6"
))

points_CIs.df$condition <- ordered(points_CIs.df$condition, levels = c('M','A'))

plot_points_CI <- dualChart(points_CIs.df,ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = -1,percentScale=T)

ggsave(plot = plot_points_CI, filename = "plot_points_CI.pdf", device="pdf", width = 3.75, height = 1.5, units = "in", dpi = 300)
ggsave(plot = plot_points_CI, filename = "plot_points_CI.png", device="png", width = 3.75, height = 1.5, units = "in", dpi = 300)
