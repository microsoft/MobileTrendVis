timingCI_byOrdering <- function(result.df,task_num,exp_ordering,exp_condition) {
  
  result_subset.df <- subset(result.df,task_index==task_num)
  result_subset.df <- result_subset.df[log(result_subset.df$completion_time) < mean(log(result_subset.df$completion_time)) + 3 * sd(log(result_subset.df$completion_time)),]
  result_subset.df <- subset(result_subset.df,ordering==exp_ordering & condition==exp_condition)
  result_subset.df$log_response_time <- log(result_subset.df$completion_time)
  
  ttest <- t.test(result_subset.df$log_response_time, conf.level = 0.95)
  
  lowerBound_CI <- exp(ttest$conf.int[1])
  upperBound_CI <- exp(ttest$conf.int[2])
  mean_completion_time <- exp(mean(result_subset.df$log_response_time))
  
  timing.df <- data.frame(task_num,exp_ordering,exp_condition,mean_completion_time,lowerBound_CI,upperBound_CI,length(result_subset.df$completion_time))
  colnames(timing.df) <- c('task','ordering','condition','completion_time','lowerBound_CI','upperBound_CI','n')
  
  return(timing.df)
  
}

#task 1
timing_CIs_byOrdering.df <- timingCI_byOrdering(trials_test.df,1,0,'multiples')
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,1,4,'animation'))

#task 2
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,0,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,2,4,'animation'))

#task 3
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,0,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,3,4,'animation'))

#task 4
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,0,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,4,4,'animation'))

#task 5
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,0,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,5,4,'animation'))

#task 6
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,0,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,6,4,'animation'))

#task 7
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,0,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,7,4,'animation'))

#task 8
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,0,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,8,4,'animation'))

#task 9
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,0,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,1,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,2,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,3,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,4,'multiples'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,0,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,1,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,2,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,3,'animation'))
timing_CIs_byOrdering.df <- rbind(timing_CIs_byOrdering.df,timingCI_byOrdering(trials_test.df,9,4,'animation'))


timing_CIs_byOrdering.df$ordering <- timing_CIs_byOrdering.df$ordering %>% as.factor()

plot_timing_byOrdering <- ggplot(timing_CIs_byOrdering.df, aes(x=ordering, y=completion_time))

plot_timing_byOrdering <- plot_timing_byOrdering + geom_errorbar(aes(ymin=lowerBound_CI, ymax=upperBound_CI, color=condition),
                                                                 width=0,                    # Width of the error bars
                                                                 size = 1,
                                                                 position=position_dodge(width=0.5))

plot_timing_byOrdering <- plot_timing_byOrdering + geom_point(aes(color=condition), size=3, position=position_dodge(width=0.5))

plot_timing_byOrdering <- plot_timing_byOrdering + coord_flip()

plot_timing_byOrdering <- plot_timing_byOrdering + facet_wrap(~ task)
