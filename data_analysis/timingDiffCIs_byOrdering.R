timing_diffCI_byOrdering <- function(result.df,task_num,exp_condition) {
  
  result_subset.df <- subset(result.df,task_index==task_num)
  result_subset.df <- result_subset.df[log(result_subset.df$completion_time) < mean(log(result_subset.df$completion_time)) + 3 * sd(log(result_subset.df$completion_time)),]
  result_subset.df <- subset(result.df,condition==exp_condition)
  result_subset.df$ordering <- ordered(result_subset.df$ordering, levels = c(0,1))
  
  ttest <- t.test(log(completion_time) ~ ordering, data = result_subset.df)
  lowerBound_CI <- exp(ttest$conf.int[1]) %>% as.numeric()
  upperBound_CI <- exp(ttest$conf.int[2]) %>% as.numeric()
  
  mean_ord0 <- ttest$estimate[1] %>% as.numeric()
  mean_ord1 <- ttest$estimate[2] %>% as.numeric()
  
  ratio <- exp(mean_ord0) / exp(mean_ord1)
  
  timing_diff.df <- data.frame(task_num,'ord0 / ord1', exp_condition, ratio,lowerBound_CI, upperBound_CI, exp(mean_ord1), exp(mean_ord0))
  colnames(timing_diff.df) <- c('task','diff','condition','ratio','lowerBound_CI','upperBound_CI','mean_ord1','mean_ord0')
  
  return(timing_diff.df)
  
}

trials_test.df$ordering <- trials_test.df$ordering %>% as.factor()

#task 1
timing_diff_CIs_byOrdering.df <- timing_diffCI_byOrdering(trials_test.df,1,'multiples')
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,1,'animation'))

#task 2
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,2,'multiples'))
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,2,'animation'))

#task 3
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,3,'multiples'))
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,3,'animation'))

#task 4
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,4,'multiples'))
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,4,'animation'))

#task 5
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,5,'multiples'))
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,5,'animation'))

#task 6
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,6,'multiples'))
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,6,'animation'))

#task 7
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,7,'multiples'))
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,7,'animation'))

#task 8
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,8,'multiples'))
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,8,'animation'))

#task 9
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,9,'multiples'))
timing_diff_CIs_byOrdering.df <- rbind(timing_diff_CIs_byOrdering.df,timing_diffCI_byOrdering(trials_test.df,9,'animation'))

plot_timing_diff_byOrdering <- ggplot(timing_diff_CIs_byOrdering.df, aes(x=diff, y=ratio))

plot_timing_diff_byOrdering <- plot_timing_diff_byOrdering + geom_errorbar(aes(ymin=lowerBound_CI, ymax=upperBound_CI,color=condition),
                                                                           width=0,                    # Width of the error bars
                                                                           size = 1,
                                                                           position=position_dodge(width=0.5))

plot_timing_diff_byOrdering <- plot_timing_diff_byOrdering + geom_point(aes(color=condition), size=3,position=position_dodge(width=0.5))

plot_timing_diff_byOrdering <- plot_timing_diff_byOrdering + coord_flip()

plot_timing_diff_byOrdering <- plot_timing_diff_byOrdering + geom_hline(aes(yintercept=1), colour="#666666", linetype = 2, size=0.5)

plot_timing_diff_byOrdering <- plot_timing_diff_byOrdering + facet_wrap(~ task)
