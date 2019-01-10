library(PropCIs)

accuracyCI_byOrdering <- function(result.df,task_num,exp_ordering,exp_condition) {

  result_subset.df <- subset(result.df,task_index==task_num)
  result_subset.df <- result_subset.df[log(result_subset.df$completion_time) < mean(log(result_subset.df$completion_time)) + 3 * sd(log(result_subset.df$completion_time)),]
  result_subset.df <- subset(result_subset.df,ordering==exp_ordering & condition==exp_condition)
  
  CI <- PropCIs::exactci(sum(result_subset.df$accuracy), length(result_subset.df$accuracy), 0.95)
  
  exactCI.df <- data.frame(task_num,exp_ordering,exp_condition,mean(result_subset.df$accuracy),CI$conf.int[1],CI$conf.int[2],length(result_subset.df$accuracy))
  colnames(exactCI.df) <- c('task','ordering','condition','accuracy','lowerBound_CI','upperBound_CI','n')
  
  return(exactCI.df)
  
}

#task 1
accuracy_CIs_byOrdering.df <- accuracyCI_byOrdering(trials_test.df,1,0,'multiples')
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,1,4,'animation'))

#task 2
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,0,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,2,4,'animation'))

#task 3
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,0,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,3,4,'animation'))

#task 4
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,0,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,4,4,'animation'))

#task 5
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,0,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,5,4,'animation'))

#task 6
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,0,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,6,4,'animation'))

#task 7
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,0,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,7,4,'animation'))

#task 8
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,0,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,8,4,'animation'))

#task 9
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,0,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,1,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,2,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,3,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,4,'multiples'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,0,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,1,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,2,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,3,'animation'))
accuracy_CIs_byOrdering.df <- rbind(accuracy_CIs_byOrdering.df,accuracyCI_byOrdering(trials_test.df,9,4,'animation'))

accuracy_CIs_byOrdering.df$ordering <- accuracy_CIs_byOrdering.df$ordering %>% as.factor()

plot_accuracy_CI_byOrdering <- ggplot(accuracy_CIs_byOrdering.df, aes(x=ordering, y=accuracy))

plot_accuracy_CI_byOrdering <- plot_accuracy_CI_byOrdering + geom_errorbar(aes(ymin=lowerBound_CI, ymax=upperBound_CI, color=condition),
                                                                           width=0,                    # Width of the error bars
                                                                           size = 1,
                                                                           position=position_dodge(width=0.5))

plot_accuracy_CI_byOrdering <- plot_accuracy_CI_byOrdering + geom_point(aes(color=condition),size=3,position=position_dodge(width=0.5))

plot_accuracy_CI_byOrdering <- plot_accuracy_CI_byOrdering + scale_y_continuous(limits = c(0,1))

plot_accuracy_CI_byOrdering <- plot_accuracy_CI_byOrdering + coord_flip()

plot_accuracy_CI_byOrdering <- plot_accuracy_CI_byOrdering + facet_wrap(~ task)
