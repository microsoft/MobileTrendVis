propDiffCI_byOrdering <- function(result.df,task_num,exp_condition) {
  
  result.df$ordering <- ordered(result.df$ordering, levels = c(0,1))
  
  count_ord0 <- nrow(result.df[result.df$task_index==task_num & result.df$ordering==0 & result.df$condition==exp_condition,])
  count_ord1 <- nrow(result.df[result.df$task_index==task_num & result.df$ordering==1 & result.df$condition==exp_condition,])
  
  correct_ord0 <- nrow(result.df[result.df$task_index==task_num & result.df$ordering==0 & result.df$accuracy==1 & result.df$condition==exp_condition,])
  correct_ord1 <- nrow(result.df[result.df$task_index==task_num & result.df$ordering==1 & result.df$accuracy==1 & result.df$condition==exp_condition,])
  
  prop_ord0 <- correct_ord0 / count_ord0
  prop_ord1 <- correct_ord1 / count_ord1
  
  prop_diff <- prop_ord0 - prop_ord1
  
  prop_test <- prop.test(x=c(correct_ord0,correct_ord1), n=c(count_ord0,count_ord1), correct=FALSE)
  
  lowerBound_CI <- prop_test$conf.int[1]
  upperBound_CI <- prop_test$conf.int[2]
  
  diffCI.df <- data.frame('ord0-ord1',exp_condition,task_num,correct_ord0,count_ord0,prop_ord0,correct_ord1,count_ord1,prop_ord1,prop_diff,lowerBound_CI,upperBound_CI)
  colnames(diffCI.df) <- c('diff','condition','task','correct_ord0','count_ord0','prop_ord0','correct_ord1','count_ord1','prop_ord1','prop_diff','lowerBound_CI','upperBound_CI')
  
  return(diffCI.df)
  
}

#task 1
propDiff_CIs_byOrdering.df <- propDiffCI_byOrdering(trials_test.df,1,'multiples')
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,1,'animation'))

#task 2
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,2,'multiples'))
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,2,'animation'))


#task 3
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,3,'multiples'))
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,3,'animation'))


#task 4
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,4,'multiples'))
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,4,'animation'))


#task 5
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,5,'multiples'))
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,5,'animation'))


#task 6
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,6,'multiples'))
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,6,'animation'))


#task 7
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,7,'multiples'))
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,7,'animation'))


#task 8
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,8,'multiples'))
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,8,'animation'))


#task 9
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,9,'multiples'))
propDiff_CIs_byOrdering.df <- rbind(propDiff_CIs_byOrdering.df,propDiffCI_byOrdering(trials_test.df,9,'animation'))



plot_Diff_CI_byOrdering <- ggplot(propDiff_CIs_byOrdering.df, aes(x=diff, y=prop_diff))

plot_Diff_CI_byOrdering <- plot_Diff_CI_byOrdering + geom_errorbar(aes(ymin=lowerBound_CI, ymax=upperBound_CI,color=condition),
                                                                   width=0,                    # Width of the error bars
                                                                   size = 1,
                                                                   position=position_dodge(width=0.5))

plot_Diff_CI_byOrdering <- plot_Diff_CI_byOrdering + geom_hline(aes(yintercept=0), colour="#666666", linetype = 2, size=0.5)

plot_Diff_CI_byOrdering <- plot_Diff_CI_byOrdering + geom_point(aes(color=condition), size=3,position=position_dodge(width=0.5))

plot_Diff_CI_byOrdering <- plot_Diff_CI_byOrdering + coord_flip()

plot_Diff_CI_byOrdering <- plot_Diff_CI_byOrdering + facet_wrap(~ task)
