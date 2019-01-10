trials_test.df$condition <- ordered(trials_test.df$condition, levels = c('multiples','animation'))
trials_test.df$ordering <- ordered(trials_test.df$ordering, levels = c(0,1))

trials_test_1_mult.df <- trials_test.df[trials_test.df$task_index==1 & trials_test.df$condition == 'multiples',]
trials_test_1_mult.df <- bootdif_byOrdering(trials_test_1_mult.df)
trials_test_1_mult.df$condition <- 'multiples'
trials_test_1_anim.df <- trials_test.df[trials_test.df$task_index==1 & trials_test.df$condition == 'animation',]
trials_test_1_anim.df <- bootdif_byOrdering(trials_test_1_anim.df)
trials_test_1_anim.df$condition <- 'animation'
trials_test_1.df <- rbind(trials_test_1_mult.df,trials_test_1_anim.df)
remove(trials_test_1_mult.df,trials_test_1_anim.df)
trials_test_1.df$task <- 1

trials_test_2_mult.df <- trials_test.df[trials_test.df$task_index==2 & trials_test.df$condition == 'multiples',]
trials_test_2_mult.df <- bootdif_byOrdering(trials_test_2_mult.df)
trials_test_2_mult.df$condition <- 'multiples'
trials_test_2_anim.df <- trials_test.df[trials_test.df$task_index==2 & trials_test.df$condition == 'animation',]
trials_test_2_anim.df <- bootdif_byOrdering(trials_test_2_anim.df)
trials_test_2_anim.df$condition <- 'animation'
trials_test_2.df <- rbind(trials_test_2_mult.df,trials_test_2_anim.df)
remove(trials_test_2_mult.df,trials_test_2_anim.df)
trials_test_2.df$task <- 2

trials_test_4_mult.df <- trials_test.df[trials_test.df$task_index==4 & trials_test.df$condition == 'multiples',]
trials_test_4_mult.df <- bootdif_byOrdering(trials_test_4_mult.df)
trials_test_4_mult.df$condition <- 'multiples'
trials_test_4_anim.df <- trials_test.df[trials_test.df$task_index==4 & trials_test.df$condition == 'animation',]
trials_test_4_anim.df <- bootdif_byOrdering(trials_test_4_anim.df)
trials_test_4_anim.df$condition <- 'animation'
trials_test_4.df <- rbind(trials_test_4_mult.df,trials_test_4_anim.df)
remove(trials_test_4_mult.df,trials_test_4_anim.df)
trials_test_4.df$task <- 4

trials_test_5_mult.df <- trials_test.df[trials_test.df$task_index==5 & trials_test.df$condition == 'multiples',]
trials_test_5_mult.df <- bootdif_byOrdering(trials_test_5_mult.df)
trials_test_5_mult.df$condition <- 'multiples'
trials_test_5_anim.df <- trials_test.df[trials_test.df$task_index==5 & trials_test.df$condition == 'animation',]
trials_test_5_anim.df <- bootdif_byOrdering(trials_test_5_anim.df)
trials_test_5_anim.df$condition <- 'animation'
trials_test_5.df <- rbind(trials_test_5_mult.df,trials_test_5_anim.df)
remove(trials_test_5_mult.df,trials_test_5_anim.df)
trials_test_5.df$task <- 5

trials_test_6_mult.df <- trials_test.df[trials_test.df$task_index==6 & trials_test.df$condition == 'multiples',]
trials_test_6_mult.df <- bootdif_byOrdering(trials_test_6_mult.df)
trials_test_6_mult.df$condition <- 'multiples'
trials_test_6_anim.df <- trials_test.df[trials_test.df$task_index==6 & trials_test.df$condition == 'animation',]
trials_test_6_anim.df <- bootdif_byOrdering(trials_test_6_anim.df)
trials_test_6_anim.df$condition <- 'animation'
trials_test_6.df <- rbind(trials_test_6_mult.df,trials_test_6_anim.df)
remove(trials_test_6_mult.df,trials_test_6_anim.df)
trials_test_6.df$task <- 6

pointDiffCIs_byTask.df <- rbind(trials_test_1.df,trials_test_2.df,trials_test_4.df,trials_test_5.df,trials_test_6.df)
remove(trials_test_1.df,trials_test_2.df,trials_test_4.df,trials_test_5.df,trials_test_6.df)

pointDiffCIs_byTask.df$condition <- ordered(pointDiffCIs_byTask.df$condition, levels = c('multiples','animation'))

pointDiffCIs_byTask.df$diff <- 'ord0-ord1'
pointDiffCIs_byTask.df$mean_diff <- pointDiffCIs_byTask.df$mean_diff * -1
pointDiffCIs_byTask.df$lowerBound_CI <- pointDiffCIs_byTask.df$lowerBound_CI * -1
pointDiffCIs_byTask.df$upperBound_CI <- pointDiffCIs_byTask.df$upperBound_CI * -1

plot_pointDiff_CI_byTask <- ggplot(pointDiffCIs_byTask.df, aes(x=diff, y=mean_diff))

plot_pointDiff_CI_byTask <- plot_pointDiff_CI_byTask + geom_errorbar(aes(ymin=lowerBound_CI, ymax=upperBound_CI,color=condition),
                                                       width=0,                    # Width of the error bars
                                                       size = 1,
                                                       position=position_dodge(width=0.5))

plot_pointDiff_CI_byTask <- plot_pointDiff_CI_byTask + geom_hline(aes(yintercept=0), colour="#666666", linetype = 2, size=0.5)

plot_pointDiff_CI_byTask <- plot_pointDiff_CI_byTask + geom_point(aes(color=condition),size=3,position=position_dodge(width=0.5))

plot_pointDiff_CI_byTask <- plot_pointDiff_CI_byTask + coord_flip()

plot_pointDiff_CI_byTask <- plot_pointDiff_CI_byTask + facet_wrap(~ task)