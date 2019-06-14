# trials_test.df$condition <- ordered(trials_test.df$condition, levels = c('multiples','animation'))
# trials_test.df$condition <- revalue(trials_test.df$condition, c("animation" = "A",
# "multiples" = "M"
# ))

trials_test_1.df <- trials_test.df[trials_test.df$task_index==1,]
trials_test_1.df <- trials_test_1.df[log(trials_test_1.df$completion_time) < mean(log(trials_test_1.df$completion_time)) + 3 * sd(log(trials_test_1.df$completion_time)),]
trials_test_1.df <- bootdif(trials_test_1.df)
trials_test_1.df$task <- 1

trials_test_2.df <- trials_test.df[trials_test.df$task_index==2,]
trials_test_2.df <- trials_test_2.df[log(trials_test_2.df$completion_time) < mean(log(trials_test_2.df$completion_time)) + 3 * sd(log(trials_test_2.df$completion_time)),]
trials_test_2.df <- bootdif(trials_test_2.df)
trials_test_2.df$task <- 2

trials_test_4.df <- trials_test.df[trials_test.df$task_index==4,]
trials_test_4.df <- trials_test_4.df[log(trials_test_4.df$completion_time) < mean(log(trials_test_4.df$completion_time)) + 3 * sd(log(trials_test_4.df$completion_time)),]
trials_test_4.df <- bootdif(trials_test_4.df)
trials_test_4.df$task <- 4

trials_test_5.df <- trials_test.df[trials_test.df$task_index==5,]
trials_test_5.df <- trials_test_5.df[log(trials_test_5.df$completion_time) < mean(log(trials_test_5.df$completion_time)) + 3 * sd(log(trials_test_5.df$completion_time)),]
trials_test_5.df <- bootdif(trials_test_5.df)
trials_test_5.df$task <- 5
# 
# trials_test_6.df <- trials_test.df[trials_test.df$task_index==6,]
# trials_test_6.df <- trials_test_6.df[log(trials_test_6.df$completion_time) < mean(log(trials_test_6.df$completion_time)) + 3 * sd(log(trials_test_6.df$completion_time)),]
# trials_test_6.df <- bootdif(trials_test_6.df)
# trials_test_6.df$task <- 6

pointDiffCIs.df <- rbind(trials_test_1.df,trials_test_2.df,trials_test_4.df,trials_test_5.df)
remove(trials_test_1.df,trials_test_2.df,trials_test_4.df,trials_test_5.df)

pointDiffCIs.df$task <- pointDiffCIs.df$task %>% as.factor()
pointDiffCIs.df$task <- revalue(pointDiffCIs.df$task, c("1" = "Task 1",
                                                        "2" = "Task 2",
                                                        "4" = "Task 4",
                                                        "5" = "Task 5"
                                                        # "6" = "Task 6"
))

plot_pointDiffCI <- dualChart(pointDiffCIs.df,ymin = -0.8,ymax = 0.8,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = 0,percentScale=T)

ggsave(plot = plot_pointDiffCI, filename = "plot_pointDiffCI.pdf", device="pdf", width = 3.75, height = 1.25, units = "in", dpi = 300)
ggsave(plot = plot_pointDiffCI, filename = "plot_pointDiffCI.png", device="png", width = 3.75, height = 1.25, units = "in", dpi = 300)
