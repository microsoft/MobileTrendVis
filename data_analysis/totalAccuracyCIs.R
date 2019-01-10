source('bootCIs.R')

accuracy_by_condition <- function(result.df) {
  
  #compute mean for each user_id and representation
  result.df <- result.df[log(result.df$completion_time) < mean(log(result.df$completion_time)) + 3 * sd(log(result.df$completion_time)),]
  # result.df$score <- result.df$points / result.df$num_responses #convert to accuracy score 
  accuracy_aggregate.df <- aggregate(abs(result.df$accuracy), list(result.df$user_id,result.df$condition), sum)
  
  names(accuracy_aggregate.df) <- c("user_id","condition","accuracy")
  accuracy_aggregate.df$accuracy <- accuracy_aggregate.df$accuracy / 9 #convert to percentage of maximum points
  accuracy_aggregate.df$condition <- revalue(accuracy_aggregate.df$condition, c("animation" = "A",
                                                                                "multiples" = "M"
  ))
  accuracy_aggregate.df$condition <- ordered(accuracy_aggregate.df$condition, levels = c('M','A'))
  
  accuracy_multiples <- subset(accuracy_aggregate.df, condition=="M")$accuracy
  accuracy_animation <- subset(accuracy_aggregate.df, condition=="A")$accuracy
  
  n_accuracy_multiples <- length(accuracy_multiples)
  n_accuracy_animation <- length(accuracy_animation)
  
  if (sum(n_accuracy_multiples) == 0) {
    accuracy_ci_multiples <- c(0,0,0)
  }
  else {
    accuracy_ci_multiples <- bootstrapMeanCI(accuracy_multiples)
  }
  if (sum(n_accuracy_animation) == 0) {
    accuracy_ci_animation <- c(0,0,0)
  }
  else {
    accuracy_ci_animation <- bootstrapMeanCI(accuracy_animation)
  }
  
  accuracy_analysis = c()
  accuracy_analysis$ratio = c("M","A")
  accuracy_analysis$accuracyEstimate = c(accuracy_ci_multiples[1],accuracy_ci_animation[1])
  accuracy_analysis$ci.max = c(accuracy_ci_multiples[3],accuracy_ci_animation[3])
  accuracy_analysis$ci.min = c(accuracy_ci_multiples[2],accuracy_ci_animation[2])
  accuracy_analysis$n = c(n_accuracy_multiples,n_accuracy_animation)
  
  accuracy_analysis.df <- data.frame(factor(accuracy_analysis$ratio),accuracy_analysis$accuracyEstimate, accuracy_analysis$ci.min, accuracy_analysis$ci.max,accuracy_analysis$n)
  colnames(accuracy_analysis.df) <- c("condition", "mean", "lowerBound_CI", "upperBound_CI","n")
  
  return(accuracy_analysis.df)
}

total_accuracy_CIs.df <- accuracy_by_condition(trials_test.df)
total_accuracy_CIs.df$condition <- ordered(total_accuracy_CIs.df$condition, levels = c('M','A'))
total_accuracy_CIs.df$task <- 'Overall Accuracy'

plot_total_accuracy_CI <- dualChart(total_accuracy_CIs.df,ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = -1,percentScale=T)

ggsave(plot = plot_total_accuracy_CI, filename = "plot_total_accuracy_CI.pdf", device="pdf", width = 3.75, height = 0.35, units = "in", dpi = 300)
ggsave(plot = plot_total_accuracy_CI, filename = "plot_total_accuracy_CI.png", device="png", width = 3.75, height = 0.35, units = "in", dpi = 300)

### COMPUTE DIFF

trials_subset.df <- trials_test.df[log(trials_test.df$completion_time) < mean(log(trials_test.df$completion_time)) + 3 * sd(log(trials_test.df$completion_time)),]

# trials_test.df$score <- trials_test.df$points / trials_test.df$num_responses #convert to accuracy score
total_accuracy.df <- aggregate(trials_subset.df$accuracy, list(trials_subset.df$user_id,trials_subset.df$condition), sum)
names(total_accuracy.df) <- c('user_id','condition','points')
total_accuracy.df$points <- total_accuracy.df$points / 9 #convert to percentage of maximum points
total_accuracy.df$condition <- revalue(total_accuracy.df$condition, c("animation" = "A",
                                                                      "multiples" = "M"
))
total_accuracy.df$condition <- ordered(total_accuracy.df$condition, levels = c('M','A'))

total_accuracy_DiffCI.df <- bootdif(total_accuracy.df)
total_accuracy_DiffCI.df$task <- 'A-M'

plot_total_accuracy_diff_CI <- dualChart(total_accuracy_DiffCI.df,ymin = -0.125,ymax = 0.125,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = F,vLineVal = 0,percentScale=T)

ggsave(plot = plot_total_accuracy_diff_CI, filename = "plot_total_accuracy_diff_CI.pdf", device="pdf", width = 3.75, height = 0.5, units = "in", dpi = 300)
ggsave(plot = plot_total_accuracy_diff_CI, filename = "plot_total_accuracy_diff_CI.png", device="png", width = 3.75, height = 0.5, units = "in", dpi = 300)
