source('bootCIs.R')

points_by_condition <- function(result.df) {
  
  #compute mean for each user_id and representation
  result.df <- result.df[log(result.df$completion_time) < mean(log(result.df$completion_time)) + 3 * sd(log(result.df$completion_time)),]
  point_aggregate.df <- aggregate(abs(result.df$points), list(result.df$user_id,result.df$condition), sum)
  
  names(point_aggregate.df) <- c("user_id","condition","points")
  point_aggregate.df$points <- point_aggregate.df$points / 16 #convert to percentage of maximum points 
  point_aggregate.df$condition <- revalue(point_aggregate.df$condition, c("animation" = "A",
                                                                          "multiples" = "M"
  ))
  point_aggregate.df$condition <- ordered(point_aggregate.df$condition, levels = c('M','A'))
  
  points_multiples <- subset(point_aggregate.df, condition=="M")$points
  points_animation <- subset(point_aggregate.df, condition=="A")$points
  
  n_points_multiples <- length(points_multiples)
  n_points_animation <- length(points_animation)
  
  if (sum(n_points_multiples) == 0) {
    points_ci_multiples <- c(0,0,0)
  }
  else {
    points_ci_multiples <- bootstrapMeanCI(points_multiples)
  }
  if (sum(n_points_animation) == 0) {
    points_ci_animation <- c(0,0,0)
  }
  else {
    points_ci_animation <- bootstrapMeanCI(points_animation)
  }
  
  points_analysis = c()
  points_analysis$ratio = c("M","A")
  points_analysis$pointEstimate = c(points_ci_multiples[1],points_ci_animation[1])
  points_analysis$ci.max = c(points_ci_multiples[3],points_ci_animation[3])
  points_analysis$ci.min = c(points_ci_multiples[2],points_ci_animation[2])
  points_analysis$n = c(n_points_multiples,n_points_animation)
  
  points_analysis.df <- data.frame(factor(points_analysis$ratio),points_analysis$pointEstimate, points_analysis$ci.min, points_analysis$ci.max,points_analysis$n)
  colnames(points_analysis.df) <- c("condition", "mean", "lowerBound_CI", "upperBound_CI","n")
  
  return(points_analysis.df)
}

total_points_CIs.df <- points_by_condition(trials_test.df)
total_points_CIs.df$condition <- ordered(total_points_CIs.df$condition, levels = c('M','A'))
total_points_CIs.df$task <- 'Overall Correcness'

plot_total_points_CI <- dualChart(total_points_CIs.df,ymin = -0.1,ymax = 1.1,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = -1,percentScale=T)

ggsave(plot = plot_total_points_CI, filename = "plot_total_points_CI.pdf", device="pdf", width = 3.75, height = 0.35, units = "in", dpi = 300)
ggsave(plot = plot_total_points_CI, filename = "plot_total_points_CI.png", device="png", width = 3.75, height = 0.35, units = "in", dpi = 300)

### COMPUTE DIFF

trials_subset.df <- trials_test.df[log(trials_test.df$completion_time) < mean(log(trials_test.df$completion_time)) + 3 * sd(log(trials_test.df$completion_time)),]
total_points.df <- aggregate(trials_subset.df$points, list(trials_subset.df$user_id,trials_subset.df$condition), sum)
names(total_points.df) <- c('user_id','condition','points')
total_points.df$points <- total_points.df$points / 16 #convert to percentage of maximum points 
total_points.df$condition <- revalue(total_points.df$condition, c("animation" = "A",
                                                                  "multiples" = "M"
))
total_points.df$condition <- ordered(total_points.df$condition, levels = c('M','A'))

total_points_DiffCI.df <- bootdif(total_points.df)
total_points_DiffCI.df$task <- 'A-M'

plot_total_points_diff_CI <- dualChart(total_points_DiffCI.df,ymin = -0.125,ymax = 0.125,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = F,vLineVal = 0,percentScale=T)

ggsave(plot = plot_total_points_diff_CI, filename = "plot_total_points_diff_CI.pdf", device="pdf", width = 3.75, height = 0.5, units = "in", dpi = 300)
ggsave(plot = plot_total_points_diff_CI, filename = "plot_total_points_diff_CI.png", device="png", width = 3.75, height = 0.5, units = "in", dpi = 300)
