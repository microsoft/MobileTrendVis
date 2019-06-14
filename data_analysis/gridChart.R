library(ggplot2)
library(reshape2)

gridChart <- function(resultTable, task_num,ymin, ymax, xAxisLabel = "", yAxisLabel = "", vLineVal = 0, displayXLabels = T, displayYLabels = T, percentScale = F){
  
  tr <- as.data.frame(resultTable)
  tr <- subset(tr,task==task_num)
  
  #now need to calculate one number for the width of the interval
  tr$CI2 <- tr$upperBound_CI - tr$mean
  tr$CI1 <- tr$mean - tr$lowerBound_CI
  tr$BCI2 <- tr$upperBound_BCI - tr$mean
  tr$BCI1 <- tr$mean - tr$lowerBound_BCI
  
  # print(tr)
  g <- ggplot(tr, aes(x=condition, y=mean))
  
  g <- g + geom_errorbar(aes(ymin=mean-BCI1, ymax=mean+BCI2, color=condition),
                         width=0,                    # Width of the error bars
                         size = 0.25,
                         show.legend = F)
  
  g <- g + geom_errorbar(aes(ymin=mean-CI1, ymax=mean+CI2, color=condition),
                         width=0,                    # Width of the error bars
                         size = 0.75,
                         show.legend = F)
  
  g <- g + geom_point(aes(color=condition), size=2, show.legend = F)
  
  if (vLineVal >= ymin & vLineVal <= ymax){
    g <- g + geom_hline(aes(yintercept=vLineVal), colour="#666666", linetype = 2, size=0.5)
  }
  # g <- g + geom_hline(aes(yintercept=12.5), colour="#1F77B4", linetype = 3, size=0.5, alpha=0.5)
  # g <- g + geom_hline(aes(yintercept=25), colour="#1F77B4", linetype = 3, size=0.5, alpha=0.5)
  
  cols <- c("A" = "#1F77B4", "M" = "#FF7F0E", "A-M" = "#4C924C", "A/M" = "#4C924C")
  
  
  
  g <- g + scale_color_manual(values = cols) +
    labs(x = xAxisLabel, y = yAxisLabel)
  
  if (percentScale) {
    # g <- g + scale_y_continuous(limits = c(ymin,ymax), labels = scales::percent,breaks = seq(-0.5, 0.5, by = 0.5))
    g <- g + scale_y_continuous(limits = c(ymin,ymax), expand=c(0,0), labels = scales::percent)
  }
  else {
    g <- g + scale_y_continuous(limits = c(ymin,ymax), expand=c(0,0))
  }
  g <- g + coord_flip() +
    # theme(panel.background = element_rect(fill = '#F5F5DC', colour = 'white'),
    theme(panel.background = element_rect(fill = '#EEEEEE', colour = 'white'),
          axis.title=element_blank(),
          axis.ticks.length = unit(0, "mm"),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.position = 'none',
          panel.grid.major = element_line(colour = "#DDDDDD"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(), 
          strip.background = element_rect(fill = "NA"),
          strip.placement = "outside",
          strip.text.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "mm"),
          strip.text.y = element_blank())
  
  if (displayXLabels) {
    g <- g + theme(axis.text.x = element_text(size=6))
  }
  else {
    g <- g + theme(axis.text.x = element_blank())
  }
  
  if (displayYLabels) {
    g <- g + theme(axis.text.y=element_text(colour = "black",size=6))
  }
  else {
    g <- g + theme(axis.text.y=element_blank())
  }
  
  
  return(g)
}