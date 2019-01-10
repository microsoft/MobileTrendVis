source('bootCIs.R')

subj_CI_familiarity <- function(result.df,exp_condition) {
  
  result_subset.df <- subset(result.df,condition==exp_condition)
  result_subset.df <- subset(result_subset.df,!is.na(familiarity))

  CI <- bootstrapMeanCI(result_subset.df$familiarity)
  
  exactCI.df <- data.frame('familiarity',exp_condition,CI[1],CI[2],CI[3],length(result_subset.df$familiarity))
  colnames(exactCI.df) <- c('task','condition','mean','lowerBound_CI','upperBound_CI','n')
  
  return(exactCI.df)

}

subj_CI_confidence <- function(result.df,exp_condition) {
  
  result_subset.df <- subset(result.df,condition==exp_condition)
  result_subset.df <- subset(result_subset.df,!is.na(confidence))
  
  CI <- bootstrapMeanCI(result_subset.df$confidence)
  
  exactCI.df <- data.frame('confidence',exp_condition,CI[1],CI[2],CI[3],length(result_subset.df$confidence))
  colnames(exactCI.df) <- c('task','condition','mean','lowerBound_CI','upperBound_CI','n')
  
  return(exactCI.df)
  
}

subj_CI_ease <- function(result.df,exp_condition) {
  
  result_subset.df <- subset(result.df,condition==exp_condition)
  result_subset.df <- subset(result_subset.df,!is.na(ease))
  
  CI <- bootstrapMeanCI(result_subset.df$ease)
  
  exactCI.df <- data.frame('ease',exp_condition,CI[1],CI[2],CI[3],length(result_subset.df$ease))
  colnames(exactCI.df) <- c('task','condition','mean','lowerBound_CI','upperBound_CI','n')
  
  return(exactCI.df)
  
}


#familiarity
subj_CIs.df <- subj_CI_familiarity(participant_metadata.df,'animation')
subj_CIs.df <- rbind(subj_CIs.df,subj_CI_familiarity(participant_metadata.df,'multiples'))

#confidence
subj_CIs.df <- rbind(subj_CIs.df,subj_CI_confidence(participant_metadata.df,'animation'))
subj_CIs.df <- rbind(subj_CIs.df,subj_CI_confidence(participant_metadata.df,'multiples'))

#ease
subj_CIs.df <- rbind(subj_CIs.df,subj_CI_ease(participant_metadata.df,'animation'))
subj_CIs.df <- rbind(subj_CIs.df,subj_CI_ease(participant_metadata.df,'multiples'))


subj_CIs.df$condition <- revalue(subj_CIs.df$condition, c("animation" = "A",
                                                          "multiples" = "M"
))

subj_CIs.df$task <- subj_CIs.df$task %>% as.factor()
subj_CIs.df$task <- revalue(subj_CIs.df$task, c("familiarity" = "Familiarity",
                                                "confidence" = "Confidence",
                                                "ease" = "Ease of Use"
))

subj_CIs.df$condition <- ordered(subj_CIs.df$condition, levels = c('M','A'))

plot_subj_CI <- dualChart(subj_CIs.df,ymin = 0.5,ymax = 5.5,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = 0,percentScale=F)

ggsave(plot = plot_subj_CI, filename = "plot_subj_CI.pdf", device="pdf", width = 3.75, height = 0.75, units = "in", dpi = 300)
ggsave(plot = plot_subj_CI, filename = "plot_subj_CI.png", device="png", width = 3.75, height = 0.75, units = "in", dpi = 300)



participant_metadata.df$condition <- ordered(participant_metadata.df$condition, levels = c('multiples','animation'))

familiarity_diff_CIs.df <- bootdif_familiarity(participant_metadata.df)
familiarity_diff_CIs.df$task <- 'familiarity'

confidence_diff_CIs.df <- bootdif_confidence(participant_metadata.df)
confidence_diff_CIs.df$task <- 'confidence'

ease_diff_CIs.df <- bootdif_ease(participant_metadata.df)
ease_diff_CIs.df$task <- 'ease'

subj_diff_CIs.df <- rbind(familiarity_diff_CIs.df,confidence_diff_CIs.df,ease_diff_CIs.df)
remove(familiarity_diff_CIs.df,confidence_diff_CIs.df,ease_diff_CIs.df)

subj_diff_CIs.df$task <- ordered(subj_diff_CIs.df$task, levels = c('familiarity','confidence','ease'))

subj_diff_CIs.df$task <- subj_diff_CIs.df$task %>% as.factor()
subj_diff_CIs.df$task <- revalue(subj_diff_CIs.df$task, c("familiarity" = "Familiarity",
                                                          "confidence" = "Confidence",
                                                          "ease" = "Ease of Use"
))

plot_subj_diff_CI <- dualChart(subj_diff_CIs.df,ymin = -1,ymax = 1,yAxisLabel = "",xAxisLabel = "",displayXLabels = T,displayYLabels = T,vLineVal = 0,percentScale=F)

ggsave(plot = plot_subj_diff_CI, filename = "plot_subj_diff_CI.pdf", device="pdf", width = 3.75, height = 0.75, units = "in", dpi = 300)
ggsave(plot = plot_subj_diff_CI, filename = "plot_subj_diff_CI.png", device="png", width = 3.75, height = 0.75, units = "in", dpi = 300)

