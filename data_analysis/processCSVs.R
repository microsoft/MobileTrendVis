library(dplyr)
library(jsonlite)
library(ggplot2)

path <- "1217_data" 
print(paste("processing ",path,sep=""))

loadData <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, function(i) read.csv(file = i,col.names = FALSE,stringsAsFactors = FALSE))
  do.call(rbind, tables)
}

participant_data <- loadData(path)


#----------------------------------
# PHONE DIMS
#---------------------------------

load_data <- participant_data[grepl("\"ConsentStart\"",participant_data[,1]),] %>% data.frame()
# load_data <- participant_data[grepl("\"Load\"",participant_data[,1]),] %>% data.frame()

load_data <- sapply(load_data[,1],function(x){as.character(x)})
load_data <- lapply(load_data,function(x){fromJSON(x)})

#convert to proper names
load_data <- lapply(load_data,function(x){x[names(load_data[[1]])]})

# load_data <- lapply(load_data,function(x){unlist(x)})

#attempt to re-order temp values
phone_dims.df <- do.call(rbind,load_data) %>% 
  apply(2,function(x){unlist(x)}) %>%
  as.data.frame(stringsAsFactors=F)

complete_data <- participant_data[grepl("\"TrialsCompleted\"",participant_data[,1]),] %>% data.frame()

complete_data <- sapply(complete_data[,1],function(x){as.character(x)})
complete_data <- lapply(complete_data,function(x){fromJSON(x)})

#convert to proper names
complete_data <- lapply(complete_data,function(x){x[names(complete_data[[1]])]})

#attempt to re-order temp values
load.df <- do.call(rbind,load_data) %>% 
  apply(2,function(x){unlist(x)}) %>%
  as.data.frame(stringsAsFactors=F)

names(load.df)[names(load.df) == 'TimeStamp'] <- 'load_time'

load.df <- load.df[c('user_id','load_time')]

#attempt to re-order temp values
complete.df <- do.call(rbind,complete_data) %>% 
  apply(2,function(x){unlist(x)}) %>%
  as.data.frame(stringsAsFactors=F)

names(complete.df)[names(complete.df) == 'TimeStamp'] <- 'completion_time'

complete.df <- complete.df[c('user_id','completion_time')]

remove(load_data)
remove(complete_data)

timing.df <- inner_join(load.df,complete.df,by='user_id')

remove(load.df)
remove(complete.df)

timing.df$completion_time <- timing.df$completion_time %>% as.numeric()
timing.df$load_time <- timing.df$load_time %>% as.numeric()
timing.df$user_id <- timing.df$user_id %>% as.factor()
timing.df$duration <- timing.df$completion_time - timing.df$load_time
timing.df$duration <- timing.df$duration / 1000
timing.df$duration <- timing.df$duration / 60

#----------------------------------
# STUDY APP INITILIZATION
#---------------------------------

participant_metadata.df <- read.csv(file=paste('1217_metadata',".csv",sep = ""))

names(participant_metadata.df)[names(participant_metadata.df) == 'ï..user_id'] <- 'user_id'

drops <- c("TimeStamp","Event")

phone_dims.df <- phone_dims.df[,!(names(phone_dims.df) %in% drops)]

phone_dims.df$user_id <- phone_dims.df$user_id %>% as.factor()
participant_metadata.df$user_id <- participant_metadata.df$user_id %>% as.factor()
participant_metadata.df <- inner_join(participant_metadata.df, phone_dims.df,by = 'user_id')

participant_metadata.df$Height <- participant_metadata.df$Height %>% as.numeric()
participant_metadata.df$Width <- participant_metadata.df$Width %>% as.numeric()
participant_metadata.df$chart_dim <- apply(data.frame(participant_metadata.df$Width,participant_metadata.df$Height),1,FUN=min)
participant_metadata.df$chart_dim <- participant_metadata.df$chart_dim %>% as.numeric()
participant_metadata.df$chart_dim <- participant_metadata.df$chart_dim - 2
participant_metadata.df$chart_dim <- participant_metadata.df$chart_dim * 0.75

remove(phone_dims.df)
remove(drops)

participant_metadata.df <- inner_join(timing.df,participant_metadata.df,by='user_id')
participant_metadata.df$condition <- participant_metadata.df$condition %>% as.factor()
participant_metadata.df$ordering <- participant_metadata.df$ordering %>% as.factor()
participant_metadata.df <- select(participant_metadata.df, -Scene)
remove(timing.df)

# plot_duration <- ggplot(participant_metadata.df, aes(x=user_id,y=duration,fill=condition)) + geom_col() + coord_flip()

#----------------------------------
# TRIAL DATA  
#---------------------------------

trial_data <- participant_data[grepl("completion_time",participant_data[,1]),] %>% data.frame()

trial_data <- sapply(trial_data[,1],function(x){as.character(x)})
trial_data <- lapply(trial_data,function(x){fromJSON(x)})

#convert to proper names
trial_data <- lapply(trial_data,function(x){x[names(trial_data[[1]])]})

#attempt to re-order temp values
trials.df <- do.call(rbind,trial_data) %>%
  apply(2,function(x){unlist(x)}) %>%
  as.data.frame(stringsAsFactors=F)

remove(trial_data)

#numeric variables
trials.df$reading_interruption_time <- trials.df$reading_interruption_time %>% as.numeric()
trials.df$reading_interruptions <- trials.df$reading_interruptions %>% as.numeric()
trials.df$interruption_time <- trials.df$interruption_time %>% as.numeric()
trials.df$completion_time <- trials.df$completion_time %>% as.numeric()
trials.df$completion_time <- trials.df$completion_time / 1000
trials.df$error <- trials.df$error %>% as.numeric()
trials.df$num_responses <- trials.df$num_responses %>% as.numeric()
trials.df$interruptions <- trials.df$interruptions %>% as.numeric()
trials.df$reading_time <- trials.df$reading_time %>% as.numeric()
trials.df$reading_time <- trials.df$reading_time / 1000
trials.df$trial_index <- trials.df$trial_index %>% as.numeric()
trials.df$loop_count <- trials.df$loop_count %>% as.numeric()
trials.df$start_time <- trials.df$start_time %>% as.numeric()
trials.df$num_errors <- trials.df$num_errors %>% as.numeric()
trials.df$next_step_count <- trials.df$next_step_count %>% as.numeric()
trials.df$prev_step_count <- trials.df$prev_step_count %>% as.numeric()
trials.df$load_time <- trials.df$load_time %>% as.numeric()
trials.df$end_time <- trials.df$end_time %>% as.numeric()
trials.df$attempts <- trials.df$attempts %>% as.numeric()
trials.df$yearMin <- trials.df$yearMin %>% as.numeric()
trials.df$yearMax <- trials.df$yearMax %>% as.numeric()

#categorical variables
trials.df$task_index <- trials.df$task_index %>% as.factor()
trials.df$user_id <- trials.df$user_id %>% as.factor()
trials.df$correct_responses <- trials.df$correct_responses %>% as.factor()
trials.df$condition <- trials.df$condition %>% as.factor()
trials.df$ordering <- trials.df$ordering %>% as.factor()
trials.df$responses <- trials.df$responses %>% as.factor()
trials.df$prompt <- trials.df$prompt %>% as.factor()
trials.df$y <- trials.df$y %>% as.factor()
trials.df$x <- trials.df$x %>% as.factor()

#binary variables
trials.df$quality_control <- trials.df$quality_control %>% as.logical()
trials.df$tutorial <- trials.df$tutorial %>% as.logical()
trials.df$binary_error <- trials.df$error %>% as.logical()
trials.df$accuracy <- 1 - trials.df$binary_error %>% as.numeric()
trials.df$points <- ((1 - trials.df$error) * trials.df$num_responses) %>% as.numeric()

#remove participants who got the quality control question wrong
gotcha.df <- trials.df[trials.df$quality_control == T & trials.df$binary_error == T,]
excluded_participants <- levels(droplevels(gotcha.df$user_id))
participant_metadata.df$excluded <- participant_metadata.df$user_id %in% excluded_participants

plot_duration <- ggplot(participant_metadata.df, aes(duration,fill=excluded)) + geom_histogram(binwidth = 0.5,show.legend = F) + facet_wrap(~ condition, nrow = 1) + scale_fill_manual(values = alpha(c("black", "red"), .3)) +
  geom_vline(aes(xintercept=mean(duration)),show.legend = F,color="red") +
  geom_vline(aes(xintercept=(mean(duration) + 3 * sd(duration))),show.legend = F,color="blue")

plot_counts <- ggplot(participant_metadata.df, aes(condition,fill=excluded)) + geom_bar(show.legend = F) + facet_wrap(~ ordering, nrow = 1) + scale_fill_manual(values = alpha(c("black", "red"), .3))

#subset to non-excluded participants
excluded_metadata.df <- participant_metadata.df[(participant_metadata.df$user_id %in% excluded_participants),]
participant_metadata.df <- participant_metadata.df[!(participant_metadata.df$user_id %in% excluded_participants),]

plot_duration_final <- ggplot(participant_metadata.df, aes(duration,fill=condition)) + geom_histogram(binwidth = 0.5,show.legend = F) + facet_wrap(~ condition, nrow = 1) +
  geom_vline(aes(xintercept=mean(duration)),show.legend = F,color="red") +
  geom_vline(aes(xintercept=(mean(duration) + 3 * sd(duration))),show.legend = F,color="blue")

plot_counts_final <- ggplot(participant_metadata.df, aes(condition,fill=condition)) + geom_bar(show.legend = F) + facet_wrap(~ ordering, nrow = 1)

#subset to non-excluded participants
trials.df <- trials.df[!(trials.df$user_id %in% excluded_participants),]

#subset to test_data
trials_test.df <- trials.df[trials.df$tutorial == F,]
trials_test.df <- trials_test.df[trials_test.df$quality_control == F,]

#remove interrupted trials
interrupted_trials.df <- trials_test.df[trials_test.df$interruptions > 0,]
interrupted_trials.df <- rbind(interrupted_trials.df,trials_test.df[trials_test.df$reading_interruptions > 0,])
trials_test.df <- trials_test.df[trials_test.df$interruptions == 0,]
trials_test.df <- trials_test.df[trials_test.df$reading_interruptions == 0,]
trials_test.df$points_factor <- trials_test.df$points %>% as.factor()

trials_tuto.df <- trials.df[trials.df$tutorial == T,]

condition_ordering.df <- unique(trials_test.df %>% select(user_id, condition, ordering))

cols <- c("0" = "#e31a1c", "1" = "#bdd7e7", "2" = "#6baed6", "3" = "#2171b5")

plot_completion_times <- ggplot(trials_test.df, aes(completion_time, fill = points_factor)) + 
  geom_histogram(binwidth = 5) + 
  facet_grid(condition ~ task_index) + 
  scale_fill_manual(values = cols)

# plot_completion_times <- ggplot(trials_test.df, aes(y=user_id,x=completion_time,color=binary_error)) + geom_point(size=3) + facet_grid(condition ~ task_index) +
#   geom_vline(aes(xintercept=mean(completion_time)),show.legend = F,color="red") +
#   geom_vline(aes(xintercept=(mean(completion_time) + 3 * sd(completion_time))),show.legend = F,color="blue")

# plot_completion_times_combined <- ggplot(trials_test.df, aes(y=task_index,x=completion_time,color=binary_error,shape=condition)) + geom_point(size=3) + facet_wrap(~ user_id) +
  # geom_vline(aes(xintercept=mean(completion_time)),show.legend = F,color="red") +
  # geom_vline(aes(xintercept=(mean(completion_time) + 3 * sd(completion_time))),show.legend = F,color="blue")

plot_errors <- ggplot(trials_test.df, aes(error, fill = points_factor)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_grid(condition ~ task_index) +
  scale_fill_manual(values = cols)

plot_points <- ggplot(trials_test.df, aes(points, fill = points_factor)) + 
  geom_histogram(binwidth = 1) + 
  facet_grid(condition ~ task_index) +
  scale_fill_manual(values = cols)

# plot_errors_combined <- ggplot(trials_test.df, aes(y=task_index,x=error,color=binary_error,shape=condition)) + geom_point(size=3) + facet_wrap(~ user_id)

#----------------------------------
# FAMILIARITY
#---------------------------------

familiarity_data <- participant_data[grepl("Familiarity",participant_data[,1]),] %>% data.frame()

familiarity_data <- sapply(familiarity_data[,1],function(x){as.character(x)})
familiarity_data <- lapply(familiarity_data,function(x){fromJSON(x)})

#convert to proper names
familiarity_data <- lapply(familiarity_data,function(x){x[names(familiarity_data[[1]])]})

#attempt to re-order temp values
familiarity.df <- do.call(rbind,familiarity_data) %>% 
  apply(2,function(x){unlist(x)}) %>%
  as.data.frame(stringsAsFactors=F)

remove(familiarity_data)

#numeric variables
familiarity.df$TimeStamp <- familiarity.df$TimeStamp %>% as.numeric()
familiarity.df$Response <- familiarity.df$Response %>% as.numeric()
names(familiarity.df)[names(familiarity.df) == 'Response'] <- 'familiarity'

# #categorical variables
familiarity.df$Event <- familiarity.df$Event %>% as.factor()
familiarity.df$Question <- familiarity.df$Question %>% as.factor()

familiarity.df$user_id <- familiarity.df$user_id %>% as.factor()
familiarity.df <- familiarity.df[!(familiarity.df$user_id %in% excluded_participants),]
familiarity.df <- select(familiarity.df, -Question, -Event, -TimeStamp)

participant_metadata.df$user_id <- participant_metadata.df$user_id %>% as.factor()
participant_metadata.df <- left_join(participant_metadata.df,familiarity.df,by='user_id')
remove(familiarity.df)

plot_familiarity <- ggplot(participant_metadata.df, aes(familiarity,fill=condition)) + 
  geom_histogram(bins = 5, show.legend = F, na.rm = T) + 
  facet_wrap(~ condition, nrow = 1)

#----------------------------------
# CONFIDENCE
#---------------------------------

confidence_data <- participant_data[grepl("Confidence",participant_data[,1]),] %>% data.frame()

confidence_data <- sapply(confidence_data[,1],function(x){as.character(x)})
confidence_data <- lapply(confidence_data,function(x){fromJSON(x)})

#convert to proper names
confidence_data <- lapply(confidence_data,function(x){x[names(confidence_data[[1]])]})

#attempt to re-order temp values
confidence.df <- do.call(rbind,confidence_data) %>% 
  apply(2,function(x){unlist(x)}) %>%
  as.data.frame(stringsAsFactors=F)

remove(confidence_data)

#numeric variables
confidence.df$TimeStamp <- confidence.df$TimeStamp %>% as.numeric()
confidence.df$Response <- confidence.df$Response %>% as.numeric()
names(confidence.df)[names(confidence.df) == 'Response'] <- 'confidence'

# #categorical variables
confidence.df$Event <- confidence.df$Event %>% as.factor()
confidence.df$Question <- confidence.df$Question %>% as.factor()
confidence.df$user_id <- confidence.df$user_id %>% as.factor()

confidence.df$user_id <- factor(confidence.df$user_id)
confidence.df <- confidence.df[!(confidence.df$user_id %in% excluded_participants),]
confidence.df <- select(confidence.df, -Question, -Event, -TimeStamp)

participant_metadata.df$user_id <- participant_metadata.df$user_id %>% as.factor()
participant_metadata.df <- left_join(participant_metadata.df,confidence.df,by='user_id')
remove(confidence.df)

plot_confidence <- ggplot(participant_metadata.df, aes(confidence,fill=condition)) + 
  geom_histogram(bins = 5, show.legend = F, na.rm = T) + 
  facet_wrap(~ condition, nrow = 1)

#----------------------------------
# Ease
#---------------------------------

ease_data <- participant_data[grepl("Ease",participant_data[,1]),] %>% data.frame()

ease_data <- sapply(ease_data[,1],function(x){as.character(x)})
ease_data <- lapply(ease_data,function(x){fromJSON(x)})

#convert to proper names
ease_data <- lapply(ease_data,function(x){x[names(ease_data[[1]])]})

#attempt to re-order temp values
ease.df <- do.call(rbind,ease_data) %>% 
  apply(2,function(x){unlist(x)}) %>%
  as.data.frame(stringsAsFactors=F)

remove(ease_data)

#numeric variables
ease.df$TimeStamp <- ease.df$TimeStamp %>% as.numeric()
ease.df$Response <- ease.df$Response %>% as.numeric()
names(ease.df)[names(ease.df) == 'Response'] <- 'ease'

# #categorical variables
ease.df$Event <- ease.df$Event %>% as.factor()
ease.df$Question <- ease.df$Question %>% as.factor()
ease.df$user_id <- ease.df$user_id %>% as.factor()

ease.df$user_id <- factor(ease.df$user_id)
ease.df <- ease.df[!(ease.df$user_id %in% excluded_participants),]
ease.df <- select(ease.df, -Question, -Event, -TimeStamp)

participant_metadata.df$user_id <- participant_metadata.df$user_id %>% as.factor()
participant_metadata.df <- left_join(participant_metadata.df,ease.df,by='user_id')
remove(ease.df)

plot_ease <- ggplot(participant_metadata.df, aes(ease,fill=condition)) + 
  geom_histogram(bins = 5, show.legend = F, na.rm = T) + 
  facet_wrap(~ condition, nrow = 1)

