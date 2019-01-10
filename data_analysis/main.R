#libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(jsonlite)
library(reshape2)

#helper functions for calculating and plotting CIs
source('bootCIs.R')
source('dualChart.R')
source('gridChart.R')

#load data
source('processCSVs.R', encoding = 'UTF-8')

#timing CIs
source('timingCIs.R')
source('timingDiffCIs.R')

#accuracy CIs
source('accuracyCIs.R')
source('accuracy_diffCIs.R')

#partial accuracy CIs
source('pointsCIs.R')
source('pointsDiffCI.R')

#subjective CIs
source('subj_CI.R')
