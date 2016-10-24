# ------------------------------------------------- #
# ------------------------------------------------- #
# File: 07-subgroup-analyses.R
# This file contains sub-group analyses examining 
# how the treatment effects vary across groups in 
# the study sample.
# ------------------------------------------------- #
# ------------------------------------------------- #

# ------------------------- #
#       PRELIMINARIES       #
# ------------------------- #

library(plyr)
library(broom)
library(stringr)
library(sandwich)
library(lmtest)

# sets working directory
source('other-scripts/io_options.r')
source('other-scripts/variables.r')
source('functions/subgroup_analysis.r')

# reads in merged survey data from all waves.
data <- read.csv(paste(INPUT_PATH, '/all_waves_cleaned.csv', sep=''), stringsAsFactors=FALSE)
# str(data)
dim(data)

data$block_id <- as.factor(data$block_id)
treatment <- 'treatment'
covariates <- c('block_id')


se <- 'HC0'
outcome <- outcome_set_list$main[[1]]

# TREATMENT EFFECTS BY GENDER
# ---------------------------
subgroup <- 'female'
summary(data[,subgroup])
result_female <- subgroup_analysis(data, outcome, treatment, subgroup, covariates, se=se)
result_female$glmOutRobust


# TREATMENT EFFECTS BY AGE
# ------------------------

# NOTE: we cut age into several substantively meaningful groups:
# college-age: 18-24
# post-college: 25-30
# young family: 31-40
# middle-aged and senior: 41-

data$age <- 2016 - data$yearBorn
cutpoints <- c(18, 24, 30, 40, max(data$age, na.rm=TRUE))
labels <- c('18-24', '25-30', '31-40', '41-87')

table(data[,subgroup])
# hist(data[,subgroup])
data$age_group <- cut(data[,'age'], breaks=cutpoints, include.lowest=TRUE, labels=labels)
table(data$age, data$age_group, exclude=NULL)
# hist(as.numeric(data$age_group))
subgroup <- 'age_group'
result_age <- subgroup_analysis(data, outcome, treatment, subgroup, covariates, se=se)
result_age$glmOutRobust

