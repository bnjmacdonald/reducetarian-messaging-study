# File: 08-alternative-analyses.R
# This file contains analyses for examining other aspects of the
# reducetarian data.

# ...


# constants for plots

source('other-scripts/io_options.r')
source('other-scripts/variables.r')
source('functions/balanceTable.R')
source('functions/effectTable.r')
source('functions/coefPlots.R')

# reads in merged survey data from all waves.
data <- read.csv(paste(INPUT_PATH, '/all_waves_cleaned.csv', sep=''), stringsAsFactors=FALSE)
# str(data)
# colnames(data)
dim(data)

data$block_id <- as.factor(data$block_id)  # converts block_id to factor.
data$treatment[data$treatment=='veg'] <- 'eliminate'  # renames 'veg' treatment arm
# table(data[,'treatment'])

treatment <- 'treatment'
block_var <- 'block_id'
outcome <- outcome_set_list[['main']][2]


# MODERATION: moderating effect of perceived difficulty
moderator <- 'goalReduceMeat.1'
formula <- formula(paste(outcome,' ~ ',treatment,'*',moderator,'+', block_var, sep=''))
glmOut1 <- glm(formula, data = data[!is.na(data[,outcome]),], family='gaussian')
glmOutRobust <- correctSE(glmOut1, data[!is.na(data[,outcomes]),], 'HC0')
var_names <- rownames(glmOutRobust$glmOutRobust)
glmOutRobust$glmOutRobust[!str_detect(var_names, '^block'),]

# MODERATION: moderating effect of feeling thermometer.
moderator <- 'feelTherm.1'
formula <- formula(paste(outcome,' ~ ',treatment,'*',moderator,'+', block_var, sep=''))
glmOut2 <- glm(formula, data = data[!is.na(data[,outcome]),], family='gaussian')
glmOutRobust <- correctSE(glmOut2, data[!is.na(data[,outcomes]),], 'HC0')
var_names <- rownames(glmOutRobust$glmOutRobust)
glmOutRobust$glmOutRobust[!str_detect(var_names, '^block'),]


"eliminate" appeal had a 0.25 servings SMALLER effect on meat reduction for every 1 unit increase in feeling thermometer score.

    i.e. counterintuitively, the more someone reported liking vegetarians at baseline, the smaller the eliminate effect.

"reduce" appeal had a 0.21 servings LARGER effect on meat reduction for every 1 unit increase in feeling thermometer score. 

    i.e. the more someone reported liking vegetarians at baseline, the greater the effect of the reduce appeal on reductions in meat consumption.
