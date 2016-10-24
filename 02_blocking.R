# ------------------------------------------------- #
# ------------------------------------------------- #
# file: 02-pilot-blocking.R
# description:
# this file reads in cleaned data from the baseline 
# pilot survey and creates blocks and treatment 
# assignments. 
# ------------------------------------------------- #
# ------------------------------------------------- #

library(blockTools)
library(reshape2)
library(RItools)
library(plyr)
library(glmnet)
library(rjson)
library(stringr)

# ------------------------- #
#       PRELIMINARIES       #
# ------------------------- #

source('other-scripts/io_options.r')
source('functions/utils.r')

# reads in the baseline survey data.
data <- read.csv(paste(INPUT_PATH, '/01_wave1_cleanedData.csv', sep=''), stringsAsFactors=FALSE)
dim(data)

# counts number of NAs in each column.
sort(apply(data, MARGIN=2, FUN=function(col) sum(is.na(col))))
# table(data$FFQtotalSumMeat, exclude=NULL)
# table(data$howOftenEatMeat, exclude=NULL)

# NOTE: most variables have very few NAs (i.e. < 10 out of 3076 obs). feelTherm is an exception, with 310 NAs. female is the vraiable with the next-most NAs (83 NAs).

# ID variable
id_var <- 'mTurkID'


# ------------------------------------- #
# POST-HOC ANALYSIS OF BLOCKING FROM    #
# PILOT STUDY                           #
# the purpose to this section is to use #
# the pilot data to examine what        #
# baseline variables did well at        #
# predicting endline/chg FFQ.           #
# ------------------------------------- #

# Main takeaways:
# * adding a bunch of covariates results in a pretty good R^2 for FFQtotalSumMeat.3 (about 0.55) and FFQtotalSumMeat_chg (about 0.77). The latter result depends heavily on inclusion of FFQtotalSumMeat.1 (without it, R-squared is 0.45). Results are pretty similar for 2-day recall.
# * using baseline meatNames rather than aggregated FFQtotalSumMeat.1 does not help much. So best to stick with FFQtotalSumMeat.1 for blocking.


# reads in the cleaned final pilot data.
data_pilot <- read.csv('../../pilot-study/data-analysis/cleaned-data/all_waves_cleaned.csv', stringsAsFactors=FALSE)
dim(data_pilot)
str(data_pilot)
summary(data_pilot)

outcome_vars_ffq <- c('FFQtotalSumMeat.3', 'FFQtotalSumMeat_chg')
outcome_vars_2day <- c('TwoDayTotalSumMeat.3', 'TwoDayTotalSumMeat_chg')

# removes outliers
trim_percentile_upper <- 0.975
trim_percentile_lower <- 1 - trim_percentile_upper
data_pilot <- remove_outliers(data_pilot, c(outcome_vars_ffq, outcome_vars_2day), trim_percentile_upper, trim_percentile_lower)

# regresses outcomes on block dummies in order to estimate R-squared from blocking.
outcome_data <- data_pilot[,c(outcome_vars_ffq, outcome_vars_2day)]
num_regressions <- ncol(outcome_data)
r_sq_results <- data.frame(matrix(NA, ncol=2, nrow=num_regressions))
colnames(r_sq_results) <- c('outcome', 'r_squared')
r_sq_results[,'outcome'] <- c(outcome_vars_ffq, outcome_vars_2day)

for (i in 1:nrow(r_sq_results)) {
    this_lm <- lm(outcome_data[,i] ~ as.factor(block_id), data=data_pilot)
    r_sq_results[i, 'r_squared'] <- summary(this_lm)$r.squared
}

# computes auto-correlation in FFQ and Two-day recall.
cor(data_pilot$FFQtotalSumMeat.1, data_pilot$FFQtotalSumMeat_chg, use='complete.obs')
cor(data_pilot$FFQtotalSumMeat.1, data_pilot$FFQtotalSumMeat.3, use='complete.obs')
cor(data_pilot$TwoDayTotalSumMeat.1, data_pilot$TwoDayTotalSumMeat_chg, use='complete.obs')
cor(data_pilot$TwoDayTotalSumMeat.1, data_pilot$TwoDayTotalSumMeat.3, use='complete.obs')

# regresses FFQ outcomes on various wave 1 measures.
# meatNames <- c('FFQfreqChicken.1', 'FFQfreqTurkey.1', 'FFQfreqFish.1', 'FFQfreqPork.1', 'FFQfreqBeef.1', 'FFQfreqOther.1')
covariates1 <- c('female', 'yearBorn', 'income', 'everEatenVeg', 'ATDpurchContToSuff.1', 'ATDstandOfLive.1', 'FFQtotalSumMeat.1', 'highestEDU', 'goalEliminateMeat.1', 'goalReduceMeat.1', 'ATDreduceTrend.1', 'ATDenviron.1', 'howOftenEatMeat.1', 'location')
formula1 <- formula(paste(outcome_vars_ffq[1], ' ~ ', paste(covariates1, collapse='+'), sep=''))
formula2 <- formula(paste(outcome_vars_ffq[2], ' ~ ', paste(covariates1, collapse='+'), sep=''))
lmout1 <- lm(formula1, data=data_pilot)
lmout2 <- lm(formula2, data=data_pilot)
summary(lmout1)
summary(lmout2)

# regresses two-day recall outcomes on various wave 1 measures.
covariates2 <- c('female', 'yearBorn', 'income', 'everEatenVeg', 'ATDpurchContToSuff.1', 'ATDstandOfLive.1', 'TwoDayTotalSumMeat.1', 'highestEDU', 'goalEliminateMeat.1', 'goalReduceMeat.1', 'ATDreduceTrend.1', 'ATDenviron.1', 'howOftenEatMeat.1', 'location')
formula3 <- formula(paste(outcome_vars_2day[1], ' ~ ', paste(covariates2, collapse='+'), sep=''))
formula4 <- formula(paste(outcome_vars_2day[2], ' ~ ', paste(covariates2, collapse='+'), sep=''))
lmout3 <- lm(formula3, data=data_pilot)
lmout4 <- lm(formula4, data=data_pilot)
summary(lmout3)
summary(lmout4)

# estimates LASSOs
X1 <- apply(model.matrix(lmout1)[,-1], MARGIN=2, FUN=function(x) (x-mean(x))/sd(x))  # standardizes feature matrix.
# round(apply(X1, MARGIN=2, FUN=mean), 5)
# apply(X1, MARGIN=2, FUN=var)
lasso1 <- cv.glmnet(x=X1, y=data_pilot[complete.cases(data_pilot[,c(outcome_vars_ffq[1], covariates1)]),outcome_vars_ffq[1]])
coefs1 <- coef(lasso1, s='lambda.min')[,1]
coefs1 <- sort(coefs1[coefs1 > 0], decreasing=TRUE)
coefs1

X2 <- apply(model.matrix(lmout2)[,-1], MARGIN=2, FUN=function(x) (x-mean(x))/sd(x))
lasso2 <- cv.glmnet(x=X2, y=data_pilot[complete.cases(data_pilot[,c(outcome_vars_ffq[2], covariates1)]),outcome_vars_ffq[2]])
coefs2 <- coef(lasso2, s='lambda.min')[,1]
coefs2 <- sort(coefs2[coefs2 > 0], decreasing=TRUE)
coefs2

# predictive variables in lasso:
# FFQ
# goalEliminateMeat
# howOftenEatMeat
# goalReduceMeat
# yearBorn
# income
# highestEDU
# ATDenviron
# ATDreduceTrend

# ------------------------------------------- #
# COMPARES PILOT AND MAIN STUDY BASELINE DATA #
# ------------------------------------------- #

summary(data_pilot$FFQtotalSumMeat.1)
summary(data$FFQtotalSumMeat)

summary(data_pilot$yearBorn)
summary(data$yearBorn)

summary(data_pilot$ATDpurchContToSuff.1)
summary(data$ATDpurchContToSuff)

table(data_pilot$highestEDU)
table(data$highestEDU)

summary(data_pilot$female)
summary(data$female)

summary(data_pilot$female)
summary(data$female)

table(data_pilot$howOftenEatMeat.1)
table(data$howOftenEatMeat)

# ------------------------- #
# CHOICE OF BLOCK VARIABLES #
# ------------------------- #

potential_numeric_vars <- c('howOftenEatMeat', 'everEatenVeg', 'intentChangeMeat', 'ATDpurchContToSuff', 'ATDstandOfLive', 'ATDenviron', 'ATDreduceTrend', 'ATDhealthier', 'goalEliminateMeat', 'goalReduceMeat', 'yearBorn', 'income', 'highestEDU', 'female')
potential_binary_vars <- c('location')
potential_vars <- c(potential_numeric_vars, potential_binary_vars)
formula <- as.formula(paste('FFQtotalSumMeat ~ ', paste(potential_vars, collapse='+'), sep=''))
summary(lm(formula, data=data))

# variables for blocking.
numeric_block_vars <- potential_numeric_vars
binary_block_vars <- potential_binary_vars
block_vars <- c('FFQtotalSumMeat', numeric_block_vars)

# ------------------------- #
#         BLOCKING          #
# this section produces a   #
# block ID for each         #
# observation.              #
# ------------------------- #

# NOTE TO SELF:
# a few observations have NA values in some columns and so do not get included in the 
# blocking algorithm. For these observations, we just assign them to treatment randomly 
# (and give them an NA block id). I subsequently refer to these as the "non-blocked" 
# observations.

set.seed(7623411)

arms <- c('control', 'reduce', 'veg')
num_arms <- length(arms)  # number of experimental arms (includes control arm).

# data for blocking
data_block <- data[,c(id_var, block_vars)]
summary(data_block)
str(data_block)

# standardizes each variable.
data_block[,block_vars] <- apply(data_block[,block_vars], MARGIN=2, FUN=function(col) (col - mean(col, na.rm=TRUE))/sd(col, na.rm=TRUE))
# double-checks that every var is now centered and has variance = 1
round(sapply(data_block[,block_vars], mean, na.rm=TRUE), 5)
sapply(data_block[,block_vars], var, na.rm=TRUE)

# separates observations with any NAs from the rest of the data.
obs_with_na <- data_block[!complete.cases(data_block),]
data_block_narm <- data_block[complete.cases(data_block),]  # removes cases with any NAs.
dim(obs_with_na)
dim(data_block_narm)
summary(data_block_narm)

# # converts categorical variables to design matrices
# design_matrices <- list()
# for (i in 1:length(potential_binary_vars)) {
#     formula <- formula(paste('~', potential_binary_vars[i], sep=''))
#     design_matrices[[i]] <- model.matrix(formula, data=data_block_narm)
# }
# data_block_narm <- cbind(data_block_narm, do.call('cbind', design_matrices))
# data_block_narm <- data_block_narm[,!colnames(data_block_narm) %in% potential_binary_vars]
# dim(data_block_narm)
# str(data_block_narm)

# creates experimental blocks.
blocks <- block(data_block_narm, id.vars=id_var, n.tr=num_arms, block.vars=block_vars, verbose=FALSE, groups=NULL)  # block.vars=colnames(data_block_narm)[-which(colnames(data_block_narm) %in% c(id_var))]

data_block_narm$block_id <- createBlockIDs(blocks, data_block_narm, id.var=id_var)
obs_with_na$block_id <- NA  # these "non-blocked" observations were not used in blocking. 


# --------------------------------- #
#       TREATMENT ASSIGNMENT        #
# this section assigns each         #
# observation to a treatment        #
# condition (using the block output #
# from above).                      #
# --------------------------------- #

# assigns each unit to a treatment condition.
assign <- assignment(blocks, namesCol=arms)
df_assign <- assign[[1]][[1]][,1:num_arms]  # stores as dataframe
df_assign_long <- melt(df_assign, id.vars=NULL)
colnames(df_assign_long) <- c('treatment', id_var)

# merges with data_block_narm
data_block_narm <- join(data_block_narm, df_assign_long, by=id_var, type='left')
# data2$treatment <- as.numeric(data2$treatment)
summary(data_block_narm)

# assigns the "non-blocked" observations to a treatment arm (using replacement)
obs_with_na$treatment <- arms[sample(1:num_arms, size=nrow(obs_with_na), replace=TRUE)]

# merges blocked and non-blocked data
data_assigned <- join(data_block_narm, obs_with_na, by=id_var, type='full')

# checks merge:
dim(data_assigned)
nrow(data_assigned) == nrow(data_block_narm) + nrow(obs_with_na)
ncol(data_assigned) == ncol(data_block_narm) & ncol(data_assigned) == ncol(obs_with_na)
setequal(data_assigned[,id_var], data[,id_var])

table(data_assigned$treatment)
table(data_assigned$block_id)


# ------------------------- #
#     EXAMINES BALANCE      #
# ------------------------- #

# creates vector of random assignments for comparison
random <- sample(1:num_arms, size=nrow(data_assigned), replace=TRUE)  

# creates balance table.
balance <- ddply(data_assigned[,c('treatment', block_vars)], .(treatment), numcolwise(mean, na.rm=TRUE))
balance
# ddply(data_assigned[,c('treatment', block_vars)], .(random), numcolwise(mean, na.rm=TRUE))

# conducts pairwise t.tests for each blocked variable
balance_list <- list()
balance_list_random <- list()
for (i in 1:length(block_vars)) {
    balance_list[[i]] <- round(pairwise.t.test(data_assigned[,block_vars[i]], data_assigned$treatment, p.adjust.method='none', paired=FALSE, alternative='two.sided', na.rm=TRUE)$p.value, 4)
    balance_list_random[[i]] <- round(pairwise.t.test(data_assigned[,block_vars[i]], random, p.adjust.method='none', paired=FALSE, alternative='two.sided', na.rm=TRUE)$p.value, 4)
}
names(balance_list) <- block_vars
names(balance_list_random) <- block_vars
balance_list  # looks pretty good, especially when compared to balance_list_random.
balance_list_random


# ------------------------- #
#          EXPORT           #
# ------------------------- #

# exports a csv containing mturk id, block_id, and treatment assignment
write.csv(data_assigned[,c(id_var, 'treatment', 'block_id')], paste(OUTPUT_PATH, '/all-treatment-assignments.csv', sep=''), row.names=FALSE)
# write.csv(data_assigned[,c(id_var)], 'treatment-assignments/all-mturk-ids.csv', row.names=FALSE)

# exports one csv for each treatment assignment containing the MTurk IDs for that treatment group (this is helpful for Qualtrics embedding and for emailing a list of Turkers). Also creates a JSON file to set as embedded data in Qualtrics.
treatment_assignment_list <- list()
treatment_assignment_list[[1]] <- flatten_vec(data_assigned[,c(id_var)])  # all mturk ids
data_outputs <- list()
for (i in 1:num_arms) {
    # num_obs <- length(data_assigned$treatment[data_assigned$treatment==arms[i]])
    data_output <- data_assigned[data_assigned$treatment==arms[i], id_var]
    write.csv(data_output, paste(OUTPUT_PATH, '/', arms[i],'-assignments.csv', sep=''), row.names=FALSE)
    data_outputs[[i]] <- data_output
    treatment_assignment_list[[i+1]] <- flatten_vec(data_output)
}

names(treatment_assignment_list) <- c('all_mturk_ids', arms)
json_all_mturk_ids <- toJSON(treatment_assignment_list)
write(json_all_mturk_ids, paste(OUTPUT_PATH, '/treatment-assignments.json', sep=''))

# check that each data_output has set intersection 0 with other outputs.
for (i in 1:(length(data_outputs)-1)) {
    print(intersect(data_outputs[[i]], data_outputs[[i+1]]))
}

# checks that csv files match length of json data.
for (i in 1:length(treatment_assignment_list)) {
    print(length(str_split(treatment_assignment_list[[i]], pattern=',')[[1]]))
    if (i > 1) {
        print(length(str_split(treatment_assignment_list[[i]], pattern=',')[[1]]) == length(data_outputs[[i-1]]))
    }
}
