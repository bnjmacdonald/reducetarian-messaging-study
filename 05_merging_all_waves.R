# ------------------------------------------------- #
# ------------------------------------------------- #
# File: 05-merging-all-waves.R
# This file merges together the cleaned data 
# (contained in the cleaned-data directory) from
# each of the 3 waves and saves the merged data to
# file. 
# ------------------------------------------------- #
# ------------------------------------------------- #

library(plyr)

source('other-scripts/io_options.r')
source('other-scripts/variables.r')
source('functions/utils.r')

# reads in the cleaned wave 1 survey data.
wave1_data <- read.csv(paste(INPUT_PATH, '/01_wave1_cleanedData.csv', sep=''), stringsAsFactors=FALSE)
dim(wave1_data)
# str(wave1_data)

# reads in the cleaned wave 2 survey data.
wave2_data <- read.csv(paste(INPUT_PATH, '/02_wave2_cleanedData.csv', sep=''), stringsAsFactors=FALSE)
table(is.na(wave2_data), exclude=NULL)  # should all be FALSE.
dim(wave2_data)
# str(wave2_data)

# reads in the cleaned wave 3 survey data.
wave3_data <- read.csv(paste(INPUT_PATH, '/03_wave3_cleanedData.csv', sep=''), stringsAsFactors=FALSE)
dim(wave3_data)
str(wave3_data)

# merges together the three survey waves, keeping only participants who completed all three waves.
all_data <- merge(wave3_data, wave2_data, by='mTurkID', all=FALSE, suffixes=c('.3', '.2'))
all_data <- merge(all_data, wave1_data, by='mTurkID', all=FALSE, suffixes=c('.3', '.1'))
# intersect(colnames(wave1_data), colnames(wave3_data))
# colnames(all_data)
dim(all_data)

# creates "change" variables between wave 1 and wave 3
all_data <- create_chg_vars(df=all_data, vars=both_wave_vars)
# summary(all_data[,paste(vars_to_create_chg, '_chg', sep='')])

# removes outliers
trim_percentile_upper <- 0.975
trim_percentile_lower <- 1 - trim_percentile_upper
all_data <- remove_outliers(all_data, all_vars, trim_percentile_upper, trim_percentile_lower)


# standardizes variables
all_vars_std <- paste(all_vars,'_std',sep='')
all_data[,all_vars_std] <- apply(all_data[,all_vars], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
# print(round(apply(all_data[,all_vars_std], MARGIN=2, FUN=mean, na.rm=TRUE), 6))
# print(apply(all_data[,all_vars_std], MARGIN=2, FUN=var, na.rm=TRUE))


# saves cleaned merged data to file
write.csv(all_data, paste(OUTPUT_PATH, '/all_waves_cleaned.csv', sep=''), row.names=FALSE)
