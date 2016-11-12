# ------------------------------------------------- #
# ------------------------------------------------- #
# CLEANING SCRIPT FOR AWAL REDUCETARIAN STUDY       #
# WAVE 2 TREATMENT WAVE                             #
# ------------------------------------------------- #
# ------------------------------------------------- #


library(plyr)
library(stringr)

source('other-scripts/io_options.r')

# reads in the wave 2 survey data.
if (str_detect(INPUT_PATH, 'shareable')) {
    wave2_data <- read.csv(paste(INPUT_PATH, '/Reducetarian_treatment_wave_2.csv', sep=''), header = TRUE, stringsAsFactors = FALSE, na.strings = c('','NA'))
} else {
    all_content <- readLines(paste(INPUT_PATH, '/Reducetarian_treatment_wave_2.csv', sep=''))
    skip_second <- all_content[-2]
    wave2_data <- read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE, na.strings = c('','NA'))

    # creates variable dictionary
    var_dict <- read.csv(textConnection(all_content[1:2]), header=FALSE, stringsAsFactors=FALSE, na.strings=c('', 'NA'))
    write.csv(t(var_dict), 'var-dicts/wave2_var_dict.csv', row.names=FALSE)
}

dim(wave2_data)
sapply(wave2_data, FUN=function(x) sum(is.na(x)))

# # count number of NAs in each column.
# sort(apply(wave2_data, MARGIN=2, FUN=function(col) sum(is.na(col))))

# reads in the block assignments
assignments <- read.csv(paste(TREATMENTS_PATH, '/all-treatment-assignments.csv', sep=''), stringsAsFactors=FALSE)
# dim(assignments)
# head(assignments)

# changes column name of manual MTurk ID entry variable to mTurkID for consistency with other cleaned waves.
colnames(wave2_data)[which(colnames(wave2_data) %in% 'QID82_1')] <- 'mTurkID'

# ------------------------------------- #
# CHANGES COLUMN NAMES FOR READABILITY  #
# ------------------------------------- #

origNames <- c('QID96',  # do you intend to change you fruit and veg consumption in next 30 days? 
               'QID100'  # do you intend to change you meat consumption in next 30 days? 
               )

newNames <- c("intentChangeFruitVeg", # do you intend to change you fruit and veg consumption in next 30 days? 
              "intentChangeMeat"  # do you intend to change you meat consumption in next 30 days?
              )
# cbind(origNames, newNames)

for (i in 1:length(origNames)) {
  colnames(wave2_data)[which(colnames(wave2_data)==origNames[i])] <- newNames[i]
}
# colnames(wave2_data)[which(colnames(wave2_data) %in% origNames)] <- newNames
colnames(wave2_data)


# -------------------- #
# checks MTurk IDs from wave 2.

# checks if embedded data (MID) does not equal manual entry (mTurkID) for any rows.
table(wave2_data$MID == wave2_data$mTurkID, exclude=NULL)
na_test <- (wave2_data$MID != wave2_data$mTurkID) | (is.na(wave2_data$mTurkID) | is.na(wave2_data$MID))
# wave2_data[na_test, c('mTurkID', 'MID', 'progress', 'finished')]

# NOTE: wih one exception, all rows in which there is an MID but not mTurkID are where progress == 4. The one exception is where someone entered there MTurkID twice.

# TODO: figure out whether this one exception (with mTurkID == A2LY23NHYISUQ2A2LY23NHYISUQ2) should be replaced with MID or not. Depends on what this person put for MTurk ID in waves 1 and 3.

# UNUSED:
# takes MTurk ID from manual entry and stores it in mTurkID if mTurkID is blank
# wave2_data$mTurkID[is.na(wave2_data$mTurkID)] <- wave2_data$Q1.2_1_TEXT[is.na(wave2_data$mTurkID)]
# table(is.na(wave2_data$mTurkID), exclude=NULL)

# merges together the wave 2 data with the block assignments.
wave2_data_merged <- join(wave2_data, assignments, by='mTurkID', type='left')

# -------------------- #
# checks if treatment is NA for any participants (and removes them if so.)
# Q: why do some individuals have NA treatment? A: must be because they did not complete wave 1. 

table(wave2_data_merged$treatment, exclude=NULL)
wave2_data_merged[is.na(wave2_data_merged$treatment), c('mTurkID', 'MID', 'progress', 'finished', 'treatment', 'block_id')]
wave2_data_merged <- wave2_data_merged[!is.na(wave2_data_merged$treatment),]

# NOTE TO SELF: there are 10 cases with an NA. 8 of these have progress less than 11% and can be ignored. The other two didn't merge with assignment properly because of MTurkID.
# TODO: come back to these two mTurkIds.

# -------------------- #
# replaces block_id for participants not assigned to a block.
# NOTE: over 100 have block_id==NA because they were not blocked (but were still included in the experiment). This line of code replaces these NA values with 0 so that they are included in the analyses.

wave2_data_merged$block_id[is.na(wave2_data_merged$block_id)] <- 0

# -------------------- #
# cleans wave 2 variables.

# INTENT TO CHANGE FRUIT AND VEG
table(wave2_data_merged$intentChangeFruitVeg, exclude=NULL)
unique(wave2_data_merged$intentChangeFruitVeg, na.rm=FALSE)
wave2_data_merged$intentChangeFruitVeg <- as.numeric(ordered(wave2_data_merged$intentChangeFruitVeg, levels=c("Greatly decrease", "Decrease", "Somewhat Decrease", "Maintain current levels", "Somewhat increase", "Increase", "Greatly increase")))

# INTENT TO CHANGE MEAT
table(wave2_data_merged$intentChangeMeat, exclude=NULL)
unique(wave2_data_merged$intentChangeMeat, na.rm=FALSE)
wave2_data_merged$intentChangeMeat <- as.numeric(ordered(wave2_data_merged$intentChangeMeat, levels=c("Greatly decrease", "Decrease", "Somewhat Decrease", "Maintain current levels", "Somewhat increase", "Increase", "Greatly increase")))


# -------------------- #
# saves cleaned wave 2 data to file

cols_to_keep <- c('mTurkID', 'treatment', 'block_id', newNames)
write.csv(wave2_data_merged[,cols_to_keep], paste(OUTPUT_PATH, '/02_wave2_cleanedData.csv', sep=''), row.names=FALSE)


