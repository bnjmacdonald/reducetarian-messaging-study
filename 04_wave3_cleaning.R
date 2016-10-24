# ------------------------------------------------- #
# ------------------------------------------------- #
# CLEANING SCRIPT FOR AWAL REDUCETARIAN STUDY       #
# WAVE 3 ENDLINE SURVEY                             #
# ------------------------------------------------- #
# ------------------------------------------------- #

library(stringr)

source('other-scripts/io_options.r')

# imports uncleaned dataset
# reads in the wave 2 survey data.
if (str_detect(INPUT_PATH, 'shareable')) {
    uncD <- read.csv(paste(INPUT_PATH, '/Reducetarian_endline_survey_wave_3.csv', sep=''), header=TRUE, stringsAsFactors=FALSE, na.strings = c('','NA'))
} else {
    all_content <- readLines(paste(INPUT_PATH, '/Reducetarian_endline_survey_wave_3.csv', sep=''))
    skip_second <- all_content[-2]
    uncD <- read.csv(textConnection(skip_second), header=TRUE, stringsAsFactors=FALSE, na.strings = c('','NA'))

    # creates variable dictionary
    var_dict <- read.csv(textConnection(all_content[1:2]), header=FALSE, stringsAsFactors=FALSE, na.strings=c('', 'NA'))
    write.csv(t(var_dict), 'var-dicts/wave3_var_dict.csv', row.names=FALSE)
}

dim(uncD)
colnames(uncD)

# ----------------------------- #
# EXAMINES PROPORTION OF SURVEY #
# COMPLETED BY EACH RESPONDENT  #
# ----------------------------- #

# Q8.1 is a binary (1,NA) variable that indicates whether someone saw the last page of the survey. 
table(uncD$Q8.1, exclude=NULL)

# an approximate estimate for the proportion of the survey that was filled out
uncD$approximatePropCompleted = rowSums(!is.na(uncD)) / ncol(uncD)
# hist(uncD$approximatePropCompleted, breaks=50)
quantile(uncD$approximatePropCompleted, seq(0,1, by=0.01))


# ------------------------------------- #
# CHANGES COLUMN NAMES FOR READABILITY  #
# ------------------------------------- #

origNames <- c("approximatePropCompleted",
               "V8",  # start date
               "V9",  # end date
               "V10",  # finished
               "MID",
               "Q1.2_1_TEXT",  # MID manual entry
               "Q1.2_2_TEXT",  # MID manual entry
              'Q2.1.1_1','Q2.1.1_2', 'Q2.1.1_3',  # FFQ frequency of consumption
               'Q2.1.1_4', 'Q2.1.1_5','Q2.1.1_6',
               'Q2.1.1_7', 'Q2.1.1_8', 'Q2.1.1_9',
               'Q2.1.1_10', 'Q2.1.1_11', 'Q2.1.1_12',
               'Q2.1.1_13', 'Q2.1.1_14', 
               'Q2.3',   # how often do you eat meat
               'Q3.1',  # do you intend to change you fruit and veg consumption in next 30 days? 
               'Q3.2',  # do you intend to change you meat consumption in next 30 days? 
               "Q4.1_1", "Q4.1_2", "Q4.1_4", "Q4.1_6", "Q4.1_7",  # attitudes towards animals 
               "Q4.2_2", "Q4.2_3", "Q4.2_4", "Q4.2_7", # two most important considerations when purchasing meat
               "Q4.2_11","Q4.2_12", "Q4.2_13", "Q4.2_13_TEXT",
               "Q4.4_1", "Q4.4_2", "Q4.4_3", "Q4.4_4", "Q4.4_5", "Q4.4_6", "Q4.4_7",  # animal intelligence
               "Q4.5_1", "Q4.5_2", "Q4.5_3", "Q4.5_4", "Q4.5_5", "Q4.5_6", "Q4.5_7",  # animal suffering
               "Q5.1", # number of pieces of media
               "Q5.2", # number of ppl discussed with
               "Q5.4_1", # imagine you decided to become veg. friends and family supportive?
               "Q5.4_5", # imagine you decided to become veg. friends and family negative things?
               "Q6.1",  # feeling thermometer towards vegetarians
               "Q7.1_2", "Q7.1_3"  # rate how difficult each goal would be for you. 
               )

newNames <- c("approximatePropCompleted",
              "startDate",
              "endDate",
              "finished",
              "MID",
              "mTurkID",
              "mTurkIDrepeat",
              'FFQfreqDairy', 'FFQfreqChicken', 'FFQfreqTurkey',  # FFQ frequency of consumption
              'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef',  
              'FFQfreqOther', 'FFQfreqEggs','FFQfreqFruit',
              'FFQfreqVegetables','FFQfreqNuts', 'FFQfreqBeans',  
              'FFQfreqVegMeats', 'FFQfreqGrains',
              'howOftenEatMeat',   #how often do you eat meat
              "intentChangeFruitVeg", # do you intend to change you fruit and veg consumption in next 30 days? 
              "intentChangeMeat",  # do you intend to change you meat consumption in next 30 days?
              "ATDpurchContToSuff", "ATDstandOfLive", "ATDenviron", "ATDreduceTrend", "ATDhealthier", # attitudes towards animals
              "CONSIDprice", "CONSIDnutrition", "CONSIDhormones", "CONSIDstandLiving", 
              "CONSIDtaste", "CONSIDenviron", "CONSIDother", "CONSIDotherText", 
              "intellCows", "intellPigs", "intellChicken", "intellFish", "intellHumans", "intellDogs", "intellHorses",  # animal intelligence
              "suffCows", "suffPigs", "suffChicken", "suffFish", "suffHumans", "suffDogs", "suffHorses",  # animal suffering
              "mediaConsumed", # media consumed
              "numDiscussions",  # number of ppl discussed with
              "becomeVegSupport", # imagine you decided to become veg. friends and family supportive?
              "becomeVegNegative", # imagine you decided to become veg. friends and family negative things?
              "feelTherm",  # feeling thermometer towards vegetarians.
              "goalEliminateMeat", "goalReduceMeat"  # rate how difficult each goal would be for you. 
              )
# cbind(origNames, newNames)

for (i in 1:length(origNames)) {
  colnames(uncD)[which(colnames(uncD)==origNames[i])] <- newNames[i]
}
# colnames(uncD)[which(colnames(uncD) %in% origNames)] <- newNames
colnames(uncD)

# dC is going to be the cleaned dataframe. 
dC <- uncD[,newNames]


# ----------------------------- #
# CLEANS MAIN SURVEY QUESTIONS  #
# ----------------------------- #

# FFQ - number of servings per week.
# as an estimate for number of servings per week, we took the mid-point of each range 
# never = 0
# less than 1/week = 1
# 1-6 times/week = 3.5
# 1-3 times/day = 14
# 4 or more times/day = 28

unique(dC$FFQfreqDairy)

startColfreq <- which(colnames(dC) == 'FFQfreqDairy')
endColfreq <- which(colnames(dC) == 'FFQfreqGrains')
dC[,startColfreq:endColfreq][dC[,startColfreq:endColfreq]=='never' & !is.na(dC[,startColfreq:endColfreq])] <- 0
dC[,startColfreq:endColfreq][dC[,startColfreq:endColfreq]=='less than 1 time per week' & !is.na(dC[,startColfreq:endColfreq])] <- 1
dC[,startColfreq:endColfreq][dC[,startColfreq:endColfreq]=='1-6 times per week' & !is.na(dC[,startColfreq:endColfreq])] <- 3.5
dC[,startColfreq:endColfreq][dC[,startColfreq:endColfreq]=='1-3 times per day' & !is.na(dC[,startColfreq:endColfreq])] <- 14
dC[,startColfreq:endColfreq][dC[,startColfreq:endColfreq]=='4 or more times per day' & !is.na(dC[,startColfreq:endColfreq])] <- 28

# converts all columns to numeric 
dC[,startColfreq:endColfreq] <- apply(dC[,startColfreq:endColfreq], MARGIN=2, FUN=function(x) as.numeric(x))
# checks to make sure that all columns are numeric 
apply(dC[,startColfreq:endColfreq], MARGIN=2, FUN=function(x) class(x))
# apply(dC[,startColfreq:endColfreq], MARGIN=2, FUN=function(x) table(x, exclude=NULL))

# FFQ VARIABLES:
freqNames <- c('FFQfreqDairy', 'FFQfreqChicken', 'FFQfreqTurkey',
                'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef',
                'FFQfreqOther', 'FFQfreqEggs','FFQfreqFruit',
                'FFQfreqVegetables','FFQfreqNuts', 'FFQfreqBeans',
                'FFQfreqVegMeats', 'FFQfreqGrains')

# FFQ MEAT SUM TOTAL
meatNames <- c('FFQfreqChicken', 'FFQfreqTurkey', 'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef', 'FFQfreqOther')

dC$FFQtotalSumMeat = rowSums(dC[,meatNames], na.rm=TRUE)
# rows with NA are = to 0. we don't want this, so any individual with more than 3 NAs gets an FFQtotalSumMeat of NA:
threshold <- 3
numMeatNAs <- rowSums(is.na(dC[,meatNames]))
dC$FFQtotalSumMeat[numMeatNAs >= threshold] <- NA

# HOW OFTEN EAT MEAT
table(dC$howOftenEatMeat, exclude=NULL)
unique(dC$howOftenEatMeat, na.rm=FALSE)
dC$howOftenEatMeat <- as.numeric(ordered(dC$howOftenEatMeat, levels=c('Never', 'Less than once a week', 'Once a Week', '2-3 Times a Week', 'Every other day', 'Every day')))
# table(dC$howOftenEatMeat, howOftenEatMeat, exclude=NULL)

# INTENT TO CHANGE FRUIT AND VEG
table(dC$intentChangeFruitVeg, exclude=NULL)
unique(dC$intentChangeFruitVeg, na.rm=FALSE)
dC$intentChangeFruitVeg <- as.numeric(ordered(dC$intentChangeFruitVeg, levels=c("Greatly decrease", "Decrease", "Somewhat Decrease", "Maintain current levels", "Somewhat increase", "Increase", "Greatly increase")))

# INTENT TO CHANGE MEAT
table(dC$intentChangeMeat, exclude=NULL)
unique(dC$intentChangeMeat, na.rm=FALSE)
dC$intentChangeMeat <- as.numeric(ordered(dC$intentChangeMeat, levels=c("Greatly decrease", "Decrease", "Somewhat Decrease", "Maintain current levels", "Somewhat increase", "Increase", "Greatly increase")))

# ATTITUDES
# buying meat contributes to suffering
table(dC$ATDpurchContToSuff, exclude=NULL)
unique(dC$ATDpurchContToSuff, na.rm=FALSE)
dC$ATDpurchContToSuff = as.numeric(ordered(dC$ATDpurchContToSuff, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
# levels(dC$ATDpurchContToSuff)

# animals have a good standard of living
table(dC$ATDstandOfLive, exclude=NULL)
unique(dC$ATDstandOfLive, na.rm=FALSE)
dC$ATDstandOfLive = as.numeric(ordered(dC$ATDstandOfLive, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
# levels(dC$ATDstandOfLive)

# animal ag contributes to environmental deg
table(dC$ATDenviron, exclude=NULL)
unique(dC$ATDenviron, na.rm=FALSE)
dC$ATDenviron = as.numeric(ordered(dC$ATDenviron, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
# levels(dC$ATDenviron)

# more and more people are reducing their meat conusmption 
table(dC$ATDreduceTrend, exclude=NULL)
unique(dC$ATDreduceTrend, na.rm=FALSE)
dC$ATDreduceTrend = as.numeric(ordered(dC$ATDreduceTrend, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
# levels(dC$ATDreduceTrend)

#  most people would be healthier
table(dC$ATDhealthier, exclude=NULL)
unique(dC$ATDhealthier, na.rm=FALSE)
dC$ATDhealthier = as.numeric(ordered(dC$ATDhealthier, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
# levels(dC$ATDhealthier)

# WHAT ARE TWO MAIN THINGS YOU CONSIDER WHEN PURCHASING MEAT
# TODO: clean these variables.
# price
table(dC$CONSIDprice, exclude=NULL)
unique(dC$CONSIDprice, na.rm=FALSE)
# nutrition
table(dC$CONSIDnutrition, exclude=NULL)
unique(dC$CONSIDnutrition, na.rm=FALSE)
# hormones
table(dC$CONSIDhormones, exclude=NULL)
unique(dC$CONSIDhormones, na.rm=FALSE)
# taste
table(dC$CONSIDtaste, exclude=NULL)
unique(dC$CONSIDtaste, na.rm=FALSE)
#... TBC

# PERCEPTIONS OF ANIMAL INTELLIGENCE
intellCols <- c("intellCows", "intellPigs", "intellChicken", "intellFish", "intellHumans", "intellDogs", "intellHorses")
# apply(dC[,intellCols], MARGIN=2, FUN=function(x) table(x, exclude=NULL))
for (i in 1:length(intellCols)) {
  dC[,intellCols[i]] <- as.numeric(ordered(dC[,intellCols[i]], levels=c('Very unintelligent', 'Unintelligent', 'Somewhat unintelligent', 'Neither intelligent nor unintelligent', 'Somewhat intelligent', 'Intelligent', 'Very intelligent')))
}

# PERCEPTIONS OF ANIMAL SUFFERING
suffCols <- c("suffCows", "suffPigs", "suffChicken", "suffFish", "suffHumans", "suffDogs", "suffHorses")
# apply(dC[,suffCols], MARGIN=2, FUN=function(x) table(x, exclude=NULL))
# TODO: need to download numeric values for this variable from Qualtrics in order to clean.

# MEDIA CONSUMED
table(dC$mediaConsumed, exclude=NULL)
mediaConsumedLevels <- c('0 pieces of media', '1 piece of media',paste(seq(2,9), ' pieces of media', sep=''), '10 or more pieces of media')
dC$mediaConsumed <- as.numeric(ordered(dC$mediaConsumed, levels=mediaConsumedLevels)) - 1
# table(dC$mediaConsumed, mediaConsumed2, exclude=NULL)

# DISCUSSIONS ABOUT MEAT CONSUMPTION AND/OR FACTORY FARMING
table(dC$numDiscussions, exclude=NULL)
numDiscussionsLevels <- c('0 people', '1 person',paste(seq(2,9), ' people', sep=''), '10 or more people')
dC$numDiscussions <- as.numeric(ordered(dC$numDiscussions, levels=numDiscussionsLevels)) - 1
# table(dC$numDiscussions, numDiscussions2, exclude=NULL)

# FEELING THERMOMETER TOWARDS VEGETARIANS
table(dC$feelTherm, exclude=NULL)
# hist(dC$feelTherm)

# GOAL ELIMINATE MEAT
table(dC$goalEliminateMeat, exclude=NULL)
unique(dC$goalEliminateMeat, na.rm=FALSE)
dC$goalEliminateMeat = as.numeric(ordered(dC$goalEliminateMeat, levels=c("Very Difficult", "Difficult", "Somewhat Difficult", "Neutral", "Somewhat Easy", "Easy", "Very Easy")))
# levels(dC$goalEliminateMeat)
#plots goal with meat intake 

# GOAL REDUCE MEAT
table(dC$goalReduceMeat, exclude=NULL)
unique(dC$goalReduceMeat, na.rm=FALSE)
# converts to an orderd factor variable 
dC$goalReduceMeat = as.numeric(ordered(dC$goalReduceMeat, levels=c("Very Difficult", "Difficult", "Somewhat Difficult", "Neutral", "Somewhat Easy", "Easy", "Very Easy")))
# levels(dC$goalReduceMeat)  


# ---------------------------------------- #
#             CLEANS MTURK IDs             #
# ---------------------------------------- #

# reads in data from MTurk
mturk_batch <- read.csv(paste(INPUT_PATH, '/mturk-wave1-batch.csv', sep=''), header=TRUE,stringsAsFactors=FALSE, na.strings = c('','NA'))
dim(mturk_batch)

# subsets to MTurkIDs that are in the MTurk batch data (i.e. workers who entered a completed survey code).
df_complete <- dC[which(dC$mTurkID %in% mturk_batch$WorkerId),]
dim(df_complete)
length(unique(df_complete$mTurkID))

# DEALS WITH DUPLICATE MTURK IDS
# there are a handful of duplicate IDs. Looks like some of them are where survey was incomplete and then person came back and completed the survey later. Looks like this might also be a Qualtrics issue, sine many of the duplicates are very close in time? Our solutions is to keep the duplicate ID that completed more of the survey. Would be good to do some more detective work to figure out where these duplicates are coming from, though since there are not many it is not worth it right now.

# extracts duplicate IDs
duplicate_mturkid <- names(table(df_complete$mTurkID)[table(df_complete$mTurkID) > 1])
length(duplicate_mturkid)
# creates dataframe of duplicate IDs
df_duplicates <- df_complete[which(df_complete$mTurkID %in% duplicate_mturkid),]  
# df_duplicates[order(df_duplicates$mTurkID), c('mTurkID', 'approximatePropCompleted', 'startDate', 'endDate')]  # sorts dataframe by mTurkID

# removes duplicated MTurk IDs, keeping the value with the highest % of the survey completed.
df_complete <- df_complete[order(df_complete$approximatePropCompleted, decreasing=TRUE),]  # sorts by proportion completed (descending)
df_complete <- df_complete[!duplicated(df_complete$mTurkID),]
dim(df_complete)
length(unique(df_complete$mTurkID))

# EXAMINES EMBEDDED MTURK ID VERSUS MANUAL ENTRY
table(df_complete$MID == df_complete$mTurkID, exclude=NULL)
# table(dC$MID == dC$mTurkID, exclude=NULL)

# ---------------------------------------- #
# EXPORTS THE FINAL DATAFRAME FOR ANALYSIS #
# ---------------------------------------- #

# removes uncleaned columns
uncleaned <- c("becomeVegSupport", "becomeVegNegative", suffCols)
# colnames(dC)

df_export <- df_complete[,!colnames(df_complete) %in% uncleaned]
dim(df_export)
# hist(df_export$approximatePropCompleted, breaks=20)

write.csv(df_export, paste(OUTPUT_PATH, "/03_wave3_cleanedData.csv", sep=''), row.names=FALSE)

