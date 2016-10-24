# ------------------------------------------------- #
# ------------------------------------------------- #
# CLEANING SCRIPT FOR AWAL REDUCETARIAN STUDY       #
# WAVE 1 BASELINE SURVEY                            #
# ------------------------------------------------- #
# ------------------------------------------------- #

library(stringr)

source('other-scripts/io_options.r')

# import uncleaned dataset
if (str_detect(INPUT_PATH, 'shareable')) {
    uncD <- read.csv(paste(INPUT_PATH, '/Reducetarian_baseline_survey_wave_1.csv', sep=''), header=TRUE, stringsAsFactors=FALSE, na.strings = c('','NA'))
} else {
    all_content <- readLines(paste(INPUT_PATH, '/Reducetarian_baseline_survey_wave_1.csv', sep=''))
    skip_second <- all_content[-2]
    uncD <- read.csv(textConnection(skip_second), header=TRUE, stringsAsFactors=FALSE, na.strings = c('','NA'))

    # creates variable dictionary
    var_dict <- read.csv(textConnection(all_content[1:2]), header=FALSE, stringsAsFactors=FALSE, na.strings=c('', 'NA'))
    write.csv(t(var_dict), 'var-dicts/wave1_var_dict.csv', row.names=FALSE)
}
dim(uncD)

# DICTIONARY
# imports the definition/meaning of question #'s that appear in row 1 of "uncleanedData"
# dict <- read.csv("wave1_variableDictionary.csv", stringsAsFactors = FALSE)

# ----------------------------- #
# EXAMINES PROPORTION OF SURVEY #
# COMPLETED BY EACH RESPONDENT  #
# ----------------------------- #

# Q9.1 is a binary (1,0) variable that indicates whether someone saw the last page of the survey. 
table(uncD$Q9.1, exclude=NULL)
table(uncD$Q1.1, exclude=NULL)  # number of people who said "no, I dont want to participate in this study."

# computes (approximate) estimate for the proportion of the survey that was filled out by each respondent
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
              'Q2.2.1_1','Q2.2.1_2', 'Q2.2.1_3',  # FFQ frequency of consumption
               'Q2.2.1_4', 'Q2.2.1_5','Q2.2.1_6',
               'Q2.2.1_7', 'Q2.2.1_8', 'Q2.2.1_9',
               'Q2.2.1_10', 'Q2.2.1_11', 'Q2.2.1_12',
               'Q2.2.1_13', 'Q2.2.1_14', 
               'Q2.4',   # how often do you eat meat
               'Q2.5',  # have you ever eaten a veg diet? 
               "Q2.6_1_TEXT", # when did you start veg diet?
               "Q2.6_2_TEXT", # when did you end veg diet? 
               'Q3.1',  # do you intend to change you fruit and veg consumption in next 30 days? 
               'Q3.2',  # do you intend to change you meat consumption in next 30 days? 
               "Q4.2_1", "Q4.2_2", "Q4.2_4", "Q4.2_6", "Q4.2_7",  # attitudes towards animals 
               "Q4.3_2", "Q4.3_3", "Q4.3_4", "Q4.3_7", # two most important considerations when purchasing meat
               "Q4.3_11","Q4.3_12", "Q4.3_13", "Q4.3_13_TEXT",
               "Q5.1_1", # imagine you decided to become veg. friends and family supportive?
               "Q5.1_5", # imagine you decided to become veg. friends and family negative things?
               "Q6.1",  # feeling thermometer towards vegetarians
               "Q7.1_2", "Q7.1_3",  # rate how difficult each goal would be for you. 
               "Q8.2", # state
               "Q8.3", # gender
               "Q8.4", "Q8.4_TEXT", # education 
               "Q8.5", #year born 
               "Q8.6" # peresonal income
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
              'everEatenVeg',  # have you ever eaten a veg diet?
              "startVegDiet", # when did you start veg diet?
              "endVegDiet", # when did you end veg diet?
              "intentChangeFruitVeg", # do you intend to change you fruit and veg consumption in next 30 days? 
              "intentChangeMeat",  # do you intend to change you meat consumption in next 30 days?
              "ATDpurchContToSuff", "ATDstandOfLive", "ATDenviron", "ATDreduceTrend", "ATDhealthier", # attitudes towards animals
              "CONSIDprice", "CONSIDnutrition", "CONSIDhormones", "CONSIDstandLiving", 
              "CONSIDtaste", "CONSIDenviron", "CONSIDother", "CONSIDotherText", 
              "becomeVegSupport", # imagine you decided to become veg. friends and family supportive?
              "becomeVegNegative", # imagine you decided to become veg. friends and family negative things?
              "feelTherm",  # feeling thermometer towards vegetarians.
              "goalEliminateMeat", "goalReduceMeat",  # rate how difficult each goal would be for you. 
              "location", 
              "female", 
              "highestEDU",
              "highestEDUother",
              "yearBorn",
              "income"
              )
# cbind(origNames, newNames)

for (i in 1:length(origNames)) {
  colnames(uncD)[which(colnames(uncD)==origNames[i])] <- newNames[i]
}
# colnames(uncD)

# dC is going to be the cleaned dataframe. 
dC <- uncD[,newNames]


# ----------------------------- #
# CLEANS MAIN SURVEY QUESTIONS  #
# ----------------------------- #

# FFQ - number of servings per week.
# as an estimate for number of servings per week, I took the mid-point of each range 
# never = 0
# less than 1/week = 1
# 1-6 times/week = 3.5
# 1-3 times/day = 14
# 4 or more times/day = 28

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

# FFQ VARIABLES:
freqNames <- c('FFQfreqDairy', 'FFQfreqChicken', 'FFQfreqTurkey',
                'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef',
                'FFQfreqOther', 'FFQfreqEggs','FFQfreqFruit',
                'FFQfreqVegetables','FFQfreqNuts', 'FFQfreqBeans',
                'FFQfreqVegMeats', 'FFQfreqGrains')

# FFQ SUMMING ACROSS MEATS

# a. FFQ MEAT SUM TOTAL (multiplied by serving size)
meatNames <- c('FFQfreqChicken', 'FFQfreqTurkey', 'FFQfreqFish', 'FFQfreqPork', 'FFQfreqBeef', 'FFQfreqOther')

dC$FFQtotalSumMeat = rowSums(dC[,meatNames], na.rm=TRUE)
# rows with NA are = to 0. we don't want this, so any individual with more than 3 NAs gets an FFQtotalSumMeat of NA:
threshold <- 3
numMeatNAs <- rowSums(is.na(dC[,meatNames]))
dC$FFQtotalSumMeat[numMeatNAs >= threshold] <- NA 

# GENDER
# paste(sort(unique(dC$female, na.rm=FALSE)), collapse="\',\'")
f <- c('Cis Female', 'Cis female', 'demale', 'F','FEMALE','FEMALE ','Femail','Femaile','Femal','Femal3e','Female','Female ','Female  ','Femlae','Fenale','Fermale','Gemale', 'Trans woman', 'Woman', 'biological female', 'female', 'f','femail','femaile','female','female ', 'girl', 'woman')
m <- c('100% USDA male', 'Cis Male', 'M','MALE','MAle','Male','Male ','Male (and THANK YOU for this inclusive format!)', 'Mslr', 'Transmasculine', 'm', 'mALE', 'male','male ','male0000000000000000000000000000000000000000000000000000000000000000000000000000000000male','man')
missing <- c(as.character(seq(0,100,by=1)), "Agendered", "Gender Queer", "Genderqueer", 'Non-binary','Nonbinary', 'None', 'Unisex', 'Wisconsin', 'genderqueer', 'nonbinary, assigned female at birth', 'other', NA)

dC$female[which(dC$female %in% missing)] <- NA
dC$female[which(dC$female %in% f)] <- 1
dC$female[which(dC$female %in% m)] <- 0
dC$female = as.numeric(dC$female)
# table(dC$female, exclude=NULL)

# HOW OFTEN EAT MEAT
table(dC$howOftenEatMeat, exclude=NULL)
unique(dC$howOftenEatMeat, na.rm=FALSE)
dC$howOftenEatMeat <- as.numeric(ordered(dC$howOftenEatMeat, levels=c('Never', 'Less than once a week', 'Once a Week', '2-3 Times a Week', 'Every other day', 'Every day')))
# table(dC$howOftenEatMeat, howOftenEatMeat, exclude=NULL)

# EVER EATEN VEGAN
table(dC$everEatenVeg, exclude=NULL)
unique(dC$everEatenVeg, na.rm=FALSE)
class(dC$everEatenVeg)
dC$everEatenVeg <- ifelse(dC$everEatenVeg == 'Yes', 1, 0)

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
dC$ATDpurchContToSuff <- as.numeric(ordered(dC$ATDpurchContToSuff, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
# levels(dC$ATDpurchContToSuff)

# animals have a good standard of living
table(dC$ATDstandOfLive, exclude=NULL)
unique(dC$ATDstandOfLive, na.rm=FALSE)
dC$ATDstandOfLive <- as.numeric(ordered(dC$ATDstandOfLive, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
# levels(dC$ATDstandOfLive)

# animal ag contributes to environmental deg
table(dC$ATDenviron, exclude=NULL)
unique(dC$ATDenviron, na.rm=FALSE)
dC$ATDenviron = as.numeric(ordered(dC$ATDenviron, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
levels(dC$ATDenviron)

# more and more people are reducing their meat conusmption 
table(dC$ATDreduceTrend, exclude=NULL)
unique(dC$ATDreduceTrend, na.rm=FALSE)
dC$ATDreduceTrend = as.numeric(ordered(dC$ATDreduceTrend, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
levels(dC$ATDreduceTrend)

#  most people would be healthier
table(dC$ATDhealthier, exclude=NULL)
unique(dC$ATDhealthier, na.rm=FALSE)
dC$ATDhealthier = as.numeric(ordered(dC$ATDhealthier, levels=c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))
levels(dC$ATDhealthier)


# WHAT ARE TWO MAIN THINGS YOU CONSIDER WHEN PURCHASING MEAT
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
# TODO: clean this variable.

# FEELING THERMOMETER TOWARDS VEGETARIANS
# TODO: clean this variable.
table(dC$feelTherm, exclude=NULL)

# LOCATION
sort(table(dC$location, exclude=NULL))
# barplot(sort(table(dC$location, exclude=NULL)))
unique(dC$location, na.rm=FALSE)

# EDUCATION
table(dC$highestEDU, exclude=NULL)
unique(dC$highestEDU, na.rm=FALSE)
table(dC$highestEDUother[dC$highestEDU=='Other'], exclude=FALSE)

# cleans 'other' entries for education
sec_test <- 'GED'
current_univ_test <- '[Cc]urrent|without degree'
post_sec_test <- '[Aa]ssociate|ASSOCIATE|bachelor|AAS|2 YR|2 yr|A.S.|[Tt]echnical|Trade|Training|[Vv]ocational|culinary'
higher_test <- 'MBA|[Mm]aster'
na_test <- 'Enforcement'

dC$highestEDU[str_detect(dC$highestEDUother, sec_test) & !is.na(dC$highestEDUother)] <- "Secondary/high school"
dC$highestEDU[str_detect(dC$highestEDUother, current_univ_test) & !is.na(dC$highestEDUother)] <- "Some university-level education, without degree"
dC$highestEDU[str_detect(dC$highestEDUother, post_sec_test) & !is.na(dC$highestEDUother)] <- "Other post-secondary education, with degree"
dC$highestEDU[str_detect(dC$highestEDUother, higher_test) & !is.na(dC$highestEDUother)] <- "Higher degree (e.g. MSc, PhD)"
dC$highestEDU[str_detect(dC$highestEDUother, na_test) & !is.na(dC$highestEDUother)] <- NA
dC$highestEDU[dC$highestEDU=='Other'] <- NA
# cbind(dC$highestEDU[!is.na(dC$highestEDUother)], dC$highestEDUother[!is.na(dC$highestEDUother)])

# converts to an ordered factor
dC$highestEDU = as.numeric(ordered(dC$highestEDU, levels=c("Less than secondary/high school", "Secondary/high school", "Some university-level education, without degree", "Other post-secondary education, with degree", "University-level education, with degree (e.g. BA, BSc, ...)", "University-level education, with professional degree (e.g. Law, Medicine, ...)", "Higher degree (e.g. MSc, PhD)")))
# levels(dC$highestEDU)

# INCOME
table(dC$income, exclude=NULL)
unique(dC$income, na.rm=FALSE)

# converts to ordered factor
incomeNumeric = as.numeric(ordered(dC$income)) - 1
table(dC$income, incomeNumeric)  # levels are in correct order by default.
dC$income <- incomeNumeric  # replaces character version of income with numeric indicator

levels(dC$income) # levels are already correct 

# AGE
table(dC$yearBorn, exclude=NULL)
unique(dC$yearBorn, na.rm=FALSE)
class(dC$yearBorn)

# GOAL ELIMINATE MEAT
table(dC$goalEliminateMeat, exclude=NULL)
unique(dC$goalEliminateMeat, na.rm=FALSE)
dC$goalEliminateMeat = as.numeric(ordered(dC$goalEliminateMeat, levels=c("Very Difficult", "Difficult", "Somewhat Difficult", "Neutral", "Somewhat Easy", "Easy", "Very Easy")))
# levels(dC$goalEliminateMeat)

# GOAL REDUCE MEAT
table(dC$goalReduceMeat, exclude=NULL)
unique(dC$goalReduceMeat, na.rm=FALSE)
dC$goalReduceMeat = as.numeric(ordered(dC$goalReduceMeat, levels=c("Very Difficult", "Difficult", "Somewhat Difficult", "Neutral", "Somewhat Easy", "Easy", "Very Easy")))
levels(dC$goalReduceMeat)  


# ---------------------------------------- #
#             CLEANS MTURK IDs             #
# ---------------------------------------- #

# reads in data from MTurk
mturk_batch <- read.csv(paste(INPUT_PATH, '/mturk-wave1-batch.csv', sep=''), header=TRUE,stringsAsFactors=FALSE, na.strings = c('','NA'))
dim(mturk_batch)

# subsets to MTurkIDs that are in the MTurk batch data (i.e. workers who entered a completed survey code).
df_complete <- dC[which(dC$mTurkID %in%mturk_batch$WorkerId),]
dim(df_complete)
length(unique(df_complete$mTurkID))

# DEALS WITH DUPLICATE MTURK IDS
# there are a handful of duplicate IDs. Looks like this might be a Qualtrics issue, sine many of the duplicates are very close in time? Our solutions is to keep the duplicate ID that completed more of the survey. Would be good to do some more detective work to figure out where these duplicates are coming from, though since there are not many it is not a high priority.

# extracts duplicate IDs
duplicate_mturkid <- names(table(df_complete$mTurkID)[table(df_complete$mTurkID) > 1])
length(duplicate_mturkid)
# creates dataframe of duplicate IDs
df_duplicates <- df_complete[which(df_complete$mTurkID %in% duplicate_mturkid),]  

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
uncleaned <- c("becomeVegSupport", "becomeVegNegative")

df_export <- df_complete[,!colnames(df_complete) %in% uncleaned]
dim(df_export)

write.csv(df_export, paste(OUTPUT_PATH, "/01_wave1_cleanedData.csv", sep=''), row.names=FALSE)

