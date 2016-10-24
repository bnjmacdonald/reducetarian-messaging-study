# File: variables.r
# This file defines groups of variables.

# Other variables of interest:
# treatment: string representing experimental arm.
# block_id: integer representing randomized block ID
# mTurkID: hashed ID representing respondent unique identifier

source('functions/utils.r')

clean_names <- list(FFQfreqDairy='Dairy',
                    FFQfreqChicken='Chicken',
                    FFQfreqTurkey='Turkey',
                    FFQfreqFish='Fish',
                    FFQfreqPork='Pork',
                    FFQfreqBeef='Beef',
                    FFQfreqOther='Other meat',
                    FFQfreqEggs='Egg',
                    FFQfreqVegMeats='Veg meat',
                    FFQtotalSumMeat='Total meat servings (FFQ)',
                    howOftenEatMeat='How often eat meat',
                    ATDpurchContToSuff='Contributes to suffering',
                    ATDstandOfLive='Animals have good std of living',
                    ATDenviron='Contributes to env. degradation',
                    ATDreduceTrend='Americans reducing meat consumption',
                    ATDhealthier='People healthier with less meat',
                    feelTherm='Feeling thermometer',
                    intentChangeMeat='Intent to change meat consumption',
                    intellCows='Cows perceived intelligence',
                    intellPigs='Pigs perceived intelligence',
                    intellChicken='Chicken perceived intelligence',
                    intellFish='Fish perceived intelligence',
                    intellHumans='Humans perceived intelligence',
                    intellDogs='Dogs perceived intelligence',
                    intellHorses='Horses perceived intelligence',
                    mediaConsumed='num pieces media consumed',
                    numDiscussions='num discussions',
                    goalEliminateMeat='Perceived ease eliminate meat',
                    goalReduceMeat='Perceived ease reduce meat 25%',
                    intentChangeFruitVeg='Intent to change fruit/veg consumption',
                    FFQfreqFruit='Fruit servings (FFQ)',
                    FFQfreqVegetables='Veg servings (FFQ)',
                    FFQfreqNuts='Nuts servings (FFQ)',
                    FFQfreqBeans='Beans servings (FFQ)',
                    FFQfreqGrains='Grain servings (FFQ)')

ffq_meat_raw_vars <- c('FFQfreqDairy', 
                       'FFQfreqChicken',
                       'FFQfreqTurkey',
                       'FFQfreqFish',
                       'FFQfreqPork',
                       'FFQfreqBeef',
                       'FFQfreqOther',
                       'FFQfreqEggs',
                       'FFQfreqVegMeats')

ffq_meat_agg_vars <- c('FFQtotalSumMeat')

how_often_meat_vars <- c('howOftenEatMeat')

attitude_vars <- c("ATDpurchContToSuff",
                   "ATDstandOfLive",
                   "ATDenviron",
                   "ATDhealthier",
                   "feelTherm",
                   "ATDreduceTrend")

intent_vars <- c("intentChangeMeat")  # "CONSIDstandLiving"

intell_vars <- c("intellCows",
                 "intellPigs",
                 "intellChicken",
                 "intellFish",
                 "intellHumans",
                 "intellDogs",
                 "intellHorses")

exposure_vars <- c('mediaConsumed',
                   'numDiscussions')

difficulty_vars <- c("goalEliminateMeat",
                     "goalReduceMeat")

# suff_vars <- c("suffCows", "suffPigs", "suffChicken", "suffFish", "suffHumans", "suffDogs", "suffHorses")
# support_vars <- c("becomeVegSupport", "becomeVegNegative")

placebo_intent_vars <- c('intentChangeFruitVeg')
placebo_ffq_vars <- c('FFQfreqFruit', 'FFQfreqVegetables','FFQfreqNuts', 'FFQfreqBeans')
other_vars <- c('FFQfreqGrains')

outcome_set_list <- list(main=add_suffixes(ffq_meat_agg_vars),
                         main_check=add_suffixes(how_often_meat_vars),
                         intent=add_suffixes(intent_vars),
                         meat_breakdown=add_suffixes(ffq_meat_raw_vars),
                         attitude=add_suffixes(attitude_vars),
                         intelligence=intell_vars,
                         difficulty=add_suffixes(difficulty_vars),
                         exposure=exposure_vars,
                         placebo_ffq=add_suffixes(placebo_ffq_vars),
                         placebo_intent=add_suffixes(placebo_intent_vars))

# if (length(do.call('c',outcome_set_list)) != length(clean_names)) stop('Clean names not same length as outcome set lists.')

baseline_only_vars <- c()
both_wave_vars <- c(ffq_meat_raw_vars, ffq_meat_agg_vars, how_often_meat_vars, attitude_vars, intent_vars, difficulty_vars, placebo_intent_vars, placebo_ffq_vars, other_vars)
endline_only_vars <- c(intell_vars, exposure_vars)

baseline_vars <- paste(both_wave_vars, '.1', sep='')
endline_vars <- c(paste(both_wave_vars, '.3', sep=''), endline_only_vars)
change_vars <- paste(both_wave_vars, '_chg', sep='')

all_vars <- c(baseline_vars, endline_vars, change_vars)


clean_names.1 <- list()
clean_names.3 <- list()
clean_names_chg <- list()
clean_names.chg <- list()
for (i in 1:length(clean_names)) {
    if (names(clean_names)[[i]] %in% endline_only_vars) {
        clean_name.3 <- paste(clean_names[[i]], ' (endline)', sep='')
        var_name.3 <- paste(names(clean_names)[[i]], '', sep='')
        clean_names.3[[i]] <- clean_name.3
        names(clean_names.3)[[i]] <- var_name.3
    } else {
        clean_name.1 <- paste(clean_names[[i]], ' (baseline)', sep='')
        var_name.1 <- paste(names(clean_names)[[i]], '.1', sep='')
        clean_names.1[[i]] <- clean_name.1
        names(clean_names.1)[[i]] <- var_name.1

        clean_name.3 <- paste(clean_names[[i]], ' (endline)', sep='')
        var_name.3 <- paste(names(clean_names)[[i]], '.3', sep='')
        clean_names.3[[i]] <- clean_name.3
        names(clean_names.3)[[i]] <- var_name.3
        
        clean_name_chg <- paste(clean_names[[i]], ' (change)', sep='')
        var_name_chg <- paste(names(clean_names)[[i]], '_chg', sep='')
        clean_names_chg[[i]] <- clean_name_chg
        names(clean_names_chg)[[i]] <- var_name_chg

        clean_name.chg <- paste(clean_names[[i]], ' (change)', sep='')
        var_name.chg <- paste(names(clean_names)[[i]], '.chg', sep='')
        clean_names.chg[[i]] <- clean_name.chg
        names(clean_names.chg)[[i]] <- var_name.chg
    }
}
clean_names <- c(clean_names.1, clean_names.3, clean_names_chg, clean_names.chg)
