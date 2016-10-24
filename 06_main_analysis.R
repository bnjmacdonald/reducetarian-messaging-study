# ------------------------------------------------- #
# ------------------------------------------------- #
# File: 06-main-analysis.R
# This file contains the main treatment effect 
# analyses.
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
library(Hmisc)
library(reshape2)
library(ggplot2)

# constants for plots
dpi <- 300
height <- 16
height_per_subplot <- 2.5
width <- 12
theme_set(theme_bw())

source('other-scripts/io_options.r')
source('other-scripts/variables.r')
source('functions/balanceTable.R')
source('functions/effectTable.r')
source('functions/coefPlots.R')
source('functions/wfdr.R')


# reads in merged survey data from all waves.
data <- read.csv(paste(INPUT_PATH, '/all_waves_cleaned.csv', sep=''), stringsAsFactors=FALSE)
# str(data)
# colnames(data)
dim(data)

data$block_id <- as.factor(data$block_id)  # converts block_id to factor.
data$treatment[data$treatment=='veg'] <- 'eliminate'  # renames 'veg' treatment arm
# table(data[,'treatment'])

# ------------------------- #
#       BALANCE TABLE       #
# ------------------------- #

# list of variables to use in balance checks
balance_vars <- c(baseline_vars)
rownames <- unlist(clean_names[balance_vars])
cbind(balance_vars, rownames)
digits <- 2

# creates balance tables.
balance_table_reduce <- balanceTable(data, balance_vars, treatment='treatment', exp_arms=c('control', 'reduce'), digits=digits, rownames=rownames)
balance_table_veg <- balanceTable(data, balance_vars, treatment='treatment', exp_arms=c('control', 'eliminate'), digits=digits, rownames=rownames)

# saves balance tables to file.
latexBalanceTable(balance_table_reduce, file="tables/balance_reduce.tex", longtable=FALSE, where='ht', caption='Balance table for reduce appeal', rowlabel='')
latexBalanceTable(balance_table_veg, file="tables/balance_veg.tex", longtable=FALSE, where='ht', caption='Balance table for eliminate appeal', rowlabel='')


# ------------------------- #
#       MAIN EFFECTS        #
# ------------------------- #

# computes a table of means by treatment groups.
# ddply(data[,c('treatment', main_outcome_vars)], .(treatment), colwise(mean), na.rm=TRUE)

groups = c('control', 'reduce', 'eliminate')
fig_output_dir <- 'figures/'
tab_output_dir <- 'tables/'
resultTable_list <- rep(list(list()), length(outcome_set_list))  # all results will be stored in this list.
names(resultTable_list) <- names(outcome_set_list)

# CREATES TABLES OF EFFECTS
# NOTE: this loop is unnecessarily slow because of the inclusion of
# block covariates, which dramatically slow down the implementation 
# of vcovHC() in effectTable().
for (i in 1:length(outcome_set_list)) {  # for each set of outcomes (see 'variables.r')...
    this_outcome_set <- outcome_set_list[[i]]
    this_set_name <- names(outcome_set_list)[i]
    # computes treatment effect table for 'reduce' message (without block covariates)
    this_effectTable_reduce <- effectTable(outcomes=this_outcome_set, 
                                    treatment='treatment', 
                                    covariates=vector(), 
                                    data=data,
                                    se="HC0",
                                    family="gaussian",
                                    effectRow=3,
                                    includeMeans=TRUE,
                                    exp_arms=groups[c(1,2)])

    # computes treatment effect table for 'reduce' message (with block covariates)
    this_effectTable_reduce_block <- effectTable(outcomes=this_outcome_set, 
                                    treatment='treatment', 
                                    covariates=c('block_id'), 
                                    data=data,
                                    se="HC0",
                                    family="gaussian",
                                    effectRow=3,
                                    includeMeans=TRUE,
                                    exp_arms=groups[c(1,2)])

    # computes treatment effect table for 'eliminate' message (without block covariates)
    this_effectTable_veg <- effectTable(outcomes=this_outcome_set, 
                                    treatment='treatment',
                                    covariates=vector(),
                                    data=data,
                                    se="HC0",
                                    family="gaussian",
                                    effectRow=2,
                                    includeMeans=TRUE,
                                    exp_arms=groups[c(1,3)])

    # computes treatment effect table for 'eliminate' message (with block covariates)
    this_effectTable_veg_block <- effectTable(outcomes=this_outcome_set, 
                                    treatment='treatment',
                                    covariates=c('block_id'),
                                    data=data,
                                    se="HC0",
                                    family="gaussian",
                                    effectRow=2,
                                    includeMeans=TRUE,
                                    exp_arms=groups[c(1,3)])

    # minor fix to table rownames.
    rownames(this_effectTable_reduce$resultTable) <- gsub('_', '.', rownames(this_effectTable_reduce$resultTable))
    rownames(this_effectTable_reduce_block$resultTable) <- gsub('_', '.', rownames(this_effectTable_reduce_block$resultTable))
    rownames(this_effectTable_veg$resultTable) <- gsub('_', '.', rownames(this_effectTable_veg$resultTable))
    rownames(this_effectTable_veg_block$resultTable) <- gsub('_', '.', rownames(this_effectTable_veg_block$resultTable))

    # save results to a list.
    resultTable_list[[i]] <- list(this_effectTable_reduce$resultTable, this_effectTable_reduce_block$resultTable, this_effectTable_veg$resultTable, this_effectTable_veg_block$resultTable)
    print(paste('Finished producing results for: ', this_outcome_set, sep=''))
}
save(resultTable_list, file=paste(OUTPUT_PATH, '/resultTable_list.Rdata', sep=''))
# load('results/resultTable_list.Rdata')

# exports treatment effect tables to latex
cols_to_keep <- seq(1,5, by=1)
digits <- 2
for (i in 1:length(outcome_set_list)) {
    this_outcome_set <- outcome_set_list[[i]]
    this_set_name <- names(outcome_set_list)[i]
    these_resultTables <- resultTable_list[[i]]
    tab_filename_reduce <- paste(this_set_name, 'reduce.tex', sep='_')
    tab_filename_reduce_block <- paste(this_set_name, 'reduce_block.tex', sep='_')
    tab_filename_veg <- paste(this_set_name, 'veg.tex', sep='_')
    tab_filename_veg_block <- paste(this_set_name, 'veg_block.tex', sep='_')
    
    # cleans rownames of result tables.
    these_resultTables <- lapply(these_resultTables, FUN=function(x) {
        rownames(x) <- unlist(clean_names[rownames(x)])
        return(x)
    })

    this_set_name <- gsub('_', ' ', this_set_name)
    latexCoefTable(dataframe=these_resultTables[[1]][,cols_to_keep], 
                   file=paste(tab_output_dir, tab_filename_reduce, sep=''),
                   longtable=FALSE,
                   where='ht',
                   caption=paste('Reduce appeal effects on ', this_set_name, ' outcomes (without blocking)', sep=''),
                   ncols=length(cols_to_keep)+1,
                   digits=digits,
                   single_col=TRUE)
    latexCoefTable(dataframe=these_resultTables[[2]][,cols_to_keep], 
                   file=paste(tab_output_dir, tab_filename_reduce_block, sep=''),
                   longtable=FALSE,
                   where='ht',
                   caption=paste('Reduce appeal effects on ', this_set_name, ' outcomes (with blocking)', sep=''),
                   ncols=length(cols_to_keep)+1,
                   digits=digits,
                   single_col=TRUE)
    latexCoefTable(dataframe=these_resultTables[[3]][,cols_to_keep], 
                   file=paste(tab_output_dir, tab_filename_veg, sep=''),
                   longtable=FALSE,
                   where='ht',
                   caption=paste('Eliminate appeal effects on ', this_set_name, ' outcomes (without blocking)', sep=''),
                   ncols=length(cols_to_keep)+1,
                   digits=digits,
                   single_col=TRUE)
    latexCoefTable(dataframe=these_resultTables[[4]][,cols_to_keep], 
                   file=paste(tab_output_dir, tab_filename_veg_block, sep=''),
                   longtable=FALSE,
                   where='ht',
                   caption=paste('Eliminate appeal effects on ', this_set_name, ' outcomes (with blocking)', sep=''),
                   ncols=length(cols_to_keep)+1,
                   digits=digits,
                   single_col=TRUE)
}


# CREATES COEFFICIENT PLOTS
for (i in 1:length(outcome_set_list)) {
    this_outcome_set <- outcome_set_list[[i]]
    this_set_name <- names(outcome_set_list)[i]
    these_resultTables <- resultTable_list[[i]]

    # constructs dataframe for ggplot
    data_reduce <- these_resultTables[[2]]
    data_reduce$treatment <- 'reduce'
    data_reduce$variable <- unlist(clean_names[rownames(data_reduce)])
    data_veg <- these_resultTables[[4]]
    data_veg$treatment <- 'eliminate'
    data_veg$variable <- unlist(clean_names[rownames(data_veg)])
    data_to_plot <- rbind(data_reduce, data_veg, make.row.names=FALSE)
    ncol <- 1  # number of columns in the graphic.
    this_height <- height_per_subplot*nrow(data_to_plot)/2
    this_width <- width
    if (any(data_veg$variable != data_reduce$variable)) stop('Variables names will not match plot sub-figures...')
    if (this_set_name %in% c('main', 'main_check', 'intelligence', 'exposure')) {
        this_height <- height_per_subplot*nrow(data_to_plot)
        if (this_set_name %in% c('intelligence')) {
            ncol <- 2
            this_width <- width * 1.5
            this_height <- height
        }
        fig_filename <- paste(this_set_name, '_coefplot.png', sep='')
        p <- ggcoefPlot(data=data_to_plot,
                        treatment='treatment',
                        effect='Effect',
                        se='SE',
                        n='# obs',
                        ncol=ncol,
                        coordFlip=TRUE,
                        hlineZero=TRUE)
        ggsave(filename=paste(fig_output_dir, fig_filename, sep=''), p, width=this_width, height=this_height, units="cm", dpi=dpi)
        print(paste('Saved to file: ', fig_filename, sep=''))
        next
    } else if (this_set_name %in% 'meat_breakdown') {
        ncol <- 3
        this_height <- height
    } else if (nrow(data_to_plot) > 10) {
        ncol <- 2
        this_height <- height_per_subplot*nrow(data_to_plot)/4
        this_width <- width * 1.5
    } 
    # creates plot with endline outcomes.
    p3 <- ggcoefPlot(data=data_to_plot[grep('\\(endline\\)', data_to_plot$variable),],
                    treatment='treatment',
                    effect='Effect',
                    se='SE',
                    n='# obs',
                    ncol=ncol,
                    coordFlip=TRUE,
                    hlineZero=TRUE)
    fig_filename_3 <- paste(this_set_name, '_coefplot_3.png', sep='')
    ggsave(filename=paste(fig_output_dir, fig_filename_3, sep=''), p3, width=this_width, height=this_height, units="cm", dpi=dpi)
    print(paste('Saved to file: ', fig_filename_3, sep=''))

    # creates plot with change outcomes.
    pchg <- ggcoefPlot(data=data_to_plot[grep('\\(change\\)', data_to_plot$variable),],
                    treatment='treatment',
                    effect='Effect',
                    se='SE',
                    n='# obs',
                    ncol=ncol,
                    coordFlip=TRUE,
                    hlineZero=TRUE)
    fig_filename_chg <- paste(this_set_name, '_coefplot_chg.png', sep='')
    ggsave(filename=paste(fig_output_dir, fig_filename_chg, sep=''), pchg, width=this_width, height=this_height, units="cm", dpi=dpi)
    print(paste('Saved to file: ', fig_filename_chg, sep=''))
}


# CREATES HISTOGRAMS FOR MAIN OUTCOMES
# plots the distributions of the main outcome variables by treatment group
data_melted <- melt(data[, c('treatment', outcome_set_list$main)], id='treatment')
p <- ggplot(data_melted, aes(x=value, fill=treatment)) + 
    facet_grid(treatment~variable) + 
    geom_density(alpha=0.5) + 
    labs(title=NULL) + # , x='Change in total meat servings consumed in past 30 days' # 'FFQ total meat servings consumed in past 30 days, by treatment group'
    theme(legend.position="bottom", legend.direction="horizontal", text = element_text(size=11), plot.margin = unit(c(0,0,0,0), 'cm'))
fig_filename <- 'main_outcomes_distribution.png'
ggsave(filename=paste(fig_output_dir, fig_filename, sep=''), p, width=width, height=height, units="cm", dpi=dpi)


# CREATES PLOTS SHOWING MEANS OF EACH EXPERIMENTAL ARM.
# NOTE: these plots were not finalized and are not shown 
# in the working paper. Hence, they may contain some errors
# that need to be corrected.
for (i in 1:length(outcome_set_list)) {
    this_outcome_set <- outcome_set_list[[i]]
    this_set_name <- names(outcome_set_list)[i]
    if (this_set_name %in% c('attitude', 'meat_breakdown', 'difficulty', 'placebo_ffq')) {
        # plots the attitude results
        p <- ggmeanPlot(data=data,
                        treatment='treatment', 
                        outcomes=this_outcome_set[grep('.3', this_outcome_set)],
                        xticks=groups,
                        legendNames=groups,
                        coordFlip=FALSE,
                        showData=FALSE,
                        hlineZero=FALSE,
                        extendRangeBy=NULL)
        fig_filename <- paste(this_set_name, '_3.png', sep='')
        ggsave(filename=paste(fig_output_dir, fig_filename, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
        print(paste('Saved to file: ', fig_filename, sep=''))

        p <- ggmeanPlot(data=data,
                        treatment='treatment',
                        outcomes=this_outcome_set[grep('_chg', this_outcome_set)],
                        xticks=groups,
                        legendNames=groups,
                        coordFlip=FALSE,
                        showData=FALSE,
                        hlineZero=TRUE,
                        extendRangeBy=NULL)
        fig_filename <- paste(this_set_name, '_chg.png', sep='')
        ggsave(filename=paste(fig_output_dir, fig_filename, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
        print(paste('Saved to file: ', fig_filename, sep=''))
    } else if (this_set_name %in% c('intelligence')) {
        p <- ggmeanPlot(data=data,
                        treatment='treatment', 
                        outcomes=this_outcome_set,
                        xticks=groups,
                        legendNames=groups,
                        coordFlip=FALSE,
                        showData=FALSE,
                        hlineZero=FALSE,
                        extendRangeBy=NULL,
                        freey=TRUE)
        fig_filename <- paste(this_set_name, '_3.png', sep='')
        ggsave(filename=paste(fig_output_dir, fig_filename, sep=''), p, width=width, height=height, units="cm", dpi=dpi)
        print(paste('Saved to file: ', fig_filename, sep=''))

    } else if (this_set_name %in% c('exposure')) {
        p <- ggmeanPlot(data=data,
                        treatment='treatment', 
                        outcomes=this_outcome_set,
                        xticks=groups,
                        legendNames=groups,
                        coordFlip=FALSE,
                        showData=FALSE,
                        hlineZero=FALSE,
                        extendRangeBy=NULL)
        fig_filename <- paste(this_set_name, '_3.png', sep='')
        ggsave(filename=paste(fig_output_dir, fig_filename, sep=''), p, width=width, height=height-8, units="cm", dpi=dpi)
        print(paste('Saved to file: ', fig_filename, sep=''))
    } else {
      # plots the (non-attitude) results.
      for (outcome in this_outcome_set) {
          hlineZero <- grepl('_chg', outcome)
          p <- ggmeanPlot(data=data,
                          treatment='treatment',
                          outcomes=outcome,
                          xticks=groups,
                          legendNames=groups,
                          coordFlip=TRUE,
                          showData=FALSE,
                          hlineZero=hlineZero,
                          extendRangeBy=1.25)
          fig_filename <- paste(paste(this_set_name, outcome, sep='_'), '.png', sep='')
          fig_filename <- gsub('.3', '_3', fig_filename)
          ggsave(filename=paste(fig_output_dir, fig_filename, sep=''), p, width=width, height=height-8, units="cm", dpi=dpi)
          print(paste('Saved to file: ', fig_filename, sep=''))
      }
    }
}


# ------------------------- #
#    P-VALUE CORRECTIONS    #
# ------------------------- #
# load('results/resultTable_list.Rdata')

extract_pvalues <- function(list, index) {
    pvalues <- do.call('rbind', lapply(list, FUN=function(l) {
        cbind(rownames(l[[index]]),l[[index]][,'p-value'])
    }))
    pvalues <- data.frame(pvalues)
    colnames(pvalues) <- c('variable', 'pvalue')
    pvalues$pvalue <- as.numeric(as.character(pvalues$pvalue))
    pvalues$variable <- as.character(pvalues$variable)
    return(pvalues)
}

reduce_pvalues <- extract_pvalues(resultTable_list, 2)
veg_pvalues <- extract_pvalues(resultTable_list, 4)

wfdr_wrapper <- function(data, pvalue_col, alpha=0.05, w=NULL, sort=TRUE) {
    wfdr_result <- wfdr(data[,pvalue_col], alpha, w)
    names
    result <- cbind(data, wfdr_result)
    colnames(result) <- c(colnames(data), 'rejected')
    if (sort) {
    result <- result[order(-result$rejected, result[, pvalue_col]),]
    } 
    return(result)
}

get_weights <- function(data, n_important, important_total_prop) {
    n_tests <- nrow(data)
    important_weight <- n_tests*important_total_prop/n_important
    important_weights <- rep(important_weight, n_important)
    sum_important <- sum(important_weights)
    n_unimportant <- nrow(data) - n_important
    unimportant_weight <- (n_tests)*(1-important_total_prop)/n_unimportant
    unimportant_weights <- rep(unimportant_weight, n_unimportant)
    weights <- c(important_weights, unimportant_weights)
    # print(weights)
    if (sum(weights)!=n_tests) stop('Sum of weights must equal number of tests.')
    return(weights)
}

sort <- TRUE
reduce_weights <- get_weights(reduce_pvalues, 2, 0.5)
veg_weights <- get_weights(veg_pvalues, 2, 0.5)
reduce_corrected <- wfdr_wrapper(reduce_pvalues, 'pvalue', alpha=0.05, w=reduce_weights, sort=sort)
veg_corrected <- wfdr_wrapper(veg_pvalues, 'pvalue', alpha=0.05, w=veg_weights, sort=sort)

rownames(reduce_corrected) <- unlist(clean_names[reduce_corrected$variable])
reduce_corrected$variable <- NULL
rownames(veg_corrected) <- unlist(clean_names[veg_corrected$variable])
veg_corrected$variable <- NULL

reduce_corrected$rejected <- ifelse(reduce_corrected$rejected==TRUE, 'Yes', 'No')
veg_corrected$rejected <- ifelse(veg_corrected$rejected==TRUE, 'Yes', 'No')

topn <- 30
ncols <- 3
reduce_wfdr_corrections_filepath <- paste(tab_output_dir, 'reduce_wfdr_corrections.tex', sep='')
latex(reduce_corrected[1:topn,], 
        file=reduce_wfdr_corrections_filepath,
        longtable=FALSE,
        where='ht',
        caption='Results of multiple hypothesis testing corrections using weighted FDR (reduce appeal effects)',
        col.just=rep('c', ncols),
        colheads=colnames(reduce_corrected),
        booktabs=FALSE)
file_str <- paste(readLines(reduce_wfdr_corrections_filepath), collapse="\n")
file_str <- gsub("\\{table\\}", "\\{table\\*\\}", file_str)
write(file_str, reduce_wfdr_corrections_filepath)

veg_wfdr_corrections_filepath <- paste(tab_output_dir, 'veg_wfdr_corrections.tex', sep='')
latex(veg_corrected[1:topn,], 
        file=veg_wfdr_corrections_filepath,
        longtable=FALSE,
        where='ht',
        caption='Results of multiple hypothesis testing corrections using weighted FDR (eliminate appeal effects)',
        col.just=rep('c', ncols),
        colheads=colnames(veg_corrected),
        booktabs=FALSE)
file_str <- paste(readLines(veg_wfdr_corrections_filepath), collapse="\n")
file_str <- gsub("\\{table\\}", "\\{table\\*\\}", file_str)
write(file_str, veg_wfdr_corrections_filepath)


# --------------------------- #
#       SANITY CHECKS         #
# --------------------------- #

# double-checks that result table was constructed correctly for main outcomes.
this_result_table_reduce <- resultTable_list[[1]][[2]]
this_result_table_eliminate <- resultTable_list[[1]][[4]]
control_mean1 <- mean(data[data$treatment == 'control', outcome_set_list[[1]][1]], na.rm=TRUE)
control_mean2 <- mean(data[data$treatment == 'control', outcome_set_list[[1]][2]], na.rm=TRUE)
reduce_mean1 <- mean(data[data$treatment == 'reduce', outcome_set_list[[1]][1]], na.rm=TRUE)
reduce_mean2<- mean(data[data$treatment == 'reduce', outcome_set_list[[1]][2]], na.rm=TRUE)
eliminate_mean1 <- mean(data[data$treatment == 'eliminate', outcome_set_list[[1]][1]], na.rm=TRUE)
eliminate_mean2 <- mean(data[data$treatment == 'eliminate', outcome_set_list[[1]][2]], na.rm=TRUE)


mean_test <- control_mean1 == this_result_table_reduce[1, 'Control mean'] &
            control_mean2 == this_result_table_reduce[2, 'Control mean'] &
            reduce_mean1 == this_result_table_reduce[1, 'Treated mean'] &
            reduce_mean2 == this_result_table_reduce[2, 'Treated mean'] &
            control_mean1 == this_result_table_eliminate[1, 'Control mean'] &
            control_mean2 == this_result_table_eliminate[2, 'Control mean'] &
            eliminate_mean1 == this_result_table_eliminate[1, 'Treated mean'] &
            eliminate_mean2 == this_result_table_eliminate[2, 'Treated mean']

if (!mean_test) stop('Effect tables were not constructed correctly. Saved results will not be valid.')

formula1 <- formula(paste(outcome_set_list[[1]][1], '~ treatment + block_id', sep=''))
glm1 <- glm(formula1, data=data)
vcov1 <- vcovHC(glm1, type="HC0")
glmOutRobust1 <- coeftest(glm1, vcov = vcov1)
formula2 <- formula(paste(outcome_set_list[[1]][2], '~ treatment + block_id', sep=''))
glm2 <- glm(formula2, data=data)
vcov2 <- vcovHC(glm2, type="HC0")
glmOutRobust2 <- coeftest(glm2, vcov = vcov2)

reduce_effect1 <- round(glmOutRobust1['treatmentreduce','Estimate'], 3)
reduce_effect2 <- round(glmOutRobust2['treatmentreduce','Estimate'], 3)
eliminate_effect1 <- round(glmOutRobust1['treatmenteliminate','Estimate'], 3)
eliminate_effect2 <- round(glmOutRobust2['treatmenteliminate','Estimate'], 3)

effect_test <- reduce_effect1 == this_result_table_reduce[1, 'Effect'] &
                reduce_effect2 == this_result_table_reduce[2, 'Effect'] &
                eliminate_effect1 == this_result_table_eliminate[1, 'Effect'] &
                eliminate_effect2 == this_result_table_eliminate[2, 'Effect']

if (!effect_test) stop('Effect tables were not constructed correctly. Saved results will not be valid.')
