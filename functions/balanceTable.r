# ----------------------------------------- #
#           RANDOMIZED VILLAGES             #
# ----------------------------------------- #

# --------------------------------- #
# Function: balanceTable
# ---------------------------
# This function creates a balance table. The first two columns are
# mean and N for treatment group, next two columns are mean and N
# for control group. Final two columns are mean difference and p-value.
#
# Parameters:
# data: a dataframe.
# covariates: vector of strings containing covariate names to use
# in balance table.
# treatment: name of treatment variable.
# exp_arms: vector of length 2 representing which values in the treatment variable refer to the control group and which values refer to the treatment group. Defaults to c(0,1).
# rownames: vector of strings representing row names to use.
# tolatex: default=False. Set to True if you want the function to return a latex table. Otherwise, will return a dataframe containing the results.
# --------------------------------- #

balanceTable <- function(data, covariates, treatment, exp_arms=c(0,1), digits=3, rownames=NULL) {
    expBalance <- as.data.frame(matrix(NA, nrow=length(covariates), ncol=6))
    colnames(expBalance) <- c("Mean", "N", "Mean", "N","Mean diff.", "p-value")
    # filter data to two experimental arms.
    data <- data[data[,treatment] == exp_arms[1] | data[,treatment] == exp_arms[2], ]
    for (i in 1:length(covariates)){
        meanTreated <- mean(data[data[,treatment]==exp_arms[2],covariates[i]], na.rm=T)
        nTreated <- length(na.omit(data[data[,treatment]==exp_arms[2],covariates[i]]))
        meanControl <- mean(data[data[,treatment]==exp_arms[1],covariates[i]], na.rm=T)
        nControl <- length(na.omit(data[data[,treatment]==exp_arms[1],covariates[i]]))
        meanDiff <- meanTreated - meanControl
        ttestDiff <- t.test(data[,covariates[i]]~data[,treatment], var.equal=FALSE)
        
        expBalance[i, ] <- c(meanTreated, nTreated, meanControl, nControl,meanDiff, ttestDiff$p.value)
    }
    rownames(expBalance) <- covariates
    if (!is.null(rownames)) {
        rownames(expBalance) <- rownames
    }
    
    #print(aggregate(data[,covariates], by=list(data$wave2_experiment_assigned), FUN=function(x) mean(x, na.rm=T)), digits=3)
    expBalance <- round(expBalance, digits=digits)
    return(expBalance)
}

# --------------------------------- #
# Function: latexBalanceTable
# ---------------------------
# This function takes in a balance table in the form of a dataframe
# and returns the balance table in latex format.
# --------------------------------- #
latexBalanceTable <- function(dataframe, file, ...) {
    latex_balance <- latex(dataframe,
          file=file,
          cgroup=c('Treatment', 'Control', 'Difference'),
          colheads=colnames(dataframe),
          n.cgroup=c(2,2,2),
          col.just=rep('c', 6),
          ...)
    return(latex_balance)
}