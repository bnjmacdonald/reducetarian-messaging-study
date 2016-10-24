# ----------------------------------------- #
# Function: effectTable
# --------------------- 
# This function creates a table of treatment effects for a vector of outcome variables. 
# Given a vector of outcome variables, a treatment, and a vector of covariates, this 
# function computes the treatment effect from a linear regression for each outcome 
# variable. 
# 
# Parameters:
# outcomes: string vector of outcome variable names.
# treatment: string of treatment variable name.
# covariates: string vector of covariate variable names.
# data: dataframe containing outcomes, treatment, and covariates.
# family: family type as input to glm function. see help(family).
# se: what standard error correction to use. Currently, only HC0, HC1, and clustered 
# standard errors are available.
# cluster: what variable to cluster on if clustered standard errors are requested.
# effectRow: numeric row number from regression table which is the treatment effect of interest.
# rowNames: string vector containing names of rows.
# includeMeans: boolean value for whether to include treatment and control group means in the table.
# exp_arms: vector of length 2 representing which values in the treatment variable refer to the control group and which values refer to the treatment group. Defaults to c(0,1). Second value should be for treatment group.
# ----------------------------------------- #


effectTable <- function(outcomes, treatment, covariates=vector(), data, 
                    family="gaussian", se = "HC0", cluster="",
                    effectRow, rowNames=outcomes, includeMeans=FALSE, exp_arms=c(0, 1)){
    # initializes a table to store the results.
    resultTable <- initResultTable(outcomes, treatment, data, includeMeans, exp_arms)
    glmOutputs <- list()
    robustOutputs <- list()
    robustVcovs <- list()

    # for each outcome, runs a regression with that outcome and stores the results 
    # in resultTable.
    for (i in 1:length(outcomes)){
        formula <- getFormula(outcomes[i], treatment, covariates)
        glmOut <- glm(formula, data = data[!is.na(data[,outcomes[i]]),], family=family)
        glmOutputs[[i]] <- glmOut
        glmOutRobust <- correctSE(glmOut, data[!is.na(data[,outcomes[i]]),], se, cluster)
        robustOutputs[[i]] <- glmOutRobust[[1]]
        robustVcovs[[i]] <- glmOutRobust[[2]]
        resultTable[i,c("Effect", "SE", "p-value", "# obs")] <- c(round(glmOutRobust[[1]][effectRow,c(1:2,4)], digits=3), nobs(glmOut))

    }
    rownames(resultTable) <- rowNames

    # returns the results.
    output <- list(resultTable, robustOutputs, robustVcovs, glmOutputs)
    names(output) <- c("resultTable", "robustOutputs", "robustVcovs", "glmOutputs")
    return(output)
}


# ----------------------------------------- #
# Function: initResultTable
# --------------------- 
# This function initializes a table to store results in the effectTable function.
# ----------------------------------------- #
initResultTable <- function(outcomes, treatment, data, includeMeans, exp_arms) {
    # store mean of treatment and control groups in resultTable.
    if (includeMeans){
        resultTable <- as.data.frame(matrix(NA, nrow=length(outcomes), ncol=6))
        colnames(resultTable) <- c("Control mean", "Treated mean", "Effect", "SE", "p-value", "# obs")
        for (i in 1:length(outcomes)){
            resultTable[i,1] <- mean(data[data[,treatment]==exp_arms[1], outcomes[i]], na.rm=TRUE)
            resultTable[i,2] <- mean(data[data[,treatment]==exp_arms[2], outcomes[i]], na.rm=TRUE)
        }       
    } else {
        resultTable <- as.data.frame(matrix(NA, nrow=length(outcomes), ncol=4))
        colnames(resultTable) <- c("Effect", "SE", "p-value", "# obs")
    }
    return(resultTable)
}


# ----------------------------------------- #
# Function: getFormula
# --------------------- 
# This function returns a formula object given an outcome, treatment, and vector of 
# covariates.
# ----------------------------------------- #
getFormula <- function(outcome, treatment, covariates){
    if (length(covariates)==0) { # if no covariates...
        formula <- as.formula(paste(outcome,"~",treatment))
    } else { 
        formula <- as.formula(paste(outcome,"~",treatment,"+",paste(covariates,collapse="+")))
    }
    return(formula)
}


# ----------------------------------------- #
# Function: correctSE
# --------------------- 
# This function adjusts the standard errors of a glm object. Currently, only HC0, HC1, and
# clustered standard errors are supported.
# ----------------------------------------- #
correctSE <- function(model, data, se="HC0", cluster=""){
    if (se == "HC0"){
        vcov <- vcovHC(model, type="HC0")
        glmOutRobust <- coeftest(model, vcov = vcov)
    } else if (se == "HC1") {
        vcov <- vcovHC(model, type="HC1")
        glmOutRobust <- coeftest(model, vcov = vcov)
    } else if (se == "cluster"){
        vcov <- vcovCluster(model, cluster=data[, cluster])
        glmOutRobust <- coeftest(model, vcov = vcov)
    } else {
        stop("invalid se entry. Only HC0, HC1, and cluster are currently supported.")
    }
    result <- list(glmOutRobust,vcov)
    names(result) <- c("glmOutRobust", "vcovRobust")
    return(result)
}

# ----------------------------------------- #
# Function: expLogLevel
# --------------------- 
# This function takes in a coefficient from a log-level regression and returns a converted 
# coefficient in terms of % change. 
# ----------------------------------------- #
expLogLevel <- function(coef) {
    return(round((exp(coef)-1)*100, 2))
}
expLogLog <- function(coef, marginalChange) {
    return(round((marginalChange^coef - 1)*100,2))
}


# ----------------------------------------- #
# Function: tableDD
# --------------------- 
# This function creates a table of means for a control group and 
# treatment group in a two-period differences-in-differences analysis.
# 
# Parameters:
# listOfModelLists: a list of outputs from effectTable().
# ----------------------------------------- #
tableDD <- function(listOfModelLists) {
    allDDmeans <- list()
    for (i in 1:length(listOfModelLists)) {
        allDDmeans[[i]] <- getDDmeans(listOfModelLists[[i]])
    }
    result <- do.call("rbind", allDDmeans)
    return(result)
}

getDDmeans <- function(modelList) {
    nOutcomes <- length(modelList$robustOutputs)
    means <- data.frame(matrix(NA, ncol= 8, nrow = nOutcomes))
    colnames(means) <- c("controlBaseline", "controlEndline", "controlChange", "treatmentBaseline", "treatmentEndline", "treatmentChange", "DiD", "p-value")
    rownames(means) <- rownames(modelList$resultTable)
    for (i in 1:nOutcomes) {
        theseMeans <- ggplotDDwrapper(modelList, i)
        means[i, 1] <- theseMeans[theseMeans$time==2009 & theseMeans$treated=="Control Villages", "outcome"]
        means[i, 2] <- theseMeans[theseMeans$time==2014 & theseMeans$treated=="Control Villages", "outcome"]
        means[i, 3] <- means[i, 2] - means[i, 1]
        means[i, 4] <- theseMeans[theseMeans$time==2009 & theseMeans$treated=="Treated Villages", "outcome"]
        means[i, 5] <- theseMeans[theseMeans$time==2014 & theseMeans$treated=="Treated Villages", "outcome"]
        means[i, 6] <- means[i, 5] - means[i, 4]
        means[i, 7] <- means[i, 6] - means[i, 3]
        means[i, 8] <- modelList$resultTable[i, "p-value"]
    }
    return(means)
}


# --------------------------------- #
# Function: latexCoefTable
# ---------------------------
# This function takes in a coef table in the form of a dataframe
# and returns the coef table in latex format.
# see https://cran.r-project.org/web/packages/Hmisc/Hmisc.pdf for
# information about latex() parameters.
# --------------------------------- #
latexCoefTable <- function(dataframe, file, ncols, digits=3, single_col=TRUE, colheads=c("Control mean", "Treated mean", "Effect", "SE", "p-value"), ...) {
    latex_coef_table <- latex(round(dataframe, digits=digits),
          file=file,
          rowlabel='',
          # n.cgroup=c(2,2,2),
          col.just=rep('c', ncols),
          colheads=colheads,  # "Control avg.", "Treated avg.", 
          booktabs=FALSE,
          ...)
    if (single_col) {
        file_str <- paste(readLines(file), collapse="\n")
        file_str <- gsub("\\{table\\}", "\\{table\\*\\}", file_str)
        write(file_str, file)
    }
    
    return(latex_coef_table)
}