
source('functions/effectTable.r')

# ----------------------------------------- #
# FUNCTION: subgroup_analysis
# ---------------------------
# produces subgroup analyses by interacting
# a subgroup variable with the treatment.
# ----------------------------------------- #
subgroup_analysis <- function(data, 
                              outcome,
                              treatment,
                              subgroup,
                              covariates,
                              family="gaussian",
                              se="HC0",
                              cluster="") {
    formula <- formula(paste(outcome, '~', treatment, '*', subgroup, '+', paste(covariates, collapse='+')))
    glm_out <- glm(formula, data=data[!is.na(data[,outcome]),], family=family)
    glm_robust <- correctSE(glm_out, data[!is.na(data[,outcome]),], se, cluster)
    return(glm_robust)
}
