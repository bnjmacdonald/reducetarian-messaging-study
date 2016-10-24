# file: utils.r
# description: contains miscellaneous functions.

# function: remove_outliers.
# description: removes outliers from a dataframe (df) for
#   each variable in the vector var_names. Removes top x% of
#   outliers based on trim_percentile_upper and bottom y% of
#   outliers based on trim_percentile_lower.
remove_outliers <- function(df, var_names, trim_percentile_upper, trim_percentile_lower) {
    # removes outliers.
    for (i in 1:length(var_names)) {
        lower_value <- quantile(df[,var_names[i]], trim_percentile_lower, na.rm=TRUE)
        upper_value <- quantile(df[,var_names[i]], trim_percentile_upper, na.rm=TRUE)
        df[var_names[i]][df[,var_names[i]] < lower_value | df[var_names[i]] > upper_value] <- NA
    }
    return(df)
}

# function: create_chg_vars.
# description: creates "change" variables based on list of
#   vars in df. It is assumed that vars are named the same 
#   in wave 1 as in wave 3.
create_chg_vars <- function(df, vars) {
    for (i in 1:length(vars)) {
        var_name_1 <- paste(vars[i],'.1', sep='')
        var_name_2 <- paste(vars[i],'.3', sep='')
        var_name_chg <- paste(vars[i],'_chg', sep='')
        df[,var_name_chg] <- df[,var_name_2] - df[,var_name_1]
    }
    return(df)
}

# function: flatten_vec.
# description: flattens a vector into a single string.
flatten_vec <- function(vec) {
    return(paste(vec, collapse=','))
}

# function for adding ".1", ".3", and "_chg" suffixes to variables.
add_suffixes <- function(vars, include_wave1=FALSE) {
    vars1 <- vars3 <- varschg <- NULL
    
    if (include_wave1) {
        vars1 <- paste(vars, '.1', sep='')
    }
    vars3 <- paste(vars, '.3', sep='')
    varschg <- paste(vars, '_chg', sep='')
    return(c(vars1, vars3, varschg))
}
