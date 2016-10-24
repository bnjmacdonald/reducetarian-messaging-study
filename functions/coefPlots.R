# --------------------------------- #
# File: coefPlots.r
# --------------------------------- #

library(reshape2)
library(ggplot2)

# --------------------------------- #
# Function: ggcoefPlot
# ---------------------------
# This function creates a 2D plot showing a treatment effect, using the
# ggplot2 library.
#
# Arguments:
# data: a dataframe.
# treatment: name of treatment variable.
# outcomes: names of outcomes.
# coordFlip: boolean indicating whether coordinates should be flipped.
# showData: boolean indicating whether raw data should be shown underneath.
# hlineZero: boolean indicating whether a horizontal line at zero should be placed.
# --------------------------------- #

ggcoefPlot <- function(data, effect, treatment, se, n, xticks=NULL, ncol=NULL, limits=NULL, coordFlip=FALSE, hlineZero, freey=FALSE) {
    # computes upper and lower CIs
    data$lowerCI <- data[,effect] - qt(0.975, df=data[,n]-1)*data[,se]
    data$upperCI <- data[,effect] + qt(0.975, df=data[,n]-1)*data[,se]

    # modify outcome labels for plotting.
    uniq_outcomes <- unique(data$variable)
    for (i in 1:length(uniq_outcomes)) {
        data$variable[data$variable == uniq_outcomes[i]] <- paste("(",i,") ", uniq_outcomes[i], sep="")
    }

    # creates the plot.
    ylabel <- 'Mean difference, relative to control'
    p <- ggplot(data, aes_string(x=treatment, y=effect, colour=treatment, fill=NULL))
    # ylabel <- outcomes[1]
    if (nrow(data) > 1) {
        if (freey) {
            p <- p + facet_wrap(~variable, ncol=ncol, scales="free_y")
        } else {
            p <- p + facet_wrap(~variable, ncol=ncol)
        }
        # ylabel <- NULL
    }

    if (hlineZero) {
        p <- p + geom_hline(yintercept=0, linetype=2, color='gray60')
    }

    p <- p + geom_point(size=3) +
            geom_errorbar(aes(ymin=lowerCI, ymax=upperCI, colour=treatment), width=0, size=1.7) +
            # scale_x_discrete(labels=xticks) +
            labs(x=NULL, y=ylabel) +
            scale_colour_discrete(name="") +
            guides(fill=guide_legend(title=NULL, reverse = TRUE)) +
            # coord_cartesian(ylim = c(5,10)) + 
            theme(legend.position="none", legend.direction="horizontal", legend.box="horizontal", text = element_text(size=11), plot.margin = unit(c(0,0,0,0), 'cm'))

    if (coordFlip) {
        p <- p + coord_flip()
    }
    return(p)
}

# --------------------------------- #
# Function: ggmeanPlot
# ---------------------------
# This function creates a 2D plot showing a treatment effect, using the
# ggplot2 library.
#
# Arguments:
# data: a dataframe.
# treatment: name of treatment variable.
# outcomes: names of outcomes.
# coordFlip: boolean indicating whether coordinates should be flipped.
# showData: boolean indicating whether raw data should be shown underneath.
# hlineZero: boolean indicating whether a horizontal line at zero should be placed.
# --------------------------------- #

ggmeanPlot <- function(data, treatment, outcomes, xticks, legendNames, limits=NULL, coordFlip=FALSE, showData=TRUE, hlineZero, extendRangeBy=NULL, freey=FALSE) {
    # modify outcome labels for plotting.
    for (i in 1:length(outcomes)) {
        names(outcomes)[i] <- paste("(",i,") ", outcomes[i], sep="")
    }

    # creates the melted dataframe.
    tempDF <- data[,c(treatment,outcomes)]
    colnames(tempDF)[which(colnames(tempDF) %in% outcomes)] <- names(outcomes)
    tempDF <- melt(tempDF, id.var = treatment)
    tempDF[,"treatShift"] <- tempDF[,treatment] # + 1
    tempDF[,"temp"] <- factor(1)
    tempDF[,treatment] <- factor(tempDF[,treatment])
    colnames(tempDF)[1] <- "treatment"
    
    # creates summary statistics for each outcome.
    tempDF2 <- aggregate(tempDF$value, by=list(treatment=tempDF$treatment, outcome=tempDF$variable), FUN=function(x) {
            x <- x[!is.na(x)]
            n <- length(x)
            mean <- mean(x)
            se <- sd(x)/sqrt(n)
            return(cbind(mean,se,n))
        })
    tempDF2[,3:5] <- tempDF2[,3]
    colnames(tempDF2) <- c("treatment", "variable", "mean", "se", "n")
    
    # creates the plot.
    p <- ggplot(tempDF2, aes(x=treatment, y=mean, colour=treatment, fill=NULL))
    ylabel <- outcomes[1]
    if (length(outcomes) > 1) {
        if (freey) {
            p <- p + facet_wrap(~variable, scales="free_y")
        } else {
            p <- p + facet_wrap(~variable)  # , scales="free_y"
        }
        
        ylabel <- NULL
    }

    if (hlineZero) {
        p <- p + geom_hline(yintercept=0, linetype=2, color='gray60')
    }
    
    if (showData) {
        p <- p + geom_point(data=tempDF, aes(x=treatShift, y=value, fill="Raw Data", colour="Raw Data"), position=position_jitter(width=0.1, height=0.03), color="gray75", size=1) +
                scale_fill_discrete(labels="Raw Data")
    }
    
    p <- p + geom_point(size=2.2) +
            geom_errorbar(aes(ymin=mean - qt(0.975, df=n-1)*se, ymax=mean + qt(0.975, df=n-1)*se, colour=treatment),width=0, size=1) +
            scale_x_discrete(labels=xticks) +
            labs(x=NULL, y=ylabel) +
            scale_colour_discrete(name="", labels=legendNames) +
            guides(fill=guide_legend(title=NULL, reverse = TRUE)) +
            # coord_cartesian(ylim = c(5,10)) + 
            theme(legend.position="bottom", legend.direction="horizontal", legend.box="horizontal", text = element_text(size=11), plot.margin = unit(c(0,0,0,0), 'cm'))

    if (!is.null(extendRangeBy)) {
        yrange <-  ggplot_build(p)$panel$ranges[[1]]$y.range
        yrange_mid <- yrange[1] + (yrange[2] - yrange[1])/2
        yrange_new <- (yrange[2] - yrange[1])*extendRangeBy
        if (extendRangeBy > 1) {
            p <- p + ylim(yrange[1] - yrange_new/2, yrange[2] + yrange_new/2)
            # p <- p + scale_y_continuous(limits = c(yrange[1] - yrange_new/2, yrange[2] + yrange_new/2))
        } else {
            p <- p + ylim(yrange_mid - yrange_new/2, yrange_mid + yrange_new/2)
        }
    }
    # if (!is.null(limits)) {
    #   p <- p + scale_y_continuous(limits=c(quantile(tempDF[,'value'], limits[1], na.rm=TRUE), quantile(tempDF[,'value'], limits[2], na.rm=TRUE))) # breaks=seq(0,1, by=0.2)
    # }

    if (coordFlip) {
        p <- p + coord_flip()
    }
    return(p)
}

