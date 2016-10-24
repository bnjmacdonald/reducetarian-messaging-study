## wFDR function
## Implements wFDR as explained in
## "A False Discovery Framework for Mitigating Publication Bias"
## by Annie Franco and Bradley Spahn

# wfdr takes a vector of p-values, a (possibly weighted) fdr-level, and an optional vector 
# of weights and returns a logical vector indicating whether each hypothesis was
# rejected or not.
wfdr <- function(p, alpha = .05, w = NULL){
  if(missing(w) | is.null(w)){ 
    w <- rep(1, length(p))
    message("No weights specified.Setting weights to 1.")
  }
  if(sum(w) != length(p)) stop("Error: weights must sum to length(p).")
  
  order.p <- order(p)
  suppressWarnings(max.reject <- max(which(p[order.p] <= cumsum(w[order.p]) / length(p) * alpha)))
  if(max.reject == -Inf) max.reject <- 0
  reject <- c(rep(T, max.reject), rep(F, length(p) - max.reject))
  reject <- reject[order(order.p)]
  reject
}

# # Here’s a test -----------------------------------------------------------

# # Generate some data
# m0 <- 5 # Number of null tests
# m1 <- 5 # Number of non-null tests
# alpha <- .05 # (Possibly weighted) False discovery rate
# p <- c(runif(m0), rbeta(m1, shape1 = 1, 50)) # random p-values.
# w <- c(rep(1.5,5), rep(.5,5)) # (Optional) weights. Must sum to m.
# w <- w / sum(w) * (m0 + m1)
# #w <- rep(1,10)

# # Run the function
# rejections <- wfdr(p = p, alpha = alpha, w = w)

# # Visual check that it's working properly. 
# # All p-values less than the highest p-value under the dashed line get rejected. 
# # The solid black line is the unweighted FDR boundary.
# # p-values larger than .1 aren't plotted.

# p.df <-data.frame(w = w, p = p, Rejected = rejections)
# require(ggplot2)
# order.p <- order(p)
# p.df <- p.df[order.p,]
# p.df$order <- 1:length(p)
# p.df$cumwt <- cumsum(p.df$w) * alpha / length(p)

# ggplot(p.df, aes(x = order, y = p, colour = Rejected)) + 
#   geom_point() +
#   geom_abline(slope = .005, intercept = 0) + 
#   geom_line(aes(x = order, y = cumwt), linetype = "dashed", colour = "black")+
#   scale_y_continuous("p-value Rank",lim = c(0, .1)) + 
#   scale_x_continuous("p") +
#   theme_bw() 


# # Run the function, this time without weights
# rejections <- wfdr(p = p, alpha = alpha)

# # Visual check that it's working properly. 
# # All p-values less than the highest p-value under the black line get rejected. 
# # p-values larger than .075 aren't plotted.

# p.df <-data.frame(w = rep(1, length(p)), p = p, Rejected = rejections)
# require(ggplot2)
# order.p <- order(p)
# p.df <- p.df[order.p,]
# p.df$order <- 1:length(p)
# p.df$cumwt <- cumsum(p.df$w) * alpha / length(p)

# ggplot(p.df, aes(x = order, y = p, colour = Rejected)) + 
#   geom_point() +
#   geom_abline(slope = .005, intercept = 0) + 
#   #geom_line(aes(x = order, y = cumwt), linetype = "dashed", colour = "black")+
#   scale_y_continuous("p",lim = c(0, .1)) + 
#   scale_x_continuous("p-value Rank") +
#   theme_bw() 
