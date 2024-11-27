#' Inspect residual covariances of each class.
#'
#' @param x Object of class inheriting from "poLCA" or "poLCA2".
#' @param nclass If \code{object} contains many "poLCA" object (i.e., if it a "poLCA2" object), choose the model with the given number of classes.
#' @param nreps The number of replications for the bootstrap. Default is 1000.
#'
#' @return A list of residual covariance matices.
#' @export
#'
#' @import poLCA
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' out <- poLCA(f1, nclass = 3, data = ex1.poLCA)
#' poLCA.residual.cov(out)
poLCA.residual.cov <- function(x, nclass = NULL, nreps = 1000){
  
  if(inherits(x, "poLCA2") && is.null(nclass)) stop("Specify nclass to proceed.")
  if(!any(c(inherits(x, "poLCA2"), inherits(x, "poLCA")))) stop("This function needs an object of class 'poLCA' or 'poLCA2'.")
  if(inherits(x, "poLCA2")) x <- x$LCA[[nclass]]
  
  bootS <- array(0, dim=c(ncol(x$y),ncol(x$y),nreps))
  colnames(bootS) <- rownames(bootS)  <- colnames(x$y)
  S <- cor(x$y)
  n <- x$N
  for(i in 1:nreps){
    D <- poLCA.simdata(N = n,
                       probs = x$probs,
                       P = x$P)$dat
    bootS[,,i] <- cor(D) - S
  }
  
  chi2 <- round(apply(bootS, c(1, 2), function(x,n) sum((x*sqrt(n-3))^2)/length(x),n),2)
  #colnames(chi2) <- rownames(chi2) <- colnames(x$y)
  #chi2
  
  stat <- data.frame(pair = apply(tri.names(chi2), 
        1, 
        function(x) {paste0(unlist(x), collapse = " ~~ ")}))
  stat$chi2 = chi2[lower.tri(chi2)]
  stat <- stat[order(-stat$chi2),]
  stat$p <- pchisq(stat$chi2, 1, lower.tail = FALSE)
  stat <- list(stat=stat)
  class(stat) <- c("poLCA.rescov")
  stat
  # round(apply(bootS, c(1, 2), mean),2)
  # 
  # round(apply(bootS, c(1, 2), function(x) sum((x*sqrt(800-3))^2)/length(x)),2)
  # 
  # p
  # apply(bootS, c(1, 2), quantile, probs = .95)
  # R <- apply(bootS, c(1, 2), mean)
  # stat <- data.frame(cov = R[lower.tri(R)])
  # stat$cor <- cov2cor(R)[lower.tri(R)]
  # stat$df <- x$N - 2
  # stat$t <- stat$cor * sqrt(stat$df) / sqrt(1 - stat$cor^2)
  # stat$p <- (1 - pt(abs(stat$t), df = stat$df)) * 2
  # #r <- cov2cor(R)
  # #cor
  # # r <- r[lower.tri(r)]
  # # n <- x$N
  # # vt <- r * sqrt(n - 2) / sqrt(1 - r ^ 2)
  # # vp <- (1 - pt(abs(vt), df = n - 2)) * 2
  # # vp
  # list(covmat = R, stat = stat)
}
#' @export
print.poLCA.rescov <- function(x, digit = 3, top = 20, ...){
  x <- x$stat
  x$chi2 <- round(x$chi2, digit)
  x$p <- round(x$p, digit)
  #x[,2:3] <- round(x[,2:3], digit)
  x$p <- ifelse(x$p < 10^-digit, paste0("p < ", 10^-digit), sprintf(paste0("%.", digit,"f"), x$p))
  print(x[1:top,], row.names = FALSE)
}


# Get lower triangle
tri.names <- function(x){
ltriangle <- x[lower.tri(x, diag = FALSE)]
idx <- which(lower.tri(x, diag = FALSE), arr.ind = TRUE)
cbind(rownames(x)[idx[, 1]], colnames(x)[idx[, 2]])
}
#tri.names(chi2)



# #S <- cov(x$y)
# S <- psych::polychoric(x$y)$rho
# pc <- predict.poLCA(x)$Pred # predicted class
# test <- as.data.frame(cbind(pc, x$y))
# out <- by(test[-1], test$pc, function(x, S) {S-psych::polychoric(x)$rho}, S)
# names(out) <- paste0("Residual covariance matrix of Class == ",1:length(out))
# for(i in 1:length(out)){
#   cat("\n",names(out)[i],"\n \n")
#   print(round(out[[i]], 3))
# }
# return(invisible(out))
#}

