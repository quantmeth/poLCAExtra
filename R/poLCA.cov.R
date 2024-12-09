#' Inspect the model covariance matrix and some fit statistics
#'
#' @param x object of class inheriting from "poLCA" or "poLCA2".
#' @param nclass if \code{object} contains many "poLCA" object (i.e., if it is a "poLCA2" object), choose the model with the given number of classes.
#' @param ... further objects of class "poLCA".
#'
#' @return A list of covariances and some relevant statistics.
#' @export
#'
#' @import poLCA
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' out <- poLCA(f1, nclass = 3, data = ex1.poLCA)
#' poLCA.cov(out)
poLCA.cov <- function(x, nclass = NULL, ...){
  
  if(inherits(x, "poLCA2") && is.null(nclass)) stop("Specify nclass to proceed.")
  if(!any(c(inherits(x, "poLCA2"), inherits(x, "poLCA")))) stop("This function needs an object of class 'poLCA' or 'poLCA2'.")
  if(inherits(x, "poLCA2")) x <- x$LCA[[nclass]]
  
  probs <- x$probs
  P <- x$P
  n <- x$N
  for(k in 1:length(P)){
    pr <- sapply(probs, function(x) x[k,], simplify = FALSE)
    it <- sapply(pr, function(x) 1:length(x), simplify = FALSE)
    egpr <- expand.grid(pr)
    egpr$pr <- apply(egpr, 1, prod) * P[k]
    if(k == 1){
      rez <- (cbind(expand.grid(it), pr = egpr$pr))
    } else {
      rez$pr <- rez$pr +  egpr$pr
    }
  }
  
  rez <- as.matrix(rez)
  nitem <- ncol(rez) 
  M <- as.matrix(colSums(rez[,-nitem] * rez[,nitem]))
  ES <- (t(rez[,-nitem] * rez[,nitem])) %*% rez[,-nitem] - M %*% t(M)
  
  stat <- data.frame(pair = apply(tri.names(ES), 
                                  1, 
                                  function(x) {paste0(unlist(x), collapse = " ~~ ")}))
  stat$Observed <- cov(x$y)[lower.tri(ES)]
  stat$Expected <- ES[lower.tri(ES)]
  stat$chi2 <- #(stat$Observed-stat$Expected)^2/stat$Expected * n
    cortest(cor(x$y), cov2cor(ES), n)
    #stat$chi2 = chi2[lower.tri(chi2)]
    stat <- stat[order(-stat$chi2),]
  stat$p <- pchisq(stat$chi2, 1, lower.tail = FALSE)
  stat
  chi2 <- c(chi2 = sum(stat$chi2), df = nrow(stat), p = pchisq(sum(stat$chi2), nrow(stat), lower.tail = FALSE),
            RMSEA =  sqrt((sum(stat$chi2) / (x$npar- 1))/x$npar))
  
  #,
  #          SRMR = sqrt(sum((cor(x$y)-cov2cor(ES))[lower.tri(ES)]^2)/n))
  #sqrt(mean((cor(x$y)-cov2cor(ES))^2))
  
  stat <- list(chi2 = chi2,
               stat = stat,
               SigmaHat = ES)
  class(stat) <- c("poLCA.cov")
  stat
}

#' @export
print.poLCA.cov <- function(x, digit = 3, top = min(20, nrow(x$stat)), ...){
  s <- x$chi2
  x <- x$stat[1:top,]
  x[,-1] <- round(x[,-1], digit)
  #x$chi2 <- round(x$chi2, digit)
  #x$p <- round(x$p, digit)
  #x$Observed <- round(x$chi2, digit)
  #x$Expected <- round(x$p, digit)
  #x[,2:3] <- round(x[,2:3], digit)
  x$p <- ifelse(x$p < 10^-digit, paste0("p < ", 10^-digit), sprintf(paste0("%.", digit,"f"), x$p))
  cat("Relevant statistics : \n")
  cat("Chisq :    ", round(s[1],3), "\n")
  cat("df    :    ", round(s[2],  3), "\n" )
  cat("p     :    ", ifelse(s[3] < 10^-digit, paste0("p < ", 10^-digit), sprintf(paste0("%.", digit,"f"), s[3])), "\n")
  cat("\n")
  cat("Top", top, "covariances : \n")
  cat("\n")
  print(x, row.names = FALSE)
}


# Get lower triangle
tri.names <- function(x){
  ltriangle <- x[lower.tri(x, diag = FALSE)]
  idx <- which(lower.tri(x, diag = FALSE), arr.ind = TRUE)
  cbind(rownames(x)[idx[, 1]], colnames(x)[idx[, 2]])
}
# tri.names(chi2)

cortest <- function(R1, R2, n) {
  
  if (!is.matrix(R1)) R1 <- as.matrix(R1)
  if (!is.matrix(R2)) R2 <- as.matrix(R2)
  
  Z1 <- 0.5 * log((1 + R1) / (1 - R1))
  Z2 <- 0.5 * log((1 + R2) / (1 - R2))
  Z <- (Z1 - Z2)^2 * (n-3)
  Z[lower.tri(Z)]
}