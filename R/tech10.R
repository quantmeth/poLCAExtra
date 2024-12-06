#' Inspect patterns' residuals of latent class analysis.
#'
#' @param x an object of class "poLCA" or "poLCA2"
#' @param nclass  if \code{object} contains many "poLCA" object (i.e., if it a "poLCA2" object), choose the model with the given number of classes.
#' @param alpha a type I error threshold to mark the check column.
#'
#' @return A data frame containing the patterns' residuals.
#' @export
#' 
#' @aliases poLCA.tech10 poLCA.residual.pattern
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' out <- poLCA(f1, nclass = 3, data = ex1.poLCA)
#' poLCA.residual.pattern(out)
poLCA.residual.pattern <- function(x, nclass = NULL, alpha = .05){
  
  if(inherits(x, "poLCA2") && is.null(nclass)) stop("Specify nclass to proceed.")
  if(!any(c(inherits(x, "poLCA2"), inherits(x, "poLCA")))) stop("This function needs an object of class 'poLCA' or 'poLCA2'.")
  if(inherits(x, "poLCA2")) x <- x$LCA[[nclass]]
  
  pc <- x$predcell
  #pattern <- apply(pc[,-((ncol(pc)-1):ncol(pc))],1,paste0, collapse = "")
  pattern <- names(table(apply(x$y,1,paste0, collapse = "")))

  npattern <- prod(sapply(apply(x$y, 2, unique, simplify= FALSE), length))
  missing.cell <- npattern - length(pattern)
  obs <- pc$observed
  ex <- pc$expected
  z <- (obs-ex)/sqrt(ex)
  p <- pnorm(abs(z), lower.tail = FALSE)
  chi <- z^2
  llikc <- 2 * (obs * log(obs / ex))
  pr <- data.frame(pattern = pattern,
                          observed = obs,
                          expected = ex,
                          z = z,
                          chi = chi,
                          llik.contribution = llikc,
                          p = p,
                          check = ifelse(p < alpha/2, "*",""))
  rez <- pr[order(-pr$observed),]
  rez <- list(output = rez,
              npattern = npattern,
              missing.cell = missing.cell)
  class(rez) <- "tech10"
  return(output = rez)
}

#' @rdname poLCA.residual.pattern
#' @export
poLCA.tech10 <- poLCA.residual.pattern

#' @export
print.tech10 <- function(x, digit = 2, top = 20, ...){
  pr <- x$output
  pr[2:7] <- round(pr[2:7], digit)
  cat("The 20 most frequent patterns\n\n")
  print(pr[1:top,], row.names = FALSE)
  cat("\nNumber of observed patterns: ", x$npattern-x$missing.cell)
  cat("\nNumber of empty cells: ", x$missing.cell)
  cat("\nTotal number of possible patterns: ", x$npattern)
}