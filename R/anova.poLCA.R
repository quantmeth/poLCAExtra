#' Compare multiple poLCA output
#'
#' @param object An object of class "poLCA".
#' @param ... additional objects of the same type.
#'
#' @return A table comparing the poLCA models.
#' @export
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' LCA1 <- poLCA::poLCA(f1, data = jd, nclass = 1) 
#' LCA2 <- poLCA::poLCA(f1, data = jd, nclass = 2)
#' LCA3 <- poLCA::poLCA(f1, data = jd, nclass = 3)
#' anova(LCA3, LCA2, LCA1)
anova.poLCA <- function(object, ...){
  
  x <- list(object, ...)
  
  if(!all(sapply(x, function(x) inherits(x, "poLCA")))) stop("FUCK IT")
  dl <- sapply(x, function(x) x$resid.df)
  ix <- sort(dl, decreasing = TRUE, index.return = TRUE)$ix
  mod <- x[ix]
  class(mod) <- "poLCA2"
  #ix 
  # build table 
  output <- summary(mod)
  printtab(tab = output)
  return(invisible(output))
}
# 
# x <- y <- list()
# class(x) <- class(y) <- "poLCA"
# w <- list()
# class(w) <- "lm"
# anova.poLCA(y,x)
# anova(w,x,y)
# dl <- anova(LCA1,LCA3,LCA2)
# mod <- list(LCA1,LCA2,LCA3)
# sapply(mod, function(x) length(x$P))
