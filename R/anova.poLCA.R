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
#' LCA1 <- poLCA::poLCA(f1, data = ex1.poLCA, nclass = 1) 
#' LCA2 <- poLCA::poLCA(f1, data = ex1.poLCA, nclass = 2)
#' LCA3 <- poLCA::poLCA(f1, data = ex1.poLCA, nclass = 3)
#' anova(LCA3, LCA2, LCA1)
anova.poLCA <- function(object, ...){
  
  if(length(ls <- list(...)) > 0) object <- c(list(object), ls)
  
  if(!all(sapply(object, function(x) inherits(x, "poLCA")))) stop("FUCK IT")
  dl <- sapply(object, function(x) x$resid.df)
  ix <- sort(dl, decreasing = TRUE, index.return = TRUE)$ix
  mod <- object[ix]
  #class(mod) <- "poLCA2"
  #ix 
  # build table 
  output <- data.frame(nclass = sapply(mod, function(x) length(x$P)),
                       df = sapply(mod, function(x) x$resid.df),
                       llike = sapply(mod, function(x) x$llik),
                       AIC = sapply(mod, function(x) x$aic),
                       BIC = sapply(mod, function(x) x$bic),
                       # SABIC = sapply(object, function(x) x$sabic),
                       `Classes size` = sapply(mod, function(x) paste0(sort(table(x$predclass)), collapse = "|")),
                       Entropy = sapply(mod, poLCA.entropy), #entropie
                       `Rel.Entropy` = sapply(mod, poLCA.relentropy),
                       LMR = sapply(mod, poLCA.clmr, y = mod, stat = "lmr"),
                       p = sapply(mod, poLCA.clmr, y = mod, stat = "lmr.p"))
  
  #object <- object$data
  rez <- list(output = output, 
       #data = object$data,
       LCA = mod)
  class(rez) <- "poLCA2"


  #output <- summary(mod)
  #printtab(tab = output)
  return(rez)
}

#' @export
anova.poLCA2 <- function(object, ...){
  object#$output
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
poLCA.clmr <- function(x, y, stat = c("lmr", "lmr.p")){
  #length(y)
  ncx <- sapply(y, function(w) length(w$P))
  nc <- which(length(x$P) == ncx)
  #nc <- length(x$P)
  if (nc == 1){
    NaN
  } else {
    poLCA.lmr(x, y[[nc-1]])[[stat]]
  }
}
