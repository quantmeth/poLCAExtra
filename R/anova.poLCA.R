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
#' LCA1 <- poLCA(f1, data = ex1.poLCA, nclass = 1, verbose = FALSE) 
#' LCA2 <- poLCA(f1, data = ex1.poLCA, nclass = 2, verbose = FALSE)
#' LCA3 <- poLCA(f1, data = ex1.poLCA, nclass = 3, verbose = FALSE)
#' anova(LCA3, LCA2, LCA1)
anova.poLCA <- function(object, ...){
  
  if(length(ls <- list(...)) > 0) object <- c(list(object), ls)
  
  if(!all(sapply(object, function(x) inherits(x, "poLCA")))) stop("FUCK IT")
  dl <- sapply(object, function(x) x$resid.df)
  ix <- sort(dl, decreasing = TRUE, index.return = TRUE)$ix
  mod <- object[ix]

  output <- data.frame(nclass = sapply(mod, function(x) length(x$P)),
                       df = sapply(mod, function(x) x$resid.df),
                       llike = sapply(mod, function(x) x$llik),
                       AIC = sapply(mod, function(x) x$aic),
                       BIC = sapply(mod, function(x) x$bic),
                       # SABIC = sapply(object, function(x) x$sabic),
                       #Entropy = sapply(mod, poLCA.entropy), #entropie
                       `Rel.Entropy` = sapply(mod, poLCA.relentropy),
                       LMR = sapply(mod, .poLCA.clrt, y = mod, stat = "lmr"),
                       p = sapply(mod, .poLCA.clrt, y = mod, stat = "lmr.p"),
                       `Classes size` = sapply(mod, function(x) paste0(sort(table(x$predclass)), collapse = "|")))
  
  rez <- list(output = output,
       LCA = mod,
       data = object$data)
  
  class(rez) <- "poLCA2"

  return(rez)
}

#' @export
anova.poLCA2 <- function(object, ...){
  object#$output
}

.poLCA.clrt <- function(x, y, stat = c("lmr", "lmr.p")){
  #length(y)
  ncl <- length(x$P)
  ncx <- sapply(y, function(w) length(w$P))
  nc <- which(ncl == ncx)
  #n1 <- any(x$P %in% 1)
  #nc <- length(x$P)
  if (ncl == 1){
    rep(NaN, length(stat))
  } else {
    if(nc == 1){
      jd <- as.data.frame(cbind(x$y,x$x[,-1]))
      flrt <- as.formula(paste0("cbind(",paste0(colnames(x$y), collapse = ","),")~",paste0(c(1, colnames(x$x)[-1]), collapse = "+")))
      yy <- poLCA(flrt, data = jd, nclass = ncl-1)
    } else {
      yy <-  y[[nc-1]]
    }
    unlist(poLCA.lrt(x, yy)[stat])
  }
}
