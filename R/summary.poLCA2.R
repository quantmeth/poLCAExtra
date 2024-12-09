#' Summary results of object "poLCA2"
#'
#' @param object an object of class "poLCA2".
#' @param alpha type I error rate. Default is .05.
#' @param crit.rmsea A criterion for RMSEA. Default is .08.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return A data.frame containing the number of classes identified by different fit statistics.
#' @export
#'
#' @examples
#' f2 <- cbind(V1, V2, V3, V4, V5, V6, V7) ~ 1
#' LCAE <- poLCA(f2, nclass = 1:4, data = ex2.poLCA)
#' summary(LCAE)
summary.poLCA2 <- function(object, alpha = .05, crit.rmsea = .08, ...){
  tech <- c("aic","bic","sabic","aic3","caic","chisq","gsq","poc","lmr","vlmr","blmr","bvlmr", "rmsea")
  dec <- c(test.csq(object, stat = "aic",   alpha),
           test.csq(object, stat = "bic",   alpha),
           test.csq(object, stat = "sabic", alpha),
           test.csq(object, stat = "aic3",  alpha),
           test.csq(object, stat = "caic",  alpha),
           test.csq(object, stat = "Chisq", alpha),
           test.csq(object, stat = "Gsq",   alpha),
           test.poc(object, alpha),
           test.lrt(object, alpha),
           test.blrt(object,alpha),
           test.rmsea(object, crit.rmsea))
  rez <- data.frame(tech = tech, dec = dec)
  rez[rez == -999] <- NA
  rez
}



test.csq <- function(x, stat, alpha = .05){
  chi2 <- sapply(x$LCA, function(x) x[[stat]])
  df <- sapply(x$LCA, function(x) x$npar)
  dec <- which(pchisq(chi2[-length(chi2)]-chi2[-1],df[-1]-df[-length(chi2)], lower.tail = FALSE) > alpha)
  if(length(dec) == 0){
    -999  
  }else{
    min(dec)
  }
}

test.csq2 <- function(x, stat, alpha = .05){
  chi2 <- sapply(x$LCA, function(x) x[[stat]])
  df <- sapply(x$LCA, function(x) x$npar)
  dec <- which(pchisq(chi2,df, lower.tail = FALSE) > alpha)
  if(length(dec) == 0){
    -999  
  }else{
    min(dec)
  }
}
test.poc <- function(object, alpha){
  dec <- which(sapply(sapply(object$LCA, poLCA.cov)[1,], function(x) x)[3,] > alpha)
  if(length(dec) == 0){
    -999  
  }else{
    min(dec)
  }
}

test.rmsea <- function(object, alpha){
  dec <- which(poLCA.rmsea(object) < .08)
  if(length(dec) == 0){
    -999  
  }else{
    min(dec)
  }
}

test.blrt <- function(x, alpha){
  out <- poLCA.blrt(x)$output[c("lmr.p","vlmr.p")]
  apply(out,2, function(x) {
    dec <- which(x > alpha)
    if(length(dec) == 0){
      -999  
    }else{
      min(dec)
    }
  })
}

test.lrt <- function(x, alpha){
  out <- sapply(x$LCA, .poLCA.clrt, y = x$LCA, stat =c("lmr.p","vlmr.p"))
  apply(out,1, function(x) {
    dec <- which(x > alpha)
    if(length(dec) == 0){
      -999  
    }else{
      min(dec)
    }
  })
}
