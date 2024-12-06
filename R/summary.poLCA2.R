#' Summary results of object "poLCA2"
#'
#' @param object an object of class "poLCA2".
#' @param alpha type I error rate. Default is .05.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return A data.frame containing the number of classes identified by different fit statistics.
#' @export
#'
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6, V7) ~ 1
#' out <- poLCA(f1, nclass = 1:4, data = ex2.poLCA)
#' summary(out)
summary.poLCA2 <- function(object, alpha = .05, ...){
  tech <- c("aic","bic","sabic","aic3","caic","Chisq","Gsq","poc","lmr","vlmr","blmr","bvlmr")
  dec <- c(test.csq(object, stat = "aic",   alpha),
           test.csq(object, stat = "bic",   alpha),
           test.csq(object, stat = "sabic", alpha),
           test.csq(object, stat = "aic3",  alpha),
           test.csq(object, stat = "caic",  alpha),
           test.csq(object, stat = "Chisq", alpha),
           test.csq(object, stat = "Gsq",   alpha),
           test.poc(object, alpha),
           test.lrt(object, alpha),
           test.blrt(object,alpha))
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

test.poc <- function(object, alpha){
  dec <- which(sapply(sapply(object$LCA, poLCA.cov)[1,], function(x) x)[3,] > alpha)
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
