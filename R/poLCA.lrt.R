#' Lo-Mendell-Rubin adjusted likelihood ratio test
#'
#' @param model1 First poLCA model.
#' @param model2 Second poLCA model to compare to the first one.
#'
#' @return A list containing the Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test, the Lo-Mendell-Rubin adjusted likelihood ratio, the degree of freedom and the lmr's p-value.
#' @export
#' 
#' @import stats
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' LCA2 <- poLCA(f1, nclass = 2, data = ex1.poLCA)
#' LCA3 <- poLCA(f1, nclass = 3, data = ex1.poLCA)
#' 
#' poLCA.lrt(LCA3, LCA2)
#' # or
#' poLCA.lrt(LCA3)
poLCA.lrt <- function(model1, model2 = NULL){
  
  if(is.null(model2)){
    nclass <- length(model1$P) - 1
    f0 <-  as.formula(paste0("cbind(",paste0(colnames(model1$y),collapse = ","),")~",
                             paste0(c(1, model1$x[-1]), collapse = "+")))
    if(nclass == 0) stop("Too few classes for model 2.")
    model2 <- poLCA(f0, data = model1$y, nclass = nclass, verbose = FALSE)
  }
  
  if(model1$N != model2$N){
    warning("Numbers of cases is different between models.")
  }
  
  n <- min(c(model1$N, model2$N))
  vlmr <- 2 * (model1$llik -  model2$llik)
  lmr <- vlmr / (1 + (((3 * length(model1$P) - 1) - 
                         (3 * length(model2$P) - 1)) * log(n))^-1)
  df <- model1$npar - model2$npar
  lmr.p <- pchisq(q = lmr, df = df, lower.tail = FALSE)
  vlmr.p <- pchisq(q = vlmr, df = df, lower.tail = FALSE)
  out <- list(vlmr = vlmr,
              lmr = lmr,
              df = df,
              lmr.p = lmr.p,
              vlmr.p = vlmr.p)
  return(out)
}
