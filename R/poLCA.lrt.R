#' Lo-Mendell-Rubin adjusted likelihood ratio test 
#'
#' @param model1 First poLCA model.
#' @param model2 Second poLCA model to compare to the first one.
#'
#' @return A list containing the likelihood ratio, the Lo-Mendell-Rubin adjusted likelihood ratio, the degree of freedom and the lmr's p-value.
#' @export
#' 
#' @import stats
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' LCA2 <- poLCA(f1, nclass = 2, data = ex1.poLCA)
#' LCA3 <- poLCA(f1, nclass = 3, data = ex1.poLCA)
#' poLCA.lrt(LCA3, LCA2)
poLCA.lrt <- function(model1, model2){
  
  if(model1$N != model2$N){
    warning("Numbers of cases is different between models")
  }

  n <- min(c(model2$N, model1$N))
  lr <- 2 * (model2$llik -  model1$llik)
  lmr <- lr / (1 + (((3 * length(model2$P) - 1) - 
                         (3 * length(model1$P) - 1)) * log(n))^-1)
  df <- model2$npar - model1$npar
  lmr.p <- pchisq(q = lmr, df = df, lower.tail = FALSE)
  out <- list(lr = lr,
              lmr = lmr,
              df = df,
              lmr.p = lmr.p)
  return(out)
}
