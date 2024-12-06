#' Compute relative entropy of an "poLCA" object
#'
#' @param model An object of class "poLCA".
#'
#' @return A vector containing the relative entropy.
#' @export
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' out <- poLCA(f1, nclass = 3, data = ex1.poLCA)
#' poLCA.relentropy(out)
poLCA.relentropy <- function(model){
  pik <- as.data.frame(model$posterior)
  1 - (sum(-1 * pik * log(pik), na.rm = TRUE) /
         (nrow(pik)* log(ncol(pik))) )
  }
