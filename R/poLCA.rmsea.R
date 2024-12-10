#' RMSEA for poLCA objects
#'
#' @param object an object of class "poLCA or "poLCA2" 
#'
#' @return A vector containting RMSEA value of each model
#' @export
#'
#' @examples
#' f2 <- cbind(V1, V2, V3, V4, V5, V6, V7) ~ 1
#' LCAE <- poLCA(f2, nclass = 1:4, data = ex2.poLCA)
#' poLCA.rmsea(LCAE)
poLCA.rmsea <- function(object){
  if(inherits(object, "poLCA")){
    #object <- list(LCA = object)
    poLCA.cov(object)$chi2[[4]]
  } 
  sapply(sapply(object$LCA, poLCA.cov)[1,], function(x) x)[4,]
}
