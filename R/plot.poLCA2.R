#' Plot results of a model from poLCA2
#'
#' @param x An object of class "poLCA2".
#' @param nclass The number of classes determined.
#' @param ... Further arguments for other methods, ignored for "poLCA2".
#'
#' @return A plot output.
#' @export
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' out <- poLCA(f1, nclass = 1:3, data = ex1.poLCA)
#' plot(out, nclass = 3)
plot.poLCA2 <- function(x, nclass = NULL, ...){
  if(is.null(nclass)) stop("Specify nclasses to proceed.")
  mod <- x[["LCA"]]
  mod1 <- mod[[nclass]] 
  plot(mod1)
}
