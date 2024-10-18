#' Inspect residual covariances of each class.
#'
#' @param x Object of class inheriting from "poLCA" or "poLCA2".
#' @param nclass If \code{object} contains many "poLCA" object (i.e., if it a "poLCA2" object), choose the model with the given number of classes.
#'
#' @return A list of residual covariance matices.
#' @export
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' out <- poLCA(f1, nclass = 3, data = ex1.poLCA)
#' poLCA.residual.cov(out)
poLCA.residual.cov <- function(x, nclass = NULL){
  
  if(inherits(x, "poLCA2") && is.null(nclass)) stop("Specify nclass to proceed.")
  if(!any(c(inherits(x, "poLCA2"), inherits(x, "poLCA")))) stop("This function needs an object of class 'poLCA' or 'poLCA2'.")
  if(inherits(x, "poLCA2")) x <- x$LCA[[nclass]]
  
  S <- cov(x$y)
  pc <- predict.poLCA(x)$Pred # predicted class
  test <- as.data.frame(cbind(pc, x$y))
  out <- by(test[-1], test$pc, function(x, S) {S-cov(x)}, S)
  names(out) <- paste0("Residual covariance matrix of Class == ",1:length(out))
  for(i in 1:length(out)){
    cat("\n",names(out)[i],"\n \n")
    print(round(out[[i]], 3))
  }
  return(invisible(out))
}





#pc <- x$predcell
#pc <- pc[,-((ncol(pc)-1):ncol(pc))]
# pc <- predict.poLCA(x)
# oc <- x$y
# pc == oc
# cov(pc)
# pattern <- apply(pc[,-((ncol(pc)-1):ncol(pc))],1,paste0, collapse = "")
# 
# 
# oc <- x$y # observed cells
# pp <- x$probs # probs
# pc <- predict.poLCA(x)$Pred #predicted classe
# ec <- data.frame(matrix(ncol = ncol(oc), nrow = nrow(oc)))
# colnames(ec) <- colnames(oc)
# 
# 
# 
# 
# 
# a <- sapply(out, function(x){
#   cat(names(x),"\n \n")
#   cat(round(x, 3))
# }, simplify = FALSE)
# 
# 
# 
# test <- cbind(pc, x$y)
