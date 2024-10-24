#' Predict method for poLCA and poLCA2 objects
#' 
#' @description Get probabilities of each subject to be in a given class and the most likely class.
#'
#' @param object Object of class inheriting from "poLCA" or "poLCA2".
#' @param nclass If \code{object} contains many "poLCA" object (i.e., if it a "poLCA2" object), choose the model with the given number of classes.
#' @param ...	 additional arguments affecting the predictions produced.
#'
#' @return A data frame with the probabilities of each subject to be in each classes and the predicted classes.
#' @export
#'
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' out <- poLCA(f1, nclass = 1:3, data = ex1.poLCA)
#' predict(out, nclass = 3)
predict.poLCA2 <- function(object, nclass = NULL, ...){
  
  if(is.null(nclass)) stop("Specify nclass to proceed.")
  ncx <- sapply(object$LCA, function(w) length(w$P))
  nc <- which(nclass == ncx)
  
  
  mod <- object[["LCA"]]
  mod1 <- mod[[nc]] 
  probs <- as.data.frame(cbind(mod1$posterior, 
                               mod1$predclass))
  colnames(probs) <- c(paste0("Pr(Class==", 1:length(mod1$P),")"),"Pred")
  return(probs)
}

#' @export
predict.poLCA <- function(object, ...){
  probs <- as.data.frame(cbind(object$posterior, 
                               object$predclass))
  colnames(probs) <- c(paste0("Pr(Class==", 1:length(object$P),")"),"Pred")
  return(probs)
}
