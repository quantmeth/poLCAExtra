#' Testing several latent class analyses of polytomous outcome variables
#' 
#' @description #' \code{poLCA} is an adaptation of the eponym function in the package \code{poLCA} to account for many models simultaneously 
#'
#' @param formula A formula expression of the form response ~ predictors. The details of model specification are given below.
#' @param data A data frame containing variables in formula. Manifest variables must contain only integer values, and must be coded with consecutive values from 1 to the maximum number of outcomes for each variable. All missing values should be entered as NA.
#' @param nclass A vector containing the number of latent classes to assume in the models. Setting nclass=1 results in poLCA estimating the loglinear independence model. Default is 1 to the maximum number of classes possible.
#' @param verbose Logical, indicating whether poLCA should output to the screen the results of the model. If FALSE, no output is produced. The default is FALSE.
#' @param ... Any other argument for \code{poLCA}.
#'
#' @return \code{poLCA} returns an object of class "poLCA" if \code{length(nclass) == 1} or returns an object of class "poLCA2" if \code{length(nclass) > 1}; a list containing the following elements :
#'
#' \itemize{
#'   \item \code{output} - A summary table of the fit.
#'   \item \code{data} - The data used.
#'   \item \code{LCA} - A list of objects of class "poLCA" (\code{poLCA} model)
#' }
#' 
#' 
#' @author 
#' P.-O. Caron
#' 
#' @references
#' 
#' Linzer, D. A. & Lewis, J. F. (2011). poLCA: An R Package for Polytomous Variable Latent Class Analysis. \emph{Journal of Statistical Software}, \emph{42}(10), 1-29. \url{https://www.jstatsoft.org/v42/i10/}
#' 
#' @import poLCA
#' 
#' @export
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' poLCA(f1, nclass = 1:3, data = ex1.poLCA)
poLCA <- function(formula, data, nclass = NULL, verbose = FALSE, ...){
  
  if(!is.null(nclass)){
    maxcl <- nclass
  } else {
    maxcl <- 1:poLCA.maxclasses(data)
  }
  
  
  if(length(maxcl) > 1){
    mod <- sapply(as.list(maxcl), 
                  poLCA, 
                  data = data, #data
                  # maxiter = maxiter, #maxiter #### TODO #### 
                  formula = formula, # formula
                  verbose = verbose, #) #... ####
                  ...)  

    class(mod) <- "poLCA2"
    
    rez <- list(output = summary(mod), 
                data = data,
                LCA = mod)
    
    class(rez) <- "poLCA2"
    
  } else {
    
    rez <- poLCA::poLCA(formula = formula, 
                        data = data,
                        nclass = maxcl, 
                        ...)
    rez$sabic <- -2*(rez$llik)+rez$npar*log(rez$Nobs+2)
    rez$data <- data
    
  }
  
  # rez <- list(output = rez, 
  #             data = data,
  #             LCA = mod)
  return(rez)
}


poLCA.maxclasses <- function(x){
  N <- nrow(x)
  K <- t(matrix(apply(x, 2, max)))
  tot <- min(N, (prod(K)-1)) 
  floor((tot + 1) / sum(K - 1)-1)
}


