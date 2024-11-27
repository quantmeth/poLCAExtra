#' Summary method for "poLCA2" objects
#'
#' @param object An object of class "poLCA2".
#' @param ... Further arguments for other methods, ignored for "poLCA2".
#'
#' @note Should not be called directly
#'
#' @return Return a table with relevant. 
#'
# summary.poLCA2 <- function(object, ...){
#   output <- data.frame(nclass = sapply(object, function(x) length(x$P)),
#                     df = sapply(object, function(x) x$resid.df),
#                     llike = sapply(object, function(x) x$llik),
#                     AIC = sapply(object, function(x) x$aic),
#                     BIC = sapply(object, function(x) x$bic),
#                    # SABIC = sapply(object, function(x) x$sabic),
#                     `Classes size` = sapply(object, function(x) paste0(sort(table(x$predclass)), collapse = "|")),
#                     Entropy = sapply(object, poLCA.entropy), #entropie
#                     `Relative Entropy` = sapply(object, poLCA.relentropy),
#                     LMR = sapply(object, poLCA.clmr, y = object, stat = "lmr"),
#                     p = sapply(object, poLCA.clmr, y = object, stat = "lmr.p"))
#   return(output)
# }
# 



#LCA3$N
#LCA