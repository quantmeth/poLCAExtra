#' Compare the statistics of an poLCA2 object
#'
#' @param x an poLCA2 object.
#' @param stat the desired statistics to compare. Default is "AIC". It can be also "BIC", "saBIC", "CAIC" or "AIC3".
#' @param alpha the significance threshold. 
#'
#' @return A data frame with 
#' \itemize{
#'   \item \code{The classes comparison} - which nclass are compared.
#'   \item \code{chisq} - The chi square.
#'   \item \code{df} - The degree of freedom.
#'   \item \code{p} - The p-value.
#'   \item \code{remark} - A note indicating if it a significant decrease, a significant increase or no statistical difference based on \code{alpha}.
#' }
#' @export
#'
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' LCAE <- poLCA(f1, nclass = 1:3, data = ex1.poLCA)
#' poLCA.compare(LCAE)
poLCA.compare <- function(x, stat = "AIC", alpha = .05){
  
  stat <- tolower(stat)
  st <- sapply(x$LCA, function(x, stat) x[[stat]], stat = stat)
  dl <- sapply(x$LCA, function(x, stat) x[[stat]], stat = "resid.df")
  nc <- sapply(x$LCA, function(x, stat) length(x$P))

  chisq <- st[-1] - st[-length(st)]
  df <- dl[-length(st)] - dl[-1]
  p <- pchisq(abs(st[-length(st)] - st[-1]), dl[-length(st)] - dl[-1], lower.tail = FALSE)
  round(p, 3)
  test <- paste0(nc[-1], " vs ", nc[-length(nc)])
  remark <- ifelse((p < alpha) & sign(chisq) == -1, "Significant decrease",
               ifelse(p > alpha, "No statistical difference", "Significant increase"))
  data.frame(test.classes = test, 
             chisq = chisq,
             df = df,
             p = p,
             remark = remark)
}
