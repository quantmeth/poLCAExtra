#' Print results of poLCA2 objects
#'
#' @param x Print the table comparing latent class models.
#' @param digit integer indicating the number of decimal places.
#' @param ... Further arguments for other methods, ignored for "poLCA2".
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' poLCA(f1, nclass = 1:3, data = ex1.poLCA)
print.poLCA2 <- function(x, digit = 3, ...) {
  #conjuger et mettre le point
  #cat("\n The recommended number of classes is ", x$output["nclass"][min(which(x$output["p"] < .05)), 1],"\n\n")
  tab <- x$output
  tab <- printtab(tab)
  print(tab)
}


printtab <- function(tab, digit = 3){
  tab[,-6] <- round(tab[,-6], digit)
  tab$p <-  ifelse(tab$p == 0,"< .001", sprintf(paste0("%.", digit,"f"), tab$p))
  tab$Relative.Entropy <-  sprintf(paste0("%.", digit,"f"), tab$Relative.Entropy)
  tab$LMR <-  sprintf(paste0("%.", digit,"f"), tab$LMR)
  tab[is.na(tab)] <- "NaN"
  tab[tab == "NaN"] <- ""
  print(tab)
}