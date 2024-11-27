#' Plot of r3step, bootstraped 3-step test of latent class analysis
#'
#' @param x An object of class "3step".
#' @param ci Confidence intervals (if they have to be changed).
#' @param ... Further arguments for other methods, ignored for "r3step".
#' 
#'
#' @return A ggplot output.
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' jd <- ex1.poLCA
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' LCAE <- poLCA(f1, jd, nclass = 1:4, nrep = 4, maxiter = 1000)
#' results <- r3step("continuous", LCAE, nclass = 3)
#' plot(results)
plot.r3step <- function(x, ci = NULL, ...){
  if(!is.null(ci)) {
    if(!any(ci == .50)) {
      ci <- c(ci, .50)
    }
    ci <- sort(ci)
    x$M <- t(apply(x$btstrp, 1, quantile, probs = ci))
  }
  
  M <- as.data.frame(x$M)
  low <- colnames(M)[1]
  upp <- colnames(M)[ncol(M)]
  
  M$Class <- rownames(M)
  
  ggplot(data = M,
         aes(x = .data$Class,
             y = .data$`50%`),
         size = 5) + 
    geom_errorbar(aes(ymin = .data[[low]],
                      ymax = .data[[upp]]), 
                  width = .25) +
    geom_point() + 
    ylab(x$DV)
}
