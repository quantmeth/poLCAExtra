#' Plot of d3step, bootstraped 3-step test of latent class analysis
#'
#' @param x An object of class "3step".
#' @param ci Confidence intervals (if they have to be changed).
#' @param ... Further arguments for other methods, ignored for "d3step".
#'
#' @return  A ggplot output.
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' jd <- ex1.poLCA
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' LCAE <- poLCA(f1, jd, nclass = 1:4, nrep = 4, maxiter = 1000)
#' results <- r3step("categorical", LCAE, nclass = 3)
#' plot(results)
plot.d3step <- function(x, ci = NULL, ...){
  
  if(!is.null(ci)) {
    if(!any(ci == .50)) {
      ci <- c(ci, .50)
    }
    ci <- sort(ci)
    M <- apply(x$btstrp, 1:2, quantile, probs = ci)
    #M <- aperm(M, c(3,2,1))
    M <- aperm(M, c(2,1,3))
    mt <- aperm(M, c(1,3,2))
  } else {
    M <- x$M
    mt <- aperm(M, c(1,3,2))
  }
  
  low <- colnames(M)[1]
  upp <- colnames(M)[ncol(M)]
  
  M2 <- as.data.frame(apply(mt, 3,
                            function(x) {x},
                            simplify = TRUE))
  M2$Class <- rownames(M)
  
  M2[[x$DV]] <- rep(sub(".*=(.*)\\).*", "\\1", colnames(M[1,,])),
                    each = nrow(x$btstrp))
  
  ggplot(data = M2,
         aes(x = .data[[x$DV]],
             y = .data$`50%`,
             color=.data$Class,
             group=.data$Class),
         size = 5) +   
    geom_errorbar(aes(ymin = .data[[low]],
                      ymax = .data[[upp]]), 
                  width = .25,
                  position = position_dodge(.2)) +
    geom_point(position = position_dodge(.2)) + 
    ylab(paste0("Pr(",x$DV," = x)"))
}