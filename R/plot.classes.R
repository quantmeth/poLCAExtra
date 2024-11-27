#' Plot results of a model from poLCA2
#'
#' @param x An object of class "poLCA" or "poLCA2".
#' @param nclass If the object is of class "poLCA2" and only a single model have to be plotted.
#' @param alpha The type I error rate for confidence intervals.
#' @param ...  Further Objects of class "poLCA".
#'
#' @return A ggplot2 output.
#' 
#' @import ggplot2
#' @importFrom dplyr summarise group_by
#' @importFrom tidyr gather
#' 
#' @export plot.classes
#'
#' @examples
#' f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)
#' LCA2 <- poLCA(f1, data = ex1.poLCA, nclass = 2)
#' LCA3 <- poLCA(f1, data = ex1.poLCA, nclass = 3)
#' plot.classes(LCA2, LCA3)
#' LCAE <- poLCA(f1, data = ex1.poLCA, nclass = 2:3)
#' plot.classes(LCAE)
#' plot.classes(LCAE, nclass = 3)
plot.classes <- function(x, ..., nclass = NULL, alpha = .05){
  
  if(!(inherits(x, "poLCA2") || inherits(x, "poLCA"))) stop("Please provide a poLCA or poLCA2 object.")
  
  if(inherits(x, "poLCA")){  #(length(list(...)) > 0) && 
    LCA <- list(x, ...)
    cc <- sapply(LCA, function(x) inherits(x, "poLCA"))
    x$LCA <- LCA[cc]
    class(x) <- "poLCA2"
  }
  
  mod <- x[["LCA"]]
  
  if(!is.null(nclass)){
    nc <- sapply(mod, function(x) length(x$P))
    mod <- mod[which(nc %in% nclass)]
  }
  
  if(length(mod) == 0) stop("Discrepancies between the nclass argument and sizes of LCA classes in object.")
  
  out <- sapply(mod, function(w){
    d <- w$y
    d$Classes <- as.factor(predict(w)$Pred)
    d$Nclasses <- as.factor(paste0("nClass = ",length(w$P)))
    d
  }, simplify = FALSE)
  jdtest <- do.call("rbind",out)
  
  # if(inherits(x, "poLCA2")){
  #   if(is.null(nclass) || (length(nclass)>1)){
  #     #PREPARE FOR ALL
  #     if(is.null(nclass)) {mod2 <- x$LCA} else {mod2 <- x$LCA[nclass]}
  #     out <- sapply(mod2, function(w){
  #       d <- w$y
  #       d$Classes <- as.factor(predict(w)$Pred)
  #       d$Nclasses <- as.factor(paste0("nClass = ",length(w$P)))
  #       d
  #     }, simplify = FALSE)
  #     jdtest <- do.call("rbind",out)
  #     
  #   } else {
  #     # PREPARE FOR ONE
  #    
  #     nc <- sapply(mod, function(x) length(x$P))
  #     mod[[which(nc == )]]
  #     ix <- sort(dl, decreasing = TRUE, index.return = TRUE)$ix
  #     mod <- object[ix]
  #     
  #     x <- mod[[nclass]] 
  #   }
  # }
  # 
  # if(inherits(x, "poLCA")){
  #   jdtest <- x$y
  #   jdtest$Classes <- as.factor(predict(x)$Pred)
  #   jdtest$Nclasses <- as.factor(length(x$P))
  # } 
  # 
  
  v <- colnames(jdtest)[!(colnames(jdtest) %in% c("Classes", "Nclasses"))]
  
  jdtest <- tidyr::gather(jdtest, key = "Variable", value = "Value", -.data$Classes, -.data$Nclasses) 
  
  jd.gr <- dplyr::group_by(jdtest, .data$Classes, .data$Variable, .data$Nclasses)
  jds <- dplyr::summarise(jd.gr ,
                          y = mean(.data$Value), 
                          se = sd(.data$Value)/sqrt(dplyr::n()), 
                          ymin = .data$y + qnorm(alpha/2)*.data$se, 
                          ymax = .data$y + qnorm(1-alpha/2)*.data$se,
                          Value = .data$y,
                          .groups = "keep")
  
  
  p <- ggplot2::ggplot(jdtest, aes(x = .data$Variable, 
                                   y = .data$Value, 
                                   fill = .data$Classes, 
                                   group = .data$Classes, 
                                   color = .data$Classes)) + 
    geom_jitter(alpha = .1) +
    geom_point(jds, mapping = aes(x = .data$Variable, y = .data$y)) + 
    geom_errorbar(jds, mapping = aes(ymin = .data$ymin, ymax = .data$ymax), alpha = 1) +
    theme_minimal()
  if(inherits(x, "poLCA2")){
    p <- p + facet_wrap(~.data$Nclasses) + theme_bw()
  }
  p
}
