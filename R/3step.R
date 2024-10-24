#' Bootstraped 3-step testing of latent class analysis
#'
#' @param formula Either a vector containing a string indicating the dependent variable to test or an object of class "formula". The details of model specification are given under ‘Details’
#' @param x Object of class inheriting from "poLCA" or "poLCA2".
#' @param nclass f \code{object} contains many "poLCA" object (i.e., if it a "poLCA2" object), choose the model with the given number of classes.
#' @param nreps The number of replications for the bootstrap. Default is 1000.
#' @param alpha For the confidence interval. Default if 5\%, of\code{alpha = .05} 
#' @param ... further arguments passed to or from other methods.
#'
#' @return A list containing the comparison indices in a vector and an array containing the conditional median and confidence interval of each classes.
#' 
#' @details Some examples of formula :
#' \itemize{
#'   \item  \code{DV ~ 1} is the same as \code{"DV"} or \code{DV ~ classes}.
#'   \item Add covariate as \code{DV ~ IV} or \code{DV ~ IV + classes}
#' }
#' d3step and r3step is the same function. It will detect automatically if \code{DV} is a factor (d3step) or continuous (r3step).
#' 
#' @export
#' 
#' @aliases r3step d3step
#' 
#' @import nnet
#' @import stats
#' @import utils
#' 
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' out <- poLCA(f1, nclass = 1:3, data = ex1.poLCA)
#' r3step("continuous", out, nclass = 3)
#' d3step("categorical", out, nclass = 3)
d3step <- function(formula, x, nclass = NULL, nreps = 1000, alpha = .05, ...){
  
  if(inherits(x, "poLCA2")){
    if(is.null(nclass)) stop("Specify nclass to proceed.")
    ncx <- sapply(x$LCA, function(w) length(w$P))
    nc <- which(nclass == ncx)
  } else {
    nclass <- nc <- length(x$P)
  }
  
  
  
  #if(is.null(nclass) && inherits(x, "poLCA2")) stop("Specify nclass to proceed.")
  
  if(!inherits((formula), "formula")) {
    formula <- as.formula(paste0(formula,"~ classes"))
  }
  
  
  
  # if(!any(all.vars(formula) == "classes")) {
  #   formula <- as.formula(paste0(formula,"+ classes"))
  # }
  
  D <- x$data
  probs <- predict(x, nclass)[,1:nclass]
  test <- t(apply(probs, 1, cumsum))
  
  N <- nrow(D)
  
  v <- all.vars(formula)[[1]]
  nv <- all.vars(formula)[!all.vars(formula) %in% v]
  
  
  f0 <- as.formula(paste0(v, " ~ ",  paste0(c(1, all.vars(formula) [all.vars(formula) != "classes"][-1]), collapse = "+"))) 
  f1 <- as.formula(paste0(c(f0," + classes"), collapse = ""))
  
  out <- list()
  
  #
  if(is.factor(D[,v])) {
    
    cla <- "d3step"
    # ls <- sapply(D[v], unique)
    # ls[names(ls) != "classes"] <- 0
    # ls <- as.data.frame(expand.grid(ls))
    # colnames(ls)[1] <- "classes"
    # ls$classes <- as.factor(ls$classes)
    
    # ne marche pas ?
    #f0 <- as.formula(paste0(v, "~ ", all.vars(formula) [all.vars(formula) != "classes"][-1]))
    
    #
    res <- matrix(NA, ncol = 4, nrow = nreps)
    M <- array(dim = c(nclass, dim(unique(D[v]))[1], nreps))
    
    for(i in 1:nreps){
      D$classes <- as.factor(rowSums(test < matrix(runif(N), ncol = ncol(test), N)) + 1)
      
      a <- capture.output(r1 <- nnet::multinom(formula = f1, data = D))
      a <- capture.output(r0 <- nnet::multinom(formula = f0, data = D))
      
      rs <- anova(r0, r1)
      res[i,] <- c(LR = rs$`LR stat.`[2],
                   AIC = r1$AIC - r0$AIC,
                   df = rs$`   Df`[2],
                   p = rs$`Pr(Chi)`[2])
      
      
      if(dim(unique(D[v]))[1] > 2){
        M[,,i] <- predict(r1, newdata = data.frame(classes = levels(D$classes)), type = "probs")
      }else{
        pr <- predict(r1, newdata = data.frame(classes = levels(D$classes)), type = "probs")
        M[,,i] <- cbind(1-pr, pr)
      }
      #M[,,i] <-  predict(r, newdata = ls$classes, type="probs")
      #prob_df <- data.frame(D[v], predict(r1, newdata = D, type = "probs"))
      #M[,,i] <- aggregate(prob_df[, -1], by = list(D[,v]), FUN = mean, drop = FALSE, simplify = FALSE)[,-1]
      #tb <- 
      #prob_df <- 
      
      # M[,,i] <- as.matrix(aggregate(prob_df[, -1], 
      #                               by = list(D[,v]), 
      #                               FUN = mean)[,-1],
      #                     nclass,
      #                     dim(unique(D[v]))[1])
      
      
    }
    
    
    stats <- colMeans(res)   
    names(stats) <-  c("LR", "AIC", "df", "p")
    
    colnames(M) <- c(paste0("Pr(",v,"==", 
                            levels(D[,v]),
                            ")"))
    rownames(M) <- c(paste0("Pr(Class==", 1:nclass,")"))
    btstrp <- M
    M <- apply(M, 1:2, quantile, probs=c(alpha/2, .5 , 1-alpha/2))
    #M <- aperm(M, c(3,2,1))
    M <- aperm(M, c(2,1,3))
  } else{
    
    cla <- "r3step"
    
    res <- matrix(NA, ncol = (5), nrow = nreps)
    M <-  array(dim = c(nclass, nreps))
    
    
    for(i in 1:nreps){
      D$classes <- as.factor(rowSums(test < matrix(runif(N), ncol = ncol(test), N)) + 1)
      
      
      # option X ####
      
      h0 <- lm(f0, data = D)
      h1 <- lm(f1, data = D)
      
      
      rs <- anova(h0, h1)
      res[i,] <- c(LR = (logLik(h1) - logLik(h0))[1],
                   AIC = AIC(h1) - AIC(h0),
                   df = (summary(h1)$df - summary(h0)$df)[1],
                   R2 = summary(h1)$r.squared - summary(h0)$r.squared,
                   p =  rs[2,"Pr(>F)"])
      # rs["Pr(>F)"]
      # AIC(h1) - AIC(h0)
      # logLik(h1)- logLik(h0)
      # (summary(h1)$df - summary(h0)$df)[1]
      # pchisq(AIC(h1) - AIC(h0), (summary(h1)$df - summary(h0)$df)[1] , lower.tail = TRUE)
      # 
      # # option 1 ####
      # r <- aov(data = D, formula = formula)
      # rs <- summary(r)[[1]]
      # 
      M[,i] <- predict(h1, data.frame(classes = levels(D$classes)))
      
      
      # r$coefficients
      # r$model
      # res[i,] <- c(F = rs$`F value`[which(substr(row.names(rs), 1, 7) == "classes")],
      #              df = rs$Df[c(which(substr(row.names(rs), 1, 7) == "classes"),length(rs$Df))], 
      #              p = rs$`Pr(>F)`[which(substr(row.names(rs), 1, 7) == "classes")],
      #              M = predict(r, ls))
    }
    
    stats <- colMeans(res)
    names(stats) <-  c("LR", "AIC", "df","R2","p")
    rownames(M) <- c(paste0("Pr(Class==", 1:nclass,")"))
    btstrp <- M
    M <- apply(M, 1, quantile, probs=c(alpha/2, .5, 1-alpha/2))
    #colnames(M) <- c(paste0("Pr(Class==", 1:nclass,")"))
    M <- t(M)
  }
  
  out <- list(stats = stats, 
       M = M, 
       DV = v,
       btstrp = btstrp)
  
  class(out) <- cla
  
  return(out)
  
}

#' @rdname d3step
#' @export
r3step <- d3step

