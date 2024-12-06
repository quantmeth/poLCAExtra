#' Bootstrapped likelihood ratio tests
#'
#' @param object an object of class "poLCA" or "poLCA2".
#' @param nreps the number of replications for the bootstrap. Default is 50.
#' @param ... any other argument for \code{poLCA}.
#'
#' @return A data frame containing mainly :
#' \itemize{
#' \item Average bootstraped Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test;
#' \item Average bootstraped Lo-Mendell-Rubin adjusted likelihood ratio test;
#' \item Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
#' \item Lo-Mendell-Rubin adjusted likelihood ratio test p-value.}
#' 
#' @details This function can take some time especially with "poLCA2" class objects as they contain many "poLCA" objects. 
#'
#' @author 
#' P.-O. Caron
#' 
#' @references
#' 
#' Linzer, D. A. & Lewis, J. F. (2011). poLCA: An R Package for Polytomous Variable Latent Class Analysis. \emph{Journal of Statistical Software}, \emph{42}(10), 1-29. \url{https://www.jstatsoft.org/v42/i10/}
#' 
#' @importFrom MASS ginv 
#' @export
#'
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' out <- poLCA(f1, nclass = 3:4, data = ex1.poLCA)
#' poLCA.blrt(out)
poLCA.blrt <- function(object, nreps = 50, ...){

  jd <- object$data
  N <- nrow(jd)
  
  if(inherits(object,"poLCA2")){
    P <- object$output$nclass
    object <- object$LCA[[1]]
  } else if(inherits(object,"poLCA")) {
    P <- length(object$P)
  }
  
  x <- colnames(object$x)
  y <- colnames(object$y)
  K <- t(matrix(apply(object$y, 2, max)))
  flrt <- as.formula(paste0("cbind(",paste0(y, collapse = ","),")~",paste0(c(1, x[-1]), collapse = "+")))
    #data, formula
  
  test <- (min(P)>1) || (length(P) == 1)
  if(test) P <- c(min(P) - 1, P)
  npar <- P-1 + sum((K - 1)) * P
  
  rez <- matrix(NA, ncol = nreps, nrow = length(P))
  for(i in 1:nreps){
    idx <- sample(N, replace = TRUE) 
    D <- jd[idx,]
    # rez[,i] <- sapply(P, quicker.poLCA, x=x,
    #                   y=D,S=S,J=J,N=N, K.j = K.j,
    #                   probs.start = probs.start, maxiter = maxiter, 
    #                   tol = tol, nrep = nrep)
    mod <- sapply(as.list(P), 
                  FUN = poLCA, 
                  data = D, #data
                  # maxiter = maxiter, #maxiter #### TODO #### 
                  formula = flrt, # formula
                  verbose = FALSE,
                  simplify = FALSE)#,#) #... ####
                  #...)  
    rez[,i] <- sapply(mod, function(x) x$llik)
    
  }
  
  ret <- data.frame(nclass = P,
                    npar=npar,
                    llik = rowMeans(rez))
  
  ret$vlmr <- c(NaN, 2 * (ret$llik[-1] - ret$llik[-nrow(ret)]))
  ret$lmr <- ret$vlmr / c(NA,(1 + (((3 * ret$nclass[-nrow(ret)] - 1) - 
                                      (3 * ret$nclass[-1] - 1)) * log(N))^-1))
  ret$df <- c(NaN, ret$npar[-1] - ret$npar[-nrow(ret)])
  ret$vlmr.p <- pchisq(q = ret$vlmr, df = ret$df, lower.tail = FALSE)
  ret$lmr.p <- pchisq(q = ret$lmr, df = ret$df, lower.tail = FALSE)

  
  if(test) ret <- ret[-1,]
  ret <- list(output = ret)
  class(ret) <- c("poLCAblrt")
  return(ret)
}

#' @export
print.poLCAblrt <- function(x, digit = 3, ...){
  cat("Bootstrapped likelihood ratio tests \n")
  cat("\n")
  tab <- x$output #as.data.frame(x)
  tab <- round(tab, digit)
  tab$vlmr.p <-  ifelse(tab$vlmr.p == 0,"< .001", sprintf(paste0("%.", digit,"f"), tab$vlmr.p))
  tab$lmr.p <-  ifelse(tab$lmr.p == 0,"< .001", sprintf(paste0("%.", digit,"f"), tab$lmr.p))
  tab$lmr <-  sprintf(paste0("%.", digit,"f"), tab$lmr)
  tab$vlmr <-  sprintf(paste0("%.", digit,"f"), tab$vlmr)
  tab[is.na(tab)] <- "NaN"
  tab[tab == "NaN"] <- ""
  print(tab, row.names = FALSE)
}

# 
# 
# quicker.poLCA <- function(R,
#                           x,
#                           y,
#                           S,
#                           J,
#                           N,
#                           K.j,
#                           probs.start = NULL, 
#                           maxiter = 1000, 
#                           tol = 1e-10, 
#                           nrep = 1, ...){
#   
#   poLCA.updatePrior <-
#     function(b,x,R) {
#       b <- matrix(b,ncol=(R-1))
#       exb <- exp(x %*% b)
#       p <- cbind(1,exb)/(rowSums(exb)+1)
#       return(p)
#     }
#   
#   poLCA.vectorize <-
#     function(probs) {
#       classes <- nrow(probs[[1]])
#       vecprobs <- unlist(lapply(probs,t))
#       numChoices <- sapply(probs,ncol)
#       return(list(vecprobs=vecprobs,numChoices=numChoices,classes=classes))
#     }
#   
#   poLCA.postClass.C <-
#     function(prior,vp,y) {
#       ret <-  .C("postclass",
#                  as.double(t(prior)),
#                  as.double(vp$vecprobs),
#                  as.integer(t(y)),
#                  as.integer(length(vp$numChoices)),
#                  as.integer(dim(y)[1]),
#                  as.integer(vp$numChoices),
#                  as.integer(vp$classes),
#                  posterior = double(dim(y)[1]*vp$classes)
#       )
#       ret$posterior <- matrix(ret$posterior,ncol=vp$classes,byrow=TRUE)
#       return(ret$posterior)
#     }
#   
#   poLCA.probHat.C <-
#     function(rgivy,y,vp) {
#       ret <-  .C("probhat",
#                  as.integer(t(y)),
#                  as.double(t(rgivy)),
#                  as.integer(length(vp$numChoices)),
#                  as.integer(dim(y)[1]),
#                  as.integer(vp$numChoices),
#                  as.integer(vp$classes),
#                  ph = double(sum(vp$numChoices)*vp$classes)
#       )
#       return(ret$ph)
#     }
#   
#   poLCA.dLL2dBeta.C <-
#     function(rgivy,prior,x) {
#       classes <- dim(prior)[2]
#       numx <- dim(x)[2]
#       ret <-  .C("d2lldbeta2",
#                  as.double(t(rgivy)),
#                  as.double(t(prior)),
#                  as.double(t(x)),
#                  as.integer(dim(x)[1]),
#                  as.integer(classes),
#                  as.integer(numx),
#                  grad = double((classes-1)*numx),
#                  hess = double(((classes-1)*numx)^2)                
#       )
#       return(list(grad=ret$grad,hess=-matrix(ret$hess,ncol=((classes-1)*numx),byrow=TRUE)))
#     }
#   
#   poLCA.ylik.C <-
#     function(vp,y) {
#       ret <-  .C("ylik",
#                  as.double(vp$vecprobs),
#                  as.integer(t(y)),
#                  as.integer(dim(y)[1]),
#                  as.integer(length(vp$numChoices)),
#                  as.integer(vp$numChoices),
#                  as.integer(vp$classes),
#                  lik = double(dim(y)[1]*vp$classes)
#       )
#       ret$lik <- matrix(ret$lik,ncol=vp$classes,byrow=TRUE)
#       return(ret$lik)
#     }
#   #START ####
#   
#   probs.start.ok <- TRUE
#   ret <- list()
#   if (R==1) {
#     ret$probs <- list()
#     for (j in 1:J) {
#       ret$probs[[j]] <- matrix(NA,nrow=1,ncol=K.j[j])
#       for (k in 1:K.j[j]) { ret$probs[[j]][k] <- sum(y[,j]==k)/sum(y[,j]>0) }
#     }
#     ret$llik <- sum(log(poLCA.ylik.C(poLCA.vectorize(ret$probs),y)) - log(.Machine$double.xmax))
#   } else {
#     ret$llik <- -Inf
#     ret$attempts <- NULL
#     for (repl in 1:nrep) { 
#       error <- TRUE; firstrun <- TRUE
#       probs <- probs.init <- probs.start
#       while (error) {
#         error <- FALSE
#         b <- rep(0,S*(R-1))
#         prior <- poLCA.updatePrior(b,x,R)
#         if ((!probs.start.ok) | (is.null(probs.start)) | (!firstrun) | (repl>1)) { 
#           probs <- list()
#           for (j in 1:J) { 
#             probs[[j]] <- matrix(runif(R*K.j[j]),nrow=R,ncol=K.j[j])
#             probs[[j]] <- probs[[j]]/rowSums(probs[[j]]) 
#           }
#           probs.init <- probs
#         }
#         vp <- poLCA.vectorize(probs)
#         iter <- 1
#         llik <- matrix(NA,nrow=maxiter,ncol=1)
#         llik[iter] <- -Inf
#         dll <- Inf
#         while ((iter <= maxiter) & (dll > tol) & (!error)) {
#           iter <- iter+1
#           rgivy <- poLCA.postClass.C(prior,vp,y)      # calculate posterior
#           vp$vecprobs <- poLCA.probHat.C(rgivy,y,vp)  # update probs
#           if (S>1) {
#             dd <- poLCA.dLL2dBeta.C(rgivy,prior,x)
#             b <- b + MASS::ginv(-dd$hess) %*% dd$grad     # update betas
#             prior <- poLCA.updatePrior(b,x,R)       # update prior
#           } else {
#             prior <- matrix(colMeans(rgivy),nrow=N,ncol=R,byrow=TRUE)
#           }
#           llik[iter] <- sum(log(rowSums(prior*poLCA.ylik.C(vp,y))) - log(.Machine$double.xmax))
#           dll <- llik[iter]-llik[iter-1]
#           if (is.na(dll)) {
#             error <- TRUE
#           } else if ((S>1) & (dll < -1e-7)) {
#             error <- TRUE
#           }
#         }
#         firstrun <- FALSE
#       }
#       ret$attempts <- c(ret$attempts,llik[iter])
#       if (llik[iter] > ret$llik) {
#         ret$llik <- llik[iter] 
#       }
#     }
#   }
#   ret$llik
# }
# 
