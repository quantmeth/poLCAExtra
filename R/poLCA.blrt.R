#' Bootstrapped Likelihood Ratio Test
#'
#' @param object an object of class "poLCA" or "poLCA2".
#' @param nreps the number of replications for the bootstrap. Default is 100.
#' @param quick an option tostop faster with "poLCA2" objects. It will stop at the first \code{p < alpha}.
#' @param alpha a type I error threshold to stop the blrt test.
#' @param verbose option to display the spinner (\code{TRUE}) or not.
#' @param ... any other argument for \code{poLCA}.
#'
#' @return A data frame containing results of the Bootstrapped Likelihood Ratio Test.
#' 
#' @details This function can take some time especially with "poLCA2" class objects as they contain many "poLCA" objects. 
#'
#' @aliases tech14 poLCA.blrt
#'
#' @author 
#' P.-O. Caron
#' 
#' @references
#' 
#' Linzer, D. A. & Lewis, J. F. (2011). poLCA: An R Package for polytomous variable latent class analysis. \emph{Journal of Statistical Software}, \emph{42}(10), 1-29. \url{https://www.jstatsoft.org/v42/i10/}
#' 
#' McLachlan, G. (2000). \emph{Finite mixture models}. Wiley
#' 
#' @importFrom MASS ginv 
#' @export
#'
#' @examples
#' f1 <- cbind(V1, V2, V3, V4, V5, V6) ~ 1
#' out <- poLCA(f1, nclass = 3:4, data = ex1.poLCA)
#' \dontrun{
#' poLCA.blrt(out)
#' }
poLCA.blrt <- function(object, nreps = 100, quick = TRUE, alpha = .05,  verbose = TRUE, ...){
  
  jd <- object$data
  N <- nrow(jd)
  
  if(inherits(object,"poLCA2")){
    P <- object$output$nclass
    qwe <- object$LCA[[1]]
    LCA <- object$LCA
  } else if(inherits(object,"poLCA")) {
    P <- length(object$P)
    qwe <- object
    LCA <- object
  }
  
  y <- colnames(qwe$y)
  K <- t(matrix(apply(qwe$y, 2, max)))
  test <- (min(P)>1) || (length(P) == 1)
  
  if(test) {
    P <- c(min(P) - 1, P)
    x <- colnames(qwe$x)
    
    flrt <- as.formula(paste0("cbind(", paste0(y, collapse = ","),")~", paste0(c(1, x[-1]), collapse = "+")))
    LCA <- c(list(poLCA(flrt,
                        nclass = P[1],
                        data = jd,
                        verbose = FALSE,
                        calc.se = FALSE)),#,#,
             #...),
             LCA)
  }
  
  npar <- P - 1 + sum((K - 1)) * P
  rez <- matrix(NA, ncol = nreps, nrow = length(P)-1)
  
  f0 <- as.formula(paste0("cbind(",paste0(paste0("Y",1:length(y)), collapse = ","),")~1"))
  
  poLCA2.simulate <- function(x){
    D <- poLCA::poLCA.simdata(N = nrow(x$data),
                              probs = x$probs,
                              P = x$P)$dat
    D2 <- poLCA(f0, 
                data = D, 
                nclass = length(x$P):(length(x$P)+1), 
                verbose = FALSE,
                calc.se = FALSE)
    2*diff(sapply(D2$LCA, function(x) x$llik))
  }
  
  lca.idx <- as.matrix(1:(length(LCA)-1))
  if(quick){
    lca.idx <- t(lca.idx)
  }
  
  llike1 <- sapply(LCA, function(x) x$llik)
  llike2 <- 2*-(llike1[-length(llike1)]-llike1[-1])
  
  # TO PARALLEL ####
  
  if(verbose) cat("poLCA.blrt() may take some times. \n"); spinner <- c("/", "|", "\\", "-")
  for(k in 1:ncol(lca.idx)){
    for(i in 1:nreps){
      #if(!(i%%10)) 
      if(verbose) cat("\r", spinner[(i %% length(spinner)) + 1], "Calcul en cours", sep = " ")
      rez[lca.idx[,k],i] <- sapply(LCA[lca.idx[,k]], poLCA2.simulate)
    }
    #drez <- cbind(llike, rez)
    p.value <- apply(cbind(llike2, rez), 1, function(x) mean(x[-1] >= x[1]))
    if(quick && (any(na.omit(p.value) >= alpha))) break
  }
  if(verbose) cat("\r\u2714 done                      \n")
  ret <- list(output = na.omit(data.frame(test = paste0(P[-1]," vs ",P[-length(P)]),
                                          H0_llik = llike1[-length(llike1)],
                                          `2loglik_diff` = llike2,
                                          npar = npar[-1]-npar[-length(npar)],
                                          mean = rowMeans(rez),#rowMeans(drez[,-1]),
                                          `s.e.` = apply(rez,1,sd),
                                          p = p.value)))
  class(ret) <- c("poLCAblrt")
  return(ret)
}

#' @rdname poLCA.blrt 
#' @export
poLCA.tech14 <- poLCA.blrt 

#' @export
print.poLCAblrt <- function(x, digit = 3, alpha = .05, ...){
  cat("Vuong-Lo-Mendell-Rubin Likelihood Ratio Test \n")
  cat("\n")
  tab <- x$output #as.data.frame(x)
  tab[-1] <- round(tab[-1], digit)
  tab$p <-  ifelse(tab$p == 0,"< .001", sprintf(paste0("%.", digit,"f"), tab$p))
  print(tab, row.names = FALSE)
  cat("\n")
  cat(paste0("At ", (1-alpha) * 100,"% condidence, blrt recommends ", min(which(tab$p>=alpha))," classes.\n"))
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
