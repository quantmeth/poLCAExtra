# in development
poLCA.inspect.cov <- function(f1, data = NULL, max.cl = NULL, nreps = 500){
  if(inherits(f1, "poLCA")){}#dev
  if(inherits(object, "poLCA2")){}#dev
  
  cl <- makeCluster(detectCores()[1]-1)
  registerDoParallel(cl)
  # CALL PACKAGES ####

  
  # MAKE THINGS FIT
  #D <- data
  n <- nrow(data)
  
  # USE poLCA.maxclasses TODO ####

  if(is.null(max.cl)) {
    nv <- length(strsplit(as.character(f1)[2], ",")[[1]]) / 2
    max.cl <- nv
  } else {
    nv <- max.cl
  }
  
  K <- apply(data, 2, function(x) length(unique(x)))
  
  npar <- 1:nv - 1 + sum((K - 1)) * 1:nv
  v.resid <- n - npar
  if(any(v.resid < 0)) max.cl <- max(which(v.resid > 0))
  ## END
  
  max.lca <- poLCA(formula = f1, 
                   data = data, 
                   nclass = max.cl, 
                   verbose = FALSE,
                   calc.se = FALSE, maxiter = 1000, nrep = 4)
  S <- cov(data)
  out <- foreach(i = 1:nreps, .combine = rbind,
                 .packages = c("poLCAExtra")) %dopar% { # to parallel
                   D <- poLCA::poLCA.simdata(N = nrow(max.lca$y),
                                             probs = max.lca$probs,
                                             P = max.lca$P)$dat
                   
                   LCA <- poLCA(formula = f1, 
                                data = D, 
                                nclass = 1:max.cl, 
                                verbose = FALSE,
                                calc.se = FALSE)
                   # TODO ####
                   data.frame(nc = 1:max.cl,
                              sim = i,
                              chi2 = sapply(LCA$LCA, function(x) poLCA.cov(x)$chi2)[1,],       
                              fml = sapply(LCA$LCA, function(x) fml(poLCA.cov(x)$SigmaHat, S)  * (n-1)),
                              llik = sapply(LCA$LCA, function(x) x$llik))
                 }
  stopCluster(cl)
  # TODO ####
  out <- out %>% mutate(df = dof(ni, nc-1),
                        df2 = dof(ni, nc-2),
                        nc = as.factor(nc))
  # TODO ####
  out %>% 
    group_by(nc) %>% 
    summarise(FML = mean(fml),
              LL = mean(llik),
              CHI2 = mean(chi2),
              v = mean(df)) %>% 
    mutate(p=round(pchisq(CHI2, v, lower.tail = FALSE),3))
  # TODO ####
  ggplot(out %>% filter(nc != 1), aes(x = chi2, group = nc, fill = nc)) + geom_density(alpha = .5) +
    geom_vline(aes(xintercept = df, color = nc))
}