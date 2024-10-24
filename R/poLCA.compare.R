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
