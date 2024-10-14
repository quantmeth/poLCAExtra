library(poLCA)
set.seed(42)
probs <- list(matrix(c(.95, .05,
                       .99, .01,
                       .30, .70),
                     ncol = 2, byrow = TRUE),
              matrix(c(.95, .05,
                       .20, .80,
                       .15, .85),
                     ncol = 2, byrow = TRUE),
              matrix(c(.90, .10,
                       .20, .80,
                       .20, .80),
                     ncol = 2, byrow = TRUE),
              matrix(c(.90, .10,
                       .98, .02,
                       .23, .77),
                     ncol = 2, byrow = TRUE),
              matrix(c(.90, .10,
                       .20, .80,
                       .10, .90),
                     ncol = 2, byrow = TRUE),
              matrix(c(.90, .10,
                       .97, .03,
                       .20, .80),
                     ncol = 2, byrow = TRUE))

N = 800
simdata <- poLCA.simdata(N = N, probs)
jd <- simdata$dat
colnames(jd) <- paste0("V",1:ncol(jd))


mod1 <- poLCA(f1, data = jd, nclass = 3) 
probs <- as.data.frame(cbind(mod1$posterior, 
                             mod1$predclass))
colnames(probs) <- c(paste0("Pr(Class==", 1:length(mod1$P),")"),"Pred")
jd1 <- cbind(jd, p = probs[,4])
jd1$p <- as.factor(jd1$p)
jd$continuous <- round(ifelse(jd1$p == 1, rnorm(nrow(jd), 21, 7), 
                        ifelse(jd1$p == 2, rnorm(nrow(jd), 22.5, 7),
                               rnorm(nrow(jd), 24, 7))))
jd1$c <- jd$continuous
summary(lm(c ~ p, data = jd1))
summary(aov(c ~ p, data = jd1))

jd$categorical <- as.factor(ifelse(jd1$c > 25, 0, 1))
jd1$t <- jd$categorical
chisq.test(jd1$t~jd1$p)

library(ggplot2)
ggplot(jd1, mapping =aes(x = c, fill = p)) + geom_density()
save(file = "ex1.poLCA.rda", jd)
write.csv(ex1.poLCA,"ex1.poLCA.sansnom.csv", row.names = FALSE)

jd$test <- rnorm(N)*5+10
jd$test <- as.factor(sample(letters[1:3], replace = TRUE, size = N))
#write.csv2(jd, "test.csv" )

#
f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ 1)

LCA1 <- poLCA(f1, data = jd, nclass = 1) 
LCA2 <- poLCA(f1, data = jd, nclass = 2) 
LCA3 <- poLCA(f1, data = jd, nclass = 3) 
LCA4 <- poLCA(f1, data = jd, nclass = 4, maxiter = 10000)
LCA5 <- poLCA(f1, data = jd, nclass = 5, maxiter = 10000)
LCA6 <- poLCA(f1, data = jd, nclass = 6, maxiter = 10000)
LCA7 <- poLCA(f1, data = jd, nclass = 7, maxiter = 10000)
LCA8 <- poLCA(f1, data = jd, nclass = 8, maxiter = 10000)
LCA9 <- poLCA(f1, data = jd, nclass = 9, maxiter = 10000)

mod <- sapply(as.list(1:3), poLCA, data = jd, maxiter = 10000, formula = f1)

poLCA.entropy(LCA1)
poLCA.relentropy(LCA2)
poLCA.lrt(LCA1,LCA2)
poLCA.maxclasses(jd)
rez <- poLCA2(f1, data = jd, maxclasses = 5)
rez
predict(rez, 2)


f1 <- as.formula(cbind(V1, V2, V3, V4, V5, V6) ~ test)
poLCA(f1, jd, nclass = 1:4)

