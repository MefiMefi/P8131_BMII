## R code
library(dplyr)
library(MASS)
library(nnet)
dat <- c(65, 34, 54, 47, 100, 100,
         130, 141, 76, 116, 111, 191,
         67, 130, 48, 105, 62, 104)
dat_tb<- array(dat, c(2, 3, 3),
               dimnames = list(Contact = c("Low", "High"),
                               Response = c("Low.sat", "Median.sat", "High.sat"),
                               Area = c("Tower", "Apartment", "House")))
ftable(dat_tb)
################################
## question 1
################################
## 1.1
hous <- margin.table(dat_tb, margin =c(2, 3))
prop.table(hous, margin = 2) %>% round(., 4)
chisq.test(hous)
## 1.2
hous <- margin.table(dat_tb, margin =c(1, 3))
prop.table(hous, margin = 2) %>% round(., 4)
chisq.test(hous)
## 1.3
cont <- margin.table(dat_tb, margin =c(1, 2))
prop.table(cont, margin = 1) %>% round(., 4)
chisq.test(cont)
################################
## question 2
################################
dat_full <- data.frame(low = as.vector(dat_tb[,1,]),
                       median = as.vector(dat_tb[,2,]),
                       high = as.vector(dat_tb[,3,]),
                       int = rep(c(1, 2), 3),
                       cate = rep(c(2, 1, 3), each = 2))
## 2.1
## model 1 without interaction
m1 <- multinom(cbind(dat_full$low, dat_full$median, dat_full$high)
               ~ factor(int) + factor(cate), data = dat_full)
summary(m1)
exp(coef(m1))
exp(confint(m1))
## model 2 with interaction
m2 <- multinom(
               ~ factor(int) + factor(cate) + factor(cate)*factor(int), data = dat_full)
summary(m2)
exp(coef(m2))
exp(confint(m2))
## LRT
TS1 <- deviance(m1) - deviance(m2)
p1 <- 1-pchisq(TS1, 4)
## model 3 with house type
m3 <- multinom(cbind(dat_full$low, dat_full$median,
                     dat_full$high) ~ factor(cate), data = dat_full)
TS2 <- deviance(m3) - deviance(m1)
p2 <- 1-pchisq(TS2, 2)
## model 4 with contact with others
m4 <- multinom(cbind(dat_full$low, dat_full$median,
                     dat_full$high) ~ factor(int), data = dat_full)
TS3 <- deviance(m4) - deviance(m1)
p3 <- 1-pchisq(TS3, 4)
## goodness of fit
pihat <- predict(m1, type = "probs")
m <- rowSums(dat_full[,1:3])
res.pearson <- (dat_full[,1:3]-pihat*m)/sqrt(pihat*m)
G.stat <- sum(res.pearson^2)
p4 <- 1-pchisq(G.stat, (6-4)*(3-1)) #chi-sq df = (n-p)*(J-1) = (6-4)*(3-1)
################################
## question 3
################################
freq <- c(dat_full$low, dat_full$median, dat_full$high)
res <- c(rep(c("L", "M", "H"), c(6, 6, 6)))
res <- factor(res, levels = c("L", "M", "H"), ordered = T)
dat_ord <- data.frame(res = res, int = rep(dat_full$int, 3),
                      cate = rep(dat_full$cate, 3), freq = freq)
m5 <- polr(res ~ factor(int) + factor(cate), dat_ord,
           weights = freq, method = "logistic")
summary(m5)
exp(coef(m5))
exp(confint(m5))
m6 <- polr(res ~ factor(cate), dat_ord, weights = freq)
TS5 <- deviance(m6) - deviance(m5)
p5<- 1-pchisq(TS5, 1)
m7 <- polr(res ~ factor(int), dat_ord, weights = freq)
TS6 <- deviance(m7) - deviance(m5)
p6<- 1-pchisq(TS6, 2)
#Pearson Chi-Square residuals from proportional odds model
pihat <- predict(m5, type = "probs")
m <- rowSums(dat_full[,1:3])
res.pearson <- ((dat_full[,1:3]-pihat*m)/sqrt(pihat*m)) %>% round(.,2) %>% as.data.frame()
