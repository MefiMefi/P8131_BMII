#################
### Problem 1 ###
#################
# 1
age <- rep(c(25, 35, 45, 55, 65, 75), 2)
case <- c(1, 4, 25, 42, 19, 5, 0, 5, 21, 34, 36, 8)
control <- c(9, 26, 29, 27, 18, 0, 106, 164, 138, 139, 88, 31)
exposure <- c(rep(1, 6), rep(0 , 6))
resp <- cbind(case, control)
logit.prosp=glm(resp~exposure+age, family=binomial(link='logit'))
summary(logit.prosp)
# 2
library(psych)
age_cat <- c(1:6)
age_cat <- factor(age_cat)
ind <- dummy.code(age_cat)
a1 <- rep(ind[,1], 2)
a2 <- rep(ind[,2], 2)
a3 <- rep(ind[,3], 2)
a4 <- rep(ind[,4], 2)
a5 <- rep(ind[,5], 2)
a6 <- rep(ind[,6], 2)
m0=glm(resp~a1+a2+a3+a4+a5+a6, family=binomial(link='logit'))
summary(m0)
sum(residuals(m0,type='deviance')^2)
m1=glm(resp~exposure+a1+a2+a3+a4+a5+a6, family=binomial(link='logit'))
summary(m1)
sum(residuals(m1,type="deviance")^2)
#################
### Problem 2 ###
#################
seed <- c(rep(1, 11), rep(0, 10)) # 1=O.aegyptiaca 75
root <- c(rep(1, 5), rep(0, 6), rep(1, 5), rep(0, 5)) # 1=bean
y <- c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0,
       3, 22, 15, 32, 3)
m <- c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45,
       4, 12, 41, 30, 51, 7)
# 1
# fit binomial (logistic) without dispersion
none.disp=glm(cbind(y,m-y)~seed+root, family=binomial(link='logit'))
summary(none.disp)
1-pchisq(none.disp$deviance, 21-3)
sum(residuals(none.disp,type='pearson')^2)
# calc dispersion param
G.stat=sum(residuals(none.disp,type='pearson')^2) # pearson chisq
G.stat
phi=G.stat/(21-3)
phi
tilde.phi=none.disp$deviance/none.disp$df.residual
tilde.phi # similar to the one estimated from pearson chisq
#######################
# test over-dispersion (half normal plot)
res=residuals(none.disp,type='pearson')
plot(qnorm((21+1:21+0.5)/(2*21+1.125)),sort(abs(res)),
     xlab='Expected Half-Normal Order Stats', ylab = "Ordered as pearson residuals")

summary(none.disp,dispersion = phi)
1-pchisq(none.disp$deviance/phi, 21-3)
