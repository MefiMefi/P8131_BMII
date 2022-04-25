data1 <- data.frame(
  dose = c(0,1,2,3,4),
  nd = c(2,8,15,23,27),
  na = 30-c(2,8,15,23,27)
)

logit.fit <- glm(cbind(nd,na) ~ dose, family = binomial(link = "logit"), data = data1)
probit.fit <- glm(cbind(nd,na) ~ dose, family = binomial(link = "probit"), data = data1)
cloglog.fit <- glm(cbind(nd,na) ~ dose, family = binomial(link = "cloglog"), data = data1)
summary(logit.fit)
summary(probit.fit)
summary(cloglog.fit)


library(tidyverse)
summary_tab_1 <- 
  data.frame(
    model = c("Logit", "Probit", "c-log-log"),
    est_beta = c(logit.fit$coefficients[[2]],probit.fit$coefficients[[2]],cloglog.fit$coefficients[[2]]),
    CI_beta_L = c(
      logit.fit$coefficients[[2]]-(1.96*(summary(logit.fit)$coefficients[2,2])),
      probit.fit$coefficients[[2]]-(1.96*(summary(probit.fit)$coefficients[2,2])),
      cloglog.fit$coefficients[[2]]-(1.96*(summary(cloglog.fit)$coefficients[2,2]))
    ),
    CI_beta_U = c(
      logit.fit$coefficients[[2]]+(1.96*(summary(logit.fit)$coefficients[2,2])),
      probit.fit$coefficients[[2]]+(1.96*(summary(probit.fit)$coefficients[2,2])),
      cloglog.fit$coefficients[[2]]+(1.96*(summary(cloglog.fit)$coefficients[2,2]))
    ),
    Deviance = c(
      logit.fit$deviance,
      probit.fit$deviance,
      cloglog.fit$deviance
    ),
    p_ = c(
      predict(logit.fit,newdata = data.frame(dose = 0.01), type = "response"),
      predict(probit.fit,newdata = data.frame(dose = 0.01), type = "response"),
      predict(cloglog.fit,newdata = data.frame(dose = 0.01), type = "response")
    )
    
  )
# a function to calculate se for any h(beta)
se_of_h_beta <- function(fit_object, h_expr){
  beta_0 = fit_object$coefficients[["(Intercept)"]]
  beta_1 = fit_object$coefficients[[2]]
  h_beta = eval(h_expr)
  
  I_beta = vcov(fit_object)
  #inv_I_beta = solve(I_beta) already inversed!
  partial_d_beta0 = eval(D(h_expr,"beta_0"))
  partial_d_beta1 = eval(D(h_expr,"beta_1"))
  
  partial_d_mtx = matrix(c(partial_d_beta0,partial_d_beta1),2,1)
  print(partial_d_mtx)
  se = sqrt(t(partial_d_mtx) %*% I_beta %*% partial_d_mtx)
  return(c(h_beta,se))
}

LD50_logit=expression(-beta_0/beta_1)

se_of_h_beta(logit.fit, LD50_logit)


f_logit=expression(-beta_0/beta_1)

se_of_h_beta(logit.fit, f_logit)