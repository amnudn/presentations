### test-R.R --- 
#----------------------------------------------------------------------
## Author: Anders Munch
## Created: Jan 30 2024 (08:22) 
## Version: 
## Last-Updated: Jan 30 2024 (16:55) 
##           By: Anders Munch
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

library(prodlim)

cdat <- cbind(SimSurv(300),patnr=sample(1:5,size=30,replace=TRUE))
setDT(cdat)

cdat[, A := 1*(patnr <= 2)]

fit <- prodlim(Hist(time,status)~A,data=cdat)
plot(fit)

cox_fit <- coxph(Surv(time,status)~strata(A), data = cdat, x = TRUE, y = TRUE)

cox_pred <- t(predictRisk(cox_fit, times = cdat[, sort(time)], newdata = data.table(A = c(0,1))))

lines(cdat[, sort(time)], 1-cox_pred[, 1], col = "red", type = "s", lwd = 5)

lines(cdat[, sort(time)], 1-cox_pred[, 2], col = "green", type = "s", lwd = 5)

######################################################################
### test-R.R ends here
