## OLS testing
library(MASS)


## create simulation time series
## yt = 1.2 + 2.2*x1 + 2.3*x2 + et
## et = rho * et_1 + wt

x <- mvrnorm(n=500,mu=c(0.05,0.08),Sigma=matrix(c(0.4,0.3,0.3,0.6),nrow=2,
                                               ncol=2,byrow=TRUE))

et <- arima.sim(list(order = c(1,0,0),ar=0.7),n=500,rand.gen=rnorm,sd=0.3)
#lm(et[2:length(et)]~et[1:(length(et)-1)])

yt <- 1.2 + x %*% c(2.2,2.3)+et
regressY <- lm(yt~x)
resi <- regressY$residuals

## regress on resi hat
regressE <- lm(resi[2:length(resi)]~resi[1:(length(resi)-1)])
rho <- regressE$coefficients[2]
## GLS
## yt - rho * yt_1 = alpha * (1-rho) + beta1*(x1_t - rho * x1_t-1) + beta * 
## (x2_t - rho * x2_t-1)
yt2 <- yt[2:length(yt)]
xt2 <- x[2:nrow(x),]
xprime <- (xt2- c(rho,rho) * x[1:(nrow(x)-1),])
regressY_GLS <- lm( (yt2-rho*yt[1:(length(yt)-1)]) ~ xprime)

alpha <- regressY_GLS$coefficients[1]/(1-rho)



