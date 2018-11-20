#see data available
data()

#we will work CO2:
head(CO2)

#make it small for calculation purposes, and plot it:
CO2.short <- CO2[1:4,]
CO2.short
CO2.short <- CO2.short[c("conc", "uptake")]
CO2.short
plot(data=CO2.short, conc~uptake)

#linear regression using linear algebra:
X <- cbind(1,CO2.short$uptake)
X
solve( t(X) %*% X ) %*% t(X) %*% CO2.short$conc

#linear regression using lm:
rm.lm <- lm(conc~uptake, data=CO2.short)
coefficients(rm.lm)

#log looks a good transformation:
rm.lmlog <- lm(log(conc)~uptake, data=CO2.short)
coefficients(rm.lmlog)

X <- cbind(1,CO2.short$uptake)
X
solve( t(X) %*% X ) %*% t(X) %*% log(CO2.short$conc)

plot(data=CO2.short, log(conc)~uptake)

anova(rm.lm)
summary(rm.lm)

anova(rm.lmlog)
summary(rm.lmlog)


par(mfrow=c(2,2))
plot(rm.lm) 

par(mfrow=c(1,1))
plot(rm.lm) 


par(mfrow=c(2,2))
plot(rm.lmlog) 

CO2.short$logconc <- log(CO2.short$conc)


fitted(rm.lmlog)
residuals(rm.lmlog)

#The degrees of freedom associated with each sum of squares is determined
#by the sample size n and the number of parameters p in the model. [We
#use p' to denote the number of parameters in the model and p (without
#the prime) to denote the number of independent variables; p' = p+1 when
#the model includes an intercept] The degrees of freedom
#associated with SS(Model) is p'=2; the degrees of freedom associated with
#SS(Regr) is always 1 less to account for subtraction of the correction factor,




#http://data.princeton.edu/r/linearmodels.html
rm.lm <- lm(conc~uptake, data=CO2)
coefficients(rm.lm)

X <- cbind(1,CO2$uptake)
X
solve( t(X) %*% X ) %*% t(X) %*% CO2$conc


rm.lm <- lm(conc~uptake+Type, data=CO2)
coefficients(rm.lm)





rr <- function(x, y) {coefficients(lm(x, data=y))}
rr(x=conc~uptake+Type, y=CO2)
rr(conc~uptake:Type, CO2)
rr(conc~uptake*Type, CO2)
summary(rm.lm)



par(mfrow=c(2,2))
plot(rm.lm) 

par(mfrow=c(1,1))
plot(rm.lm) 

fitted(rm.lm)
residuals(rm.lm)

sort(CO2$uptake)


CO2$CO2.class <- cut(CO2$uptake, breaks = c(7.7, 31.5, 40.6, 45.5), label=c("one","two","three"))
 
lm(conc~ log(uptake)+Type, data=CO2)

library(splines) #function is bs
rm.bs <- bs(Type, knots = c(7.7, 31.5, 40.6, 45.5)+ uptake) 
#end of http://data.princeton.edu/r/linearmodels.html

