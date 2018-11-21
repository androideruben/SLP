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

###############################################################################
#The degrees of freedom associated with each sum of squares is determined
#by the sample size n and the number of parameters p in the model. [We
#use p' to denote the number of parameters in the model and p (without
#the prime) to denote the number of independent variables; p' = p+1 when
#the model includes an intercept] The degrees of freedom
#associated with SS(Model) is p'=2; the degrees of freedom associated with
#SS(Regr) is always 1 less to account for subtraction of the correction factor,

############################################################################

#simulating a Poisson distribution p(x)=λ^x exp(-λ)/x!
y <- rpois(n=1000, lambda = 1) # Poisson distribution
mean(y)
var(y)
hist(y)

#simulating a Normal distribution N(0, sigma^2)
y <- rnorm(n=1000, mean=0, sd=1) # set parameters
mean(y)
var(y)
hist(y)

f <- function(x) {mean(x)}
f(y)


#regression (generalized linear models) model Poisson
##create data
x <- seq.int(from=1, to =1000)

rm.data <- as.data.frame(cbind(y, x))
str(rm.data)

rm.glm <- glm(y~x, family="poisson", data=rm.data)
summary(rm.glm)

#regression (generalized linear models) model Normal
##create data
x <- seq.int(from=1, to =1000)
y <- rnorm(n=1000, mean=0, sd=1) # set parameters

rm.data <- as.data.frame(cbind(y, x))
str(rm.data)

rm.glm <- glm(y~x, family="gaussian", data=rm.data)
summary(rm.glm)

rm.lm <- lm(y~x, data=rm.data)
summary(rm.lm)


#Function
info <-function(d) {writeLines("structure of the data")
str(d)
writeLines("Then print the first observations")
print(head(d))}

info(rm.data)


##output
### Make a nice 4 way display with two plots and two text summaries
getwd()
library(gplots)
     data(iris)
     par(mfrow=c(2,2))
     
     plot( Sepal.Length ~ Species, data=iris, border="blue", col="cyan", main="1. Sepal Length by Species" )
     
     plotmeans( Sepal.Length ~ Species, data=iris, barwidth=2, connect=FALSE, main="2. 95% C.I. means")


     reg <- lm( Sepal.Length ~ Species, data=iris )
     textplot( capture.output(summary(reg)), valign="top")
     title("3. lm Sepal Length by Species")

     par(mfrow=c(1,1))
     
     
     
#############################################################################     
#http://data.princeton.edu/r/linearmodels.html
rm.lm <- lm(conc~uptake, data=CO2)
coefficients(rm.lm)

X <- cbind(1,CO2$uptake)
X
solve( t(X) %*% X ) %*% t(X) %*% CO2$conc


rm.lm <- lm(conc~uptake+Type, data=CO2)
coefficients(rm.lm)
anova(rm.glm)




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

