########################################################################
#programmer: ruben
#date: 2018-11-29
#purpose: logistic regression
#https://stats.idre.ucla.edu/r/dae/logit-regression/
#######################################################################

library(aod)
library(ggplot2)
library(sandwich)
library(msm)
library(oddsratio)
library(psych)
library(Hmisc)

#1. Logistic regression
#https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_freq_sect029.htm
#https://support.sas.com/documentation/cdl/en/statug/63347/HTML/default/viewer.htm#statug_logistic_sect041.htm

#compare the probability of coronary heart disease for two types of diet
#Exposure <- c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #1='High Cholesterol Diet'
#Response <- c(0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1) #1='Yes, heart disease'

Exposure <- c('0','0','0','0','0','0','0','0','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1') #1='High Cholesterol Diet'
Response <- c('0','0','0','0','0','0','1','1','0','0','0','0','1','1','1','1','1','1','1','1','1','1','1') #1='Yes, heart disease'

FatComp <- data.frame(cbind(Exposure, Response))

headTail(FatComp)
Hmisc::describe(FatComp)
psych::describe(FatComp)
psych::describeBy(FatComp, FatComp$Exposure)
by(FatComp, FatComp$Exposure, summary)

	
attach(FatComp)
rm.table <- table(Exposure, Response)

margin.table(rm.table)
prop.table(rm.table,1)

chisq.test(Response, Exposure) 
#(#Continuity Adj. Chi-Square WARNING: 50% of the cells have expected counts less than 5.
##(Asymptotic) Chi-Square may not be a valid test.)
detach(FatComp)

rm.logistic <- glm(Response ~ Exposure, data=FatComp, family="binomial")
summary(rm.logistic)

#the odds of heart disease is 8.25 times higher in the high fat diet group
exp(coef(rm.logistic)) #odds ratio is beta of exposure





plot(admit ~ gre, data=data_glm)
hist(data_glm$gre, data=data_glm)


#2. Poisson regression
#https://ademos.people.uic.edu/Chapter19.html

#The cuse dataset is an old one that includes an N = 16 women, 
#age group in which they belong, their education level, whether 
#they want more children, and whether or not they're using contraceptives. 
#Location is listed above. I use the dataset from Princeton and modify/add 
#to the tutorial by G. Rodriguez. Here's a look at the data to get a sense of it.
cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header = TRUE)
unique(cuse$age)
levels(cuse$age)

rm.logistic <- glm(wantsMore~age+education, data=cuse, family=binomial()) 
summary(rm.logistic) 

#we are instead calculating the odds of getting a 0 vs. 1 outcome
#Let's walk through the output: The first thing you see is the deviance residuals, 
#which is a measure of model fit (higher is worse.) Next is our first batch of coefficients. 
#Going from left to right, the Coefficients table shows you the 
#"Estimate" change in the log odds of the outcome variable per one unit increase in the 
#predictor variable, standard error, the z-statistic AKA the Wald z-statistic, and the p-value.

confint(rm.logistic)
exp(coef(rm.logistic))

odds.ratio(rm.logistic)


Here's an example of how you'd interpret the coefficients above: For each unit change in [insert predictor variable], the log odds of [achieving the outcome of interest] increases by [coefficient].

You've never seen someone report anything in terms of log odds and you're not sure how this is a meaningful description of the outcome variable, you say? That's why we exponentiate: to put this in terms of odds ratios (see below for interpretation of ORs.)

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")



# NOT RUN {
data(hdv2003)
reg <- glm(cinema ~ sexe + age, data=hdv2003, family=binomial)
odds.ratio(reg)
odds.ratio(hdv2003$sport, hdv2003$cuisine)
odds.ratio(table(hdv2003$sport, hdv2003$cuisine))
M <- matrix(c(759, 360, 518, 363), ncol = 2)
odds.ratio(M)
odds.ratio(0.26, 0.42)
# }







#regression (generalized linear models) model Poisson
##create data
x <- seq.int(from=1, to =1000)




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

rm.data <- as.data.frame(cbind(x,y))
str(rm.data)

rm.glm <- glm(y~x, family="poisson", data=rm.data)
summary(rm.glm)

#regression (generalized linear models) model Normal
##create data
x <- seq.int(from=1, to =1000)
y <- rnorm(n=1000, mean=0, sd=1) # set parameters

rm.data <- as.data.frame(cbind(x,y))
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

data()
#####################################################################################
#NOAA

#long data (concatenate)
y <- rpois(n=1000, lambda = 1) # Poisson distribution
rm.data <- as.data.frame(cbind(x,y))

y <- rnorm(n=1000, mean=0, sd=1) # set parameters
rm.data2 <- as.data.frame(cbind(x,y))

rm.long <- rbind(rm.data, rm.data2)
info(rm.long)
View(rm.long)

#wide data (merge) option 1:
rm.wide <- merge(rm.data, rm.data2,by=c("x"))
info(rm.wide)
View(rm.wide)

#wide data (merge) option 2, after renaming y:
colnames(rm.data2)[2] <- "y2"
View(rm.data2)
rm.wide <- merge(rm.data, rm.data2,by=c("x"))
View(rm.wide)
Ardnalab2018

###################################################################################

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

