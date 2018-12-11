########################################################################################################
#programmer: ruben
#Analysis of covariance
#reference:
#https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_glm_sect049.htm
##########################################################################################################

library(car)
library(reshape2)

#############################################################
#1. data:

#6 patients with depression are given a drug that increases levels
#of a 'happy chemical' in the brain.
#Outcome Y: depression score (measured at baseline, 2, 3, and 6 months).
#Predictors X: time, chem, gender.

#
#The covariate (the pretreatment score) is included in the model for increased precision 
#in determining the effect of drug treatments on the posttreatment count of bacilli. 
#we will perform a parallel-slopes analysis of covariance

Input = ("
id Gender time1 time2 time3 time4 chem1 chem2 chem3 chem4
1 1 20 18 15 20 1000 1100 1200 1300
2 2 22 24 18 22 1000 1000 1005 950
3 1 14 10 24 10 1000 1999 800 1700
4 1 38 34 32 34 1000 1100 1150 1100
5 2 25 29 25 29 1000 1000 1050 1010
6 2 30 28 26 14 1000 1100 1109 1500
	")

rm.wide <- read.table(textConnection(Input), header=TRUE)

rm.longA <- reshape(rm.wide, direction = "long", varying = list(names(rm.data)[3:6]), 
	v.names = "Y", idvar = c("id","Gender"), timevar = "time") 
	
rm.longA <- rm.longA[c("id", "Gender", "time", "Y")]

rm.longB <- reshape(rm.wide, direction = "long", varying = list(names(rm.data)[7:10]), v.names = "chem", 
        idvar = c("id","Gender"))
rm.longB <- rm.longB[c("id","Gender", "chem")]

rm.long <- as.data.frame(cbind(rm.longA, rm.longB, by=c("id", "Gender")))
View(rm.long)
nrow(rm.long)

rm.lm <- lm(Y~chem+time+Gender, data=rm.long)
summary(rm.lm)
anova(rm.lm)

#############################################################
#2. Analysis of Covariance (ANCOVA) models:

#This model assumes that the slopes relating posttreatment scores to pretreatment scores are parallel 
#for all drugs. You can check this assumption by including the class-by-covariate interaction, 
#Drug*PreTreatment, in the model and examining the ANOVA test for the significance of this effect. 
#This extra test is omitted in this example, but it is insignificant, justifying the equal-slopes assumption.

rm.time1 <- subset(rm.long, time==1)
rm.time2 <- subset(rm.long, time==2)
rm.time3 <- subset(rm.long, time==3)
rm.time4 <- subset(rm.long, time==4)


rm.lm1 <- lm(Y~chem, data=rm.time1); summary(rm.lm1)
rm.lm2 <- lm(Y~chem, data=rm.time2); summary(rm.lm2)
rm.lm3 <- lm(Y~chem, data=rm.time3); summary(rm.lm3)
rm.lm4 <- lm(Y~chem, data=rm.time4); summary(rm.lm4)

par(mfrow=c(2,2))
plot(Y~chem, data=rm.time1, col="dark red")
plot(Y~chem, data=rm.time2, col="orange")
plot(Y~chem, data=rm.time3, col="green")
plot(Y~chem, data=rm.time4, col="black")
par(mfrow=c(1,1))

plot(Y~chem, data=rm.long, col="black")

#model 2: interaction Drug*PreTreatment model
rm.lm2 <- lm(Y~chem+time+Gender, data=rm.long)
summary(rm.lm2)
anova(rm.lm2)
Anova(rm.lm2, type="II") 
hist(residuals(rm.lm2),col="darkgray") #Checking assumptions of the model

#############################################################
#3. testing normality, and homoscedasticity (equal variances):

#Simple plot
plot(x=rm.long$Y, y=rm.long$chem, col=rm.long$Gender, pch = 16, xlab="PreTreatment", ylab="PostTreatment")

#Residuals vs. fitted should be approximately normal.
#The residuals "bounce randomly" around the 0 line.
#The residuals roughly form a "horizontal band" around the 0 line (variances of the errors are equal)
#No residual "stands out" (there are no outliers)

plot(fitted(rm.lm2), residuals(rm.lm2))

#############################################################
#end of program
#############################################################
























#contrasts
options(contrasts = c("contr.treatment", "contr.poly"))
contrasts(rm.data$Drug)





#Analysis of covariance
options(contrasts = c("contr.treatment", "contr.poly"))

### These are the default contrasts in R
model.1 = lm (PostTreatment ~Drug+ PreTreatment, data=rm.data)
Anova(model.1, type="II") #https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/
summary(model.1)

model.2 = lm (Pulse ~ Temp + Species, data = Data)
Anova(model.2, type="II") 
summary(model.2)

contrasts(Data$Species)




#https://rcompanion.org/rcompanion/e_04.html

library(car)

#############################################################
#1. data:
  
Input2 = ("
Species   Temp   Pulse
 ex       20.8   67.9
 ex       20.8   65.1
 ex       24     77.3
 ex       24     78.7
 ex       24     79.4
 ex       24     80.4
 ex       26.2   85.8
 ex       26.2   86.6
 ex       26.2   87.5
 ex       26.2   89.1
 ex       28.4   98.6
 ex       29    100.8
 ex       30.4   99.3
 ex       30.4  101.7
 niv      17.2   44.3
 niv      18.3   47.2
 niv      18.3   47.6
 niv      18.3   49.6
 niv      18.9   50.3
 niv      18.9   51.8
 niv      20.4   60
 niv      21     58.5
 niv      21     58.9
 niv      22.1   60.7
 niv      23.5   69.8
 niv      24.2   70.9
 niv      25.9   76.2
 niv      26.5   76.1
 niv      26.5   77
 niv      26.5   77.7
 niv      28.6   84.7
")


Data = read.table(textConnection(Input2),header=TRUE)
str(Data)
#Simple plot
plot(x   = Data$Temp, y   = Data$Pulse, col = Data$Species, pch = 16, xlab = "Temperature", ylab = "Pulse")
legend('bottomright', legend = levels(Data$Species), col = 1:2, cex = 1, pch = 16)

#############################################################
#1. data:

#Analysis of covariance
options(contrasts = c("contr.treatment", "contr.poly"))
  
### These are the default contrasts in R
model.1 = lm (Pulse ~ Temp + Species + Temp:Species, data = Data)
Anova(model.1, type="II") #https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/
summary(model.1)

model.2 = lm (Pulse ~ Temp + Species, data = Data)
Anova(model.2, type="II") 
summary(model.2)

contrasts(Data$Species)

#Simple plot with fitted lines
I.nought = -7.21091
I1 = I.nought + 0
I2 = I.nought + -10.06529
B  = 3.60275

plot(x   = Data$Temp, y   = Data$Pulse, col = Data$Species, pch = 16, xlab = "Temperature", ylab = "Pulse")
legend('bottomright', legend = levels(Data$Species), col = 1:2, cex = 1, pch = 16)
abline(I1, B, lty=1, lwd=2, col = 1)
abline(I2, B, lty=1, lwd=2, col = 2)

#p-value and R-squared of combined model 
summary(model.2)

#Checking assumptions of the model
hist(residuals(model.2),col="darkgray")

#A histogram of residuals from a linear model.  The distribution of these residuals should be approximately normal.
plot(fitted(model.2), residuals(model.2))


