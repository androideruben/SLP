##########################################################################################################################
#programmer: ruben
#Analysis of covariance and ways to redefine reference level in model
#reference:
#https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_glm_sect049.htm
##########################################################################################################################

library(car)
library(ggplot2)
library(reshape2)

##########################################################################################################################
#1. data:

# Drug two antibiotics (A and D) and a control (F)
#PreTreatment a pretreatment score of leprosy bacilli
#PostTreatment a posttreatment score of leprosy bacilli 
#
#The covariate (the pretreatment score) is included in the model for increased precision 
#in determining the effect of drug treatments on the posttreatment count of bacilli. 
#we will perform a parallel-slopes analysis of covariance

Input = ("
Drug    PreTreatment  PostTreatment 
A          11            6
A           8            0
A           5            2
A          14            8
A          19           11
A           6            4
A          10           13
A           6            1
A          11            8
A           3            0
D           6            0
D           6            2
D           7            3
D           8            1
D          18           18
D           8            4
D          19           14
D           8            9
D           5            1
D          15            9
F          16           13
F          13           10
F          11           18
F           9            5
F          21           23
F          16           12
F          12            5
F          12           16
F           7            1
F          12           20
")

#wide data
rm.data.wide <- read.table(textConnection(Input), header=TRUE)

#long data:
rm.data.long <- melt(rm.data.wide)

boxplot(rm.data.long$value, main="Pre-Post", ylab="PostTreatment")

#slopes relating posttreatment scores to pretreatment scores are parallel for all drugs
ggplot(rm.data.long, aes(x=variable, y=value, fill=Drug)) + geom_boxplot() + scale_fill_manual(values = c("red", "yellow", "green"))


##########################################################################################################################
#2. Analysis of Covariance (ANCOVA) models:

#This model assumes that the slopes relating posttreatment scores to pretreatment scores are parallel 
#for all drugs. You can check this assumption by including the class-by-covariate interaction, 
#Drug*PreTreatment, in the model and examining the ANOVA test for the significance of this effect. 
#This extra test is omitted in this example, but it is insignificant, justifying the equal-slopes assumption.

#model 1: No interactions model
rm.lm1 <- lm(PostTreatment ~Drug+ PreTreatment, data=rm.data.wide)
summary(rm.lm1)
anova(rm.lm1)
Anova(rm.lm1) 
hist(residuals(rm.lm1),col="darkgray") #Checking assumptions of the model

#model 1B: change reference level to drug=F (control):
rm.data.wideB <- within(rm.data.wide, Drug <- relevel(Drug, ref = "F"))
rm.lm1B <- lm(PostTreatment ~Drug+ PreTreatment, data=rm.data.wideB)
summary(rm.lm1B)
anova(rm.lm1B)
Anova(rm.lm1B)
hist(residuals(rm.lm1B),col="darkgray") #Checking assumptions of the model

#model 2: interaction Drug*PreTreatment model
#This model assumes that the slopes relating posttreatment scores to pretreatment scores are parallel for all drugs
rm.lm2 <- lm(PostTreatment ~Drug+ PreTreatment+Drug*PreTreatment, data=rm.data.wide)
summary(rm.lm2)  #see that DrugD:PreTreatment is not significant, that is, it is zero statistically, justifying the equal-slopes assumption
anova(rm.lm2)
Anova(rm.lm2, type="II") 
hist(residuals(rm.lm2),col="darkgray") #Checking assumptions of the model

##########################################################################################################################
#3. testing normality, and homoscedasticity (equal variances)
#remember Gauss-Markov: errors i=1, 2, ...n have expected value of zero, independent pairwise, and with same variance
#add normality for hypothesis testing

#Simple plot
plot(x=rm.data.wide$PreTreatment, y=rm.data.wide$PostTreatment, col=rm.data.wide$Drug, pch = 16, xlab="PreTreatment", ylab="PostTreatment")
  legend('bottomright', legend=levels(rm.data.wide$Drug), col=1:3, cex = 1, pch = 16)

#Residuals vs. fitted should be approximately normal.
#The residuals "bounce randomly" around the 0 line.
#The residuals roughly form a "horizontal band" around the 0 line (variances of the errors are equal)
#No residual "stands out" (there are no outliers)

plot(fitted(rm.lm2), residuals(rm.lm2))

##########################################################################################################################
#4. how to use the model 
###y_estimada= -3.8808+ 0.1090*DrugD+ 3.4461*DrugF+ 0.9872*PreTreatment:

#DrugA is obtained by making DrugD and DrugF zero, if PreTreatment=11 we have:
y_estimada= -3.8808+ 0.1090*0+ 3.4461*0+ 0.9872*11
y_estimada

#Now, what happens for DrugD and no DrugA, no DrugF para PreTreatment=11:
y_estimada= -3.8808+ 0.1090*1+ 3.4461*0+ 0.9872*11
y_estimada

#mismos resultados para modelo con refrencia a DrugF (control):
summary(rm.lm1B)

###y_estimada= -0.4347 -3.4461*DrugA -3.3372*DrugD+ 0.9872*PreTreatment,
#para drugA queda:
y_estimada= -0.4347 -3.4461*1 -3.3372*0+ 0.9872*11
y_estimada

##########################################################################################################################
#5. paired t-test can be used as well:
mean(rm.data.wide$PreTreatment)
mean(rm.data.wide$PostTreatment)

t.test(rm.data.wide$PreTreatment, rm.data.wide$PostTreatment, paired=TRUE) 

par(mfrow=c(2,2))
boxplot(rm.data.wide$PreTreatment, data=rm.data.wide, main="Pre-Post", ylab="PreTreatment")
boxplot(rm.data.wide$PostTreatment, data=rm.data.wide, main="Pre-Post", ylab="PostTreatment")

boxplot(PreTreatment~PostTreatment, data=rm.data.wide, main="Pre-Post", xlab="PostTreatment", ylab="PreTreatment")
par(mfrow=c(1,1))

##########################################################################################################################
#end of program
##########################################################################################################################







