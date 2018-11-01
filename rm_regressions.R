
#https://stats.stackexchange.com/questions/26176/removal-of-statistically-significant-intercept-term-increases-r2-in-linear-mo

set.seed(.Random.seed[1])

n <- 220
a <- 0.5
b <- 0.5
se <- 0.25

# Make sure x has a strong mean offset
x <- rnorm(n)/3 + a
y <- a + b*x + se*rnorm(x)

int.lm   <- lm(y~x)
summary(int.lm)
anova(int.lm)

noint.lm <- lm(y~x+0)  # Intercept be gone!

# For comparison to summary(.) output
rsq.int <- cor(y,x)^2
rsq.noint <- 1-mean((y-noint.lm$fit)^2) / mean(y^2)
summary(int.lm)


# No intercept!
summary(noint.lm)
anova(noint.lm)


########################################################################################
#read data:
library(readxl)

#e1_1 <- read_excel("C:\\Users\\ruben\\Documents\\rm\\archive\\AAA_Sen\\e1_1.xls")

#read data worls records athletism men and women:
e3_4 <- read_excel("\\\\cifs-prd.fda.gov/ctpusers01/ruben.montesdeoca/Ruben.MontesdeOca/Documents/ruben/e3_4.xlsx")
e3_5 <- read_excel("\\\\cifs-prd.fda.gov/ctpusers01/ruben.montesdeoca/Ruben.MontesdeOca/Documents/ruben/e3_5.xlsx")

#regressions for e3_4 and e3_5:

##e3_4:
plot(Time~Dist., data = e3_4)

e3_4$Dist.log <- log(e3_4$Dist.)
e3_4$Time.log <- log(e3_4$Time)

rm.lm34 <- lm(Time.log~Dist.log, data=e3_4) 
summary(rm.lm34)
anova(rm.lm34)
coef(rm.lm34)
plot(Time.log~Dist.log, data = e3_4)

##e3_5:
plot(Time~Dist., data = e3_5)

e3_5$Dist.log <- log(e3_5$Dist.)
e3_5$Time.log <- log(e3_5$Time)

rm.lm35 <- lm(Time.log~Dist.log, data=e3_5) 
summary(rm.lm35)
anova(rm.lm35)
coef(rm.lm35)

rm.data <- rbind(e3_4, e3_5) 
View(rm.data)
rm.lm <- lm(Time.log~Dist.log, data=rm.data) 
summary(rm.lm)
anova(rm.lm)
coef(rm.lm)
plot(Time.log~Dist.log, data=rm.data4)






plot(SPEED ~ DENSITY, data = e1_1)
#transform data
fitness$Lweight<-log(fitness$weight)
plot(Lweight~age,fitness)
plot(weight~age,fitness)
hist(fitness$weight)
hist(fitness$Lweight)

#add normal curve:
Lweight <- fitness$Lweight
h<-hist(Lweight, breaks=10, col="red", xlab="xlab", main="main")
xfit<-seq(min(Lweight),max(Lweight),length=40)
yfit<-dnorm(xfit,mean=mean(Lweight),sd=sd(Lweight))
yfit <- yfit*diff(h$mids[1:2])*length(Lweight)
lines(xfit, yfit, col="blue", lwd=2)

weight <- fitness$weight
h<-hist(weight, breaks=10, col="red", xlab="xlab", main="main")
xfit<-seq(min(weight),max(weight),length=40)
yfit<-dnorm(xfit,mean=mean(weight),sd=sd(weight))
yfit <- yfit*diff(h$mids[1:2])*length(weight)
lines(xfit, yfit, col="blue", lwd=2)