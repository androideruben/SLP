#https://rcompanion.org/rcompanion/e_04.html

library(car)

Input = ("
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


Data = read.table(textConnection(Input),header=TRUE)
str(Data)
#Simple plot
plot(x   = Data$Temp, y   = Data$Pulse, col = Data$Species, pch = 16, xlab = "Temperature", ylab = "Pulse")
legend('bottomright', legend = levels(Data$Species), col = 1:2, cex = 1, pch = 16)

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


