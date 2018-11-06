#https://www.r-bloggers.com/how-to-do-repeated-measures-anovas-in-r/

#https://stats.stackexchange.com/questions/58745/using-lmer-for-repeated-measures-linear-mixed-effect-model
#Subjects were assigned to one of several treatment groups (Subject is nested within treatment,
#each subject is assigned to only one treatment, and measurements are taken on each subject on each day)
#Measurements of each subject were taken on multiple days (Treatment is crossed with day)

mydata <- data.frame(
  Subject  = c(13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 29, 30, 31, 32, 33, 
               34, 35, 36, 37, 38, 39, 40, 62, 63, 64, 65, 13, 14, 15, 16, 17, 18, 
               19, 20, 21, 22, 23, 24, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 
               40, 62, 63, 64, 65, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 
               29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 62, 63, 64, 65), 
  Day       = c(rep(c("Day1", "Day3", "Day6"), each=28)), 
  Treatment = c(rep(c("B", "A", "C", "B", "C", "A", "A", "B", "A", "C", "B", "C", 
                      "A", "A", "B", "A", "C", "B", "C", "A", "A"), each = 4)), 
  Obs       = c(6.472687, 7.017110, 6.200715, 6.613928, 6.829968, 7.387583, 7.367293, 
                8.018853, 7.527408, 6.746739, 7.296910, 6.983360, 6.816621, 6.571689, 
                5.911261, 6.954988, 7.624122, 7.669865, 7.676225, 7.263593, 7.704737, 
                7.328716, 7.295610, 5.964180, 6.880814, 6.926342, 6.926342, 7.562293, 
                6.677607, 7.023526, 6.441864, 7.020875, 7.478931, 7.495336, 7.427709, 
                7.633020, 7.382091, 7.359731, 7.285889, 7.496863, 6.632403, 6.171196, 
                6.306012, 7.253833, 7.594852, 6.915225, 7.220147, 7.298227, 7.573612, 
                7.366550, 7.560513, 7.289078, 7.287802, 7.155336, 7.394452, 7.465383, 
                6.976048, 7.222966, 6.584153, 7.013223, 7.569905, 7.459185, 7.504068, 
                7.801867, 7.598728, 7.475841, 7.511873, 7.518384, 6.618589, 5.854754, 
                6.125749, 6.962720, 7.540600, 7.379861, 7.344189, 7.362815, 7.805802, 
                7.764172, 7.789844, 7.616437, NA, NA, NA, NA))

#model: Y=Treatment + Day + Subject(Treatment) + Day*Subject(Treatment)
#treatment (fixed)
#Day (fixed)
#the Treatment*Day interaction
#Subject nested within Treatment (random)
#Day crossed with "Subject within Treatment" (random)

m1 <- lmer(Obs ~ Treatment * Day + (1 | Subject), mydata)
anova(m1)
summary(m1)
m2 <- lmer(Obs ~ Treatment * Day + (Treatment|Subject), mydata)
m3 <- lmer(Obs ~ Treatment * Day + (Treatment:Subject), mydata)
m4 <- lmer(Obs~Treatment*Day + (1+Treatment/Subject) + (1+Day*Treatment/Subject), mydata)
summary(m1)

data()
#
ucla <- data.frame(	
id=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6),
gender=c(1,1,1,1,2,2,2,2,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2),
time=c(0,2,3,6,0,2,3,6,0,2,3,6,0,2,3,6,0,2,3,6,0,2,3,6),
happy_score=c(20,18,15,20,22,24,18,22,14,10,24,10,38,34,32,34,25,29,25,29,30,28,26,14),
chem=c(1000,1100,1200,1300,1000,1000,1005,950,1000,1999,800,1700,1000,1100,1150,1100,1000,1000,1050,1010,1000,1100,1109,1500))

rm.lm <- lm(happy_score ~ time+chem+gender, data=ucla) 
plot(rm.lm)
## 4 plots on 1 page;
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(rm.lm)
plot(rm.lm, id.n = NULL)                 # no id's
plot(rm.lm, id.n = 5, labels.id = NULL)  # 5 id numbers


rm.lmm <- lmer(happy_score ~ time+chem+gender + (1 | id), data=ucla)
summary(rm.lmm)
anova(rm.lmm)

#https://stats.stackexchange.com/questions/98958/plots-to-illustrate-results-of-linear-mixed-effect-model
library(nlme)
fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1|Subject)

newdat <- expand.grid(gender=unique(ucla$gender), time=c(min(ucla$time), max(ucla$time)))

library(ggplot2)
p <- ggplot(ucla, aes(x=time, y=ucla$happy_score, colour=ucla$gender)) +
  geom_point(size=3) +
  geom_line(aes(y=predict(rm.lmm), group=id)) +
  geom_line(data=newdat, aes(y=predict(rm.lmm, level=0, newdata=newdat), size="Population")) +
  scale_size_manual(name="Predictions", values=c("id"=0.5, "Population"=3)) +
  theme_bw(base_size=22) 
print(p)

predict(rm.lmm)
ucla$happy_score

p <- ggplot(data=ucla, aes(x=time, y=happy_score, colour=chem)) + geom_point(size=3) +
  geom_line(aes(y=predict(rm.lmm), group=id)) +
  #geom_line(data=newdat, aes(y=predict(rm.lmm, group=id), size="Population")) +
  scale_size_manual(name="Predictions", values=c(0.5, "Population"=3)) +
  theme_bw(base_size=22) 
print(p)

plot(rm.lmm)

plot(happy_score ~ time, col=gender, data=ucla)
abline(coef[1],coef[2],col=1) # distance ~ age for males
abline(coef[1],coef[2]+coef[3],col=2) # distance ~ age for females


#proc mixed data=work.uclaL;
#title4 "1. LMM using proc mixed (UCLA slides 43)";
#model happy_score=time chem gender/s;
#random intercept/subject=id;*adding random intercept takes care of repeated measurements;
#run;
#                                        Dimensions
#
#                            Covariance Parameters             2
#                            Columns in X                      4
#                            Columns in Z per Subject          1
#                            Subjects                          6
#                            Max Obs per Subject               4

#                        Covariance Parameter Estimates
#
#                                                 Standard         Z
#            Cov Parm      Subject    Estimate       Error     Value      Pr > Z
#
#            Intercept     id          45.0932     33.8021      1.33      0.0911
#            Residual                  10.2536      3.6246      2.83      0.0023

            
#                                      Fit Statistics
#                           AIC (Smaller is Better)         142.5
#
#
#                                 Solution for Fixed Effects
#                                        Standard
#             Effect         Estimate       Error      DF    t Value    Pr > |t|
#             Intercept       37.9140      9.7701       4       3.88      0.0178
#             time           -0.08481      0.3237      16      -0.26      0.7966
#             chem           -0.01275    0.003144      16      -4.05      0.0009
#             Gender          0.08429      5.6547      16       0.01      0.9883
#
#                               Type 3 Tests of Fixed Effects
#                                     Num     Den
#                       Effect         DF      DF    F Value    Pr > F
#                       time            1      16       0.07    0.7966
#                       chem            1      16      16.44    0.0009
#                       Gender          1      16       0.00    0.9883
