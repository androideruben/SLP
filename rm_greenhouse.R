###################################################################################################################################
#rm_greenhouse.R 
#programmer: Ruben Montes de Oca, CTP
#Purpose: explore http://edgar.jrc.ec.europa.eu/overview.php?v=432_GHG&SECURE=123
#Started on February, 2018
##############################################

###############################################
setwd("C:\\Users\\ruben\\Documents\\rm\\SLP\\data\\v432_CO2_excl_short-cycle_org_C_1970_2012")
getwd()

#library(readxl)
#dat1<-read_excel("v432_CO2_excl_short-cycle_org_C_1970_2012.xlsx", range="v432_CO2_excl_short-cycle_org_C!A8:G3678", col_names=TRUE, col_types=c("text", "text", "text", "text", "text", "text", "numeric"))
#save(dat1, file=file.path("C:\\Users\\ruben\\Documents\\rm\\SLP\\data\\v432_CO2_excl_short-cycle_org_C_1970_2012", "dat1.rda"))

load(file.path("C:\\Users\\ruben\\Documents\\rm\\SLP\\data\\v432_CO2_excl_short-cycle_org_C_1970_2012\\dat1.RData"))

summary(dat1)
head(dat1)

head(dat1$"1970", n=5)
mean(dat1$"1970", na.rm=TRUE)
aggregate(dat1[, 7],  mean)

# 2-Way Frequency Table 
attach(dat1)
table("1970")


mytable <- table('1970') # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages



dat2 <- dat1[,c("World Region","DBP.Baseline","DBP.Month1","DBP.Month2",
                  "DBP.Month3","DBP.Month4")]


#End of rm_greenhouse.R#############################################




