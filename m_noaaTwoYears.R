###########################################################################
#program: rm_noaa.R
#purpose: practice noaa data 

#programmer: ruben m
#starting date: december 2018
###########################################################################

library(Hmisc)
library(psych)
library(xlsx)
library(summarytools)
library(descr)

#1. read data set ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/legacy/
##read document: http://www.nws.noaa.gov/directives/sym/pd01016005curr.pdf

StormData2013_temp <- read.csv("C:\\Users\\ruben\\Documents\\rm\\slp\\class\\data\\stormdata_2013.csv", header=T, sep=",")
StormData2012_temp <- read.csv("C:\\Users\\ruben\\Documents\\rm\\slp\\class\\data\\stormdata_2012.csv", header=T, sep=",")

#2a make a usable subset by year 2012
##a. keep useful vars and obs (assume repeated events of rain are measures in different counties:
StormData2012 <- subset(StormData2012_temp[c("MONTH_NAME", "STATE", "FLOOD_CAUSE")], 
                    STATE %in% c("CALIFORNIA", "NEW YORK", "TEXAS") 
                    & !(FLOOD_CAUSE %in% c("Dam / Levee Break", "Planned Dam Release", "") ))

summary(StormData2012) #disconcerting, structure keeps all the states but prints vales for selected ones

##a. STATE
StormData2012$stateN <- ifelse(StormData2012$STATE=="CALIFORNIA",   1, 
                           ifelse(StormData2012$STATE=="NEW YORK",  2,
                           ifelse(StormData2012$STATE=="TEXAS",     3, NA)))

#labels would make stateN a char, avoid that
StormData2012$stateNL <- factor(StormData2012$stateN, levels=c(1,2,3), 
                            labels=c("California", "New York", "Texas"))	

##b. MONTH_NAME
unique(StormData2012$MONTH_NAME) #only ten months

StormData2012$monthN <- ifelse(StormData2012$MONTH_NAME=="January",      1, 
                           ifelse(StormData2012$MONTH_NAME=="February",  2,
                           ifelse(StormData2012$MONTH_NAME=="March",     3,
                           ifelse(StormData2012$MONTH_NAME=="April",     4,
                           ifelse(StormData2012$MONTH_NAME=="May",       5,
                           ifelse(StormData2012$MONTH_NAME=="June",      6,
                           ifelse(StormData2012$MONTH_NAME=="July",      7,
                           ifelse(StormData2012$MONTH_NAME=="August",    8,
                           ifelse(StormData2012$MONTH_NAME=="September", 9,
                           ifelse(StormData2012$MONTH_NAME=="October",   10,
                           ifelse(StormData2012$MONTH_NAME=="November",  11,
                           ifelse(StormData2012$MONTH_NAME=="December",  12, 0))))))))))))

StormData2012$monthNL <- factor(StormData2012$monthN, levels=c(1,2,3,4,5,6,7,8,9,10), 
                            labels=c("January", "February", "March", "April", "May", 
                                     "June", "July", "August", "September", "October"))	
##c. FLOOD_CAUSE
StormData2012$floodN1 <- ifelse(StormData2012$FLOOD_CAUSE=="Heavy Rain",                      1, 
                            ifelse(StormData2012$FLOOD_CAUSE=="Ice Jam",                      2,
                            ifelse(StormData2012$FLOOD_CAUSE=="Heavy Rain / Snow Melt",       3,
                            ifelse(StormData2012$FLOOD_CAUSE=="Dam / Levee Break",            4,
                            ifelse(StormData2012$FLOOD_CAUSE=="Heavy Rain / Burn Area",       5,
                            ifelse(StormData2012$FLOOD_CAUSE=="Planned Dam Release",          6,
                            ifelse(StormData2012$FLOOD_CAUSE=="Heavy Rain / Tropical System", 7, NA)))))))

StormData2012$floodN1L <- factor(StormData2012$floodN, levels=c(1,2,3,4,5,6,7), 
                                 labels=c("Heavy Rain", 
                                          "Ice Jam",  
                                          "Heavy Rain / Snow Melt",     
                                          "Dam / Levee Break",          
                                          "Heavy Rain / Burn Area",     
                                          "Planned Dam Release",        
                                          "Heavy Rain / Tropical System"))	
#c (cont'd). better option: group floodN:
StormData2012$floodN2 <- ifelse(StormData2012$FLOOD_CAUSE %in% c("Heavy Rain",  
                                                         "Heavy Rain / Snow Melt", 
                                                         "Heavy Rain / Burn Area",
                                                         "Heavy Rain / Tropical System"),           1, 
                            
                            ifelse(StormData2012$FLOOD_CAUSE=="Ice Jam",                               2,
                                   ifelse(StormData2012$FLOOD_CAUSE %in% c("Dam / Levee Break",  
                                                                       "Heavy Rain / Tropical System"),   NA, 
                                          NA)))

StormData2012$floodN2L <- factor(StormData2012$floodN2, levels=c(1,2), labels=c("Rain related", "Ice"))	

#2b make a usable subset by year 2013
##a. keep useful vars and obs (assume repeated events of rain are measures in different counties:
StormData2013 <- subset(StormData2013_temp[c("MONTH_NAME", "STATE", "FLOOD_CAUSE")], 
                        STATE %in% c("CALIFORNIA", "NEW YORK", "TEXAS") 
                        & !(FLOOD_CAUSE %in% c("Dam / Levee Break", "Planned Dam Release", "") ))

summary(StormData2013) #disconcerting, structure keeps all the states but prints vales for selected ones

##a. STATE
StormData2013$stateN <- ifelse(StormData2013$STATE=="CALIFORNIA",   1, 
                               ifelse(StormData2013$STATE=="NEW YORK",  2,
                               ifelse(StormData2013$STATE=="TEXAS",     3, NA)))

#labels would make stateN a char, avoid that
StormData2013$stateNL <- factor(StormData2013$stateN, levels=c(1,2,3), 
                                labels=c("California", "New York", "Texas"))	

##b. MONTH_NAME
unique(StormData2013$MONTH_NAME) #only ten months

StormData2013$monthN <- ifelse(StormData2013$MONTH_NAME=="January",      1, 
                               ifelse(StormData2013$MONTH_NAME=="February",  2,
                               ifelse(StormData2013$MONTH_NAME=="March",     3,
                               ifelse(StormData2013$MONTH_NAME=="April",     4,
                               ifelse(StormData2013$MONTH_NAME=="May",       5,
                               ifelse(StormData2013$MONTH_NAME=="June",      6,
                               ifelse(StormData2013$MONTH_NAME=="July",      7,
                               ifelse(StormData2013$MONTH_NAME=="August",    8,
                               ifelse(StormData2013$MONTH_NAME=="September", 9,
                               ifelse(StormData2013$MONTH_NAME=="October",   10,
                               ifelse(StormData2013$MONTH_NAME=="November",  11,
                                                                                                     ifelse(StormData2013$MONTH_NAME=="December",  12, 0))))))))))))

StormData2013$monthNL <- factor(StormData2013$monthN, levels=c(1,2,3,4,5,6,7,8,9,10), 
                                labels=c("January", "February", "March", "April", "May", 
                                         "June", "July", "August", "September", "October"))	
##c. FLOOD_CAUSE
StormData2013$floodN1 <- ifelse(StormData2013$FLOOD_CAUSE=="Heavy Rain",                      1, 
                                ifelse(StormData2013$FLOOD_CAUSE=="Ice Jam",                      2,
                                       ifelse(StormData2013$FLOOD_CAUSE=="Heavy Rain / Snow Melt",       3,
                                              ifelse(StormData2013$FLOOD_CAUSE=="Dam / Levee Break",            4,
                                                     ifelse(StormData2013$FLOOD_CAUSE=="Heavy Rain / Burn Area",       5,
                                                            ifelse(StormData2013$FLOOD_CAUSE=="Planned Dam Release",          6,
                                                                   ifelse(StormData2013$FLOOD_CAUSE=="Heavy Rain / Tropical System", 7, NA)))))))

StormData2013$floodN1L <- factor(StormData2013$floodN, levels=c(1,2,3,4,5,6,7), 
                                 labels=c("Heavy Rain", 
                                          "Ice Jam",  
                                          "Heavy Rain / Snow Melt",     
                                          "Dam / Levee Break",          
                                          "Heavy Rain / Burn Area",     
                                          "Planned Dam Release",        
                                          "Heavy Rain / Tropical System"))	
#c (cont'd). better option: group floodN:
StormData2013$floodN2 <- ifelse(StormData2013$FLOOD_CAUSE %in% c("Heavy Rain",  
                                                                 "Heavy Rain / Snow Melt", 
                                                                 "Heavy Rain / Burn Area",
                                                                 "Heavy Rain / Tropical System"),           1, 
                                
                                ifelse(StormData2013$FLOOD_CAUSE=="Ice Jam",                               2,
                                       ifelse(StormData2013$FLOOD_CAUSE %in% c("Dam / Levee Break",  
                                                                               "Heavy Rain / Tropical System"),   NA, 
                                              NA)))

StormData2013$floodN2L <- factor(StormData2013$floodN2, levels=c(1,2), labels=c("Rain related", "Ice"))	

#3. Long format
##add year:
StormData2012$year <- 2012
StormData2013$year <- 2013

StormData <- as.data.frame(rbind(StormData2012,StormData2013))
View(StormData)

#4. Descriptives:
psych::describeBy(StormData2012, StormData2012$MONTH_NAME)
Hmisc::describe(StormData2012)
dfSummary(StormData2012, style="grid", plain.ascii=T)

CrossTable(StormData2012$monthN, StormData2012$stateN, expected=F, prop.chisq=F)
hist(StormData2012$monthN,breaks=40,col="blue", xlab="floodN", xlim = c(-2, 12), main="floodN2", freq=F)
boxplot(StormData2012$monthN~StormData2012$stateNL, xlab="xlab", ylab="ylab") 

##########################################################################################
# End of rm_noaa.R
##########################################################################################
