## Revati Rajane
##rvr190000
library(data.table)
library(sandwich)
library(lmtest)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(plm)
library(margins)
library(broom)



library(tidyverse)
library(AER)
library(dplyr)
library(foreign)
library(thePackage)
library(plm)
car <- read.dta("F:/UTD/Spring 20/Econometrics/project/car_fatalities.dta")
summary(car)



# Data understanding
# Histograms of all the variables 
library(ggplot2)
car %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_histogram()                         # as density
#missing Value
sum(is.na(car$jaild))
sum(is.na(car$comserd)) 

ggplot(car, aes(x=allmort)) + geom_histogram(binwidth = 50)
ggplot(car, aes(x=allmort)) + geom_histogram(bins = 50)

# Data Manuplation
car$VFRate <- with(car, allmort/pop * 10000)
car$NVFRate <- with(car,mralln/pop * 10000)    
car$AVFRate < - with(car,mraidall/pop *10000)  

ggplot(gapminder_1952, aes(x=pop_by_mil)) + geom_histogram(binwidth = 50)
# # Number of accidents per year in USA
 car_year<- aggregate(x = car$allmort,                # Specify data column
           by = list(car$year),              # Specify group indicator
           FUN = sum)
 
 ggplot(aes(x=Group.1, y=x), data = car_year)  + geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen", group=1) + 
   geom_point(size = 0.5) + 
   ggtitle('Total Number of Accidents and Fatalities in the US 1982 - 1988') +
   ylab('count') +
   xlab('Year') 
 
 
 # # Number of accidents per year in USA
 car_year<- aggregate(x = car$mraidall,                # Specify data column
                      by = list(car$year),              # Specify group indicator
                      FUN = sum)
 
 ggplot(aes(x=Group.1, y=x), data = car_year)  + geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen", group=1) + 
   geom_point(size = 0.5) + 
   ggtitle('Total Number of Accidents and Fatalities( Alcohol involved) in the US 1982 - 1988') +
   ylab('count') +
   xlab('Year') 

 
 #Exploratory Data Analysis
 
 count(car)
 unique(car$state)
 unique(car$year)

 # Corelation between independent variables
 
 Data.num = car[c("VFRate" , "beertax", "spircons", "vmiles" ,"mlda" ,"comserd", "jaild", "perinc", "unrate", "pop", "yngdrv")]
 
 
 library(PerformanceAnalytics)
 
 chart.Correlation(Data.num,
                   method="pearson",
                   histogram=TRUE,
                   pch=16)
 library(psych)
 corr.test(Data.num,
           use    = "pairwise",
           method = "pearson",
           adjust = "none")
 
 
 ggcorr(Data.num, 
        label = TRUE, 
        label_alpha = TRUE)

 library(ppcor)
 pcor(Data.num, method = "pearson") 
 
 
 #linear model
 
 model_lm <- lm(VFRate~beertax ,data=car)
 summary(model_lm)
 tidy(model_lm)
 
plot1 <- car %>%
    ggplot(aes(x =beertax , y = VFRate, group = 1)) +
    geom_point(size = 0.5)
plot1<- plot1 + geom_line(aes(y=predict(lm(VFRate~beertax , data=car))), color="red", size=2) 
plot1



#Model Selection



########################### 1. Alcohol involved ############################################
#############################################################################################
model1 <- plm(mraidall  ~ beertax+spircons+unrate+mlda+dry+yngdrv+vmiles+jaild+comserd+ 
          I(comserd * jaild)+pop1517+gspch,model="pooling", index = c("state","year"), data=car)
summary(model1)
coeftest(model1,method=vcovHC) 

model2 <- plm(mraidall  ~ beertax+spircons+unrate+mlda+dry+yngdrv+vmiles+jaild+comserd+ 
          I(comserd * jaild)+pop1517+gspch,model="within", index = c("state","year"), data=car)
summary(model2)
coeftest(model2,method=vcovHC) 

pFtest(model2,model1)
#If the p-value is < 0.05 then the fixed effects model is a better choice

model3 <- plm(mraidall  ~ beertax+spircons+unrate+yngdrv+jaild+comserd
              ,model="within", index = c("state","year"), data=car)
summary(model3)
coeftest(model3,method=vcovHC) 


model4 <- plm(mraidall  ~ as.factor(year)+beertax+spircons+unrate+yngdrv+jaild+
                comserd,model="within", index = c("state","year"), data=car)
summary(model4)
coeftest(model4,method=vcovHC) 
coeftest(model4, vcov=vcovHC(model4, type="sss", cluster="time")) 

pFtest(model4,model3)
#If this number is < 0.05 then use time-fixed effects. In this example, no need to use time-fixed effects.

########################### 2. Night Vehicle fatality Rate ############################################
###############################################################################################


model1 <- plm(mralln  ~ beertax+spircons+unrate+mlda+dry+yngdrv+vmiles+jaild+comserd+ 
                I(comserd * jaild)+pop1517+gspch,model="pooling", index = c("state","year"), data=car)
summary(model1)
coeftest(model1,method=vcovHC) 

model2 <- plm(mralln  ~ beertax+spircons+unrate+mlda+dry+yngdrv+vmiles+jaild+comserd+ 
                I(comserd * jaild)+pop1517+gspch,model="within", index = c("state","year"), data=car)
summary(model2)
coeftest(model2,method=vcovHC) 

pFtest(model2,model1)
#If the p-value is < 0.05 then the fixed effects model is a better choice

model3 <- plm(mralln  ~ spircons+gspch
              ,model="within", index = c("state","year"), data=car)
summary(model3)
coeftest(model3,method=vcovHC) 


model4 <- plm(mralln  ~ spircons+gspch+as.factor(year)
                ,model="within", data=car)
summary(model4)
coeftest(model4,method=vcovHC) 
coeftest(model4, vcov=vcovHC(model4, type="sss", cluster="time")) 

pFtest(model4,model3)
#If this number is < 0.05 then use time-fixed effects. In this example, no need to use time-fixed effects.



##################################### 3.  All Vehicle fatality Rate ############################################
############################################################################################


model1 <- plm(VFRate  ~ beertax+spircons+unrate+mlda+dry+yngdrv+vmiles+jaild+comserd+ 
                I(comserd * jaild)+pop1517+gspch,model="pooling", index = c("state","year"), data=car)
summary(model1)
coeftest(model1,method=vcovHC) 

model2 <- plm(VFRate  ~ beertax+spircons+unrate+mlda+dry+yngdrv+vmiles+jaild+comserd+ 
                I(comserd * jaild)+pop1517+gspch,model="within", index = c("state","year"), data=car)
summary(model2)
coeftest(model2,method=vcovHC) 

pFtest(model2,model1)
#If the p-value is < 0.05 then the fixed effects model is a better choice

model3 <- plm(VFRate  ~ beertax+spircons+unrate++mlda+dry+gspch
              ,model="within", index = c("state","year"), data=car)
summary(model3)
coeftest(model3,method=vcovHC) 


model4 <- plm(VFRate  ~ as.factor(year)+beertax+spircons+unrate+mlda+dry+gspch
                ,model="within", index = c("state","year"), data=car)
summary(model4)
coeftest(model4,method=vcovHC) 
coeftest(model4, vcov=vcovHC(model4, type="sss", cluster="time")) 


pFtest(model4,model3)
#If this number is < 0.05 then use time-fixed effects. In this example, no need to use time-fixed effects.


