# Imports packages

install.packages(dplyr)
install.packages(readxl)
install.packages(ggplot2)
install.packages(tidyverse)
install.packages(reshape2)
install.packages(modelr)
install.packages(broom)
install.packages(jtools)
install.packages(interplot)
install.packages(leaps)
install.packages(Hmisc)
install.packages(Metrics)

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(modelr)
library(broom)
library(jtools)
library(interplot)
library(leaps)
library(Hmisc)
library(Metrics)

# This imports the Wallingford file, which provides data from 1979-2011 on average monthly d18O in precipitation.
# Variables are Month, d18O, SD, and number of samples.
# There are about 31 samples for each month, corresponding to the number of years.

precip <- read_excel("C:/Users/randy/School/Fall '21/STAT5010W/Project/Climate Data/Precip d18O Wallingford.xlsx", col_names=F)

# Cleans the data

precip <- precip[3:14,1:6]
precip <- precip[,-c(3,4)]
names(precip) <- c("Month", "d18O", "SD", "NumberofSamples")
precip$d18O <- as.numeric(precip$d18O)
precip$SD <- as.numeric(precip$SD)
precip$NumberofSamples <- as.numeric(precip$NumberofSamples)

# Data is now ready for analysis

# Next I imported the Richmond Park Climate Data. The data in this data set is taken from Heathrow Weather Station.
# Variables are year, month, mean daily max & min temp Celsius for each month, days of air frost, 
# total rainfall in millimeters, and total sunshine duration in hours.

rpClimData <- read_excel("C:/Users/randy/School/Fall '21/STAT5010W/Project/Climate Data/Richmond Park Climate Data.xlsx", col_names=T)

# Cleans data

names(rpClimData) <- make.names(names(rpClimData), unique=TRUE)

# Adds mean temperature for each month

rpClimData <- rpClimData %>% 
  mutate(meanTemp = ((tmaxC + tminC)/2)) 

# Adds mean temperature for each month in Fahrenheit 

rpClimData <- rpClimData %>% 
  mutate(meanFahrTemp = (meanTemp*(9/5)) + 32) 

# Next I imported the RP Results data. This is data on each tooth sample for each deer. Odd numbers are second molars 
# and even numbers are third molars. There are 143 samples, meaning around 6 per tooth, as there are 24 sampled teeth.

results <- read_excel("C:/Users/randy/School/Fall '21/STAT5010W/Project/Deer Data/RP Results and Data Conversions.xlsx", col_names=T, sheet=3)

# Cleans data

results <-results[,1:6]

# We must find the years and months that these samples correspond to.
# Doing so will allow us to add temperature and rainfall columns to the results data frame.
# The corresponding dates may have to be approximated based on the distance from 
# the crown and knowing the birth of the deer. Pairing this with knowledge of the time it takes 
# for teeth to form will allow us to make estimates on what date the samples correspond with.
# Estimating these dates is crucial in our data analysis because we can then compare oxygen isotope values 
# with temperature and rainfall amounts. Our estimates will be more accurate for M2, the second molar, due to 
# its shorter formation period.

# Cleans data

names(results) <- c("Sample", "PDB", "SMOW", "DrinkWater", "PredictTemp", "distanceFromCrown")

# Summary statistics for oxygen isotope

summary(results$PDB) 

# Converted temperature using one equation versus three. I simplified the three equations using algebra

results <- results %>% 
  mutate(conTemp = 1.750806698*results$PDB+19.70262559)

# Converted temperature in Fahrenheit

results <- results %>% 
  mutate(conTempFahr =  (conTemp*(9/5)) + 32)

# Comparing converted and actual temperature mean, variance, summary

mean(results$conTempFahr)
mean(rpClimData$meanFahrTemp)

sd(results$conTempFahr)
sd(rpClimData$meanFahrTemp)

summary(results$conTempFahr)
summary(rpClimData$meanFahrTemp)

# Average rainfall oxygen values annually

meanYearRain <- read_excel("C:/Users/randy/School/Fall '21/STAT5010W/Project/Climate Data/Rainfall Isotope Values.xlsx", col_names=T, sheet = 2)

# Cleans data

names(meanYearRain) <- make.names(names(meanYearRain), unique=TRUE)
names(meanYearRain) <- c("Year", "TotalRain", "D18ORainAvg")

# Total rain vs isotope values for rainfall

rainVsD18 <- lm(meanYearRain$TotalRain~meanYearRain$D18ORainAvg)

summary(rainVsD18)
plot(meanYearRain$TotalRain~meanYearRain$D18ORainAvg)
abline(rainVsD18)

# We looked at the mean annual rainfall versus oxygen isotope in rainfall plot. 
# There is a weak negative correlation between the two variables. We were hoping to extrapolate 
# this data and use it for the time that the deer lived, but we cannot do that due to how weak the relationship is.

# Extracts sample number from sample tag for results data frame

sample54 <- substr(results$Sample[1:54],3,3) 
sample143 <- substr(results$Sample[55:143],3,4) 

newSample <- as.numeric(combine(sample54, sample143))

# Adds sample number column to data frame

results <- results %>% 
  mutate(sampleNum = newSample) 

# Average d18O for each tooth

deerOMean <- tapply(results$PDB, results$sampleNum, mean) 

# Max distance from crown for each tooth

deerCrownMax <- tapply(results$distanceFromCrown, results$sampleNum, max) 

# Adds column for which deer sample belongs to

deer <- c()

for (i in 1:143) {
  if(results$sampleNum[i] == 1 || results$sampleNum[i] == 2) {deer[i] = "M11"}
  if(results$sampleNum[i] == 3 || results$sampleNum[i] == 4) {deer[i] = "R20"}
  if(results$sampleNum[i] == 5 || results$sampleNum[i] == 6) {deer[i] = "P42"}
  if(results$sampleNum[i] == 7 || results$sampleNum[i] == 8) {deer[i] = "P25"}
  if(results$sampleNum[i] == 9 || results$sampleNum[i] == 10) {deer[i] = "R19"}
  if(results$sampleNum[i] == 11 || results$sampleNum[i] == 12) {deer[i] = "R1"}
  if(results$sampleNum[i] == 13 || results$sampleNum[i] == 14) {deer[i] = "B11"}
  if(results$sampleNum[i] == 15 || results$sampleNum[i] == 16) {deer[i] = "G42"}
  if(results$sampleNum[i] == 17 || results$sampleNum[i] == 18) {deer[i] = "F37"}
  if(results$sampleNum[i] == 19 || results$sampleNum[i] == 20) {deer[i] = "U17"}
  if(results$sampleNum[i] == 21 || results$sampleNum[i] == 22) {deer[i] = "W5"}
  if(results$sampleNum[i] == 23 || results$sampleNum[i] == 24) {deer[i] = "R27"}
}

results <- results %>% 
  mutate(deer = deer) 

# Adds column for deer gender

deerGen <- c()

for (i in 1:143) {
  if(results$sampleNum[i] == 1 || results$sampleNum[i] == 2) {deerGen[i] = "Male"}
  if(results$sampleNum[i] == 3 || results$sampleNum[i] == 4) {deerGen[i] = "Female"}
  if(results$sampleNum[i] == 5 || results$sampleNum[i] == 6) {deerGen[i] = "Male"}
  if(results$sampleNum[i] == 7 || results$sampleNum[i] == 8) {deerGen[i] = "Male"}
  if(results$sampleNum[i] == 9 || results$sampleNum[i] == 10) {deerGen[i] = "Female"}
  if(results$sampleNum[i] == 11 || results$sampleNum[i] == 12) {deerGen[i] = "Male"}
  if(results$sampleNum[i] == 13 || results$sampleNum[i] == 14) {deerGen[i] = "Male"}
  if(results$sampleNum[i] == 15 || results$sampleNum[i] == 16) {deerGen[i] = "Female"}
  if(results$sampleNum[i] == 17 || results$sampleNum[i] == 18) {deerGen[i] = "Male"}
  if(results$sampleNum[i] == 19 || results$sampleNum[i] == 20) {deerGen[i] = "Male"}
  if(results$sampleNum[i] == 21 || results$sampleNum[i] == 22) {deerGen[i] = "Female"}
  if(results$sampleNum[i] == 23 || results$sampleNum[i] == 24) {deerGen[i] = "Male"}
}

results <- results %>% 
  mutate(deerGen = deerGen) 

# Gives vector of number of samples for each tooth

sampleEachTooth <- tapply(results$sampleNum,results$sampleNum,length)

# Imports vectors of approximate month, year, etc. of each sample based on constant growth of molars

averyDeerData <- read_excel("C:/Users/randy/School/Fall '21/STAT5010W/Project/Deer Data/Corrected Randy Deer Data.xlsx", col_names=T, sheet=1)

# Adds these columns to results vector (these columns are approximations)

results <- results %>% 
  mutate(monthsAlive = randyDeerData$monthsDeerLived)

results <- results %>% 
  mutate(yearAlive = randyDeerData$yearsDeerLived)

results <- results %>% 
  mutate(totalWearMM = randyDeerData$totalMMOfWear)

results <- results %>% 
  mutate(totalToothHeight = randyDeerData$totalToothHeight)

results <- results %>% 
  mutate(toothGrowthMonthly = randyDeerData$toothGrowthPerMonth)

results <- results %>% 
  mutate(month = randyDeerData$specificMonth)

results <- results %>% 
  mutate(year = randyDeerData$yearSampCorrespond)

results <- results %>% 
  mutate(season = randyDeerData$Season)

# Remove PredictTemp column

results <- results[,-5]

# Round off columns

results$yearAlive <- round(results$yearAlive, digits = 2)

results$totalWearMM <- round(results$totalWearMM, digits = 2)

results$totalToothHeight <- round(results$totalToothHeight, digits = 2)

results$toothGrowthMonthly <- round(results$toothGrowthMonthly, digits = 2)

results$month <- round(results$month, digits = 1)

# Adds climate data to results data frame based on yyyy and mm for sample using rpClimData

# Adds temperature column Fahrenheit 

meanTempF <- c()
for (i in 1:143) {
  meanTempF[i] <- rpClimData$meanFahrTemp[(rpClimData$yyyy == results$year[i] & rpClimData$mm == results$month[i])]
}    

results <- results %>% 
  mutate(meanTempF = meanTempF)

# Adds temperature column Celsius

meanTempC <- c()
for (i in 1:143) {
  meanTempC[i] <- rpClimData$meanTemp[(rpClimData$yyyy == results$year[i] & rpClimData$mm == results$month[i])]
}    

results <- results %>% 
  mutate(meanTempC = meanTempC) 

# Adds total sunshine duration column

sunHours <- c()
for (i in 1:143) {
  sunHours[i] <- rpClimData$sun.hours[(rpClimData$yyyy == results$year[i] & rpClimData$mm == results$month[i])]
}    

results <- results %>% 
  mutate(sunHours = sunHours) 

# Adds days of air frost column

airFrostDays <- c()
for (i in 1:143) {
  airFrostDays[i] <- rpClimData$af.days[(rpClimData$yyyy == results$year[i] & rpClimData$mm == results$month[i])]
}    

results <- results %>% 
  mutate(airFrostDays = airFrostDays) 

# Adds mean daily max temp celsius column

meanMaxTempC <- c()
for (i in 1:143) {
  meanMaxTempC[i] <- rpClimData$tmaxC[(rpClimData$yyyy == results$year[i] & rpClimData$mm == results$month[i])]
}    

results <- results %>% 
  mutate(meanMaxTempC = meanMaxTempC) 

# Adds mean daily min temp celsius column

meanMinTempC <- c()
for (i in 1:143) {
  meanMinTempC[i] <- rpClimData$tminC[(rpClimData$yyyy == results$year[i] & rpClimData$mm == results$month[i])]
}    

results <- results %>% 
  mutate(meanMinTempC = meanMinTempC)

# Adds total rainfall column

totalRainMM <- c()
for (i in 1:143) {
  totalRainMM[i] <- rpClimData$rain.mm[(rpClimData$yyyy == results$year[i] & rpClimData$mm == results$month[i])]
}    

results <- results %>% 
  mutate(totalRainMM = totalRainMM) 

# Corrects row 122

results[122, 1] = "RP20I"
results[122, 8] = 20
results[122, 9] = "U17"
results[122, 10] = "Male"
results[122, 11] = 51

# Adds molar type column

molarType <- c()

for (i in 1:143) {
  if((results$sampleNum[i]%%2) == 0) {
    molarType[i] = "M3"
  } 
  else {
    molarType[i] = "M2"
  }
}

results <- results %>% 
  mutate(molarType = molarType)

# Remove scientific notation (set to 0 to reset this option)

options(scipen = 100)

# Model 1 (monthly averages)

# rm(sample2)
# rm(train2)
# rm(test2)
# set.seed(231)
# sample2 <- sample(c(TRUE, FALSE), nrow(results), replace = T, prob = c(0.6,0.4))
# train2 <- results[sample2, ]
# test2 <- results[!sample2, ]

model1 <- lm(results$PDB ~ results$meanTempF*results$totalRainMM)

# Correlation matrix between variables

rcorr(as.matrix(results[,c(2, 20, 25 )]), type = c("pearson","spearman"))

rsquare(model1)

summary(model1)

# RMSE

rmse(results$PDB, predict.lm(model1))

# Residual vs Fitted Plot

model1_results <- augment(model1, train)
ggplot(model1_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted")

# QQ Plot

qq_plot <- qqnorm(model1_results$.resid)
qq_plot <- qqline(model1_results$.resid)

# Cook's Distance & Residual vs Leverage Plot

par(mfrow=c(1, 2))
plot(model1, which = 4, id.n = 5)
plot(model1, which = 5, id.n = 5)

# Model 2 (annual averages)

annualTempMax <- c(tapply(rpClimData$tmaxC, rpClimData$yyyy, mean))
annualTempMin <- c(tapply(rpClimData$tminC, rpClimData$yyyy, mean))
annualTemp <- (annualTempMax + annualTempMin) / 2
annualRain <- c(tapply(rpClimData$rain.mm, rpClimData$yyyy, mean))
annualPDB <- c(tapply(results$PDB, results$year, mean))
annualDF <- cbind(annualTemp, annualRain)
annualDF <- annualDF[-c(14,15),]
annualDF <- as.data.frame(cbind(annualDF, annualPDB))

# Creating Test and Train Data

# rm(sample)
# rm(train)
# rm(test)
# set.seed(128)
# sample <- sample(c(TRUE, FALSE), nrow(annualDF), replace = T, prob = c(0.6,0.4))
# train <- annualDF[sample, ]
# test <- annualDF[!sample, ]

# Model 2

model2 <- lm(annualDF$annualPDB ~ annualDF$annualRain*annualDF$annualTemp)

summary(model2)

# RMSE

rmse(annualDF$annualPDB, predict.lm(model2))

# Anova model 2

tidy(model2)
list(model2 = broom::glance(model2))
plot(model2)
anova(model2)

# Correlation matrix between variables

rcorr(as.matrix(annualDF[,c(1, 2, 3 )]), type = c("pearson","spearman"))

# Residual vs Fitted Plot

ggplot(model2, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted")

# QQ Plot

qq_plot <- qqnorm(model2$residuals)
qq_plot <- qqline(model2$residuals)

# Cook's Distance & Residual vs Leverage Plot

par(mfrow=c(1, 2))
plot(model2, which = 4, id.n = 5)
plot(model2, which = 5, id.n = 5)
