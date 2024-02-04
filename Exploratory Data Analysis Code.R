## STAT 5010 EDA ## RANDY NGUYEN ## 
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)

## CLIMATE DATA ##
## Precipitation d18O Wallingford ##
rain.data.W <- read_excel("Precip d18O Wallingford.xlsx",skip = 1,n_max = 12)
rain.data.W
ls(rain.data.W)
View(rain.data.W)
attach(rain.data.W)

is.factor(Month)
levels(Month)

#Scatterplot of Preciptation in Wallingford      
p1 <- ggplot(data = rain.data.W, mapping = aes(x = Month, y = d18O,group =1)) +
  geom_point() +
  labs(x="Months",y = "d18O Value (Per MIL)")
p1 +
  scale_x_discrete(limits = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  ggtitle("Monthly Isotopic Value in Precipitation Wallingford, UK (1980-2011)") + 
  geom_path(color = 4)+  theme(plot.title=element_text(hjust=0.5))


## Richmond Park Climate ##
Richmond_Park_Climate_Data <- read_excel("C:/Users/randy/OneDrive/Desktop/STAT Capstone Project/Climate Data/Richmond Park Climate Data.xlsx")
View(Richmond_Park_Climate_Data)
attach(Richmond_Park_Climate_Data)
ls(Richmond_Park_Climate_Data)


## Side-by-side boxplots for yearly rainfall in Richmond Park
#p2 <- ggplot(Richmond_Park_Climate_Data) +
#   geom_boxplot(aes(y = `rain mm`,x = factor(yyyy,labels=c("1965", "1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979")),
                  # fill = factor(yyyy))) +
#   theme(legend.position ="none")  + 
#  labs(x="years", "rain mm")+
#  ggtitle("Yearly Amount of Rainfall from Richmond Park Richmond,UK (1965-1979)")
#p2  

#GET MONTHLY rain mm
JAN <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 1,]
JANmm <- sum(JAN$`rain mm`)/15

FEB <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 2,]
FEBmm <- sum(FEB$`rain mm`)/15

MAR <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 3,]
MARmm <- sum(MAR$`rain mm`)/15

APR <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 4,]
APRmm <- sum(APR$`rain mm`)/15

MAY <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 5,]
MAYmm <- sum(MAY$`rain mm`)/15

JUN <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 6,]
JUNmm <- sum(JUN$`rain mm`)/15

JUL <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 7,]
JULmm <- sum(JUL$`rain mm`)/15

AUG <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 8,]
AUGmm <- sum(AUG$`rain mm`)/15

SEP <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 9,]
SEPmm <- sum(SEP$`rain mm`)/15

OCT <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 10,]
OCTmm <- sum(OCT$`rain mm`)/15

NOV<- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 11,]
NOVmm <- sum(NOV$`rain mm`)/15

DEC <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 12,]
DECmm <- sum(DEC$`rain mm`)/15

mm <- c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mm
monthlyRain <- c(JANmm,FEBmm,MARmm,APRmm,MAYmm,JUNmm,JULmm,AUGmm,SEPmm,OCTmm,NOVmm,DECmm)
monthlyRain

rain.df <-data.frame(mm,monthlyRain)
attach(rain.df)


#Scatterplot for monthly rainfall in Richmond Park 
p3 <- ggplot(data = rain.df, mapping = aes(x = mm, y = monthlyRain,group=1)) +
  geom_point() +
  labs(x="Months",y = "rainfall (mm)")
p3 + scale_x_discrete(limits = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  ggtitle("Monthly Rainfall in Richmond Park Richmond,UK (1965-1979)") + 
  geom_path(color = 4)

#Make New column/variable meanTemp
#tmaxC + tminC/2
Richmond_Park_Climate_Data <- Richmond_Park_Climate_Data %>%
  add_column(meanTemp = tmaxC + tminC/2)
View(Richmond_Park_Climate_Data)


#Get Mean temperature in January
Jan <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 1,]
JanT <- sum(Jan$meanTemp)/15

#Get Mean Temperature in Febuary
Feb <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 2,]
FebT <- sum(Feb$meanTemp)/15

Mar <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 3,]
MarT <- sum(Mar$meanTemp)/15

Apr <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 4,]
AprT <- sum(Apr$meanTemp)/15

May <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 5,]
MayT <- sum(May$meanTemp)/15

Jun <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 6,]
JunT <- sum(Jun$meanTemp)/15

Jul <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 7,]
JulT <- sum(Jul$meanTemp)/15

Aug <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 8,]
AugT <- sum(Aug$meanTemp)/15

Sep <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 9,]
SepT <- sum(Sep$meanTemp)/15

Oct <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 10,]
OctT <- sum(Oct$meanTemp)/15

Nov <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 11,]
NovT <- sum(Nov$meanTemp)/15

Dec <- Richmond_Park_Climate_Data[Richmond_Park_Climate_Data$mm == 12,]
DecT <- sum(Dec$meanTemp)/15
JanT

Month <- c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Month
monthlyTemp <- c(JanT,FebT,MarT,AprT,MayT,JunT,JulT,AugT,SepT,OctT,NovT,DecT)
monthlyTemp
df<-data.frame(Month,monthlyTemp)
attach(df)

#Scatterplot of Monthly temperatures in Richmond Park
p3 <- ggplot(data = df, mapping = aes(x = Month, y = monthlyTemp,group=1)) +
  geom_point() +
  labs(x="Months",y = "Temperature in °C")
p3 + scale_x_discrete(limits = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  ggtitle("Monthly Average Temperatures in Richmond Park Richmond,UK (1965-1979)") + 
  geom_path(color = 4) +   theme(plot.title=element_text(hjust=0.5))

####################################################################################################################################################
# Exploratory Data Analysis Correlation Plot
library("ggpubr")
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
climate.data <- read_excel("C:/Users/randy/OneDrive/Desktop/STAT Capstone Project/Randy's EDA/Climate Data.xlsx")
climate.data

df <- data.frame(climate.data)
df
attach(df)
ls(df)
ggscatter(climate.data,x = "temp"  ,y = "d18O",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature in °C", ylab = "d18O Isotope Values (per MIL)", main="Monthly Oxygen Isotope Values vs Monthly Temperature")+
  theme(plot.title=element_text(hjust=0.5))

cor(x=d18O,y=temp)
cor.test(x=d18O, y=temp)
var(x=d18O,y=temp)


######################################################################################################################################################
# EDA Derived Deer Data
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)

deer.data <- read_excel("C:/Users/randy/OneDrive/Desktop/STAT Capstone Project/Randy's EDA/Derived Deer Isotopes.xlsx")
deer.data
ls(deer.data)
df <- data.frame(deer.data)
df
attach(deer.data)

#Side-by-side box plots
ggplot(df, aes(x=deer,y=isotope,fill=deer)) + 
  geom_boxplot() +
  ggtitle("Oxygen Isotope Variation (1965-1975)") +
  xlab("Individual Deer") + ylab(" d18O per mil (PDB)") + scale_fill_discrete(name = "Deer") + coord_flip()


#Violin Plot
ggplot(df,aes(x=deer,y=isotope,fill=deer)) +
  geom_violin() +
  geom_boxplot(width=0.1,fill="white") +
  ggtitle("Oxygen Isotope Variation (1965-1975)") +
  xlab("Individual Deer") + ylab("d18O per mil (PDB)") + 
  scale_fill_discrete(name = "Deer") + coord_flip() + 
  theme(plot.title=element_text(hjust=0.5))

