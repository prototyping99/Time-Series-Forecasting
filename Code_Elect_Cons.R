
suppressPackageStartupMessages(c(
  library(fpp2),
  library(data.table),
  library(lubridate),
  library(xts),
  library(zoo),
  library(magrittr),
  library(plyr),
  library(dplyr),
  library(forecast),
  library(forecastHybrid),
  library(fma)))

######################################################

# Read data 
mydata <- read.csv("datasetv2.csv", header = TRUE, sep=";") # directory path 
mydata$date <- as.POSIXct(strptime(mydata$date, format = "%Y/%m/%d", tz = "GMT"))

head(mydata, 3)
#####################################

plot(mydata$date, mydata$EC, type = "l", xlab = "Date", ylab = "Electricity Consumption (GW)", main = "Monthly Electricity consuption in Gigawatts(GW) from 2002/01 to 2021/06")

#####################################
# interpol data sf data
library(signal) # load the signal package so the interp1 function is available
names(mydata)
mydata$EC <- interp1(x=as.numeric(rownames(mydata[which(!is.na(mydata$EC)),])),
                     y=mydata$EC[which(!is.na(mydata$EC))],
                     xi=as.numeric(rownames(mydata)),
                     method="spline") # interpolate with a spl
# check interpol
#####################################################
# evapotranspiration
names(mydata)
EC <- ts(mydata$EC, start=2002, frequency=12)
EC
# install.packages("seasonal"): remember to install this Mbulelo but only ones
# install.packages("trend")
library(seasonal)
require(3)
# Signal decompose
EC %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition: ET") +
  theme_bw()
####################################################################
#Modelling Start Here
#################forecasting#################################
# forecasting using ARIMA 
fitAR<-auto.arima(EC, lambda=0, biasadj=TRUE)
checkresiduals(fitAR)+
  theme_bw()

fitARIMA<- forecast(fitAR, h = 12*3) 
fitARIMA
ARIMA_Acc <- round(t(accuracy(fitARIMA)), 2)
ARIMA_Acc # Arima model accuracy
autoplot(fitARIMA)+
  autolayer(fitted(fitARIMA), series = "Fitted")+
  xlab("Year") + ylab("EC (----)") +
  ggtitle("EC: ARIMA") +
  theme_bw()
# Correlation coeficient calculation

#install.packages('ggpubr'): install this library
library("ggpubr")
CC<- round(cor(EC, fitAR$fitted, method = c("pearson")),2)
CC
ccARIMA <- data.frame(EC, fitAR$fitted)
names(ccARIMA)
ggscatter(ccARIMA, x = "EC", y = "fitAR.fitted", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Original data ", ylab = "ARIMA model")+
  theme_bw()
############################################
# NN
fitNNA <- nnetar(EC)
fotecNNA <-forecast(nnetar(EC), h=12*3)
AccuraNNA<-t(accuracy(fotecNNA))
AccuraNNA # NNA model accuracy
autoplot(fotecNNA)+
  autolayer(fitted(fotecNNA), series = "Fitted")+
  xlab("Year") + ylab("ET (kg/m^2/8day)") +
  ggtitle("Keiskammahoek 2001-2018: NNAR") +
  theme_bw()
fitNNA

# Correlation coeficient calculation 
#library("ggpubr")
CCann<- round(cor(EC, fitNNA$fitted, method = c("pearson")),2)
CCann
ccANN <- data.frame(EC, fitNNA$fitted)
names(ccANN)
ggscatter(ccANN, x = "EC", y = "fitNNA.fitted", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Original data", ylab = "ANN model")+
  theme_bw()
##################################################

# Hybrid_Model_NNA and ARIMA
Hybrid_Model_na <- hybridModel( y = EC,
                                models = "na",
                                n.args = list(repeats = 50, size = 25),
                                a.args = list(lambda="auto", biasadj = TRUE, approximation=FALSE, parallel=TRUE),
                                weights = "insample.errors", 
                                errorMethod = "MASE")
Hybrid_FC_2 <- forecast(Hybrid_Model_na, h = 12*3)
plot(Hybrid_FC_2)
Hybrid_Acc2 <- t(accuracy(Hybrid_FC_2))
Hybrid_Acc2
autoplot(Hybrid_FC_2)+
  autolayer(fitted(fotecNNA), series = "Fitted")+
  xlab("Year") + ylab("EC") +
  ggtitle("EC: Hybrid (NNAR+ARIMA)") +
  theme_bw()

# Correlation coeficient calculation 
#library("ggpubr")
CChb<- round(cor(EC, Hybrid_Model_na$fitted, method = c("pearson")),2)
CChb
ccHB <- data.frame(EC, Hybrid_Model_na$fitted)
names(ccHB)
ggscatter(ccHB, x = "EC", y = "Hybrid_Model_na.fitted", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Original data", ylab = "Hybrid ANN+ARIMA model")+
  theme_bw()
################################################
