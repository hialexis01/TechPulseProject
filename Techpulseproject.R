getwd()
library(psych)
library(dplyr)
library(data.table)
library(DataExplorer) 
library(car)
library(ggplot2)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggfortify)
library(forecast)
library(zoo)

DataProject <- read.csv("FinalDatafornow.csv", header = TRUE)
attach(DataProject)
View(DataProject)

NewRegression <- lm(SFTECH~.-DATE, data = DataProject)
summary(NewRegression)

checkforcollin <- data.frame(cor(DataProject),use ="complete.obs")
write.csv(checkforcollin, "checkforcollin.csv")

Tech<- lm(SFTECH ~ UnemplRate+ FedUndRate +
         +  CapUtil + DispIncomePerCapita
           + ProducerPriceIndex
          +RecessionIdic  + NASDAQCOM
         
         , data = DataProject)
summary(Tech)

vif(Tech)

graphers <- read.csv("graphthis.csv", header = TRUE)
attach(graphers)

SFTECH.ts <- ts(graphers$TechSector, start = c(1972,1), frequency = 12)
Empl.ts <- ts(graphers$EMPLOY, start = c(1972,1), frequency = 12)
CPI.ts <- ts(graphers$CPI, start = c(1972,1), frequency = 12)
CapUtil.ts <- ts(graphers$CAPUTIL, start = c(1972,1), frequency = 12)

set.seed(1)
ts.plot(Empl.ts, SFTECH.ts, gpars = list(col = c("black", "red")))

ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

chart_data<-melt(graphers, id = 'DATE')
names(chart_data) <- c('x', 'func', 'thevalue')
chart_data<- data.frame(chart_data)
attach(chart_data)

betterwork <- data.frame("MonthlyDate" = as.numeric(chart_data$x), "Datatype" = chart_data$func, "thevalues" = as.numeric(chart_data$thevalue))

attach(betterwork)


ggplot()+
  geom_line(data = betterwork, aes(x=MonthlyDate, y = thevalues, colour = Datatype), size = 1)+
  xlab("x axis")+
  ylab("yaxis")


plot(SFTECH.ts, ylim=c(0,180))
lines(Empl.ts, col = "green")
lines(CPI.ts, col = "red")
#lines(CapUtil.ts, col = "blue")

TechEffect <- lm(SFTECH ~ UnemplRate + CapUtil + RecessionIdic + DisposableINcome + ProducerPriceIndex + FedUndRate
                 )
summary(TechEffect)
vif(TechEffect)

stargazer(TechEffect, type = "latex", title = "Regression model", align = TRUE, dep.var.labels = "OLS")
stargazer(vif(TechEffect), type = "latex", title = "VIF", align = TRUE, dep.var.labels = "OLS")