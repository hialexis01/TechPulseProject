getwd()
install.packages("Smisc")
install.packages("dummies")
library(Smisc)
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
library(dummies)
library(stargazer)

#This is attaching files for the tech sector
DataProject <- read.csv("FinalDatafornow.csv", header = TRUE)
attach(DataProject)
View(DataProject)

#First Regression with many economic indicators
NewRegression <- lm(SFTECH~., data = DataProject)
summary(NewRegression)
vif(NewRegression)

#Check for multicollinearity 
checkforcollin <- data.frame(cor(DataProject),use ="complete.obs")
write.csv(checkforcollin, "checkforcollin.csv")


#Another attempt to ignore
Tech<- lm(SFTECH ~ Unemp+ FedFund +
         +  CapUtil + CPI +
           + PPI
          +RecessionIndic + MinWage + Monetarybase + NASDAQCOM + MedHouseSales
         , data = DataProject)
summary(Tech)
vif(Tech)


#attempt to graph the time series
graphers <- read.csv("graphthis.csv", header = TRUE)
attach(graphers)
graphers 

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

betterwork <- data.frame("MonthlyDate" = as.numeric(chart_data$x), 
                        "Datatype" = chart_data$func, "thevalues" = as.numeric(chart_data$thevalue))
attach(betterwork)


ggplot()+
  geom_line(data = betterwork, aes(x=MonthlyDate, y = thevalues, colour = Datatype), size = 1)+
  xlab("x axis")+
  ylab("yaxis")


plot(SFTECH.ts, ylim=c(0,180))
lines(Empl.ts, col = "green")
lines(CPI.ts, col = "red")
#lines(CapUtil.ts, col = "blue")


#Creating a dummy variable
Recession.dummy <- dummy.data.frame(DataProject, RecessionIdic)
Recession.dummy


TechEffect <- lm(SFTECH ~ Unemp + CapUtil + log(Dis.IncPerCap) + PPI
                + FedFund + SupplyofHouses + RecessionIndic, data = DataProject)
summary(TechEffect)
vif(TechEffect)



  
stargazer(NewRegression, Tech, TechEffect, type = "latex", title = "OLS Model (1)", align = TRUE, 
          dep.var.labels = "SFTECH", font.size = "small", single.row = TRUE,
          column.sep.width = "1pt"
           )

NewRegression.Dataframe <- data.frame(vif(NewRegression))
Tech.Dataframe <- data.frame(vif(Tech))
TechEffect.dataframe <- data.frame(vif(TechEffect))
Tech.Dataframe
TechEffect.dataframe

stargazer(NewRegression.Dataframe, Tech.Dataframe, TechEffect.dataframe, colnames = c("OLS 1", "OLS 2", "OLS3"), type = "latex", title = "VIF", 
          align = TRUE, summary = FALSE)
stargazer(ReconstructVif.one, type = "latex", title = "VIF", align = TRUE, summary = FALSE, dep.var.labels = "OLS")
stargazer(TechEffect.dataframe, type = "latex", title = "VIF", align = TRUE, dep.var.labels = "OLS", summary = FALSE)

plot(TechEffect)
plot(NewRegression)
#Housing Regression
TheTechinSF <- read.csv("Herewegoagain.csv", header = TRUE)
attach(TheTechinSF)
View(TheTechinSF)

data.frame(UnemplMSA, NASDAQCOM)
fitregress<- lm(TechyfromSF~ UnemplMSA+ HomePricesofSF+ EconCond+ NASDAQ)
summary(fitregress)
vif(fitregress)

plot(fitregress)