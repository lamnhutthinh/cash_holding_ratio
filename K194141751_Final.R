#Import the dataset.
library(tidyverse)
library(readxl)
data = read_excel("K194141751.xlsx", skip = 1)
#Name the columns.
names(data)[1] = 'Company'
names(data)[2] = 'Date'
names(data)[3] = 'Total Assets'
names(data)[10] = 'Cash from Operating Activities'
names(data)[11] = 'Net Income'
names(data)[12] = 'Tangible Assets'
names(data)[13] = 'Intangible Assets'
data = data[2:nrow(data),]
View(data)                        
# Choose necessary variables.
library(dplyr)
library(tidyverse)
library(pastecs)


data_final = data %>% 
  arrange(Date) %>% 
  transmute(Date,
            `Cash Holding` = `Cash and Equivalents`/`Total Assets`,
            ROA = `Net Income`/`Total Assets`,
            `OCF` = `Cash from Operating Activities`/`Total Assets`,
            Size = log(`Total Assets`))

data_final = data_final[2:nrow(data_final),]
# Define the Covid variable.
data_final$Covid = ifelse(data_final$Date >= '2020-03-31', 1, 0)
#Format the time for easier reading.
data_final$Time = "0"
count = 0
for (i in 2012:2021){
  for (j in 1:4){
    count = count + 1
    data_final$Time[count] = print(paste('Q',j,'/',i),sep = '')
  }
} 
data_final = data_final %>% select(-Date)
data_final = data_final %>% select(Time,everything())
#Split the data into two datasets (before 2020 and after 2020)
before_2020 = data_final[1:32,]
after_2020 = data_final[33:nrow(data_final),]
view(data_final)
view(before_2020)
view(after_2020)

#3. Provide descriptive statistics of all the variables for BEFORE and AFTER periods
stat.desc(before_2020[2:ncol(before_2020)])[c(4,5,8,9,13),1:ncol(stat.desc(before_2020[2:ncol(before_2020)]))]
stat.desc(after_2020[2:ncol(after_2020)])[c(4,5,8,9,13),1:ncol(stat.desc(after_2020[2:ncol(after_2020)]))]

#4. Provide box & whisker plot and histogram of the variable of assigned topic, i.e.,
#Leverage/Cash holding (for the entire period)
library(forcats)
library(ggplot2)
library(scales)
#Histogram plot and Density plot.
ggplot(data_final, aes(x = `Cash Holding`)) +
  ggtitle("Cash Holding Histogram Plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(color = 'black', fill = 'darkgreen',binwidth = 0.005)
ggplot(data_final, aes(x = `Cash Holding`)) +
  ggtitle("Cash Holding Density Plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_density(color = 'black', fill = 'darkgreen')
#Box plot
ggplot(data_final, aes(x = `Cash Holding`)) + 
  ggtitle("Cash Holding Box Plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(fill = 'gold')

#5. Perform multiple regression to determine the significant determinants
#5.1. With the usual individual variables

model1 = lm(`Cash Holding` ~  ROA  + Size + OCF, data = data_final)
summary(model1)

#5.2. With the usual individual variables and Covid

data_final$`ROA & Covid` = data_final$ROA*data_final$Covid
data_final$`Size & Covid` = data_final$`Size`*data_final$Covid
data_final$`OCF & Covid` = data_final$OCF*data_final$Covid

model2 = lm(`Cash Holding` ~  ROA  + Size + OCF + `ROA & Covid` + `Size & Covid` + `OCF & Covid`, data = data_final)
summary(model2)

#5.3. Predict the value of the variable of assigned topic for all quarters

predict_1 = data.frame("Period"=data_final$Time,"Prediction"=predict(model1, data_final))
predict_1

#6. Perform ARIMA model to predict the variable of interest

library(forecast)
library(quantmod)
library(tseries)
library(lmtest)
library(stats)
#ADF Test
plot.ts(data_final$`Cash Holding`)
adf.test(data_final$`Cash Holding`)
data_diff = diff(data_final$`Cash Holding`,differences = 1)
data_diff = na.omit(data_diff)
adf.test(data_diff)
#Auto Arima
plot(acf(data_diff), main = 'ACF for Cash Holding')
plot(pacf(data_diff), main = 'PACF for Cash Holding')
auto_model=auto.arima(data_diff,seasonal=F,trace = T,max.order=4, ic='aic') 
coeftest(auto.arima(data_diff,seasonal=F)) 
#Residuals Test
acf(auto_model$residuals) 
pacf(auto_model$residuals) 
Box.test(auto_model$residuals,lag=20,type='Ljung-Box')
plot(density(auto_model$residuals),main="Kernel Density of Residuals")
qqnorm(auto_model$residuals, pch = 1, frame = FALSE)
qqline(auto_model$residuals, col = "darkgreen", lwd = 2)
#Predict 
forecast2022=forecast(auto_model,h=4) 
forecast2022 
plot(forecast2022)