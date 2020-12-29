#1
library(tidyr)
library(dplyr)
library(ggplot2)
tracking_data <- read.csv("Tracking_data.csv", header = T)
td_tbl <- as_tibble(tracking_data)
names(td_tbl)

#boxplot flyhing hight average
td_tbl %>%
  filter(Longitude != 200) %>%
  ggplot(aes(x = UUID, y = Geoid_height, color=UUID))+
  geom_boxplot(na.rm = T)+
  theme_classic()
# MingYANG noticed:
# it is not clear and aesthetic, maybe you can transform height(y-axis) as log data
# the end 

#Time series flying hight
install.packages("lubridate")
library(lubridate)
TS_data <- td_tbl %>%
  group_by(UUID) %>%
  mutate(date_td = as.Date(Collecting_time)) %>%
  filter(Geoid_height >= 0 && UUID =="f8f86b8da0c4")
head(TS_data)
tail(TS_data)
Date_start1 <- as.Date("2020-06-27")
Date_end1 <- as.Date("2020-11-09")
JD_start1   <- yday(Date_start1)
xaxis <- seq(Date_start1, Date_end1, by = "day")
High_ts <- ts(TS_data$Geoid_height[1:136], start=c(2020, JD_start1), frequency = 365)
str(High_ts)
plot(xaxis, High_ts, type = "l")
# MingYANG noticed:
# it is too simple, and should be classified into 1 point plot given by Prof.ZHU in the explaination of this assignment
# the end 

#Histogram
td_tbl %>%
  group_by(UUID) %>%
  ggplot(aes( x = Speed, color=UUID))+
  geom_histogram()+
  theme_classic() 
# MingYANG noticed:
# it`s goof for combining three groups of data into one histogram, but it is not aesthetic
# the end 

#Scatter plot
td_tbl %>%
  group_by(UUID) %>%
  filter(Longitude != 0 && Latitude != 200 ) %>%
  ggplot(aes(x = Longitude, y = Latitude, color=UUID))+
  geom_point(na.rm = T)+
  theme_classic()+
  ylim(0,180)+
  xlim(0,180)
# MingYANG noticed:
# it is not clear and aesthetic, xlim and ylim should be well-matched
# the end 

#Image plot

#2
baoan_data <- read.csv("2281305.csv",header = T)
library(tidyr)
library(dplyr)
library(ggplot2)

#2.1
banan_data_tbl <- as_tibble(baoan_data)
head(banan_data_tbl)
names(banan_data_tbl)
bdt <- banan_data_tbl %>%
  filter(TMP != "+9999") %>%
  mutate(date1 = substr(DATE,1,7), tmp = as.numeric(substr(TMP,3,5))/10) %>%
  group_by(date1) %>%
  summarise(Tmean = mean(tmp))
temperture_ts <- ts(bdt$Tmean, start=c(2010,1), frequency = 12)
plot(temperture_ts, type = "l")


#2.2
temperture_components <- decompose(temperture_ts)
plot(temperture_components)
# MingYANG noticed:
# Check whether the error part follows a white noise distribution.
# the end

#2.3
acf(temperture_ts)
pacf(temperture_ts)
install.packages("forecast")
library(forecast)
model_arima <- auto.arima(temperture_ts)
model_arima
# MingYANG noticed:
# Describe the fitting process in details in your report.
# the end

#2.4
bdt2 <- bdt %>%
  filter(date1 < "2020-08")
temperture_ts2 <- ts(bdt2$Tmean, start=c(2010,1), frequency = 12)
model_arima2 <- auto.arima(temperture_ts2)
meanT_forecast <- 5
month_in_plot <- 30
forecast_2meanT <- forecast(model_arima2, meanT_forecast)
plot(forecast(model_arima2, meanT_forecast), include = month_in_plot, xlab = "time", ylab = "mean_temperture")
#check
bdt %>%
  filter(date1 > "2020-08") #check 2020-08 actual T
forecast_2meanT$mean[1] #Sep. T prediction
forecast_2meanT$mean[2] #Sep. T prediction

