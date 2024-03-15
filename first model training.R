install.packages("rlang")
install.packages("tidymodels")
install.packages("stringr")
install.packages("tidyverse")
install.packages("dplyr")

# Library for modeling
library(tidymodels)
library(stringr)
library(tidyverse)
library(dplyr)
library(parsnip)
library(broom)

#Downlad de file---------------------------------------------------------------------------------------------------------------------------------------------
url <- 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'
ruta <- 'C:/Users/samju/OneDrive/Escritorio/Final Project Unit 7/data.gz'
dta_prev <- download.file(url, destfile = ruta, mode="wb")
ruta_descomp <- 'C:/Users/samju/OneDrive/Escritorio/Final Project Unit 7'

#Unzip file--------------------------------------------------------------------------------------------------------------------------------------------------
prev_data <- untar(ruta, exdir = ruta_descomp)
descomprimidos <- list.files(ruta_descomp, full.names = TRUE)

#Read data---------------------------------------------------------------------------------------------------------------------------------------------------
datos<-read.csv('C:/Users/samju/OneDrive/Escritorio/Final Project Unit 7/noaa-weather-sample-data/jfk_weather_sample.csv', header=TRUE)
head(datos)
summary(datos)


#Subset of the original, only 5 columns----------------------------------------------------------------------------------------------------------------------
data_subs <- datos[, c("HOURLYRelativeHumidity", "HOURLYDRYBULBTEMPF", "HOURLYPrecip", "HOURLYWindSpeed", "HOURLYStationPressure")]
head(data_subs)

#Clean the columns-------------------------------------------------------------------------------------------------------------------------------------------
unique(data_subs$HOURLYPrecip)
data_subs$HOURLYPrecip <- gsub("T", 0, data_subs$HOURLYPrecip)
unique(data_subs$HOURLYPrecip)
data_subs$HOURLYPrecip <- substr(data_subs$HOURLYPrecip, 1, nchar(data_subs$HOURLYPrecip) - 1)
unique(data_subs$HOURLYPrecip)

#Convert column to numeric-----------------------------------------------------------------------------------------------------------------------------------
glimpse(data_subs)
data_subs$HOURLYPrecip <- as.numeric(data_subs$HOURLYPrecip)
glimpse(data_subs)
unique(data_subs$HOURLYPrecip)

#Rename Columns----------------------------------------------------------------------------------------------------------------------------------------------
new_data <- data_subs %>%
  rename(
    relative_humidity = HOURLYRelativeHumidity,
    dry_bulb_temp_f = HOURLYDRYBULBTEMPF,
    precip = HOURLYPrecip,
    wind_speed = HOURLYWindSpeed,
    station_pressure = HOURLYStationPressure 
  )

summary(new_data)

#EXPLORATORY DATA ANALYSIS----------------------------------------------------------------------------------------------------------------------------------
new_data <- new_data %>%
  mutate(relative_humidity = coalesce(relative_humidity, 0))
new_data <- new_data %>%
  mutate(dry_bulb_temp_f = coalesce(dry_bulb_temp_f, 0))
new_data <- new_data %>%
  mutate(precip = coalesce(precip, 0))
new_data <- new_data %>%
  mutate(wind_speed = coalesce(wind_speed, 0))
new_data <- new_data %>%
  mutate(station_pressure = coalesce(station_pressure, 0))
summary(new_data)

set.seed(1234)
new_data_split <- initial_split(new_data, prop = 0.8)
train_data <- training(new_data_split)
test_data <- testing(new_data_split)
summary(train_data)

binwidth_scott <- diff(range(train_data$relative_humidity)) / (3.5 * sd(train_data$relative_humidity) / length(train_data$relative_humidity)^(1/3))

binwidth_scott_dry <- diff(range(train_data$dry_bulb_temp_f)) / (3.5 * sd(train_data$dry_bulb_temp_f) / length(train_data$dry_bulb_temp_f)^(1/3))

binwidth_scott_precip <- diff(range(train_data$precip)) / (3.5 * sd(train_data$precip) / length(train_data$precip)^(1/3))

binwidth_scott_wind <- diff(range(train_data$wind_speed)) / (3.5 * sd(train_data$wind_speed) / length(train_data$wind_speed)^(1/3))

binwidth_scott_stat <- diff(range(train_data$station_pressure)) / (3.5 * sd(train_data$station_pressure) / length(train_data$station_pressure)^(1/3))

ggplot(train_data, aes(x = relative_humidity)) +
  geom_histogram(binwidth = 21, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de relative humidity", x = "Humidity", y = "Frecuencia")

ggplot(train_data, aes(x = dry_bulb_temp_f)) +
  geom_histogram(binwidth = 24, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de bulb temp", x = "Bulb Temp", y = "Frecuencia")

ggplot(train_data, aes(x = precip)) +
  geom_histogram(binwidth = 163, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de precip", x = "Precip", y = "Frecuencia")

ggplot(train_data, aes(x = wind_speed)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de precip", x = "Precip", y = "Frecuencia")

ggplot(train_data, aes(x = station_pressure)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de precip", x = "Precip", y = "Frecuencia")

#MODELO DE REGRESIÓN LINEAL--------------------------------------------------------------------------------------------------------------------
lm_spec <- linear_reg() %>%
  set_engine(engine = "lm")
  
train_fit<- lm_spec %>%
  fit(precip ~ relative_humidity, data = train_data)
 

train_fit1<- lm_spec %>%
  fit(precip ~ dry_bulb_temp_f, data = train_data)


train_fit2<- lm_spec %>%
  fit(precip ~ wind_speed, data = train_data)

train_fit3<- lm_spec %>%
  fit(precip ~ station_pressure, data = train_data)

summary(train_fit$fit)
summary(train_fit1$fit)
summary(train_fit2$fit)
summary(train_fit3$fit)

predicciones <- train_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$precip)

head(predicciones)

ggplot(train_data, aes(x = wind_speed, y = precip)) +
  geom_point() +  # Puntos de datos
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +  # Línea de regresión
  labs(title = "Regresión Lineal", x = "wind speed", y = "Precipitación")


#IMPROVE THE MODEL --------------------------------------------------------------------------------------------
train_fit4<- lm_spec %>%
  fit(precip ~ station_pressure + relative_humidity, data = train_data)
summary(train_fit4$fit)

train_fit5<- lm_spec %>%
  fit(precip ~ station_pressure + relative_humidity + wind_speed, data = train_data)
summary(train_fit5$fit)

polyfit1 <- lm(train_data$precip ~ poly(train_data$relative_humidity,2,raw = TRUE))
summary(polyfit1)

polyfit2 <- lm(train_data$precip ~ poly(train_data$wind_speed,2,raw = TRUE))
summary(polyfit2)
