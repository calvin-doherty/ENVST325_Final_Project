# Install packages

library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(readr)
library(PerformanceAnalytics)
library(olsrr)

# Load data

Oregon_PM2.5_2023 <- read.csv("https://raw.githubusercontent.com/calvin-doherty/ENVST325_Final_Project/main/Oregon_PM2.5_2023.csv")
Oregon_PM2.5_2022 <- read.csv("https://raw.githubusercontent.com/calvin-doherty/ENVST325_Final_Project/main/Oregon_PM2.5_2022.csv")
Oregon_PM2.5_2021 <- read.csv("https://raw.githubusercontent.com/calvin-doherty/ENVST325_Final_Project/main/Oregon_PM2.5_2021.csv")
Oregon_PM2.5_2020 <- read.csv("https://raw.githubusercontent.com/calvin-doherty/ENVST325_Final_Project/main/Oregon_PM2.5_2020.csv")
Oregon_PM2.5_2019 <- read.csv("https://raw.githubusercontent.com/calvin-doherty/ENVST325_Final_Project/main/Oregon_PM2.5_2019.csv")
Oregon_PM2.5_2018 <- read.csv("https://raw.githubusercontent.com/calvin-doherty/ENVST325_Final_Project/main/Oregon_PM2.5_2018.csv")

Bend_Weather <- read_excel("https://raw.githubusercontent.com/calvin-doherty/ENVST325_Final_Project/main/Bend Weather Data.xlsx")

Oregon_Fires <- read_csv("https://raw.githubusercontent.com/calvin-doherty/ENVST325_Final_Project/main/Fire_18_23.csv")

### Clean air quality data

colnames(Oregon_PM2.5_2023) <- colnames(Oregon_PM2.5_2022)

Oregon_18_23 <- rbind(Oregon_PM2.5_2018, Oregon_PM2.5_2019, Oregon_PM2.5_2020,
                      Oregon_PM2.5_2021, Oregon_PM2.5_2022, Oregon_PM2.5_2023)

Oregon_18_23$Date_parsed <- mdy(Oregon_18_23$Date)

Oregon_18_23$Month <- month(Oregon_18_23$Date_parsed)

Oregon_18_23$Year <- year(Oregon_18_23$Date_parsed)

colnames(Oregon_18_23)[5] <- c("PM2.5")
colnames(Oregon_18_23)[3] <- c("ID")

Bend_Air_18_23 <- Oregon_18_23 %>%
  filter(ID == 410170004)

# Convert to monthly average

Bend_Air_Monthly <- Bend_Air_18_23 %>%
  group_by(Year, Month) %>%
  summarize(mean(PM2.5))

### Clean fire data 

Oregon_Fires_clean <- data.frame(Oregon_Fires$IgnitionTime, Oregon_Fires$FinalFireSizeAcres, 
                                 Oregon_Fires$County)

colnames(Oregon_Fires_clean)[1] <- c("Ignition_time")
colnames(Oregon_Fires_clean)[2] <- c("Final_acres")
colnames(Oregon_Fires_clean)[3] <- c("County")

Oregon_Fires_clean$Ignition_parsed <- mdy_hms(Oregon_Fires_clean$Ignition_time)

Oregon_Fires_clean$Ignition_month <- month(Oregon_Fires_clean$Ignition_parsed)

Oregon_Fires_clean$Ignition_year <- year(Oregon_Fires_clean$Ignition_parsed)

# Convert to monthly average

Oregon_Fires_Monthly <- Oregon_Fires_clean %>%
  group_by(Ignition_year, Ignition_month) %>%
  summarize(mean(Final_acres))

colnames(Oregon_Fires_Monthly)[1] <- c("Year")
colnames(Oregon_Fires_Monthly)[2] <- c("Month")
colnames(Oregon_Fires_Monthly)[3] <- c("Oregon_acres")

# Isolate Deschutes County

Deschutes_Fires <- Oregon_Fires_clean %>%
  filter(County==9)

# Convert to county monthly average

Deschutes_Fires_Monthly <- Deschutes_Fires %>%
  group_by(Ignition_year, Ignition_month) %>%
  summarize(mean(Final_acres))

colnames(Deschutes_Fires_Monthly)[1] <- c("Year")
colnames(Deschutes_Fires_Monthly)[2] <- c("Month")
colnames(Deschutes_Fires_Monthly)[3] <- c("Deschutes_acres")

### Clean weather data

# Rename columns

colnames(Bend_Weather)[2] <- c("Temp_F")
colnames(Bend_Weather)[3] <- c("Precip")
colnames(Bend_Weather)[4] <- c("Humidity")
colnames(Bend_Weather)[5] <- c("Wind_speed")
colnames(Bend_Weather)[8] <- c("Wind_run_mi")

# Parse date

Bend_Weather$Date_parsed <- ymd(Bend_Weather$Date)

Bend_Weather$Month <- month(Bend_Weather$Date)

Bend_Weather$Year <- year(Bend_Weather$Date)

# Convert to monthly averages

Bend_Weather_Monthly <- Bend_Weather %>%
  group_by(Year, Month) %>%
  summarize(mean(Temp_F), mean(Precip), mean(Humidity), mean(Wind_speed), 
            mean(Wind_run_mi))
  
### Merge final datasets

FiresMerged <- full_join(Oregon_Fires_Monthly, Deschutes_Fires_Monthly, by=c("Month", "Year"))
FiresWeatherMerged <- full_join(FiresMerged, Bend_Weather_Monthly, by=c("Month", "Year"))
AllData <- full_join(FiresWeatherMerged, Bend_Air_Monthly, by=c("Month", "Year"))

colnames(AllData)[5] <- c("Temp_F")
colnames(AllData)[6] <- c("Precip")
colnames(AllData)[7] <- c("Humidity")
colnames(AllData)[8] <- c("Wind_speed")
colnames(AllData)[9] <- c("Wind_run_mi")
colnames(AllData)[10] <- c("PM2.5")

ModelData <- AllData %>%
filter(Year != '2023')

Data2023 <- AllData %>%
  filter(Year == '2023')

### Build regression model

PM2.5_Model <- lm(PM2.5 ~ Oregon_acres + Deschutes_acres + Temp_F +
                    Precip + Humidity+ Wind_speed + Wind_run_mi, data=ModelData)

summary(PM2.5_Model)

ModelData %>%
  ggplot()+
  aes(x=Oregon_acres, y=PM2.5)+
  geom_point()+
  geom_smooth(method='lm', se = FALSE, col = 'olivedrab')+
  labs(x = "Monthly area burned from wildfires in Oregon (acres)", 
       y = "Monthly average PM2.5 levels (Bend, Oregon)")+
  theme_classic()

# Outlier identified from plot, must be removed

ModelData <- ModelData %>%
  mutate(Oregon_acres_filtered = if_else(Oregon_acres < 2000, Oregon_acres, NA_real_))

Data2023 <- Data2023 %>%
  mutate(Oregon_acres_filtered = if_else(Oregon_acres < 2000, Oregon_acres, NA_real_))

ModelData %>%
  ggplot()+
  aes(x=Oregon_acres_filtered, y=PM2.5)+
  geom_point()+
  geom_smooth(method='lm', se = FALSE, col = 'olivedrab')+
  labs(x = "Monthly area burned from wildfires in Oregon (acres)", 
       y = "Monthly average PM2.5 levels (Bend, Oregon)")+
  scale_y_continuous(limits = c(0, 30))+
  theme_classic()

# Filter for outlier in model

PM2.5_Model_filtered <- lm(PM2.5 ~ Oregon_acres_filtered + Deschutes_acres + Temp_F +
                    Precip + Humidity+ Wind_speed + Wind_run_mi, data=ModelData)

summary(PM2.5_Model_filtered)

# Try log precipitation and humidity

ModelData$log.precip <- log(ModelData$Precip)
ModelData$log.humidity <- log(ModelData$Humidity)

PM2.5_Model_logged <- lm(PM2.5 ~ Oregon_acres_filtered + Deschutes_acres + Temp_F +
                             log.precip + log.humidity + Wind_speed + Wind_run_mi, data=ModelData)

summary(PM2.5_Model_logged) # doesn't have significant positive impact, won't use in predictions

# Analyze residuals

model_residuals <- rstandard(PM2.5_Model_filtered)
model_fitted <- fitted.values(PM2.5_Model_filtered)

qqnorm(model_residuals, pch=19, col="steelblue")
qqline(model_residuals)

shapiro.test(model_residuals)

plot(model_fitted, model_residuals, pch=19, col="steelblue")
abline(h=0)

# Test for multicollinearity

multi.data <- data.frame(ModelData$Oregon_acres_filtered,
                       ModelData$Deschutes_acres,ModelData$Precip,
                       ModelData$Temp_F,
                       ModelData$Humidity, ModelData$Wind_run_mi, 
                       ModelData$Wind_speed)

chart.Correlation(multi.data, histogram=TRUE, pch=19)

# Try new model based on multicollinearity

PM2.5_Model_adjusted <- lm(PM2.5 ~ Oregon_acres_filtered + Deschutes_acres +
                             Precip + Wind_speed, data=ModelData)

summary(PM2.5_Model_adjusted) # one significant variable

# Check for multicollinearity

multi.data <- data.frame(ModelData$Oregon_acres_filtered,
                         ModelData$Deschutes_acres,ModelData$Precip,
                         ModelData$Wind_speed)

chart.Correlation(multi.data, histogram=TRUE, pch=19)

# Analyze new residuals

adj_model_residuals <- rstandard(PM2.5_Model_adjusted)
adj_model_fitted <- fitted.values(PM2.5_Model_adjusted)

qqnorm(adj_model_residuals, pch=19, col="steelblue")
qqline(adj_model_residuals)

shapiro.test(adj_model_residuals)

plot(adj_model_fitted, adj_model_residuals, pch=19, col="steelblue")
abline(h=0)

# Assess model performance

model.step <- ols_step_forward_aic(PM2.5_Model_adjusted)
model.step 

### Predict PM2.5 for 2023

Data2023$Deschutes_acres[is.na(Data2023$Deschutes_acres)] <- 0

Data2023$predicted_PM2.5 <-  predict.lm(PM2.5_Model_adjusted, newdata = Data2023, 
                                        interval="confidence")

Data2023 %>%
  ggplot() +
  geom_ribbon(aes(x = Month, ymin = predicted_PM2.5[,"lwr"], 
                  ymax = predicted_PM2.5[,"upr"]), alpha = 0.2, fill = "tomato3") +
  geom_line(aes(x = Month, y = predicted_PM2.5[,"fit"]), color = "tomato", size = 1) +
  geom_point(aes(x = Month, y = predicted_PM2.5[,"fit"]), color = "tomato") +
  geom_point(aes(x = Month, y = PM2.5), color = "black") +
  labs(x = "Month", y = "PM2.5")+
  theme_classic()

# Predicted residuals

predicted_residuals <- Data2023$PM2.5 - Data2023$predicted_PM2.5

qqnorm(predicted_residuals, pch=19, col="steelblue")
qqline(predicted_residuals)

shapiro.test(predicted_residuals)











