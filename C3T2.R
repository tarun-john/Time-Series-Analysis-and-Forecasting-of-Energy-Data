library(dplyr)
library(RMySQL)
library(dbConnect)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggfortify)
library(forecast)


## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
str(irisALL)

## Use attribute names to specify specific attributes for download 
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#Downloading all attributes for all years
yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007") 
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008") 
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009") 
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

#Refine attributes needed
yr_2006 <- select(yr_2006, Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3)
yr_2007 <- select(yr_2007, Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3)
yr_2008 <- select(yr_2008, Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3) 
yr_2009 <- select(yr_2009, Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3)
yr_2010 <- select(yr_2010, Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3) 

final <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)
#Combine date & time
final <- cbind(final, paste(final$Date, final$Time), stringsAsFactors = FALSE)
#Change col name
colnames(final)[7] <- "DateTime"

final <- final[,c(ncol(final), 1:(ncol(final)-1))]
## Convert DateTime from character to POSIXct 
final$DateTime <- as.POSIXct(final$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(final$DateTime, "tzone") <- "Europe/Paris"

#Create year 
final$Year <- year(final$DateTime)
#Create rest of time frames
final$Quarter <- quarter(final$DateTime)
final$Month <- month(final$DateTime)
final$Week <- week(final$DateTime)
final$Weekdays <- weekdays(final$DateTime)
final$Day <- day(final$DateTime)
final$Hour <- hour(final$DateTime)
final$Minute <- minute(final$DateTime)


colnames(final)[4] <- "Household.Power(kW)"
colnames(final)[5] <- "Kitchen(Wh)" 
colnames(final)[6] <- "Laundry.Room(Wh)" 
colnames(final)[7] <- "Heater.&.A/C(Wh)"

#Remove unneeded
final[,c(2,3)] <- NULL

head(final)
str(final)
#Check for missing values 
sum(is.na(final))



## Subset the second week of 2008 - All Observations 
houseweek <- filter(final, Year == 2008 & Week == 2)
## Plot subset houseWeek 
plot(houseweek$`Kitchen(Wh)`)

## Subset the 9th day of January 2008 - All observations
houseday <- filter(final, Year == 2008 & Month == 1 & Day == 9)
## Plot sub-meter 1
plot_ly(houseday, x = ~houseday$DateTime, 
        y = ~houseday$`Kitchen(Wh)`, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseday, x = ~houseday$DateTime, y = ~houseday$`Kitchen(Wh)`, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday$`Laundry.Room(Wh)`, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseday$`Heater.&.A/C(Wh)`, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(final, Year == 2008 & Month == 1 & Day == 9 
                     & (Minute == 0 | Minute == 10 | Minute == 20 | 
                          Minute == 30 | Minute == 40 | Minute == 50))
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$`Kitchen(Wh)`, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$`Laundry.Room(Wh)`, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$`Heater.&.A/C(Wh)`, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(houseweek, x = ~houseweek$DateTime, y = ~houseweek$`Kitchen(Wh)`, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseweek$`Laundry.Room(Wh)`, name = 'Laundry Room', mode = 'lines')%>% 
  add_trace(y = ~houseweek$`Heater.&.A/C(Wh)`, name = "Water Heater & AC", mode = 'lines')

#Adjust granularity
houseweek4 <- filter(final, Year == 2008 & Week == 2
                     & (Minute == 0 | Minute == 15 | 
                     Minute == 30 | Minute == 45)) 

plot_ly(houseweek4, x = ~houseweek4$Weekdays, y = ~houseweek4$`Kitchen(Wh)`,
        name = "Kitchen", type = "scatter", mode = "lines")%>%
  add_trace(y = ~houseweek4$`Laundry.Room(Wh)`, name = "Laundry Room", mode = "lines")%>%
  add_trace(y = ~houseweek4$`Heater.&.A/C(Wh)`, name = "Water Heater & AC", mode = "lines")%>%
  layout(title = "2nd Week of Jan 2008",
       xaxis = list(title = "Weekdays"),
       yaxis = list (title = "Power (watt-hours)")) 






## Subset to one observation per week for 2007, 2008 and 2009
house070809weekly <- filter(final, (Year == 2007 | Year == 2008 | Year == 2009) &
                                    Weekdays == "Tuesday" & Hour == 20 & Minute == 1)
#Time series for Meter 3
ts070809weekly3 <- ts(house070809weekly$`Heater.&.A/C(Wh)`, frequency = 52, start = c(2007,1))
autoplot(ts070809weekly, ts.colour = "red", xlab = "Time", ylab = "Watt Hours", 
         main = "Heater & AC")
plot.ts(ts070809weekly, xlab = "Time (Years)", ylab = "Watt Hours", main = "Heater & AC")

#Time series for Meter 1
ts070809weekly1 <- ts(house070809weekly$`Kitchen(Wh)`, frequency = 52, start = c(2007,1))
plot.ts(ts070809weekly, xlab = "Time (Years)", ylab = "Watt Hours", main = "Kitchen")

#Time series for Meter 2
ts070809weekly2 <- ts(house070809weekly$`Laundry.Room(Wh)`, frequency = 52, start = c(2007,1))
plot.ts(ts070809weekly, xlab = "Time (Years)", ylab = "Watt Hours", main = "Laundry Room")

#Time series Household
ts070809weekly <- ts(house070809weekly$`Household.Power(kW)`, frequency = 52, start = c(2007,1))
plot.ts(ts070809weekly, xlab = "Time (Years)", ylab = "kW-hours", main = "Whole House")





#Linear Regression
fitSM3 <- tslm(ts070809weekly3 ~ trend + season)
fitSM2 <- tslm(ts070809weekly2 ~ trend + season)
fitSM1 <- tslm(ts070809weekly1 ~ trend + season)

summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)
## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time")

## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM2 <- forecast(fitSM2, h=20, level=c(80,90))
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM2, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time")

## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM1 <- forecast(fitSM1, h=20, level=c(80,90))
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM1, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time")

summary(forecastfitSM1)
summary(forecastfitSM2)
summary(forecastfitSM3)



#Decompose
## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(ts070809weekly3)
## Plot decomposed sub-meter 3
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

## Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(ts070809weekly2)
## Plot decomposed sub-meter 2
plot(components070809SM2weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM2weekly)

## Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(ts070809weekly1)
## Plot decomposed sub-meter 2
plot(components070809SM1weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM1weekly)



#Holt-Winters Forecasting
## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- ts070809weekly3 - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))
## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))
## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 25), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 10), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- ts070809weekly2 - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)
plot(decompose(tsSM2_070809Adjusted))
## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25)) 
## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 25), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 5), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

## Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- ts070809weekly1 - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)
plot(decompose(tsSM1_070809Adjusted))
## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25)) 
## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 25), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")
## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 5), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

