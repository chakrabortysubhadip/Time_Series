#loading data
data = read.csv("DHS_Daily_Report.csv")

#Tranforming Date Variable
data$Date = strptime(data$Date, "%m/%d/%Y")
data$Date = format(data$Date, "%Y-%m-%d")
data$Date = as.Date(data$Date)

#selecting variables
library(dplyr)
data = data %>% select(Date,
                       Total.Individuals.in.Shelter,
                       Easter,
                       Thanksgiving,
                       Christmas)

#transforming Y variable
colnames(data)[2] = "y"

#creating dataframes
future <- subset(data, data$Date > '2020-11-11')
dataset <- subset(data, data$Date <= '2020-11-11')

###############################################################################

#training and test set
training_set = subset(dataset, dataset$Date <= '2020-09-30')
test_set = subset(dataset, dataset$Date > '2020-09-30')

#Easter
library(dplyr)
easter_dates = subset(data, data$Easter == 1)
easter_dates = easter_dates$Date
easter <- tibble(holiday = 'Easter',
                 ds = as.Date(easter_dates),
                 lower_window = -3, upper_window = 1)

#Thanksgiving
thanksgiving_dates = subset(data, data$Thanksgiving == 1)
thanksgiving_dates = thanksgiving_dates$Date
thanksgiving <- tibble(holiday = 'Thanksgiving',
                 ds = as.Date(thanksgiving_dates),
                 lower_window = -7, upper_window = 4)

#Christmas
christmas_dates = subset(data, data$Christmas == 1)
christmas_dates = christmas_dates$Date
christmas <- tibble(holiday = 'Xmas',
                 ds = as.Date(christmas_dates),
                 lower_window = -4, upper_window = 3)

#Merging all holidays
holidays <- bind_rows(easter, thanksgiving, christmas)

#######################################################################

#Prophet preparation
df = training_set %>% select(Date, y)
colnames(df)[1] <- "ds"

#Prophet
library(prophet)
m <- prophet(growth = "linear",
             holidays = holidays, 
             yearly.seasonality = TRUE, 
             weekly.seasonality = TRUE, 
             daily.seasonality = FALSE, 
             seasonality.mode = 'additive',
             seasonality.prior.scale = 10, 
             changepoint.prior.scale = 0.005,
             holidays.prior.scale = 10)
m = fit.prophet(m, df)

#creating prediction dataframe
test_period <- make_future_dataframe(m, periods = nrow(test_set))
tail(test_period)

#Forecasting
prophet_forecast <- predict(m, test_period)

#plotting the predicting values and the seasonalities
plot(m, prophet_forecast)
prophet_plot_components(m, prophet_forecast)
plot(m, prophet_forecast) + add_changepoints_to_plot(m)

#accuracy
predictions_prophet <- tail(prophet_forecast$yhat, nrow(test_set))
accuracy(predictions_prophet, test_set$y)

#Cross validation
df.cv <- cross_validation(m, horizon = 31, initial = 2100, units = 'days')
accuracy(df.cv$yhat, df.cv$y)

############################################################################

#Prophet preparation
df = dataset %>% select(Date, y)
colnames(df)[1] <- "ds"

#Prophet
library(prophet)
m <- prophet(growth = "linear",
             holidays = holidays, 
             yearly.seasonality = TRUE, 
             weekly.seasonality = TRUE, 
             daily.seasonality = FALSE, 
             seasonality.mode = 'additive',
             seasonality.prior.scale = 10, 
             changepoint.prior.scale = 0.005,
             holidays.prior.scale = 10)
m = fit.prophet(m, df)

#creating prediction dataframe
future_period <- make_future_dataframe(m, periods = nrow(future))
tail(future_period)

#Forecasting
prophet_forecast <- predict(m, future_period)

#plotting the predicting values and the seasonalities
plot(m, prophet_forecast)

#getting future forcasts
future_prophet <- tail(prophet_forecast$yhat, nrow(future))

#####################################################################

#save forecasts
write.csv(predictions_prophet,
          file = "R/Forecast/predictions_prophet_r.csv",
          row.names = FALSE)
write.csv(future_prophet,
          file = "R/Future/future_prophet_r.csv",
          row.names = FALSE)  


