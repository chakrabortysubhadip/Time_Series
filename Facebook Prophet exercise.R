#get data
df = read.csv("peyton-manning.csv")

#holidays
library(dplyr)
playoffs <- tibble(holiday = 'playoff',
                   ds = as.Date(c('2008-01-13', '2009-01-03', 
                                  '2010-01-16',
                 '2010-01-24', '2010-02-07', '2011-01-08',
                 '2013-01-12', '2014-01-12', '2014-01-19',
                 '2014-02-02', '2015-01-11', '2016-01-17',
                 '2016-01-24', '2016-02-07')),
                 lower_window = -1,
                 upper_window = 2)
superbowls <- tibble(holiday = 'superbowl',
                     ds = as.Date(c('2010-02-07', '2014-02-02', 
                                    '2016-02-07')),
                     lower_window = -3,
                     upper_window = 5)
holidays <- bind_rows(playoffs, superbowls)

#Prophet model
library(prophet)
m = prophet(yearly.seasonality = TRUE,
            weekly.seasonality = TRUE,
            daily.seasonality = FALSE,
            holidays = holidays,
            seasonality.mode = "multiplicative",
            seasonality.prior.scale = 10,
            holidays.prior.scale = 10,
            changepoint.prior.scale = 0.05)
m = fit.prophet(m,df)

#Cross Validation
df.cv = cross_validation(m,
                         horizon = 30,
                         units = 'days',
                         period = 20,
                         initial =2750)
#accuracy
library(forecast)
accuracy(df.cv$yhat, df.cv$y)

#forecast
future = make_future_dataframe(m,
                               periods = 100)
forecast = predict(m, future)

#visualization
plot(m, forecast)
prophet_plot_components(m, forecast)





