#get data
data = read.csv("Churrasco.csv")

#training and test set
training_set = data[1:251,]
test_set = data[252:261,]

#exogenous variables
train_exog = as.matrix(training_set[,3:5])
test_exog = as.matrix(test_set[,3:5])

#dependent variable
train_y = ts(data = training_set$Churrasco,
             start = c(2016, 4),
             frequency = 365/7)
test_y = ts(data = test_set$Churrasco,
             start = c(2020, 47),
             frequency = 365/7)

#visualization
plot.ts(train_y)

#SARIMAX
library(forecast)
model = auto.arima(train_y,
                   xreg = train_exog)
summary(model)

#forecast
forecast = forecast(model, xreg = test_exog)

#accuracy
accuracy(forecast$mean, test_y)


