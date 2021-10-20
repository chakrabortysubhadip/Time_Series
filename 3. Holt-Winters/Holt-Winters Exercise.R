#get the data
#install.packages("TSA")
library(TSA)
data("airmiles")

#visualizatio
plot.ts(airmiles)

#training and tes set
training_set = window(airmiles, end = c(2004,5))
test_set = window(airmiles, start = c(2004,6))

#Holt-Winters
model = HoltWinters(training_set,
                    seasonal = "multiplicative")

#forecast
library(forecast)
forecast = forecast(model, h = 12)

#accuracy
accuracy(forecast$mean, test_set)




