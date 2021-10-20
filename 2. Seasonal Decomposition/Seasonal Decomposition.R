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

#tranforming the Y variables into Timeseries
library(lubridate)
dataset$y <- ts(dataset$y, 
                frequency = 365,
                start =c(2013, yday(head(dataset$Date, 1))))
#tranforming the Y variables into Timeseries
future$y <- ts(future$y, 
               frequency = 365,
               start = c(2020, yday(head(future$Date, 1))))

#visualization
plot.ts(dataset$y,
        ylab = "Demand")

##################################################################

#decomposition
decomposition = decompose(dataset$y, type = "additive")
plot(decomposition)

#other seasonality plot
library(ggplot2)
ggseasonplot(dataset$y)
