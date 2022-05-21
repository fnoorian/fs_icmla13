require(timeDate)
require(lubridate)
require(xts)
require(som)
library(forecast)

Sys.setenv(TZ='UTC')
setRmetricsOptions(myFinCenter="UTC")

source("utils.R")

source("elecdata.R")

#############################################
# training and testing set

train = load.max[index(load.max) < timeDate("1999-01-01"),]
test = load.max[index(load.max) >= timeDate("1999-01-01"),]  

############################################
fit.itself = function(x) x
fit.ets = function(x) fitted.values(ets(x))
fit.arima = function(x) fitted.values(auto.arima(x, max.p=14, max.q=14))
ts7 = function(x) ts(as.vector(x), frequency=7)

fit.mape = function(func, x, observed) { 
    fit = func(ts7(x))[1:length(observed)]
    print(round(mape(observed, fit), 2))
}

day.ahead.mape = function(func, train.window, test.window) {

	train.load = load.max[train.window]
	test.load = load.max[test.window]
	pred = NULL

	i = 0
	while (length(test.load) > 0) {
		obs = first(test.load)
		newpred = forecast(func(ts7(train.load)), h=1)$mean
		#print(newpred)
		pred = rbind(pred, xts(newpred, index(obs)))
		train.load = c(train.load, obs)
		test.load = test.load[-1]
		i = i+1
		print(i)
	}

	mape(obs=load.max[test.window], pred = pred)
}
##########################
# Day Ahead

train.window = "1998-12"
test.window = "1999-01"
print(paste("Day-Ahead on Period:", test.window))
a1 = day.ahead.mape(auto.arima,  train.window, test.window)
cat("ARIMA: "); print(a1)
e1 = day.ahead.mape(ets,  train.window, test.window)
cat("ETS:   "); print(e1)

train.window = "1998-11"
test.window = "1998-12"
print(paste("Day-Ahead on Period:", test.window))
a2 = day.ahead.mape(auto.arima,  train.window, test.window)
cat("ARIMA: "); print(a2)
e2 = day.ahead.mape(ets,  train.window, test.window)
cat("ETS:   "); print(e2)

train.window = "1998-10"
test.window = "1998-11"
print(paste("Day-Ahead on Period:", test.window))
a3 = day.ahead.mape(auto.arima,  train.window, test.window)
cat("ARIMA: "); print(a3)
e3 = day.ahead.mape(ets,  train.window, test.window)
cat("ETS:   "); print(e3)

dayahead.arima = round(data.frame(a1, a2, a3, mean(c(a1,a2,a3))), 2)
dayahead.ets = round(data.frame(e1, e2, e3, mean(c(e1,e2,e3))), 2)
colnames(dayahead.arima) = c("Jan", "Dec", "Nov", "Avg")
colnames(dayahead.ets) = c("Jan", "Dec", "Nov", "Avg")

print("Day Ahead")
print("ARIMA:")
print(dayahead.arima)
print("ETS:")
print(dayahead.ets)

###############
# Month-Ahead
# the lag in the training is to account for one day shift in the weekend holiday
# so do not remove it

train.window = "1997-11"
test.window = "1998-11"
print(paste("Month-Ahead on Period:", test.window))
cat("Self:  "); fit.mape(fit.itself, lag(load.max, -1)[train.window], load.max[test.window])
cat("ARIMA: "); fit.mape(fit.arima,  lag(load.max, -1)[train.window], load.max[test.window])
cat("ETS:   "); fit.mape(fit.ets,    lag(load.max, -1)[train.window], load.max[test.window])

train.window = "1997-12"
test.window = "1998-12"
print(paste("Month-Ahead on Period:", test.window))
cat("Self:  "); fit.mape(fit.itself, lag(load.max, -1)[train.window], load.max[test.window])
cat("ARIMA: "); fit.mape(fit.arima,  lag(load.max, -1)[train.window], load.max[test.window])
cat("ETS:   "); fit.mape(fit.ets,    lag(load.max, -1)[train.window], load.max[test.window])

train.window = "1998-01"
test.window = "1999-01"
print(paste("Month-Ahead on Period:", test.window))
cat("Self:  "); fit.mape(fit.itself, lag(load.max, -1)[train.window], load.max[test.window])
cat("ARIMA: "); fit.mape(fit.arima,  lag(load.max, -1)[train.window], load.max[test.window])
cat("ETS:   "); fit.mape(fit.ets,    lag(load.max, -1)[train.window], load.max[test.window])

