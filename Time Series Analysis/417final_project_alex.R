colorado_data = read.csv("C:/Users/jacma/OneDrive/school work/UMBC/Stat 417/projectFile.csv")
co2 = ts(colorado_data[,4],start=c(1976,1),frequency=12)
plot(co2,xlab='year',main='Carbon Dioxide in Colorado (Niwot Ridge)')

co2_3year = ts(co2[1:36],start=c(1976,1),frequency=12) # first 3 years
plot(co2_3year,xlab='year',main='Carbon Dioxide in Colorado (Niwot Ridge)')

co2_last10 = ts(co2[433:552],start=c(2012,1),frequency=12) # last 10 years
plot(co2_last10,xlab='year',main='Carbon Dioxide in Colorado (Niwot Ridge)')
training = ts(co2_last10[-(109:121)],start=c(2012,1), frequency=12)
test = ts(co2_last10[109:120],start=c(2021,1),frequency=12)

### Time Series Regression - decompose, model components, combine
decomp = stl(training, 'per')
season = decomp$time.series [,1]
trend = decomp$time.series [,2]
res = decomp$time.series [,3]

# Checking Residuals, Modelling, Choosing Best Model
acf(res) # should be low autocorr at lag points > 0
res_ar = ar.yw(res,order.max=NULL) # residual model
resid_ts = ts(res_ar$resid)
# acf(resid_ts[-c(1:8)], main="ACF of residuals from ar_yw fit")
res_fit1 = arima(res, order=c(8,0,0), include.mean=TRUE) 
# res_fit1$aic # 102.3028, w/ intercept
res_fit2 = arima(res, order=c(8,0,0), include.mean=FALSE) 
# res_fit2$aic # 100.3044, lower w/o intercept
# choose res_fit1
coef_fit2 = coef(res_fit2)
res_arima = arima.sim(list(ar = coef_fit2), sd=sqrt(res_fit2$sigma2), n = 108)

# Modelling Trend
time = 1:108
plot(trend)
trend_data = as.data.frame(cbind(trend,time))
trendf1 = lm(trend ~ I(time), data=trend_data) # trend model
summary(trendf1) # R^2 = 0.9963, t_hat = 393.4 + 0.208*time, VERY low p-vals
tcoeff = trendf1$coefficients
trend_mod = tcoeff[1]+ tcoeff[2]*time # very strong model for trend

# Modelling Seasonality
plot(season) # dummy variables?
season_data = as.data.frame(cbind(season,time))
szn1 = lm(season~I(4*sin(5*time)), data=season_data) # season model
lines(time, 0.007703+4*sin(5*time),col='red')
summary(szn1)
# scoeff = szn1$coefficients
# szn_mod = scoeff[1] + scoeff[2]*...

# model1 <- trend_mod + szn_mod + res_arima # combine components
# y_hat <- ts(model1, start=c(2011,1), frequency=12)
# plot(training)
# lines(y_hat, col="green") # CONSIDER SEVERAL MODELS
# test coefficients of model as well, do another regression but predict training

y = as.numeric(test)
# cat('TSR Mean Absolute Error:', sum(abs(y-yhat))/length(y),'\n')
# cat('TSR Mean Squared Error:', sum((y-yhat)^2)/length(y),'\n')
# cat('TSR Mean Percent Error:', 100*(sum((y-yhat)/y)/n),'\n')

# predict(y_hat, newdata=test, interval="prediction")

### Exponential Smoothing - HoltWinters
c_forecasts1 = HoltWinters(training, beta=FALSE, gamma=FALSE) # level only
# c_forecasts1 # alpha close to 1, only current time point
# plot(c_forecasts1) # good but shifted right?
c_forecasts2 = HoltWinters(training)
# c_forecasts2 # alpha close to 1, beta = 0 gamma = 1,
# plot(c_forecasts2)

library(forecast)
exp_smo_future1 = forecast(c_forecasts1,h=12) # predict 2021, compare to actual
# plot(exp_smo_future1, main='Level Only') 
exp_smo_future2 = forecast(c_forecasts2,h=12) # print/plot for forecasts
# plot(exp_smo_future2, main='Level and Seasonality') # MUCH better

# co2_Resid <- ts(c_forecasts2$residuals, start=c(2011)) # $residuals is NULL?
# ssResid <- co2_Resid[-(1:12)] # remove empty elements
# acf(ssResid, lag.max=20)
# Box.test(ssResid, lag=20, type="Ljung-Box") # box test, want high p-value

# cat('Exp. Smoothing Mean Absolute Error:', sum(abs(y-yhat))/length(y),'\n')
# cat('Exp. Smoothing Mean Squared Error:', sum((y-yhat)^2)/length(y),'\n')
# cat('Exp. Smoothing Mean Percent Error:', 100*(sum((y-yhat)/y)/n),'\n')

###  ARIMA - auto.arima, acf and pacf. ARIMA(p,d,q)
# plot(co2_last10) # not stationary in mean
co2diff1 = diff(co2_last10,differences=1) # d = 1
# plot.ts(co2diff1) # stationary in mean and variance, not dependent on t
# acf(co2diff1,lag.max=20) # plot=FALSE for actual values
# pacf(co2diff1,lag.max=20)
auto.arima(training) # weird output
co2_arima = arima(x=training,order=c(2,1,0)) 
arima_forecast = forecast(co2_arima,h=12) # forecast 2021, compare to actual
# plot(arima_forecast)

acf(arima_forecast$residuals,lag.max=20) # check residuals of ARIMA model
Box.test(arima_forecast$residuals, lag=20, type="Ljung-Box") # box test
# VERY NOT GOOD, EXTREMELY LOW P-VALUE AND LOTS OF AUTOCORRELATION

# *****yhat will be replaced with forecast values*****
# cat('ARIMA Smoothing Mean Absolute Error:', sum(abs(y-yhat))/length(y),'\n')
# cat('ARIMA Smoothing Mean Squared Error:', sum((y-yhat)^2)/length(y),'\n')
# cat('ARIMA Smoothing Mean Percent Error:', 100*(sum((y-yhat)/y)/n),'\n')