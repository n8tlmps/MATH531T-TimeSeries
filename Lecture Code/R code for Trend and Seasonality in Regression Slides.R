Whatever is after the symbol # is a comment

data(AirPassengers) # Calling the built-in R data

plot(AirPassengers) # plotting the time-series data. Notice that the data is already in the form of a time series. To check this, type AirPassengers and hit enter.
plot(log(AirPassengers))  # Plotting the log-transform of data

tim <- time(AirPassengers) # Extracting time as the explanatory variate from the time series framework of data
cycle(AirPassengers) # Introducing month as the season
month <- as.factor(cycle(AirPassengers)) # Introducing month as the season
reg1 <- lm(log(AirPassengers)~tim+month) # Fitting the model reg1 with linear trend and seasonal effect
summary(reg1) # Details of the fit for model reg1

plot(log(AirPassengers))
points(tim,predict.lm(reg1),type='l',col='red') # superimpose the fit of model reg1 on the plot of the data
# Exponentiating the fitted values to reverse the log transformation
plot(AirPassengers)
points(tim,exp(predict.lm(reg1)),type='l',col='red') # superimpose the fit of model reg1 on the plot of the data

# Diagnostic plots for reg1 model
par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(reg1$fitted, reg1$residuals) # plot of fitted values vs residuals
qqnorm(reg1$residuals) #qq-plot of residuals
qqline(reg1$residuals) # plotting the line, along which the dots in qq-plot should lie
plot(reg1$residuals) # plotting the residuals vs time
abline(h=0,lty=2) # plotting a horizontal line at 0
acf(reg1$residuals) #sample acf plot of residuals

tim2 <- tim^2
reg2 <- lm(log(AirPassengers)~tim+tim2+month)
summary(reg2) # Details of the fit for model reg2

par(mfrow=c(1,1))
plot(log(AirPassengers))
points(tim,predict.lm(reg2),type='l',col='red') # superimpose the fit of model reg2 on the plot of the data

# Exponentiating the fitted values to reverse the log transformation
plot(AirPassengers)
points(tim,exp(predict.lm(reg2)),type='l',col='red') # superimpose the fit of model reg2 on the plot of the data


# Diagnostic plots for reg2 model
par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(reg2$fitted, reg2$residuals) # plot of fitted values vs residuals
qqnorm(reg2$residuals) #qq-plot of residuals
qqline(reg2$residuals) # plotting the line, along which the dots in qq-plot should lie
plot(reg2$residuals) # plotting the residuals vs time
abline(h=0,lty=2) # plotting a horizontal line at 0
acf(reg2$residuals) #sample acf plot of residuals


data(AirPassengers) # Calling the built-in R data
tim <- time(AirPassengers) # Extracting time as the explanatory variate from the time series framework of data
month <- as.factor(cycle(AirPassengers)) # Introducing month as the season
reg1 <- lm(log(AirPassengers)~tim+month) # Fitting the model reg1 with linear trend and seasonal effect
summary(reg1) # Details of the fit for model reg1

tim.new <- seq(1960,1960+5/12,length=6)
month.new <- factor(1:6)
data.new <- data.frame(tim=tim.new,month=month.new)
predict.lm(reg1,data.new,interval='prediction')
