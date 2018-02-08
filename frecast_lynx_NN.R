

library(forecast)

# load a time-series data from 1821 to 1934
data <- lynx
str(data)
plot(data)


data <- as.data.frame(data)

# model data with a NEURAL NETWORK algorithm
(fit <- nnetar(lynx, lambda=0.5))


# simulate data for the next 20 years for 9 possible scenarios

sim <- ts(matrix(0, nrow=20, ncol=9), start=end(lynx)[1]+1)

for(i in seq(9))
  sim[,i] <- simulate(fit, nsim=20)

library(ggplot2)
autoplot(lynx) + forecast::autolayer(sim)


fcast <- forecast(fit, PI=TRUE, h=20)
autoplot(fcast)


######################################################################################
######################################################################################
######################################################################################
######################################################################################

library(timeDate)
library(forecast)
library(lattice)
library(ggplot2)
library(caret)

set.seed(1234)

T <- seq(0,100,length=100)
Y <- 10 + 2*T + rnorm(100)

fit <- nnetar(Y)

plot(forecast(fit,h=30))
points(1:length(Y),fitted(fit),type="l",col='red')


######################################################################################
######################################################################################
######################################################################################
######################################################################################

# http://kourentzes.com/forecasting/2017/02/10/forecasting-time-series-with-neural-networks-in-r/


# use TStools package for Neural Networks developed by Koutentzes

# AirPassengers

if (!require("devtools"))
install.packages("devtools")
devtools::install_github("trnnick/TStools")


library(TStools)
TS_Data <- AirPassengers

# kept the last 24 observations as a test set and will use the rest to fit the neural networks. 
# Currently there are two types of neural network available, both feed-forward: 
# (i) multilayer perceptrons (use function mlp); and extreme learning machines (use function elm).


# define test data and training data

###...DO it........


# Fit MLP

# 20 networks with 5 hidden layers (default setting)
mlp.fit <- mlp(TS_Data)
plot(mlp.fit)
print(mlp.fit)
# it is possible to change the number of neural networks and the hidden layers
mlp.fit <- mlp(TS_Data, hd = 4, reps = 15)  
plot(mlp.fit)
print(mlp.fit)

