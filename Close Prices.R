set.seed(1111)
library(astsa)

Price = read.csv("LRLCY_1.csv", header = T)
Price = ts(Price, start = c(2007, 5), end = c(2023, 4), frequency = 12)
tsplot(Price, main = "L'Oréal Close Price")
tsplot(diff(Price,2), main = "L'Oréal Close Price")

acf(Price, main = "Sample ACF")
pacf(Price, main = "Sample PACF")

#training and testing data
Price = diff(Price, 2)
n = length(Price)
train = (Price)[1:(n * .95)]
test = (Price)[181 : n]

acf(train, main = "Sample ACF")
pacf(train, main = "Sample PACF")

#model selection
xhat <- train
val <- matrix ( NA , 10 , 10)
val1 <- matrix ( NA , 10 , 10)
val2 <- matrix ( NA , 10 , 10)
for ( i in 0:9)
{
  for ( j in 0:9)
  {
    out = sarima ( xhat , p =i , d = 0 , q = j )
    val [ i +1 , j +1] = out$AIC
    val1 [ i +1 , j +1] = out$AICc
    val2 [ i +1 , j +1] = out$BIC
  }
}
which ( val == min ( val ) , arr.ind = TRUE )
which ( val1 == min ( val1 ) , arr.ind = TRUE )
which ( val2 == min ( val2 ) , arr.ind = TRUE )


sarima.for(train, n.ahead = 10, p = 2, d = 0, q = 8)
sarima.for(train, n.ahead = 10, p = 1, d = 0, q = 1)

fit1 = arima(train, c(2,0,8))
fore1 = predict(fit1, n.ahead = 10)

fit2 = arima(train, c(1,0,1))
fore2 = predict(fit2, n.ahead = 10)

#Forecast errors
sum1 = 0
sum2 = 0

for (i in 1:10) {
  sum1 = sum1 + ((fore1$pred[i] - test[i])^2)
  sum2 = sum2 + ((fore2$pred[i] - test[i])^2)
}

#Power Spectrum Estimate
mvspec(Price, spans = 5, col=rgb(.05,.6,.75), lwd=2)
mvspec(Price, spans = 10, col=rgb(.05,.6,.75), lwd=2)
mvspec(Price, spans = 20, col=rgb(.05,.6,.75), lwd=2)
mvspec(Price, spans = 40, col=rgb(.05,.6,.75), lwd=2)


