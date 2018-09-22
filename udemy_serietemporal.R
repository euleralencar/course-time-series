install.packages("forecast")
install.packages("ggplot2")
require(forecast)
require(ggplot2)

# Transformações

require(forecast)
require(ggplot2)

autoplot(AirPassengers)

t1 <- BoxCox(AirPassengers, lambda=0)
autoplot(t1)

t2 <- BoxCox(AirPassengers, lambda=0.1)
autoplot(t2)

lbd <- BoxCox.lambda(AirPassengers)
t3 <- BoxCox(AirPassengers, lambda=lbd)
autoplot(t3)

t4 <- diff(AirPassengers)
autoplot(t4)

t5 <- log10(AirPassengers)
autoplot(t5)

split.screen(figs=c(2,2))
screen(1)
plot(t1)
screen(2)
plot(t2)
screen(3)
plot(t3)
screen(4)
plot(t4)
close.screen(all=T)
dev.off() # ou pode usar isso


# Média Moveis

autoplot(fdeaths)

fdeaths2 = ma(fdeaths, order=5)
autoplot(fdeaths2)

fdeaths3 <- ma(fdeaths, order=12)
autoplot(fdeaths3)

fdeaths4 <- tsclean(fdeaths)
# Identify and replace outliers and 
# missing values in a time series
autoplot(fdeaths4)

plot(fdeaths)
lines(fdeaths2, col = "red")
lines(fdeaths3, col = "blue")
lines(fdeaths4, col = "green")

legend("topright", legend = c("Orig", "MA5", "MA12", "TSC"),
       col = c("black", "red", "blue", "green"), lty=1.2, cex=0.8)


# Naive com R

#gerar dados
set.seed (4312)
x = cumsum(sample(c(-1,1),101,T))
print(x)
serie = ts(x, start = c(1900), end = c(2000), frequency = 1)
print(serie)
autoplot(serie)

prev = naive(serie, h=5)
class(prev)
print(prev)
print(prev$fitted)
print(prev$residuals)

prev2 <- naive(serie, h=5, level=c(95,99))
print(prev2)
autoplot(prev2)

split.screen(figs=c(2,1))
screen(1)
plot(prev)
screen(2)
plot(prev2)
close.screen(all=T)

autoplot(AirPassengers)
prev3 = snaive(AirPassengers, h=12)
print(prev3)
autoplot(prev3)

# Verificando se o NAIVE deu certo.
prev3$mean
window(AirPassengers, start=c(1960))

#############

autoplot(fdeaths)
mean(fdeaths)
prev = meanf(fdeaths, h=5)
print(prev)
autoplot(prev)

# Vamos utilizar somentes os ultimos anos
fdeaths2 <- window(fdeaths, start=c(1976,1), end=c(1979,12))
autoplot(fdeaths2)
mean(fdeaths2)
prev2 = meanf(fdeaths2, h=5)
print(prev2)
autoplot(prev)


#Comparando
plot(prev)
lines(prev2$mean, col="red")


# Drift

autoplot(austres)
# Pega o ?ltimo valor
prev = rwf(austres, h=12, drift= F)
autoplot(prev)
# Faz a interpola??o com 1 e ultimo ponto
prev2 = rwf(austres, h=12, drift= T)
autoplot(prev2)

print(prev2)

## Outros Modelos

# Decomposicao

autoplot(AirPassengers)
prev <- stlf(AirPassengers, h =48)
print(prev)
autoplot(prev)

# Suavaizacao Exponencial

autoplot(austres)
mdl1 = holt(austres, h=16)
autoplot(mdl1)

mdl1$model

mdl2 <- holt(austres, h=16, alpha=0.2)
autoplot(mdl2)

plot(mdl1)
lines(mdl2$mean, col="red")

#damped = amortecido
mdl3 <- holt(austres, damped = T, phi=0.9, h=16)
autoplot(mdl3)

mdl4 <- holt(austres, damped = T, phi=0.8, h=16)
autoplot(mdl4)

#comparando
plot(mdl3)
lines(mdl4$mean, col = "red")

print(mdl3$mean)
print(mdl4$mean)

mdl5 <- hw(JohnsonJohnson, seasonal="additive", h=16)
autoplot(mdl5)

mdl6 <- hw(JohnsonJohnson, seasonal="multiplicative", h=16)
autoplot(mdl6)

plot(mdl5)
lines(mdl6$mean, col = "red")

#Modelo com amortecimento
mdl7 <- hw(JohnsonJohnson, seasonal="multiplicative", damped = T, phi=0.9, h=16)
autoplot(mdl7)

mdl8 <- ets(JohnsonJohnson)
print(mdl8)

prev <- forecast(mdl8, h=6, levels=c(85,90))
autoplot(prev)
print(prev$mean)

autoplot(mdl8$residuals)
autoplot(mdl8$fitted)

autoplot(decompose(JohnsonJohnson))

mdl9 <- ets(JohnsonJohnson, model="ZAA", damped=T)
print(mdl9)

mdl10 <- ets(JohnsonJohnson, model="ZZZ", damped=T)
print(mdl10)


# Auto Arima

modelo <- auto.arima(co2, trace = T)
print(modelo)
# Best model: ARIMA(1,1,1)(1,1,2)[12]     
# AIC=180.78   AICc=180.97   BIC=205.5

modelo2 <- auto.arima(co2, trace = T, stepwise=F, approximation=F)
print(modelo2)
# Best model: ARIMA(0,1,3)(0,1,1)[12]  
# AIC=176.86   AICc=177   BIC=197.47

prev1 <- forecast(modelo, h=12)
autoplot(prev1)

prev2 <- forecast(modelo2, h=12)
autoplot(prev2)

print(prev1)
print(prev2)

plot(prev1)
lines(prev2$mean, col="red")

#### Regressão

plot(Seatbelts)

x1 <- tslm(DriversKilled ~ trend, data=Seatbelts)
x2 <- tslm(DriversKilled ~ season, data=Seatbelts)
x3 <- tslm(DriversKilled ~ trend + season, data=Seatbelts)

#verifica as medidas pra escolha de modelo
CV(x1)
CV(x2)
CV(x3)
#previsão baseada na regressão
r1 <- forecast(x1, h=12)
r2 <- forecast(x2, h=12)
r3 <- forecast(x3, h=12)

autoplot(r1)
autoplot(r2)
autoplot(r3)

plot(r1)
lines(r2$mean, col="red")
lines(r3$mean, col="blue")
legend("topright", legend=c("Tend","Sazon","Tend+Sazon"),
       col=c("black","red","blue"),lty=1.2, cex=0.8)

# Rede Neural Autoregressivo

modelo <- nnetar(co2)
print(modelo)

prev = forecast(modelo, h=24)
print(prev)

autoplot(prev)

# Inclusindo uma variável aleatória

plot(Seatbelts)

autoplot(Seatbelts[,c("DriversKilled")])

cintos <- window(Seatbelts[,c("DriversKilled")], start= c(1980,1), end=c(1983,12))
print(cintos)

modelo1 <- auto.arima(cintos)
print(modelo1)

prev1 = forecast(modelo1, h=12)
print(prev1)
autoplot(prev1)

motoristas <- as.vector(window(Seatbelts[,c("drivers")], start= c(1980,1), end=c(1983,12)))

modelo2 <- auto.arima(cintos, xreg = motoristas)
print(modelo2)

motorista2 <- as.vector(window(Seatbelts[,c("drivers")], start= c(1984,1), end=c(1984,12)))
prev2 <- forecast(modelo2, xreg=motorista2)
print(prev2)
autoplot(prev2)

plot(prev1)
lines(prev2$mean, col="red")
