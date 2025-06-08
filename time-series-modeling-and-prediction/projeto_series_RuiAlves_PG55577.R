#Trabalho Series Temporais

#base de dados trabalho
#Número mensal de mortes acidentais nos EUA (1973–1978).

#grafico base
data("USAccDeaths")
USAccDeaths
(USAccDeaths)
?USAccDeaths
Y <- ts(USAccDeaths, start = 1973, frequency = 12)
t <- 1:length(Y)
plot(t, Y, xlab="Meses", ylab = "Mortes", type = "l")

# Carregar pacotes
library(ggplot2)
library(scales)

# Dataset embutido
dados <- USAccDeaths

# Criar índice de datas (mensal de Jan 1973 a Dez 1978)
indice <- seq(as.Date("1973-01-01"), as.Date("1978-12-01"), by = "month")

# Criar data.frame com datas e observações
data <- data.frame(
  day = as.POSIXct(indice),
  obs = as.numeric(dados)
)

# Plot com ggplot2
p <- ggplot(data, aes(x = day, y = obs)) +
  theme_bw() +
  geom_line(color = "steelblue", size = 1) +
  xlab("Data") +
  ylab("Número de Mortes") +
  ggtitle("Mortes Mensais por Acidentes nos EUA (1973–1978)") +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(limits = c(min(data$obs) - 500, max(data$obs) + 500)) +
  scale_x_datetime(
    breaks = "3 months",
    labels = date_format("%b-%Y"),
    expand = c(0, 0),
    limits = c(min(data$day), max(data$day))
  )

# Mostrar o gráfico
print(p)

#Outliers
boxplot(USAccDeaths)
boxplot(USAccDeaths)$out
which(USAccDeaths==11317)
data[7,]#data do outlier - Julho (7) 1973


#Treino/Teste
length(USAccDeaths) #72 Valores => Temos de deixar um de cada mes (1 ano) no teste (12 - obs)

#Componentes da Series
plot(decompose(USAccDeaths)$x) #serie original
plot(decompose(USAccDeaths)$trend) #tendencia
plot(decompose(USAccDeaths)$random) #parte aleatoria
plot(decompose(USAccDeaths)$seasonal) #parte sazonal
plot(decompose(USAccDeaths))
plot(decompose(USAccDeaths, type = "multiplicative"))
decompose(USAccDeaths)
?decompose


# modelo aditivo (parece o mais adequado)
decompose(USAccDeaths)
plot(decompose(USAccDeaths)) #fazer antes e depois do treino!

################### estabilizacao da variancia (transformacao de Box-Cox) => ANTES DE DIVIDIR A SERIE

library(forecast)
lambda=BoxCox.lambda(USAccDeaths,lower=-1,method="loglik")
lambda
# lambda = -0.65

lambdatotal=BoxCox(USAccDeaths,lambda)
plot(lambdatotal,main = "Mortes por Acidentes nos EUA com transformação de Box-Cox")
#ATENCAO AO VALOR DO LAMBDA


# decide-se trabalhar com lambda = -0.65

par(mfrow=c(1,2))
acf(lambdatotal)
pacf(lambdatotal)

# ou usanda a library "astsa" tem um comando que gera logo as duas 
#acf (FAC em Portugues) e pacf (FACP em Portugues)

library(astsa)

acf2(lambdatotal, main = "FAC e FACP da série transformada")


#serie1 corresponde à serie para modelacao/treino e estimacao
# serie2 corresponde à serie para confirmacao ou teste e previsão

n = length(lambdatotal)
n #length é 72
# Série de treino: Jan 1973 a Dez 1977 (60 observações)
serie1 <- window(lambdatotal, end = c(1977, 12))

# Série de teste: Jan 1978 a Dez 1978 (12 observações)
serie2 <- window(lambdatotal, start = c(1978, 1))

n1 = length(serie1)
n1
n2 <- length(serie2) #ideal seria deixar 2 anos (<20%)
n2
# Visualização (opcional)
plot(serie1, main = "Série de Treino (Box-Cox)")
plot(serie2, main = "Série de Teste (Box-Cox)")

# Gráfico das séries de treino e teste
plot(serie1, ylab = expression(paste("Box-Cox(Y"[t], ")")), 
     xlab = "tempo (em meses)", xlim = c(1973, 1979.2), main = "")
lines(serie2, lty = 2)
legend(x = 1978.2, y = max(lambdatotal), bty = "n",
       legend = c("Série de treino (Box-Cox)", "Série de teste (Box-Cox)"),
       lty = c(1, 2), col = c("black", "black"))

# ACF e PACF da série de treino
acf(serie1, main = "", ylab = "FAC", xlab = "lag")
pacf(serie1, main = "", ylab = "FACP", xlab = "lag")


############# teste de estacionariedade (raiz unit?ria) => ESQUECER - SÓ VER ESTACIOANRIDADE NO FINAL SÓ PARA VER PREVISÕES
#ver Slides Parte III "When to choose ADF or KPSS test?"
## teste ADF
# H0: Existe pelo menos uma raiz dentro do c?rculo unit?rio (a Série não é estacionária)
# H1: não existem ra?zes dentro do c?rculo unit?rio (a Série é estacionária)

par(mfrow=c(1,2));plot(serie1);acf(serie1)

## teste ADF (H0: TS não estacionária)

library(tseries)
p = trunc(12 * (n1/100)^(1/4))
p
adf.test(serie1,k=p)


## teste KPSS (H0: TS estacionária)
library(urca)
kpss.test(serie1)


# conclus?o: não é estacionária na média, a trend tem de ser removida

#############

# modelo automatico proposto pelo comando auto.arima -> ARIMA(0,1,1)(0,1,1)[12] 
#este modelo dado pelo comando auto.arima não tem constante/drift

library(forecast)
auto.arima(serie1)#para saber se sao significativos - estimativa/SE tem de ser maior que 2
summary(auto.arima(serie1)) #para ver RMSE, AIC e BIC
#ou
auto.arima(serie1,allowdrift =F)
library(lmtest)
coeftest(auto.arima(serie1)) #ver se sao significativos
#calcular rmse (queremos inferiror a 0.05)
sqrt((1/n1)*sum(auto.arima(serie1)$residuals^2))
accuracy(auto.arima(serie1))
summary(auto.arima(serie1))

#dificilmente se considera o modelo proposto por este ultimo comando
#é só "exploratorio"


############# ordem da diferenciação d=? (estabilizar tendência)
#dá sugestao de quantas diferenciacoes regular e sazonal
library(forecast)
#regular
?ndiffs
ndiffs(serie1)   # d=0 => autoarima sugeria diferenciação regular de grau 1, pelo plot parece que ainda não é estacionaria na media
#sazonal
nsdiffs(serie1,m=12)    # D=1

# para aplicar a diferenciacao regular (tirar a tendencia) => Estocástica
diff(serie1) #direto - obtemos outra serie logo
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(diff(serie1), main = "Série de treino com diferenciação regular de grau 1 ")
acf(diff(serie1), main = "FAC")
pacf(diff(serie1), main = "FACP")

# ou => prof prefere essa 
?arima
dif = arima(serie1, order=c(0,1,0)) #criar um modelo de series temporais
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(dif$residuals, , ylab="Box-Cox(USAccDeaths)", xlab = "tempo (em meses)", main="") #nova serie vai ser os residuos do modelo arima
acf(dif$residuals, main = "FAC(d=1)")
pacf(dif$residuals, main = "FACP(d=1)") #nao esta bom, autocorrelacao temporal forte



## teste ADF (H0: não estacionária; rejeitar se ET < VC)
library(tseries)
p = trunc(12 * (n1/100)^(1/4)); p
testeADF=adf.test(dif$residuals,k=p)
testeADF


## teste KPSS (H0: TS estacionária)
library(urca)
testeKPSS=kpss.test(dif$residuals)
testeKPSS
adf1=ur.df(dif$residuals,type="none",lags=1)
summary(adf1)

# conclusão: apos diferenciação de 1 ordem, a Série já é estacionária

### teste PP ???
library(urca)
pp = ur.pp(dif$residuals)
summary(pp)


# definimos d=1 (regular)
dif1 = dif$residuals



############## sazonalidade

### determinar s

# através da FAC
acf(dif1)
acf(serie1)
# s = 12

# através do periodograma ?? - o valor não faz sentido!!!
periodogram = spectrum(as.vector(serie1), plot=F)
imax = which.max(periodogram$spec)
periodo = 1/periodogram$freq[imax]
periodo

# consideramos s=12

#Pag 5 Documento
### determinar D, P e Q

seas1 = arima(serie1, order = c(0,1,0), seasonal = list(order = c(1,0,0), period = 12)) #P = 1
seas1
#qnorm(0.975) aproximo a 2 na prática


# o coeficiente é significativamente diferente do parametro=P=1 que estou a testar que equivale
#a testar o parametro que deu 0.8250?
seas1$coef[1] + c(-1,1) * qnorm(0.975) * sqrt(seas1$var.coef[1,1])# = (0.7046093; 0.9453183)
#Sim. O valor 0 está fora do intervalo de confiança (0.7046 ; 0.9453), portanto:
#sar1=0.8250 e logo P=1 é significativo

## varios modelos:    -> escolhe-se P=1, D=0, Q=1

s1 = arima(serie1, order = c(0,1,0), seasonal = list(order = c(1,0,0), period = 12))
summary(s1) #summary ja da as medidas abaixo
MSE1 = sum(s1$residuals^2)/length(s1$residuals); MSE1
RMSE1=sqrt(MSE1)
RMSE1
BIC(s1)
library(qpcR)
AIC(s1)


#NAO!s2 = arima(serie1, order = c(0,1,0), seasonal = list(order = c(0,1,1), period = 12))# => nao se coloca D = 1, seguir metodologia Cap.5
#summary(s2)
#MSE2 = sum(s2$residuals^2)/length(s2$residuals); MSE2

s3 = arima(serie1, order = c(0,1,0), seasonal = list(order = c(1,0,1), period = 12))
summary(s3)
MSE3 = sum(s3$residuals^2)/length(s3$residuals); MSE3
RMSE3=sqrt(MSE3)
RMSE3
BIC(s3)
AICc(s3)

#nãos4 = arima(serie1, order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))
#summary(s4)
#MSE4 = sum(s4$residuals^2)/length(s4$residuals); MSE4

# o melhor é SARIMA(0,1,0)(1,0,1 [12])
seas2 = arima(serie1, order = c(0,1,0), seasonal = list(order = c(1,0,1), period = 12))#igual ao que tinhamos => pois tinha menor AIC
summary(seas2)
coeftest(seas2)#dois coefs significativos

# o coeficiente é significativamente diferente de 0?
seas2$coef[1] + c(-1,1) * qnorm(0.975) * sqrt(seas2$var.coef[1,1])# = 0.9221941 1.0338898   -> sim


#aqui
###################### determinar p e q

# já temos: d=1, s=12, P=1, D=0, Q=1.

m0 = arima(serie1, order = c(0,1,0), seasonal = list(order = c(1,0,1), period = 12))
summary(m0)
AIC(m0)

res.m0 = m0$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m0, main="resíduos SARIMA(0,1,0)(1,0,1)12")
acf(res.m0, main="FAC resíduos")
pacf(res.m0, main="FACP resíduos")
# identifica-se um MA(1)

layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m0, ylab="BOXCox(USAccDeaths)", xlab = "tempo (em meses)", main="")
acf(res.m0, main = "", ylab = "FAC", xlab="lag")
pacf(res.m0, main = "", ylab = "FACP", xlab="lag")




## p=1, q=0

m1 = arima(serie1, order = c(1,1,0), seasonal = list(order = c(0,1,1), period = 12))#nao era preciso
summary(m1)
MSE1 = sum(m1$residuals^2)/length(m1$residuals); MSE1
# MSE = 0.000467962
AIC(m1)
coeftest(m1)

res.m1 = m1$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m1)
acf(res.m1)
pacf(res.m1)


## p=2, q=0

m2 = arima(serie1, order = c(2,1,0), seasonal = list(order = c(0,1,1), period = 12))
summary(m2)
MSE2 = sum(m2$residuals^2)/length(m2$residuals); MSE2
# MSE = 0.0004197023
AIC(m2)
coeftest(m2)

res.m2 = m2$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m2)
acf(res.m2)
pacf(res.m2)


## p=1, q=1

m3 = arima(serie1, order = c(1,1,1), seasonal = list(order = c(0,1,1), period = 12))
summary(m3)
MSE3 = sum(m3$residuals^2)/length(m3$residuals); MSE3
# MSE = 0.0004295717
AIC(m3)
coeftest(m3)

res.m3 = m3$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m3)
acf(res.m3)
pacf(res.m3)


## p=2, q=1

m4 = arima(serie1, order = c(2,1,1), seasonal = list(order = c(0,1,1), period = 12))
summary(m4)
MSE4 = sum(m4$residuals^2)/length(m4$residuals); MSE4
# MSE = 0.0004195523
# ma1 não é significativo
AIC(m4)
coeftest(m4)

## p=3, q=0

m5 = arima(serie1, order = c(3,1,0), seasonal = list(order = c(0,1,1), period = 12))
summary(m5)
MSE5 = sum(m5$residuals^2)/length(m5$residuals); MSE5
# MSE = 0.0004195888
# ar3 não ? significativo
AIC(m5)
coeftest(m5)

## p=2, q=0, P=1, Q=1

m6 = arima(serie1, order = c(2,1,0), seasonal = list(order = c(1,1,1), period = 12))
summary(m6)
MSE6 = sum(m6$residuals^2)/length(m6$residuals); MSE6
# MSE = 0.0004195888
# sar1 não ? significativo
AIC(m6)
coeftest(m6)

## p=2, q=0, P=1, Q=1, D=0

m7 = arima(serie1, order = c(2,1,0), seasonal = list(order = c(1,0,1), period = 12))
summary(m7)
MSE7 = sum(m7$residuals^2)/length(m7$residuals); MSE7
# MSE = 0.0004440538
AIC(m7)
coeftest(m7)

## p=2, q=0, P=0, Q=2, D=1

m8 = arima(serie1, order = c(2,1,0), seasonal = list(order = c(0,1,2), period = 12))
summary(m8)
MSE8 = sum(m8$residuals^2)/length(m8$residuals); MSE8
# MSE = 0.0004192859
AIC(m8)
coeftest(m8)

## p=0, q=1, P=0, Q=1, D=1

m9 = arima(serie1, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
summary(m9)
MSE9 = sum(m8$residuals^2)/length(m8$residuals); MSE8
# MSE = 0.0004398617
AIC(m9)
coeftest(m9)


res.m9 = m9$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m9)
acf(res.m9)
pacf(res.m9)


## p=0, q=2, P=0, Q=1, D=1

m10 = arima(serie1, order = c(0,1,2), seasonal = list(order = c(0,1,1), period = 12))
summary(m10)
MSE10 = sum(m10$residuals^2)/length(m10$residuals); MSE10
# MSE = 0.0004259021
AIC(m10)
coeftest(m10)

m10$coef
sqrt(m10$var)

res.m10 = m10$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m10)
acf(res.m10)
pacf(res.m10)


## p=1, q=2, P=0, Q=1, D=1

m11 = arima(serie1, order = c(1,1,2), seasonal = list(order = c(0,1,1), period = 12))
summary(m11)

coeftest(m11)
m8$coef
sqrt(m8$var)

res.m11 = m11$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m11)
acf(res.m11)
pacf(res.m11)


## p=1, q=1, P=1, Q=1, D=0

m12 = arima(serie1, order = c(1,1,1), seasonal = list(order = c(1,0,1), period = 12))
summary(m12)
coeftest(m12)

m12$coef
sqrt(m12$var)

res.m12 = m12$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m12)
acf(res.m12)
pacf(res.m12)

m13 = arima(serie1, order = c(1,0,0), seasonal = list(order = c(1,0,1), period = 12))
summary(m13)
coeftest(m13)

res.m13 = m13$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m13)
acf(res.m13)
pacf(res.m13)

m14 = arima(serie1, order = c(0,1,0), seasonal = list(order = c(1,0,1), period = 12))
summary(m14)
coeftest(m14)

res.m14 = m14$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m14)
acf(res.m14)
pacf(res.m14)

m15 = arima(serie1, order = c(1,1,0), seasonal = list(order = c(1,0,1), period = 12))
summary(m15)
coeftest(m15)

res.m15 = m15$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m15)
acf(res.m15)
pacf(res.m15)

m16 = arima(serie1, order = c(0,1,1), seasonal = list(order = c(1,0,1), period = 12))
summary(m16)
coeftest(m16)

res.m16 = m16$residuals
layout(matrix(c(1,1,2,3), 2, byrow=T))
plot(res.m16)
acf(res.m16)
pacf(res.m16)



### escolhemos: p=0, d=1, q=1, P=1, D=0, Q=1, s=12

modelo = arima(serie1, order = c(0,1,1), seasonal = list(order = c(1,0,1), period = 12))
summary(modelo)
library(lmtest)
coeftest(modelo)
MSE = sum(modelo$residuals^2)/length(modelo$residuals); MSE
AIC(modelo) #=-878.767
# MSE = 0.00000005049707


## modelo alternativo (considero este o final):
#modelo = arima(serie1, order = c(0,1,2), seasonal = list(order = c(1,0,1), period = 12))

library(lmtest)
coeftest(modelo) #todos sognificativos
#IC para os coef a 95%
modelo$coef[1] + c(-1,1) * qnorm(0.975) * sqrt(modelo$var.coef[1,1])
modelo$coef[2] + c(-1,1) * qnorm(0.975) * sqrt(modelo$var.coef[2,2])
modelo$coef[3] + c(-1,1) * qnorm(0.975) * sqrt(modelo$var.coef[3,3])

raizes = polyroot(c(1, modelo$coef[1]))  #ma
abs(raizes)#2.61653 - invertivel
raizes2 = polyroot(c(1, -modelo$coef[2]))  #sar
abs(raizes2)#1.018842 - estacionario
raizes3 = polyroot(c(1, modelo$coef[3]))  #sma
abs(raizes3)#1.735311 - invertivel
# todos em modulo maiores do que 1 => estacionaria e invertivel

#teste que a prof usa para estacionaridade
library(tseries)
testeKPSS=kpss.test(modelo$residuals)
testeKPSS #p_value = 0.1 => H0: É Estacionaria => confirma-se

#teste ADF
library(tseries)
p = trunc(12 * (n1/100)^(1/4)); p
testeADF=adf.test(modelo$residuals,k=p)
testeADF #é igual (10=10), confirma-se estacionaridade, p_value = 0.08 > 0.05, nao rejeito H0:É Não estacionária 

#Modelo final quer com Yt e com operadores atraso!
########### análise de resíduos (ruído branco (gaussiano de média 0)?)

res = modelo$residuals

# gráficos
layout(matrix(c(1,2,3,4),2,byrow=T))
plot(res, main="Representação gráfica dos resíduos", ylab="") #analisar o mes em que residuo maior acontece => será que ocorreu, ser crítico
hist(res, freq=F, main="Histograma dos resíduos", xlab="resíduos", ylab="frequ?ncia absoluta")
curve(dnorm(x, mean(res), sd(res)), add=T)
acf(res, main="FAC dos resíduos", ylab="FAC")
pacf(res, main="FACP dos resíduos", ylab="FACP")



# teste da independencia
h = 2 * 12; h > n1/5
k = 3    # n? par?metros
?Box.test()
Box.test(res, lag=h, fitdf=k)
Box.test(res, lag=h, fitdf=k, type="Lj")#p_value ~ 0.9986 => Não rejeito H0: p1 = p2 = pk = 0 (erros não são correlacionados)

for (i in (k+1):36) print(c(i, round(Box.test(res, lag=i, fitdf=k, type="Lj")$p.value,4)))

# testes de normalidade dos erros
shapiro.test(res)
ks.test(res, "pnorm", mean=mean(res), sd=sd(res)) #comparar com pnorm com media e sd da amostra => prefere este 


# teste ao valor médio 
t.test(res, mu=0) #confirma-se que media dos erros é 0


library(astsa)
sarima(serie1, p=0, d=1, q=1, P=1, D=0, Q=1, S=12, no.constant=T) #resumo do modelo
sarima(serie1, p=0, d=1, q=2, P=1, D=0, Q=1, S=12, no.constant=T)
sarima(serie1, p=2, d=1, q=1, P=1, D=0, Q=1, S=12, no.constant=T)
sarima(serie1, p=1, d=1, q=1, P=1, D=0, Q=1, S=12, no.constant=T)
sarima(serie1, p=0, d=1, q=1, P=1, D=0, Q=1, S=12, no.constant=T)
sarima(serie1, p=0, d=1, q=2, P=1, D=0, Q=1, S=12, no.constant=T)
sarima(serie1, p=0, d=1, q=3, P=1, D=0, Q=1, S=12, no.constant=T)´

?checkresiduals
checkresiduals(modelo)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

res <- residuals(modelo, type = "normalized")
std_res <- scale(res)

ts.plot(std_res, main = "Standardized Residuals", ylab = "", xlab = "Time")

acf(std_res, main = "ACF of Residuals")

qqnorm(std_res, main = "Normal Q-Q Plot of Std Residuals")
qqline(std_res, col = "blue")

pvals <- sapply(1:36, function(lag) Box.test(std_res, lag = lag, type = "Ljung-Box", fitdf = 3)$p.value)
plot(1:36, pvals, type = "p", ylim = c(0, 1), main = "p values for Ljung-Box statistic",
     xlab = "lag", ylab = "p value")
abline(h = 0.05, lty = 2, col = "blue")


##############  previsões

## observados vs estimados (log)
library(forecast)
est.boxcox = fitted.values(modelo)
res.boxcox = serie1 - est.boxcox
#res.log = modelo$residuals
ts.plot(serie1, main = "Acidentes nos EUA com BoxCox")
lines(ts(est.boxcox, start=c(1973,1), frequency=12), col="red") #observados vs estimados => FINAL!!!!

library(forecast)

# Série original (não transformada)
USAcc.mod <- window(USAccDeaths, end = c(1977,12))  # mesma janela que serie1

# Inverter a transformação Box-Cox
est <- InvBoxCox(est.boxcox, lambda)  # lambda = -0.65 (voltar aos valores originais)

# Resíduos no domínio original
res <- USAcc.mod - est

# Gráfico: observados vs estimados
ts.plot(USAcc.mod, main = "Acidentes nos EUA (original)", ylab = "N.º de mortes")
lines(ts(est, start = start(USAcc.mod), frequency = frequency(USAcc.mod)), col = "red")
legend("topleft", legend = c("Observado", "Estimado"), col = c("black", "red"), lty = 1)

## previsões
# Previsão no domínio transformado (Box-Cox)
boxcox.prev = predict(modelo, n.ahead=12)#12 valores de teste
boxcox.prev$pred
boxcox.prev$se

# Série real de teste (original, não transformada)
USAcc.prev <- window(USAccDeaths, start = c(1978, 1))  # 12 meses seguintes

# Reverter transformação Box-Cox
prev <- InvBoxCox(boxcox.prev$pred, lambda); prev

# Erro da previsão (domínio original)
erro.prev <- USAcc.prev - prev; erro.prev

a <- forecast(modelo, h = 12, level = 95)
l.inf <- InvBoxCox(a$lower, lambda)
l.sup <- InvBoxCox(a$upper, lambda)
prev  <- InvBoxCox(a$mean, lambda)
fit   <- InvBoxCox(a$fitted, lambda)
USAcc.mod <- window(USAccDeaths, end = c(1977, 12))
res <- USAcc.mod - fit

## gráficos previsões

ts.plot(lambdatotal, main = "Box-Cox da Série de Acidentes EUA (Treino/Teste)", ylab = "Box-Cox(USAccDeaths)")
lines(ts(est.boxcox, start = start(lambdatotal), frequency = frequency(lambdatotal)), col = "red")
lines(ts(boxcox.prev$pred, start = c(1978, 1), frequency = 12), col = "blue")

ts.plot(USAccDeaths, main = "Mortes por Acidentes nos EUA (Treino/Teste)", xlab="Meses (1973-1978)", ylab = "Mortes")
lines(ts(est, start=c(1973,1), frequency=12), col="red")
lines(ts(prev, start = c(1978, 1), frequency=12), col="blue")
#legend(locator(1), legend = c("Observed", "Estimated", "Forecasted"), lty = c(1,1,1), col = c("black", "red", "blue"))

# Série observada no período de teste (1978)
USAcc.prev <- window(USAccDeaths, start = c(1978,1))

# Gráfico: Observado, Estimado (até 1977), e Previsão (1978)
ts.plot(USAccDeaths, 
        main = "Acidentes mensais nos EUA (Treino/Teste) com Intervalos de Confiança a 95%", 
        ylab = "N.º de mortes por acidente")

lines(ts(est, start = c(1973,1), frequency = 12), col = "red")    # estimado (1973–1977)
lines(ts(prev, start = c(1978,1), frequency = 12), col = "blue")  # previsto (1978)

#legend(locator(1), 
 #      legend = c("Observado", "Estimado (Treino)", "Previsto (Teste)"), 
  #     lty = c(1,1,1), 
   #    col = c("black", "red", "blue"))
inf.ts = ts(l.inf, start=c(1978,1), frequency=12)
sup.ts = ts(l.sup, start=c(1978,1), frequency=12)
lines(inf.ts, col="blue", lty=2)
lines(sup.ts, col="blue", lty=2)

## representação gráfica dos intervalos de previsão


prev_graf = ts(c(est[204],prev), start=c(2016,12), frequency=12)
serie_graf14 = ts(dados[169:218,7], start=c(2014,1), frequency=12)
est_graf14 = ts(est[169:204], start=c(2014,1), frequency=12)

plot(serie_graf14, main = "", ylab="TOVT (2015=100)", ylim = c(75,145), xlab="tempo (em meses)", axes = FALSE);box()
axis(side=1,c(2014,2015,2016,2017,2018))
axis(side=2,c(80,90,100,110,120,130,140))
lines(est_graf14, col="red")
lines(prev_graf, col="blue")

legend(x = 2013.95, y = 144, legend = c("Série observada", "Estimativas", "Previsões", "Intervalos de Previsão (95%)"), col = c("black", "red", "blue", "blue"), lty = c(1,1,1,2), bty="n")


###### medidas de avaliaçãoo - Da serie de teste


## erro quadrático médio (mean squared error - MSE)

library(Metrics)
mse(USAcc.prev, prev) #96513.94


#ou
erro = USAcc.prev-prev
eqm = sum(erro^2)/length(erro); eqm


## raiz do erro quadrático médio (RMSE)

library(Metrics)
rmse(USAcc.prev, prev)#310.67

#ou
erro = USAcc.prev-prev
reqm = sqrt(sum(erro^2)/length(erro)); reqm


## erro percentual absoluto médio (mean absolute percentage error - MAPE)

library(Metrics)
mape(USAcc.prev, prev)#0.029

#ou
erro = USAcc.prev-prev
epam = 100* sum(abs(erro/USAcc.prev))/length(USAcc.prev); epam


## erro escalado absoluto médio (MASE)

library(Metrics)
mase(USAcc.prev, prev, step_size = 12)

#ou
eeam = function(obs, prev, s){
  n = length(obs)
  soma1 = 0
  for (i in (s+1):n){soma1 = soma1 + abs(obs[i]-obs[i-s])}
  eamns = soma1/(n-s)
  soma2 = 0
  for (i in 1:n){soma2 = soma2 + abs((obs[i]-prev[i])/eamns)}
  eeam = soma2/n
  return(eeam)}
eeam(USAcc.prev, prev, s=12)


## U-Theil 

library(forecast)
accuracy(f = prev, x = USAcc.prev)

#ou
Ustat = function(obs, prev){
  n = length(obs)
  soma1 = 0
  for (i in 1:(n-1)){soma1 = soma1 + ((prev[i+1]-obs[i+1])/obs[i])^2}
  soma2 = 0
  for (i in 1:(n-1)){soma2 = soma2 + ((obs[i+1]-obs[i])/obs[i])^2}
  Ustat = sqrt(soma1/soma2)
  return(Ustat)}
Ustat(USAcc.prev, prev)






####  CALCULAR AS MEDIDAS DE AVALIAÇÃO PARA A Série DE ESTIMAÇÃO


## erro quadr?tico médio (mean squared error - MSE)

library(Metrics)
mse(USAcc.mod, est)#309669.4

#ou
resid = USAcc.mod - est
eqm = sum(resid^2)/length(resid); eqm


## raiz do erro quadr?tico médio (RMSE)

library(Metrics)
rmse(USAcc.mod, est)# 556.4795

reqm = sqrt(sum(resid^2)/length(resid)); reqm


## erro percentual absoluto médio (mean absolute percentage error - MAPE)

library(Metrics)
mape(USAcc.mod, est)# 0.03645094

epam = 100* sum(abs(resid/USAcc.mod))/length(USAcc.mod); epam


## erro escalado absoluto médio (MASE)

library(Metrics)
mase(USAcc.mod, est, step_size = 12)

#ou
eeam = function(obs, prev, s){
  n = length(obs)
  soma1 = 0
  for (i in (s+1):n){soma1 = soma1 + abs(obs[i]-obs[i-s])}
  eamns = soma1/(n-s)
  soma2 = 0
  for (i in 1:n){soma2 = soma2 + abs((obs[i]-prev[i])/eamns)}
  eeam = soma2/n
  return(eeam)}
eeam(USAcc.mod, est, s=12)


## U-Theil 

library(forecast)
accuracy(f = est, x = USAcc.mod)

#ou
Ustat = function(obs, prev){
  n = length(obs)
  soma1 = 0
  for (i in 1:(n-1)){soma1 = soma1 + ((prev[i+1]-obs[i+1])/obs[i])^2}
  soma2 = 0
  for (i in 1:(n-1)){soma2 = soma2 + ((obs[i+1]-obs[i])/obs[i])^2}
  Ustat = sqrt(soma1/soma2)
  return(Ustat)}
Ustat(USAcc.mod, est)

#Abordagem Deterministica - Não colquei no trabalho Final

# Carregar série temporal
dados <- USAccDeaths
n <- length(dados)

# Criar variáveis explicativas
tempo <- 1:n
meses <- cycle(dados)  # variável para os meses (1 a 12)

# Ajustar modelo determinístico com tendência linear e efeito mensal
modelo_det <- lm(dados ~ tempo + factor(meses))
summary(modelo_det)

# Transformar a tendência em série temporal com mesma estrutura de USAccDeaths
tendencia_ts <- ts(tendencia, start = start(dados), frequency = frequency(dados))

# Plot da série original com a tendência determinística
plot(dados, main = "Série USAccDeaths com Tendência Determinística", 
     ylab = "N.º de mortes", xlab = "Tempo")
lines(tendencia_ts, col = "red", lwd = 2)
legend("topleft", legend = c("Série original", "Tendência"), 
       col = c("black", "red"), lty = 1, lwd = 2)

# Criar dataframe com componentes
df2 <- data.frame(
  mortes = as.numeric(dados),
  mes = months(time(dados)),           # nome do mês
  tempo = tempo,
  tendencia = fitted(modelo_det)
)

# Carregar pacote para agrupar
library(dplyr)

# Calcular tendência média por mês
df_summary <- df %>%
  group_by(mes) %>%
  summarise(tendencia_media = mean(tendencia))

# Ordenar os meses corretamente
df_summary$mes <- factor(df_summary$mes, levels = month.name)

# Plotar a tendência média por mês
barplot(df_summary$tendencia_media,
        names.arg = df_summary$mes,
        las = 2, col = "steelblue",
        ylab = "Tendência média", 
        main = "Tendência Média por Mês (Modelo Determinístico)")
