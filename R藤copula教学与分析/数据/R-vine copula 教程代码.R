rm(list = ls())
library(readxl)
library(tseries)
library(FinTS)

#导入数据
data = read_xlsx("data.xlsx") 
data_series = ts(data[,2:6])
ret_series = log(data_series/lag(data_series,1L))
ret_series

#将数据里六个时间序列分别取出
m1 = as.ts(ret_series[,1])
m2 = as.ts(ret_series[,2])
m3 = as.ts(ret_series[,3])
m4 = as.ts(ret_series[,4]) 
m5 = as.ts(ret_series[,5])



#三、描述性统计和数据检验
#3.1ADF检验
adf.test(m1)
adf.test(m2)
adf.test(m3)
adf.test(m4)
adf.test(m5)


#3.2jarque检验
jarque.bera.test(m1)
jarque.bera.test(m2)
jarque.bera.test(m3)
jarque.bera.test(m4)
jarque.bera.test(m5)


#3.3LB检验
Box.test (m1, lag = 20, type = "Ljung")
Box.test (m2, lag = 20, type = "Ljung")
Box.test (m3, lag = 20, type = "Ljung")
Box.test (m4, lag = 20, type = "Ljung")
Box.test (m5, lag = 20, type = "Ljung")



#3.4ARCH-LM检验
library(FinTS)
ArchTest (m1, lags=20, demean = FALSE) 
ArchTest (m2, lags=20, demean = FALSE) 
ArchTest (m3, lags=20, demean = FALSE) 
ArchTest (m4, lags=20, demean = FALSE) 
ArchTest (m5, lags=20, demean = FALSE) 


#3.5相关系数矩阵
cor(ret_series)

#3.6描述性统计
library(pastecs)
stat.desc(ret_series,norm = TRUE)

#3.7绘制自相关和偏自相关图
m1_acf = acf(m1,plot = T)
am1_pacf = pacf(m1,plot = T)
m2_acf = acf(m2,plot = T)
m2_pacf = pacf(m2,plot = T)
m3_acf = acf(m3,plot = T)
m3_pacf = pacf(m3,plot = T)
m4_acf = acf(m4,plot = T)
m4_pacf = pacf(m4,plot = T)
m5_acf = acf(m5,plot = T)
m5_pacf = pacf(m5,plot = T)


#6.4.1采用auto.arima自动定阶
library(zoo)
library(forecast)
auto.arima(m1) #arima(1,0,0)
auto.arima(m2) #arima(1,0,0)
auto.arima(m3) #arima(0,0,1)
auto.arima(m4) #arima(2,0,2)
auto.arima(m5) #arima(0,0,1)



#构建arma-garch模型
library(rugarch)
spec_m1 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                            garchOrder = c(1, 1), 
                                            submodel = NULL, 
                                            external.regressors = NULL, 
                                            variance.targeting = FALSE), 
                      
                      mean.model     = list(armaOrder = c(1, 0), 
                                            external.regressors = NULL), 
                      distribution.model="sstd", 
                      start.pars = list(), 
                      fixed.pars = list())
garch_m1 <- ugarchfit(spec = spec_m1, data = m1, solver.control = list(trace=0))

spec_m2 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                            garchOrder = c(1, 1), 
                                            submodel = NULL, 
                                            external.regressors = NULL, 
                                            variance.targeting = FALSE), 
                      
                      mean.model     = list(armaOrder = c(1, 0), 
                                            external.regressors = NULL), 
                      distribution.model="sstd", 
                      start.pars = list(), 
                      fixed.pars = list())
garch_m2 <- ugarchfit(spec = spec_m2, data = m2, solver.control = list(trace=0))

spec_m3 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                            garchOrder = c(1, 1), 
                                            submodel = NULL, 
                                            external.regressors = NULL, 
                                            variance.targeting = FALSE), 
                      
                      mean.model     = list(armaOrder = c(0, 1), 
                                            external.regressors = NULL), 
                      distribution.model="sstd", 
                      start.pars = list(), 
                      fixed.pars = list())
garch_m3 <- ugarchfit(spec = spec_m3, data = m3, solver.control = list(trace=0))

spec_m4 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                            garchOrder = c(1, 1), 
                                            submodel = NULL, 
                                            external.regressors = NULL, 
                                            variance.targeting = FALSE), 
                      
                      mean.model     = list(armaOrder = c(2, 2), 
                                            external.regressors = NULL), 
                      distribution.model="sstd", 
                      start.pars = list(), 
                      fixed.pars = list())
garch_m4 <- ugarchfit(spec = spec_m4, data = m4, solver.control = list(trace=0))

spec_m5 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                            garchOrder = c(1, 1), 
                                            submodel = NULL, 
                                            external.regressors = NULL, 
                                            variance.targeting = FALSE), 
                      
                      mean.model     = list(armaOrder = c(0, 1), 
                                            external.regressors = NULL), 
                      distribution.model="sstd", 
                      start.pars = list(), 
                      fixed.pars = list())
garch_m5 <- ugarchfit(spec = spec_m5, data = m5, solver.control = list(trace=0))

sigma_matrix = matrix(data = c(garch_m1@fit$sigma,garch_m2@fit$sigma,garch_m3@fit$sigma,
                               garch_m4@fit$sigma,garch_m5@fit$sigma),
                      nrow = length(garch_m1@fit$sigma),ncol = 5,byrow=FALSE)
residual_matrix = matrix(data = c(garch_m1@fit$residuals,garch_m2@fit$residuals,garch_m3@fit$residuals,
                                  garch_m4@fit$residuals,garch_m5@fit$residuals),
                         nrow = length(garch_m1@fit$residuals),ncol = 5,byrow=FALSE)

#计算Copula
std_sigma_matrix = matrix(nrow = 486,ncol = 5)
copuladata = matrix(nrow = 486,ncol = 5)
for (i in c(1:5)) {
  std_sigma_matrix[,i] = residual_matrix[,i]/sigma_matrix[,i]
  f = ecdf(as.numeric(std_sigma_matrix[,i]))
  copuladata[,i] = f(std_sigma_matrix[,i])
}


library(VineCopula)
library(copula)
library(CDVine)

Rst = RVineStructureSelect(copuladata,family = c(1:6),progress = TRUE,se = TRUE,method = 'itau',rotations = TRUE)
summary(RVM)
RVineTreePlot(RVM)
contour(RVM)
RVM$pair.AIC
RVM$pair.logLik
RVM$par
RVM$se
RVM$tau
