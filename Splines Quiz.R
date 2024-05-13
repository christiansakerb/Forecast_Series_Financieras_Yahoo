install.packages("tsfeatures")
install.packages("fields")
install.packages("forecast")
install.packages("quantmod")

R.Version()

library(tidyverse)

##R.version.string

library(earth)
library(mgcv)
library(caret)     # para optimizar proceso de tunning

#Otros de manipulación de datos
library(tseries)  #manipulación series de tiempo
library(tsfeatures) # Obtención de features relevantes
library(tidyverse) #Data manipulation
library(fields)  #Data manipulation
library(forecast)  # para función pronosticos
library(quantmod) #Para obtener datos financiera
library(dygraphs)  # data visualization
library(dplyr)    # data wrangling
library(ggplot2)  # gráficos 


theme_set(theme_bw())
options(warn = - 1) 

###Función para obtener datos:
start<-format(as.Date("2016-05-01"),"%Y-%m-%d")
end<-format(as.Date("2018-01-28"),"%Y-%m-%d")

precios <-function(simbolo)
{
  ##Obtener precios stocks de Yahoo FInance
  datos <- getSymbols(simbolo, auto.assign = FALSE, from=start, to=end)
  ## Elimar faltantes:
  datos<-na.omit(datos)
  ##mantener columnas con precios cierre 4:
  datos <- datos[,4]
  ##Para hacerlo datos accesibles en el global environment:
  assign("Serie", datos, envir = .GlobalEnv)
}

precios("Meta")

dim(Serie)
##Juntar los datos y renombrarlos:
Pr<-merge.xts(Serie) %>% na.omit()
head(Pr)
colnames(Pr) <- c("META")
head(Pr)
##Serie tiempo, grafica interactiva

Precios<- dygraph(Pr[,c(1)], main="Precios") %>%
  dyAxis("y", label = "Precios") %>%
  dyRangeSelector(dateWindow = c("2016-05-01", "2018-01-28"))%>%
  dyOptions(colors = RColorBrewer::brewer.pal(4,"Set1"))
Precios

xpr <- as.xts(Pr, dateFormat = "Date")
Pr_index <- fortify.zoo(xpr)
head(Pr_index)
nrow(Pr)


###
#Partimos serie para train & test
h <- round(length(Pr)*0.05, digits = 0 )
h
Pr_tra <- Pr[1:(nrow(Pr) - h), ]
Pr_tes<- Pr[(nrow(Pr) - h + 1):nrow(Pr), ]

Pr_tes

Pr_df_tra<-as.data.frame(Pr_tra)

tail(Pr_df_tra)
str(Pr_df_tra)
dim(Pr_df_tra)

head(Pr_tes)
dim(Pr_tes)

#Generación de features o rezagos y obtención base datos

#Obtenemos features:
tsfeatures(Pr_df_tra$META)
pacf_features(Pr_df_tra$META)
#Visualiamos los rezagos autorregresivos, haciendo estacionaria la serie:
dPr<-diff(Pr_df_tra$META)%>% na.omit() # Train section.
pacf(dPr)

#analisis de componentes de la serie
mstl(Serie)%>% autoplot()

#Quitar el componeten de tendencia
detre = mstl(Serie)
df_detrend <- Serie - detre[,2]
autoplot(df_detrend)

adf.test(Serie)
adf.test(dPr)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Index, Amzn, .interactive = FALSE)

lag_pr = lag(Pr_df_tra,n=1L) 
lag2_pr = lag(Pr_df_tra,n=2L) 
lag3_pr = lag(Pr_df_tra,n=3L)
lag4_pr = lag(Pr_df_tra,n=4L)
lag8_pr = lag(Pr_df_tra,n=8L)

#Unimos los nuevos features o retrasos en una base
Pr_all_train<-cbind(Pr_df_tra, lag_pr, lag2_pr, lag3_pr, lag4_pr,lag8_pr) %>%na.omit()
colnames(Pr_all_train)<-c("META","l1","l2","l3","l4","l8")
head(Pr_all_train)  

#tendencia
Pr_all_train$trend = 1:nrow(Pr_all_train)
str(Pr_all_train)

head(Pr_all_train)


#Separacion de X
x_train <- Pr_all_train %>% 
  select(starts_with(c("l", "t")))
head(x_train)

#Separacion de y
y_train <- Pr_all_train %>% 
  select(META)
tail(y_train)

#2) MARS
mars_mod <- earth(
  META ~ ., data=Pr_all_train, pmethod="backward" )
print(mars_mod)
summary(mars_mod)
plot(mars_mod, which = 1)

#Pr_test2<-as.data.frame(Pr_tes)

######
# Ahora para predecir con test debemos crear las variables

Pr_tes_para_predecir <- Pr[(nrow(Pr) - h + 1-8):nrow(Pr), ]
dim(Pr_tes_para_predecir)
Pr_df_tes_para_predecir = as.data.frame(Pr_tes_para_predecir)
dim(Pr_df_tes_para_predecir)

head(Pr_df_tes_para_predecir)



#####
lag_pr_t = lag(Pr_df_tes_para_predecir,n=1L) 
lag2_pr_t = lag(Pr_df_tes_para_predecir,n=2L) 
lag3_pr_t = lag(Pr_df_tes_para_predecir,n=3L)
lag4_pr_t = lag(Pr_df_tes_para_predecir,n=4L)
lag8_pr_t = lag(Pr_df_tes_para_predecir,n=8L)
#unir dfs y borrar nulos
Pr_all_test<-cbind(Pr_df_tes_para_predecir, lag_pr_t, lag2_pr_t, lag3_pr_t, lag4_pr_t,lag8_pr_t) %>%na.omit()
colnames(Pr_all_test)<-c("META","l1","l2","l3","l4","l8")


trend_consecutivo = max(Pr_all_train$trend)
Pr_all_test$trend =  seq(1, nrow(Pr_all_test), by = 1)
Pr_all_test$trend = Pr_all_test$trend+trend_consecutivo 


head(Pr_all_test)  
tail(Pr_all_train)

dim(Pr_all_test)

#Separacion de X en test
x_test <- Pr_all_test %>% 
  select(starts_with(c("l", "t")))
head(x_test)
dim(x_test)

#Separacion de y
y_test <- Pr_all_test %>% 
  select(META)
tail(y_test)

#Predecir en conjunto test
pred_mars = predict(mars_mod, x_test)

df_resultados =  cbind(pred_mars[,1],y_test)
df_resultados


# Calcular las diferencias entre los valores reales y predichos
diferencias <- df_resultados[,2] - df_resultados[,1]
# Calcular el MSE
mse <- mean(diferencias^2)

# Calcular el MAE
mae <- mean(abs(diferencias))

# Calcular el RMSE
rmse <- sqrt(mse)

# Mostrar los resultados
print(paste("MSE:", mse))
print(paste("MAE:", mae))
print(paste("RMSE:", rmse))
            