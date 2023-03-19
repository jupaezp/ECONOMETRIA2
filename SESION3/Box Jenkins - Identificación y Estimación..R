##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2022-I
#      SESIÓN MONITORIA : Metodologia Box-Jenkins Primera parte: identificación y 
#               estimación de series de tiempo univariadas.
#             Series modeladas: 
#                   - Índice de producción industrial  
#                   - spread entre la tasa de bonos norteamericanos a 5 años y Tbills
#                   - Tasa de desempleo mensual EE.UU. (1959 - 2012)
##____________________________________________________________________________________
##_____________________________________________________________________________________

#Activamos algunos paquetes que vamos a utilizar en toda la metodología

library(forecast)    # Para hacer pronósticos con modelos arima
library(lmtest)      # Significancia individual de los coeficientes ARIMA
library(urca)        # Prueba de raíz unitaria
library(tseries)     # Para estimar modelos de series de tiempo y hacer pruebas de supuestos.
library(readxl)      # Para leer archivos de excel
library(stargazer)   # Para presentar resultados más estéticos.
library(psych)       # Para hacer estadísticas descriptivas
library(seasonal)    # Para desestacionalizar series
library(aTSA)        # Para hacer la prueba de efectos ARCH
library(astsa)       # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA  
library(xts)         # Para utilizar objetos xts 
library(tidyverse)   # Conjunto de paquetes (incluye dplyr y ggplot2)
library(readxl)      # Para leer archivos excel 
library(car)         # Para usar la función qqPlot

# Limpiar el Environment
rm(list = ls()) 

# Nota: En R uno puede trabajar series de tiempo con 3 objetos:
# ts --> Intervalos de tiempo fijos, objeto estándar de series de tiempo en R)
# zoo
# xts --> xts significa "eXtensible time series".  xts es una subclase de zoo.
#Son objetos que están diseñados para ser más flexibles y poderosos en términos 
#de modelamiento; diseñados para facilitar el uso de las series temporales.  
#Además, son muy útiles para el modelamiento de series financieros con intervalos de tiempo que nos son fijos.

#
#---- EJEMPLO 1: Industrial Production Index (indprod) ----
#

# Vamos a trabajar con una serie del Industrial Production Index de los EE.UU (indprod). 
# el cuál es un indicador económico que mide la producción real de los Estados Unidos en términos de manufactura, minería, electicidad y gas. 
# Fuente: https://fred.stlouisfed.org/series/INDPRO#0 (Tomados de la FRED de la Reserva Federa de St. Louis)

# Ojo: Al ser un índice tiene que tener un año base en el cuál el valor del índice sea 100
# (para el ejemplo el año base fue 2012, es decir Index 2012=100)
# Valores mayores a 100 indican que para ese año corriente la producción manufacturera fue mayor que en el año base.
# Valores menores a 100 indican que para ese año corriente la producción manufacturera fue menor que en el año base.


#---- PASO 1- IDENTIFICACIÓN indprod ----

# Se cargan las series de tiempo
base_fred <- read_excel(file.choose()) #INDPRO

#Convertir la serie en un objeto tipo ts
indprod = ts(base_fred$INDPRO, start = 1960, frequency = 4) # Se coloca 4 para indicar que la frecuencia es trimestral

# Visualización de los datos
View(indprod)  

# Crear un objeto tipo xts
indprod_xts = xts(base_fred$INDPRO, 
                  order.by = base_fred$observation_date) # Importar como un objeto xts

# Visualización de los datos
View(indprod_xts)

# Ver la clase de cada uno de los ejemplos de series de tiempo
class(indprod)
class(indprod_xts)

### Gráficas

## Para el objeto ts
x11()   
# Usando la función estándar de gráficación en R
plot.ts(indprod, xlab="",ylab="", main="IPI trimestral de Estados Unidos (1960-2012) (Index 2012 = 100)",lty=1, lwd=2, col="blue")

# Usando ggplot2
autoplot(indprod, col="green", main = "IPI trimestre de Estados Unidos (1960-2012) (Index 2012 = 100)",xlab = "Fecha", ylab="IPI", lwd=1)

#  Para el objeto xts
x11()
# Gráfica mucho más linda e informativa
plot(indprod_xts, main = "IPI trimestre de Estados Unidos (1960-2012) (Index 2012 = 100)", ylab  = "IPI")

#Vamos a graficar la ACF y PACF de la serie en nivel.
lags=24
par(mfrow=c(1,2))
acf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PCE de USA') 
pacf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PCE de USA')

# Versión ggplot:
x11()
ggAcf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA")
ggPacf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")
# Versión paquete astsa (sirve tanto para objetos ts como para objetos xts)
acf2(indprod)

#Como podemos ver, la FAC tiene un decaimento lento a cero,lo que nos da indicios de que el proceso no es estacionario.

#Para estacionalizar la serie, es estándar en la práctica hacer tres transformaciones para ello: 

## 1. Aplicar diff(serie_original): 
# Para diferenciar la serie y eliminar la tendencia estocástica.

d.indprod= diff(indprod) # serie diferenciada

## 2. Aplicar log(serie_original): 
# para estabilizar la varianza de la serie,

l.indprod=log(indprod) # serie que se le aplica solo el logaritmo.

## 3. Aplicar diff(log(serie_original)): 
# Para diferenciar y estabilizar la varianza de la serie.
# Se interpreta como una tasa de crecimiento.

dl.indprod= diff(log(indprod))*100   # diferencia de logaritmos de la serie (tasa de crecimiento).

#Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su valor en logaritmos.
x11()
par(mfrow=c(2,2))
plot.ts(indprod, xlab="",ylab="", main="indprod en nivel para Estados Unidos 1959-2019",lty=1, lwd=2, col="blue")
plot.ts(l.indprod, xlab="",ylab="", main="indprod en logaritmo para Estados Unidos 1959-2019",lty=1, lwd=2, col="black")
plot.ts(d.indprod, xlab="",ylab="", main="Variación del indprod para Estados Unidos 1959-2019",lty=1, lwd=2, col="red")
plot.ts(dl.indprod, xlab="",ylab="", main="Tasa de crecimiento del indprod para Estados Unidos 1959-2019",lty=1, lwd=2, col="darkgreen")

# Vamos a elegir la tasa de crecimiento de indprod debido a que la varianza es mucho más estable, pese a 
# tener la misma media constante que la serie en primera diferencia.

#Ahora hacemos la ACF y la PACF, para la tasa de crecimiento del indprod donde evidenciamos un proceso débilmente dependiente. 
lags=30
par(mfrow=c(1,2))
acf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la tasa de crecimiento del PIB') 
pacf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la tasa de crecimiento del PIB')

ggAcf(dl.indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA")
ggPacf(dl.indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")

acf2(dl.indprod)

#
# Uso de Criterios de Información para la identificación del proceso ARIMA que se esté modelando:
#

# Método manual para identificar el ARIMA usando Criterios de Información. 

#Ahora vamos a ver lo que muestran los criterios AIC y BIC
AR.m <- 6 #Supondremos que el rezago autorregresivo máximo es 6 (pmax)
MA.m <- 6 #Supondremos que el rezago de promedio móvil máximo es 6. (qmax)

#---- Funciones para idenficar modelo ARIMA por Criterios de Información ----

# 1. Función para seleccionar el modelo ARIMA con el menor criterio de información

# Función que me permite crear un data frame para distintos modelos ARIMA(p,d,q)
# en donde cada fila del df me da el orden del modelo y los AIC y BIC correspondientes a dicho modelo
# Ojo: utlizar method = "ML" dentro de arima(), de lo contrario les arrojará error.

arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}

# 2. Creo la función arma_min_AIC para seleccionar el mejor modelo según AIC.
arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}

# 3. Creo la función arma_min_BIC para seleccionar el mejor modelo según BIC
arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}

# Llamo la función arma_selection_df para construir un data frame con todos los posibles modelos
# ARIMA(p, d, q) donde p y q varían entre 0 y 6. 

#Primero,trabajo con la serie en logaritmo, la cual necesita de una diferenciación para ser estacionaria (por eso d=1).

mod_d1_indprod = arma_seleccion_df(l.indprod, AR.m, MA.m, d = 1, TRUE, "ML")
view(mod_d1_indprod)

# Selecciono el mejor modelo según Criterios de Información cuando d=1
min_aic_lindprod = arma_min_AIC(mod_d1_indprod); min_aic_lindprod #ARIMA (3,1,5)
min_bic_lindprod = arma_min_BIC(mod_d1_indprod); min_bic_lindprod #ARIMA (1,1,0)

#Recuerden que en la metodología BIC prima la parsimonia, es decir, modelos con una cantidad menos de grados.

# Ahora, trabajo con la serie diferenciada (debido a que d = 0)

mod_d0_indprod = arma_seleccion_df(dl.indprod, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_d0_indprod)

# Selecciono el mejor modelo según criterios de información cuando d=0
min_aic_dlindprod = arma_min_AIC(mod_d0_indprod); min_aic_dlindprod #ARIMA (2,0,4)
min_bic_dlindprod = arma_min_BIC(mod_d0_indprod); min_bic_dlindprod #ARIMA (2,0,4)

##
# Método automático para identificar el ARIMA usando criterios de información 
# Usando la función auto.arima() del paquete forecast
##

# Nota: La misma función auto.arima es capaz de reconocer el orden de diferenciación para que la serie quede estacionaria. 
# En realidad, la función auto.arima calcula un modelo SARIMA porque además de 
# modelas la parte ARIMA de la serie, también modela la componente estacional de la misma.
# Para mayor info. sobre los modelos SARIMA: Ver Enders Capítulo 2 - Sección 11: Seasonality
auto.arima(l.indprod, method = "ML")

# Ojo: Muy importante.
      # En la práctica, lo mejor es usar tanto el método manual como el auto.arima.
      # Basarse exclusivamente en el modelo auto.arima generalmente conduce a 
      # conlusiones erroneas, por lo que no basta solo usar el modelo auto.arima.
      # El método manual siempre debe realizarse y debe ser la principal guía,
      # el auto.arima debe verse como un método de identificación complementario. 

# Ojo: Es muy importante a la hora de seleccionar un modelo ARIMA para explicar 
      # el proceso generador de datos usar tanto 
      # la gráfica de la serie, las ACF y la PACF y los Criterios de Información.

# PERO! De acá en adelante, utilizaremos los modelos que se obtuvieron utilizando el Criterio de Información BIC.
# Pese a que en series de tiempo es más común usar BIC para seleccionar el modelo, se aconseja también observar los resultados
# cuando se escoge el modelo propuesto por el AIC.


#---- PASO 2- ESTIMACIÓN indprod ----

##
# Una vez se ha identificado la serie, se procede a estimar el modelo ARIMA. 
# debe tomarse en cuenta que incluir muy pocos rezagos puede llevar a que los 
# residuales del modelo no se comporten como un Ruido Blanco, y que incluir 
# muchos parámetros puede llevar a que nuestro modelo no sea parsimonioso; se 
# pierdan muchos grados de libertad y se pierda eficiencia.
##

# Existen 3 métodos de estimación para la función arima: (por default los 3 comandos utilizan CSS)
## ML: Máxima verosimilitud (más preciso y usualmente la mejor opción para bases pequeñas donde se pueda encontrar una solución analítica).
## CSS: (más veloz generalmente. se usa como aproximación o para bases de datos grandisimas).
## CSS-ML: Una combinación de ambas.

## Sin embargo:
### ML puede no converger.
### CSS puede no hacer estimaciones lo suficientemente precisas y arrojar error. 

# Para más información ver: 
# https://stats.stackexchange.com/questions/209730/fitting-methods-in-arima#:~:text=CSS%20sets%20the%20initial%20observations,an%20estimate%20of%20these%20values.

# Nota: Para hacer la estimación de un modelo arima se pueden utilizar 3 funciones distintas: 
## arima: función del paquete stats que es la más usual emplear y sirve para hacer predicciones con la función forecast del paquete forecast.
## Arima: función del paquete forecast que es básicamente un wrapper de la función arima.
## sarima: función del paquete astsa que permite estimar modelos sarima.

# Ojo: tanto la función Arima como la función sarima están construidas sobre la función arima().
# Cualquier de las 3 funciones para estimar un ARIMA permiten estimar modelos SARIMA
# es decir, son capaces de modelar la componente estacional de una serie.

# Nota: Cómo ya se indicó previamente, para mayor precisión en las estimaciones,
# y para evitar algunos inconvenientes que a veces ocurren al utilizar el método CSS, 
# la estimación del arima se hará mediante maxima verosimilitud (ML).

arima_1.1.0_lindprod = arima(l.indprod, order = c(1,1,0), include.mean = T, method = "ML"); summary(arima_1.1.0_lindprod) # modelamiento ARIMA (1,1,0)

arima_2.0.4_dlindprod = arima(dl.indprod, order = c(2,0,4), include.mean = T, method = "ML"); summary(arima_2.0.4_dlindprod) # modelamiento ARIMA (2,0,4)

#Imprimir los resultados
stargazer(arima_1.1.0_lindprod, arima_2.0.4_dlindprod, type = "text")

#
#---- EJEMPLO 2: Spread between 5 year Treasury bonds rate and 3 month Trills (spread) ----
#

# Vamos a trabajar con una serie del spread entre la tasa de interés del bono del tesoro 
# de los Estados Unidos a 5 años y la tasa de interés de los Tbills a 3 meses.

# Ojo: La tasa de interés de los bonos del tesoro norteamericano a 3 meses es comúnmente 
#      usada como la tasa libre de riesgo de los mercados financieros, por lo que 
#      ese spread modelado se podría ver como el exceso de retorno de los bonos de 5 años del tesoro.

# Fuente: http://time-series.net/data_sets (Tomados de la página oficial de Walter Enders)

#---- PASO 1- IDENTIFICACIÓN spread ----

# Se cargan las series de tiempo
quarterly <- read_excel(file.choose()) # quarterly.7775706

#Tenemos que crear el "interest rate spread", el cual puede formarse como la diferencia entre
#el tipo de interés de los bonos del Estado de EE.UU. a 5 años (r5) y el tipo de las letras del Tesoro a 3 meses (government bonds). 
quarterly = quarterly %>% 
  mutate(spread = r5 - Tbill) 

spread = xts(quarterly$spread, order.by = quarterly$DATE) #convertimos en un objeto xts

# Gráfica base de datos 
x11()
plot(spread, main = "Spread entre los bonos a 5 años y los Tbills a 3 meses del Tesoro de los EE.UU.", ylab  = "spread")
abline(h = mean(spread), col = "green")

# ACF y PACF
lags=24
ggAcf(spread,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del spread")
ggPacf(spread,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del spread")
acf2(spread)

#La FAC tiene un decaimento rápido a cero --> nos da indicios de que la serie puede ser estacionaria. 

# Criterios de información para la identificación del modelo: 

#---- Funciones para idenficar modelo ARIMA por criterios de información ----

# Del ejemplo anterior, tenemos:
## arma_seleccion_df: función que me ordena en un data.frame todos los posibles grados 
# p y q dado los Criterios de Información AIC y BIC.
## arma_min_AIC: seleccionar el mejor modelo según AIC, es decir, el menor. 
## arma_min_BIC: seleccionar el mejor modelo según BIC, es decir, el menor. 

# Seleccionar el modelo (en este caso d = 0 porque no hay que diferenciar la serie dado que es estacionaria)
AR.m = 8; MA.m = 8
df_criterios_info_spread = arma_seleccion_df(spread, AR.m, MA.m, d = 0, TRUE, "ML")
min_aic_spread = arma_min_AIC(df_criterios_info_spread); min_aic_spread # selecciona el min AIC / ARMA(5,5)
min_bic_spread = arma_min_BIC(df_criterios_info_spread); min_bic_spread # selecciona el min BIC / ARMA(1,1)

# Nota: El modelo con el menor BIC es un ARIMA(1,0,1), no obstante, ese modelo no satisface el supuesto de no correlacion 
#       serial en los residuales por lo que es necesario utilizar el modelo con el segundo menor BIC, el cual es un ARIMA(1,0,2)

# Si revisamos la PACF, parece oscilar a partir del rezago 2, es plausible suponer que el grado q es igual a 2. 

# Selecciono el modelo ARIMA(1,0,2) dado por el Criterio de información BIC, ya que es más parsimonioso
# que el modelo ARIMA(5,0,5) dado por el Criterio de info. AIC.

# Por auto.arima
auto.arima(spread, method = "ML")

#---- PASO 2- ESTIMACIÓN spread ----

arima_1.0.2_spread = arima(spread,
                       order = c(1,0,2), include.mean = T, 
                       method = "ML"); summary(arima_1.0.2_spread) # modelamiento ARIMA (1,0,2)

#Imprimimos el resultado
stargazer(arima_1.0.2_spread, type = "text")

# ------ EJEMPLO 3: Tasa de desempleo --------

# Para el último ejemplo vamos a trabajar con la serie de la Tasa de desempleo mensual para Estados Unidos de 1959 a 2012.
# Este indicador económico mide el porcentaje de la población económicamente activa que se encuentra desempleada. Para el caso 
# estadounidense, se toma a la población económicamente activa (labor force) como todas aquellas personas de 16 o más años, que 
# en alguno de los 50 estados, se encuentren en capacidad de trabajar. La forma de calcularlo es: 
# (población desocupada/población económicamente activa)*100.

# Fuente: https://fred.stlouisfed.org/series/UNRATE (Tomado de la FRED de la Reserva Federal de St Louis)

#---- PASO 1- IDENTIFICACIÓN tasa de desempleo ----

# Se cargan las series de tiempo
base_unem <- read_excel(file.choose()) #UNRATE
view(base_unem)

UNRATE = ts(base_unem$UNRATE, start = c(1959,11), frequency = 12) # La serie empieza en noviembre de 1959 y es una serie mensual.

# Visualización de los datos
View(UNRATE)  

# Crear un objeto tipo xts
UNRATE_xts = xts(base_unem$UNRATE, 
                 order.by = base_unem$observation_date) # Importar como un objeto xts

View(UNRATE_xts)

## Graficamos la serie con la funcion del paquete stats de R
x11()   
plot.ts(UNRATE, xlab="",ylab="", main="Tasa de desempleo mensual EE.UU (1959-2012)",lty=1, lwd=2, col="purple")

# Para el objeto xts
x11()
plot(UNRATE_xts, main = "Tasa de desempleo mensual EE.UU (1959-2012)", ylab  = "UNRATE")

#Vamos a graficar la ACF y PACF de la serie en nivel.
lags=24
par(mfrow=c(1,2))
acf(UNRATE,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la Tasa de desempleo') 
pacf(UNRATE,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la Tasa de desempleo')

# La grafica de la serie no muestra el comportamiento tipico de una serie estacionaria, puesto que si bien se podria
# afirmar que su varianza es estable, lo mismo no parece ser cierto para su media. Igualmente, en la grafica FAC presenta 
# un decaimiento a cero pero este es muy lento, propio de un proceso altamente persistente. Por tanto, se asume que la 
# serie no es estacionaria y se hace necesario realizar las transformaciones explicadas más arriba.

## Como se mencionó, la serie pareciera presentar una varianza estable, por lo que solo vamos a diferenciar la serie para
# eliminar su tendencia, por medio de diff(serie_original):

d.UNRATE = diff(UNRATE) # serie diferenciada

# Vamos a graficar ahora el nivel (serie original) y la variación (serie diferenciada) de la tasa de desempleo.

x11()
par(mfrow=c(2,1))
plot.ts(UNRATE, xlab="",ylab="", main="Tasa de desempleo en nivel 1959-2012",
        lty=1, lwd=2, col="blue")
plot.ts(d.UNRATE, xlab="",ylab="", main="Variación de la Tasa de desempleo 1959-2012",lty=1,
        lwd=2, col="red")


# Igualmente graficamos la ACF y la PACF para la variación de la tasa de desempleo

x11()
lags=30
par(mfrow=c(1,2))
acf(d.UNRATE,lag.max=lags,plot=T,lwd=2,xlab="",main="ACF de la variación de la Tasa de desempleo")
pacf(d.UNRATE,lag.max=lags,plot=T,lwd=2,xlab="",main="PACF de la variación de la Tasa de desempleo")

# La gráfica de la variación del Tasa de desempleo, contrario a su version en niveles, evidencia claramente una media contante
# mientras que en la ACF ya no se presenta alta persistencia, por lo que se asume que la serie transformada 
# es estacionaria.

# Criterios de información para la identificación del modelo: 

#---- Funciones para idenficar modelo ARIMA por criterios de información ----

# De nuevo, usamos las funciones arma_seleccion_df, arma_min_AIC y arma_min_BIC:
## arma_seleccion_df: función que me ordena en un data.frame todos los posibles grados 
# p y q dado los Criterios de Información AIC y BIC.
## arma_min_AIC: seleccionar el mejor modelo según AIC, es decir, el menor. 
## arma_min_BIC: seleccionar el mejor modelo según BIC, es decir, el menor. 

# Serie en niveles (con d=1):

AR.m = 6; MA.m = 6
mod_d1_UNRATE = arma_seleccion_df(UNRATE, AR.m, MA.m, d = 1, TRUE, "ML")
view(mod_d1_UNRATE)
# Selecciono el mejor modelo según criterios de información cuando d=1
min_aic_UNRATE = arma_min_AIC(mod_d1_UNRATE); min_aic_UNRATE #ARIMA (6,1,6)
min_bic_UNRATE = arma_min_BIC(mod_d1_UNRATE); min_bic_UNRATE #ARIMA (1,1,2)

# Serie diferenciada (con d=0):

mod_d0_UNRATE = arma_seleccion_df(d.UNRATE, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_d0_UNRATE)
# Selecciono el mejor modelo según criterios de información cuando d=1
min_aic_d.UNRATE = arma_min_AIC(mod_d0_UNRATE); min_aic_d.UNRATE #ARIMA (6,0,6)
min_bic_d.UNRATE = arma_min_BIC(mod_d0_UNRATE); min_bic_d.UNRATE #ARIMA (1,0,2)

# En este caso nos guiamos por el criterio AIC, porque la ACF y la PACF muestran que el valor en el periodo
# actual depende en varios periodos pasados, por lo que conviene escoger un mayor numero de rezagos para 
# evitar violar el supuesto de no autocorrelacion, Por tanto seleccionamos el modelo ARIMA(6,0,6) (para la serie diferenciada).

# Por auto.arima
auto.arima(UNRATE, method = "ML")

#---- PASO 2- ESTIMACIÓN tasa de desempleo ----

arima_6.1.6_UNRATE = arima(UNRATE,
                           order = c(6,1,6), include.mean = T, 
                           method = "ML"); summary(arima_6.1.6_UNRATE) # modelamiento ARIMA (6,1,6)

arima_6.0.6_UNRATE = arima(d.UNRATE,
                           order = c(6,0,6), include.mean = T, 
                           method = "ML"); summary(arima_6.0.6_UNRATE) # modelamiento ARIMA (6,1,6)
#Imprimimos el resultado
stargazer(arima_6.0.6_UNRATE, arima_6.1.6_UNRATE, type = "text")






