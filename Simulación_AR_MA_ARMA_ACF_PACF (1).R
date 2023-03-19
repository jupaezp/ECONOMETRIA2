##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2023-I
#      SESIÓN MONITORIA : Simulación de procesos AR, MA y ARMA
##____________________________________________________________________________________
##_____________________________________________________________________________________

#Limpiamos el environment.
remove(list = ls())

# Algunos paquetes que si bien no se van a utilizar 

library(forecast)    # Para hacer pronósticos con modelos arima (Desactualizado, hoy en día se usa el paquete table del tidyverts)
library(lmtest)      # Significancia individual de los coeficientes ARIMA
library(urca)        # Prueba de raíz unitaria
library(tseries)     # Para estimar modelos de series de tiempo y hacer pruebas de supuestos.
library(readxl)      # Para leer archivos de excel
library(stargazer)   # Para presentar resultados/salidas de R de manera más estética y exportarlos a latex o word
library(psych)       # Para hacer estadísticas descriptivas
library(seasonal)    # Para desestacionalizar series
library(aTSA)        # Para hacer la prueba de efectos ARCH
library(astsa)       # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA  
library(xts)         # Para utilizar objetos xts 
library(tidyverse)   # Conjunto de paquetes (incluye dplyr y ggplot2)
library(car)         # Para usar la función qqPlot

#---- Explicación del Script -----

# Inicialmente vamos a simular diferentes Procesos Generadores de datos para analizar sus características
# Por ende, se van a simular procesos autoregresivos, de media móvil y ARMA
# En total se van a simular 11 series y se van a simular tanto series estacionarias como no estacionarias

#---

#---- Comentarios sobre el manejo de series de tiempo en R -----

# En R uno puede trabajar series de tiempo con 3 tipos de objetos:
### ts  (Intervalos de tiempo fijos, objeto estándar de series de tiempo en R)
### zoo
### xts (muy útil para el modelamiento de series financieros con intervalos de tiempo que nos son fijos)

# Es muy común utilizar objetos ts en el modelamiento de series de tiempo en R. 
# En el presente script, se va a ser usó de ese tipo de objeto para modelar las series de tiempo. 
# No obstante, a la hora de realizar análisis más avanzados de series de tiempo, como podría ser 
# el caso de trabajar con series de tiempo financieras, es indispensable conocer/manejar los paquetes
# zoo y xts para series de tiempo. Esos dos paquetes los estudiaremos en mayor detalle en futuras 
# monitorias. 

#---

# Número de observaciones para todas las series
obs =  1000 # cambiar el valor para ajustar el número de datos de todas las series

# Nota: La razón por la que se utilizan tantas observaciones no es por capricho sino para mostrar mejor los resultados de las simulaciones
#       de las diferentes series propuestas. Recuerden, que las propiedades teóricas de los diferentes procesos estacionarios que ven en 
#       clase se basan en teoría de grandes muestras (en realidad de tener procesos infinitos). Por tanto, entre más grande el tamaño de 
#       muestra, mejor se van a ver los resultados de las simulación. En la práctica, es muy difícil tener muestras tan grandes para series
#       de tiempo (y más aún si se trabaja con series macro de carácter mensual o trimestral), por lo que en la práctica es usual trabajar 
#       con series con mucho menos observaciones (e.g. 75 observaciones por decir algo)

# Características de las simulaciones ---- 

# Se empleará el comando arima.sim, que ya viene incluído en R, para simular de manera automática diferentes procesos de series de tiempo
# univariados (tanto estacionarios como no estacionarios). 

# De igual forma, se simular la innovación (también conocido como el error) del proceso mediante una simulación de una variable 
# aleatoria normal estándar. Este es un supuesta simplificador, para explorar con más facilidad las propiedades estadísticas de los
# diferentes procesos generadores de series de tiempo que simularemos. 

# En la práctica, es muy común que los errores de varias series de tiempo que trabajen en la práctica no sean normales y por el 
# contrario tengan una distribución diferente a la normal e inclusive una distribución no conocida. En caso tal de que los errores 
# no distribuyan de manera normal, en caso de que se quiera hacer algúntipo de inferencia estadística sobre la serie, va a ser
# necesario emplear métodos de simulación (como bootstrapping) que permite aproximar mejor la distribución real de los errores 
# (a partir de remuestreos de la muestra). La simulación por bootstrapping la pueden ver ya sea en el libro de series de timepo Enders 
# o en el libro de probabilidad de´ DeGroot

# ---

# Nota: Una gráfica de una serie de tiempo puede dar indicios de si el proceso es estacionario o no lo es. Si la gráfica muestra un movimiento 
#       alrededor de una media de largo plazo y además la varianza de la serie paraece ser constante, a indicios de que la serie tiene un comportamiento
#       estacionario. Por el contrario, si la serie no fluctua alrededor de una media o no pareciera tener una varianza constante y por el contrario
#       mostrará clusters de volatilidad (momentos de mayor fluctuación y momentos de menor fluctuación), entonces hay indicios gráficos de que la
#       serie no es estacionaria. No obstante, todo análisis gráfico de una serie tiene que ser siempre complementado por análisis de ACF y PACF
#       y por pruebas formales (como lo son pruebas de raíz unitaria para determinar el orden de integración de una serie). 

#---- Simulación de un proceso de ruido blanco ----

set.seed(1)
yt_rb = ts(arima.sim(model= list(order = c(0,0,0)), n = obs, 
                     innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

# Graficación 
x11()
autoplot(yt_rb, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,0,0) (Ruido Blanco)",lty=1, lwd=0.25, col="turquoise4") + theme_light()

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,0) (Ruido Blanco)
lags=20
x11()
par(mfrow=c(1,2))
acf(yt_rb,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,0) (Ruido Blanco)') 
pacf(yt_rb,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,0) (Ruido Blanco)')
# Versíón ggplot:
x11()
ggAcf(yt_rb,lag.max=lags,plot=T,lwd=2) + ggtitle('ACF del PGD ARIMA(0,0,0) (Ruido Blanco)') + theme_light()
x11()
ggPacf(yt_rb,lag.max=lags,plot=T,lwd=2) + ggtitle('PACF del PGD ARIMA(0,0,0) (Ruido Blanco)') + theme_light()

#---- Simulación de un proceso AR(1) estacionario ----

set.seed(8202) #Para que los valores de la simulación no cambien (facilita la reproducibilidad de los resultados )

yt1s = ts(arima.sim(model= list(order = c(1,0,0), ar=c(0.6)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt1s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (1,0,0)",lty=1, lwd=0.4, col="turquoise4") + theme_light()

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,0,0)
x11()
lags=20
par(mfrow=c(1,2))
acf(yt1s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,0,0)') 
pacf(yt1s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,0,0)')

#Otro comando para encontrar la acf y la pacf simultáneamente 
x11()
acf2(yt1s, max.lag = lags)

#---- Simulación de un proceso AR(1) no estacionario (ARIMA(1,1,0)) ----

# Nota: No se preocupen por endender que es un modelo ARIMA(p,d,q). Eso lo vamos a ver con más detalles en futuros scripts

set.seed(29101) #Para que los valores de la simulación no cambien
yt1n = ts(arima.sim(model= list(order = c(1,1,0), ar=c(0.1)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt1n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (1,1,0)",lty=1, lwd=0.4, col="darkblue") + theme_light()

# Claramente la gráfica muestra un proceso no estacionario. Se puede ver que el proceso pareciera no tener una media constante a la que converge
# en el largo plazo. Adicionalmente, pareciera ser un proceso altamente persistente, por lo que no satisface otras de las propiedades
# de un proceso estacionario que es ser débilmente dependiente. 

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,1,0) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente y por ende tampoco es estacionario.
x11()
lags=20
par(mfrow=c(1,2))
acf(yt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,0)') 
pacf(yt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,0)')

# Los procesos no estacionarios se caracterizar porque no hay una caída geométrica de la "ACF". Cuándo hay un caída 
# muy lenta de la ACF (que pareciera tener una caída "lineal" lenta), es un indicio muy fuerte de que el proces no
# es estacionario. 
# No obstante, desde ya se les reitera la importancia de complementar el análisis gráfico de una series de tiempo
# con análisis formal (usando pruebas formales), como los son las de pruebas de raíz unitaria, para saber si una series 
# es estacionaria o no lo es y para conocer el orden de integración de una serie (Todo esto lo iremos aprendendiendo a lo largo
# de la monitoria en futuros scripts)

# Vamos a diferenciar la serie 
# (Diferencia la serie es la metodología que usualmente se emplea cuando una serie presenta tendencias estocásticas.
# La presencia de tendencias estocásticas en una series hace que la serie no sea estacionaria). (Esto se profundizará en futuras monitorias)

diffyt1n <- diff(yt1n)
x11()
autoplot(diffyt1n, main = "Diferenciación de una serie I(1)", col="turquoise4", lty=1, lwd=0.25) + theme_light()

# Como se puede observar parece que a pesar de que la serie original no es estacionaria
# su primera diferencia sí lo es. 

# Para corroborar lo anterior, vemos como cambian la ACF y PACF de la serie diferenciada

x11()
lags=20
par(mfrow=c(1,2))
acf(diffyt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,0)') 
pacf(diffyt1n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,0)')

# Claramente, se observa que la ACF ahora cae de manera rápida y "geométrica" 
# lo que es un indicativo de que la primera diferencia de la serie es estacionaria 

# Nota: Si bien los métodos gráficos son un buena guía para saber si una serie
#       es estacionaria o no lo es, es fundamental utilizar pruebas formales
#       para corroborar lo que los métodos gráficos pueden estar reflejando. 
#       dichas pruebas formales las veremos en metodologías futuras. 

#---- Simulación de un proceso AR(2) estacionario ----

set.seed(2929) #Para que los valores de la simulación no cambien
yt2s = ts(arima.sim(model= list(order = c(2,0,0), ar=c(0.6,0.3)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt2s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (2,0,0)",lty=1, lwd=0.4, col="green4") + theme_light()

# El proceso gráficamente sigue pareciendo estacionario, pero claramente muestra una mayor persistencia a comparación del modelo AR(1).
# Esto se debe a que el orden mayor del AR es indicativo de que los valores presentes de la serie dependen no solo del primer sino también
# del segundo rezago, lo que sugiere una mayor persistencia en el proceso como lo muestra la gráfica. Se les sugiere volver a hacer la gráfica
# del proceso AR(1) estacionario y corroborar dicha gráfica con el proceso AR(2) estacionario y contrasten ambas gráficas. 

#Vamos a hacer la ACF y PACF del PGD ARIMA(2,0,0)
x11()
lags=20
par(mfrow=c(1,2))
acf(yt2s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(2,0,0)') 
pacf(yt2s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(2,0,0)')

# Se puede ver la caída "geométrica" de la ACF característica de las series estacionarias. No obstante, se muestra que dicha caída
# es más lenta a comparación del proceso AR(1) estacionario, lo que es un indicativo de que un proceso AR(2) estacionario presenta
# mayor persistencia que un proceso AR(1) estacionario. Esto, nuevamente, se explica porque la observación actual depende de más 
# rezagos del pasado de la serie. Se les sugiere que contrasten la ACF del AR(1) estacionrio con la ACF del AR(2) estacionario y los contrasten. 

#---- Simulación de un proceso AR(2) no estacionario (ARIMA(2,1,0)) ----

set.seed(0202) #Para que los valores de la simulación no cambien
yt2n = ts(arima.sim(model= list(order = c(2,1,0), ar=c(0.6,0.3), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1)) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt2n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (2,1,0)",lty=1, lwd=0.9, col="green4") + theme_light()

# Claramente se ve que es un proceso no estacionario, porque no hay indicios gráficamente
# de que la serie converga a una media en el largo plazo 

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,1,0) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente.
x11()
lags=20
par(mfrow=c(1,2))
acf(yt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(2,1,0)') 
pacf(yt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(2,1,0)')
        
# Nuevamente, se observa esa caída lenta "lineal" en la ACF de la serie no estacionaria 

# Vamos a diferenciar la serie y ver que pasa con la serie diferenciada 

diffyt2n <- diff(yt2n)
x11()
autoplot(diffyt2n, main = "Diferenciación de una serie I(1)", col="turquoise4")  + theme_light()

# Pareciera gráficamente que la primera diferencia de la series original es estacionaria

# Vemos como cambian la ACF y PACF de la serie diferenciada
        
x11()
lags=20
par(mfrow=c(1,2))
acf(diffyt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,0)') 
pacf(diffyt2n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,0)')

# Como se puede ver, ahora se observa una caída "geométrica" de la ACF que es muy
# características de los proceso que son estacionarios. 
# Recordarrque no solo basta con mirar la gráfica y la ACF de una serie sino
# que dichos métodos gráficos se tienen que combinar con pruebas formales para
# determinar si una serie es estacionaria o no lo es. 

#---

#---- Simulación de un MA(1) estacionario ----

# Nota: Los modelos MA(q) puros generlamente son procesos menos persistente que los 
#       procesos AR(p) puros. 

set.seed(2020) #Para que los valores de la simulación no cambien
yt3s = ts(arima.sim(model= list(order = c(0,0,1), ma=c(0.7)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt3s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,0,1)",lty=1, lwd=0.9, col="grey") + theme_light()

# Claramente, la gráfica muestra un proceso que parece ser estacionario 

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,1)
lags=20
x11()
par(mfrow=c(1,2))
acf(yt3s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,1)') 
pacf(yt3s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,1)')


#---- Simulación de un MA(1) no estacionario (ARIMA(0,1,1)) ----

set.seed(0891) #Para que los valores de la simulación no cambien
yt3n = ts(arima.sim(model= list(order = c(0,1,1), ma=c(0.7)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt3n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,1,1)",lty=1, lwd=0.8, col="black") + theme_light()

# Nota: La gráfica de la serie parace mostrar una serie no estacionaria en la medida
#       de que la series no parecer converger a una media de largo plazo y además 
#       parece haber alta persistencia en la serie, otra característica usual en las 
#       series no estacionarias 

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,1,1) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente.
lags=20
x11()
par(mfrow=c(1,2))
acf(yt3n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,1,1)') 
pacf(yt3n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,1,1)')

# Caída "lineal" lenta de la ACF

#--- 

#---- Simulación de un MA(2) estacionario ----

set.seed(10181) #Para que los valores de la simulación no cambien
yt4s = ts(arima.sim(model= list(order = c(0,0,2), ma=c(0.7,0.25)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt4s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,0,2)",lty=1, lwd=0.18, col="turquoise4") + theme_light()

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,0,2)
lags=20
x11()
par(mfrow=c(1,2))
acf(yt4s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,0,2)') 
pacf(yt4s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,0,2)')


#---- Simulación de un MA(2) no estacionario (ARIMA(0,1,2))----

set.seed(9171) #Para que los valores de la simulación no cambien
yt4n = ts(arima.sim(model= list(order = c(0,1,2), ma=c(0.7,0.25)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt4n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (0,1,2)",lty=1, lwd=0.4, col="green4") + theme_light()

# La gráfica de la serie parece indicar un proceso no estacionario dado que no pareciera
# haber convergencia hacia una media de largo plazo y parece reflejar una alta persistencia
# en el proceso generador de datos.

#Vamos a hacer la ACF y PACF del PGD ARIMA(0,1,2) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente.
x11()
lags=20
par(mfrow=c(1,2))
acf(yt4n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(0,1,2)') 
pacf(yt4n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(0,1,2)')

# Nota: Se obsera la típica caída "lineal" lenta en la ACF de un proceso no estacionario 

#---

#---- Simulación de un ARMA(1,1) estacionario ----

# Nota: Los modelos ARMA(p, q) son convenientes en la medida que permiten representar una amplia cantidad de procesos estacionarios de manera
#       parsimoniosa (i.e. con muy pocos coeficientes). De este forma, en lugar de representar un proceson con muchos rezagos, es decir, mediante
#       un AR(p) con un p muy grande, se puede encontrar una representación más parsimoniosa del proceso AR(p,q) que tenga menos parámetros. 
#       Es así que la componente de media móvil en un ARMA ayuda a ajustar mejor el modelo y reduce la necesidad de incluir tantos reazagos autoregresivos
#       para representar el proceso que se está estudiando. Finalmente, recuerden que a la hora de trabajar con series de tiempo es conveniente 
#       trabajar modelos parsimoniosos en la medida que le dan mayor flexibilidad al modelo (al menos en el pronóstico) y además consumen menos
#       grados de libertad. 

set.seed(220422) #Para que los valores de la simulación no cambien
yt5s = ts(arima.sim(model= list(order = c(1,0,1), ar=c(0.7),ma=c(0.3)), n = 1000, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt5s, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (1,0,1)",lty=1, lwd=0.7, col="orange") + theme_light()

# El proceso ARMA(1,1) gráficamente parece ser estacionario 

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,0,1)
lags=20
x11()
par(mfrow=c(1,2))
acf(yt5s,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,0,1)') 
pacf(yt5s,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,0,1)')

# Nota: Para un proceso ARMA(p,q) es más complejo el análisis de ACF y PACF. Ésto se debe a que a diferencia de una AR puro o un MA puro 
#       puede que las ACF y la PAFC del ARMA(p,q) no se corten abruptamente como si pasa en los procesos "puros". No obstante, como ya se 
#       mencionó, un proceso ARMA(p, q) permite mayor flexibilidad en el modelamiento y además generá modelos más parsimoniosos a comparación
#       de los modelos "puros". 
#       Se destaca además, que respecto a la ACF se observa la típica caída "geométrica" que uno esperaría observar de una serie estacionaria. 

#---- Simulación de un ARMA(1,1) no estacionario (ARIMA(1,1,1))----

set.seed(81711) #Para que los valores de la simulación no cambien
yt5n = ts(arima.sim(model= list(order = c(1,1,1),ar=c(0.6), ma=c(0.7)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

#Graficamos
x11()
autoplot(yt5n, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (1,1,1)",lty=1, lwd=0.7, col="red2") + theme_light()

# Nuevamente, la gráfica de la serie parece mostrar que esta no se estabailiza alrededor de una media de largo plazo. Adicionalmente, se ve
# persistencia en el proceso, contrario a lo que uno esperaría ver en una serie estacionaria. Ambas, dan indicio a que la serie pareciera no ser
# estacionaria. 

#Vamos a hacer la ACF y PACF del PGD ARIMA(1,1,1) en donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente.
lags=20
x11()
par(mfrow=c(1,2))
acf(yt5n,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PGD ARIMA(1,1,1)') 
pacf(yt5n,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PGD ARIMA(1,1,1)')

# Se observa la típica caída "lineal" lenta que uno esperaría ver en una serie no estacionaria. 

#Ejemplo

ts3 <- arima.sim(list(order = c(1,0,1), ar = 0.7, ma=0.85), n= 2000)
par(mfrow=c(2,1))
acf(ts3)
pacf(ts3)


