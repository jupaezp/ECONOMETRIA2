##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2022-I
#      SESIÓN MONITORIA : Metodologia Box-Jenkins para la identificación, 
#               estimación y pronóstico de series de tiempo univariadas
#               Series modeladas: 
#                   - Índice de Producción Industrial (IPI)
#                   - Tasa de desempleo 
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
# Paquetes del tidyverts
library(fable)       # Forma moderna de hacer pronóstiocs en R (se recomienda su uso)  
library(tsibble)     # Para poder emplear objetos de series de tiempo tsibble
library(feasts)      # Provee una colección de herramientas para el análisis de datos de series de tiempo 
 
  
# Limpiar el Environment
rm(list = ls()) 

# Nota: En R uno puede trabajar series de tiempo con 4 objetos:
# ts --> Intervalos de tiempo fijos, objeto estándar de series de tiempo en R)
# zoo
# xts --> xts significa "eXtensible time series".  xts es una subclase de zoo.
# tsibble --> ts + tibble (data frame que permite manejar series de tiempo)

# Nota: Sobre los objetos ts
# Es el tipo de objeto estándar para manejar series de tiempo en R
# Su principal ventaja es que son prácticamente compatibles con cualquier función diseñada para tabajar series de tiempo en R
# lo que los hace objetos muy universales y muy comúnmente usados
# Adicionalmente, prácticamente cualquier otro tipo de objeto de serie de tiempo se puede transformar fácilmente a un 
# objetos ts usando el comando as.ts()
# Tienen la desventaja de que solo funcionan con periodos o intervalos de tiempo fijos (mensual, trimestral, anual)
# Además, fueron los primeros objetos en diseñarse para trabajar series de tiempo en R por lo que no tienen todas las 
# funcionalidades que sí tienen otros objetos de series de tiempo más modernos como xts o tsibble

# Nota: Sobre los objetos zoo
# Son objetos para manejar series de tiempo que fueron diseñados para tener funcionalidades adicionales frente a los objetos ts
# Hoy en día se usan más los objetos que resultaron de su evolución los cuales se conocen como xts

# Nota: Sobre los objetos xts
# Son objetos para manejar series de tiempo que están diseñados para ser más flexibles y poderosos en términos 
# de modelamiento; diseñados para facilitar el uso de las series temporales.  
# Además, son muy útiles para el modelamiento de series financieros con intervalos de tiempo que nos son fijos.
# Están basados en zoo, pero son una versión avanzada de éstos. 

# Nota: Sobre los objetos tsibble
# Son objetos diseñados para ser compatibles con los paquetes "tidyverse" de R, lo que implica
# que se pueden emplear directamente en ellos las funciones de ggplot2 (graficación) y 
# de dplyr(manipulación de base de datos) del tidyverse
# Básicamente, son parecidos a un dataframe pero que permite manejar series de tiempo, lo que permite
# que trabajar varias series de tiempo de manera simultanea sea muy sencillo
# Su nombre es una combinación de: ts + tibble. ts para denotar que maneja series de tiempo y 
# tibble para denotar que son una forma especila de tibble (recuerden que tibble es un tipo 
# de dataframe destinado a trabajar optimamente con los paquetes del tidyverse)
# Son necesarios para emplear las funciones del paquete *fable*, por lo que tienen que transformar
# sus series a este tipo de objeto antes de poder utilizar las funciones de *fable*
# Para transformar un objeto de serie de tiempo a tsibble se usa el comando as_tsibble(). 
# En la actualidad solo es posible convertir directamente de objeto ts a tsibble, no es posible
# hacer la conversión directa entre xts a tsibble
# Para aprender más sobre tsibble revisar: https://tsibble.tidyverts.org/

# Nota: Tener en cuenta que para cualquier modelamiento con series de tiempo lo primero que hay que investigar
#       es si la serie está desestacionalizada o no. Si la serie presenta estacionalidad, hay dos alternativas:
## 1. Desestacionalizar la serie de tiempo empleando alguna metodología como X-13 que emplea la FED y se encuetra 
##    disponible en la función *seas* del paqueta *seasonal*
## 2. Modelar la estacionalidad en la serie, por ejemplo mediante un modelo SARIMA

# Ojo: Para los talleres se les aconseja buscar series que ya se encuentren desetacionalizadas. Una buena fuente de datos
#      que incluye muchas series desestacionaliadas (en inglés seasonal-adjusted) se encuentran en la página de la 
#      reserva federal de St. Louis: https://www.stlouisfed.org/

# Nota: Hay dos formas de emplear la metodología Box-Jenkins en R: 
## 1. Utilizando las funcionalidades estándar en R
## 2. Utilizand el paquete fable (https://otexts.com/fpp3/arima-r.html)

#
#---- Industrial Production Index (indprod) ----
#

# Vamos a trabajar con el industrial production index de los EE.UU. el cual es un indicador económico
# que mide la producción real de los Estados Unidos en términos de manufactura, minería, electicidad y gas. 

# Ojo: Al ser un índice tiene que tener un año base en el cuál el valor del índice sea 100
       # (para el ejemplo el año base fue 2012, es decir Index 2012=100)
       # Valores mayores a 100 indican que para ese año corriente la producción manufacturera fue mayor que en el año base
       # Valores menores a 100 indican que para ese año corriente la producción manufacturera fue menor que en el año base

# Vamos a trabajar con una serie del índice de produccion industrial (indprod) de Estados Unidos 
# Fuente: https://fred.stlouisfed.org/series/INDPRO#0 (Tomados de la FRED de la Reserva Federa de St. Louis)


#---- PASO 1- IDENTIFICACIÓN indprod ----

# Se cargan las series de tiempo
base_fred <- read_excel(file.choose())

# Visualización de la base de datos
glimpse(base_fred)

#Convertir la serie en un objeto tipo ts
indprod = ts(base_fred$INDPRO, start = 1960, frequency = 4) # Se coloca 4 para indiciar que la frecuencia es trimestral

# Visualización de los serie de tiempo
glimpse(indprod)  

# Crear un objeto tipo xts
indprod_xts = xts(base_fred$INDPRO, 
                  order.by = base_fred$observation_date) # Importar como un objeto xts

# Ver la clase de cada uno de los ejemplos de series de tiempo
class(indprod)
class(indprod_xts)

### Gráficas

## Para el objeto ts
x11()   
# Usando la función estándar de gráficación en R
plot.ts(indprod, xlab="",ylab="", main="IPI trimestral de Estados Unidos (1960-2012) (Index 2012 = 100)",lty=1, lwd=2, col="blue")
# Usando ggplot2
autoplot(indprod, col="turquoise4", main = "IPI trimestre de Estados Unidos (1960-2012) (Index 2012 = 100)",xlab = "Fecha", ylab="", lwd=1) + theme_light()

#  Para el objeto xts
x11()
# Gráfica mucho más linda e informativa
plot(indprod_xts, main = "IPI trimestre de Estados Unidos (1960-2012) (Index 2012 = 100)", ylab  = "IPI")

#Vamos a graficar la ACF y PACF de la serie en nivel.
lags=24
x11()
par(mfrow=c(1,2))
acf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del IPI de USA') 
pacf(indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del IPI de USA')

# Versíón ggplot:
ggAcf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del IPI de USA") + theme_light()
ggPacf(indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del IPI de USA") + theme_light()
# Versión paquete astsa (sirve tanto para objetos ts como para objetos xts)
acf2(indprod)

#Como podemos ver, la FAC tiene un decaimento lento a cero,lo que nos da indicios de que el proceso no es estacionario.

# Nota: Cuándo hay un caída muy lenta de la ACF (que pareciera tener una caída "lineal" lenta), es un indicio 
# muy fuerte de que el proceso no es estacionario. 

# Prueba de Augment Dickey Fuller

# La ADF es una prueba estándar muy utilizada para saber si una serie de tiempo tiene al menos una raíz unitaria o no
# La presencia de una raíz unaitaria hace que la serie de tiempo estudiada no sea estacionaria. A la hora de utilizar
# la metodología Box Jenkins para series ARIMA es importante trabajar con una serie estacionaria o con una una serie
# no estacionaria debidamente diferenciada para que la serie diferenciada (pueden ser varias diferencias) sea
# estacionaria y de ahí la importancia de la pruba ADF para la identificación de series estacionarias

# Dado que aún no han visto pruebas de raíz unitaria se colocará el resultado de la prueba por completitud 
# Y en scripts posteriores se les indicará como es la mecánica de la prueba y como interpretar sus resultados

# Prueba con trend

adf.trend_indprod= ur.df(indprod, type="trend", lags = 1); plot(adf.trend_indprod)
summary(adf.trend_indprod)

# Prueba con drift

adf.drift_indprod= ur.df(indprod, type="drift", lags = 1); plot(adf.drift_indprod)
summary(adf.drift_indprod) 

# Conclusión Prueba de Dickey Fuller: 

# 1. Los resultados de la prueba indican que la serie debería tener término de 
# deriva (drift)

# 2. Y que la serie no es estacionaria, por lo que hay que diferenciarla para 
# eliminar raíces unitarias

# Nota: presencia de raíz unitaria es lo mismo que la existencia de una tendencia 
#       estócástica en la serie. Solución: Diferenciar la serie.

# La serie presente una tedencia estocástica y el término de deriva es el que hace
# que los valores de la serie crezcan en el tiempo.

#Para estacionalizar la serie, es estándar en la práctica hacer tres transformaciones para ello: 

## 1. Aplicar diff(serie_original): 
      # Para diferencia la serie y eliminar la tendencia estocástica

d.indprod= diff(indprod) # serie diferenciada

## 2. Aplicar log(serie_original): 
      # para estabilizar la varianza de la serie

l.indprod=log(indprod) # serie que se le aplica solo el logaritmo 

## Aplicar diff(log(serie_original)): 
    # Para diferenciar y estabilizar la varianza de la serie
    # Se interpretra como una tasa de crecimiento   

dl.indprod= diff(log(indprod))*100   # diferencia de logaritmos de la serie (tasa de crecimiento)

#Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su valor en logaritmos.
x11()
par(mfrow=c(2,2))
plot.ts(indprod, xlab="",ylab="", main="IPI en nivel para Estados Unidos 1959-2019",lty=1, lwd=2, col="blue")
plot.ts(l.indprod, xlab="",ylab="", main="IPI en logaritmo para Estados Unidos 1959-2019",lty=1, lwd=2, col="black")
plot.ts(d.indprod, xlab="",ylab="", main="Variación del IPI para Estados Unidos 1959-2019",lty=1, lwd=2, col="red")
plot.ts(dl.indprod, xlab="",ylab="", main="Tasa de crecimiento del IPI para Estados Unidos 1959-2019",lty=1, lwd=2, col="green")

# Vamos a elegir la tasa de crecimiento de indprod debido a que la varianza es mucho más estable, pese a 
# tener la misma media constante que la serie en primera diferencia.

#Ahora hacemos la ACF y la PACF, para la tasa de crecimiento del indprod donde evidenciamos un proceso débilmente dependiente. 
x11()
lags=30
par(mfrow=c(1,2))
acf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la tasa de crecimiento del IPI') 
pacf(dl.indprod,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la tasa de crecimiento del IPI')

ggAcf(dl.indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del IPI de USA") + theme_light()
ggPacf(dl.indprod,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del IPI de USA") + theme_light()

acf2(dl.indprod)

# Se observa en la ACF la típica caída "geométrica" que uno esperaría observar de una serie estacionaria, lo cual es un indicativo
# de que la diferenciación si eliminó la raíz unitaria. 

##Si realizamos la prueba de DF sobre la serie diferenciada, podremos constatar que la serie ya sea estacionaria. 
ADF.dl.indprod <- ur.df(dl.indprod, type="drift", selectlags = "AIC")
summary(ADF.dl.indprod) #Rechazamos H0, así que la tasa de crecimiento del IPI es estacionaria en sentido débil.

#Vamos a analizar las estadísticas descriptivas de la serie en primera diferencia. 
describe(dl.indprod) #Parece que la media es distina a cero, por eso se incluye un intercepto en los arima.
describe(l.indprod)

#
# Uso de criterios de información para la identificación del proceso ARIMA que se esté modelando 
#

# Método manual para identificar el ARIMA usando criterios de información 

#Ahora vamos a ver lo que muestran los criterios AIC y BIC
AR.m <- 6 #Supondremos que el rezago autorregresivo máximo es 6 (pmax)
MA.m <- 6 #Supondremos que el rezago de promedio móvil máximo es 6. (qmax)

# 1.1 Funciones para idenficar modelo ARIMA por criterios de información (metodología manual) ----

# 1. Función para seleccionar el modelo ARIMA con el menor criterio de información

# Función que me permite crear un data frame para distintos modelos ARIMA(p,d,q)
# en donde cada fila del df me da el orden del modelo y los AIC y BIC correspondientes a dicho modelo
# Ojo: utlizar method = "ML" dentro de arima(), de lo contrario les arrojará error
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
# ARIMA(p, d, q). 

# Si se especifica d = 1, entonces se está indicando que el comando arima sea el que diferencie la serie por lo que 
# se debe prover la serie sin difereniar, en este caso proveo l.indprod a la función arma_seleccion_df

mod_d1_indprod = arma_seleccion_df(l.indprod, AR.m, MA.m, d = 1, TRUE, "ML")
View(mod_d1_indprod)

# Selecciono el mejor modelos según criterios de información cuando d=1
min_aic_lindprod = arma_min_AIC(mod_d1_indprod); min_aic_lindprod #ARIMA (3,1,5)
min_bic_lindprod = arma_min_BIC(mod_d1_indprod); min_bic_lindprod #ARIMA (1,1,0)

#Recuerden que en la metodología BIC prima la parsimonia, es decir, modelos con una cantidad menor de grados.

# Si se especifica d = 0, entonces se está indicando que el comando arima *NO* sea el que diferencie la serie por lo que 
# se debe prover la serie ya difereniada, en este caso proveo dl.indprod a la función arma_seleccion_df

mod_d0_indprod = arma_seleccion_df(dl.indprod, AR.m, MA.m, d = 0, TRUE, "ML")
View(mod_d0_indprod)

# Selecciono el mejor modelo según criterios de información cuando d=0
min_aic_dlindprod = arma_min_AIC(mod_d0_indprod); min_aic_dlindprod #ARIMA (2,0,4)
min_bic_dlindprod = arma_min_BIC(mod_d0_indprod); min_bic_dlindprod #ARIMA (2,0,4)

# 1.2 Método automático para identificar el ARIMA usando criterios de información ---- 

# 1.2.1 Usando la función auto.arima del paquete forecast ----

#
# Método automático para identificar el ARIMA usando criterios de información 
# Usando la función auto.arima() del paquete forecast 
#

# Nota: La misma función auto.arima es capaz de reconocer el orden de diferenciación para que la serie quede estacionaria. 
# En realidad, la función auto.arima calcula un modelo SARIMA porque además de 
# modelas la parte ARMA de la serie, también modela la componente estacional de la misma.
# Para mayor info. sobre los modelos SARIMA: Ver Enders Capítulo 2 - Sección 11: Seasonality
auto.arima(l.indprod, method = "ML")   # Nota: Se recomienda emplear mejor el procedimiento de fable en lugar del auto.arima

# 1.2.2 Usando la función ARIMA del paquete fable ----

# Nota: Para revisar las diferencias entre la función auto.arima de forecast y la función ARIMA de fable 
#       revisar: https://stackoverflow.com/questions/70333319/r-libraries-forecastauto-arima-vs-fablearima-whats-the-differences

#
# Notas sobre el paquete fable: 
#

## fable fue diseñado para reemplazar al paquete forecast (ambos paquetes están diseñados para hacer pronósticos 
## de series de tiempo). Además, ambos paquetes fueron diseñados por Rob J Hyndman, no obstante, el paquete fable
## es más avanzado y más moderno que forecast (es en muchos aspectos como su evolución).
## fable ofrece una colección de modelos de pronóstico para series de tiempo, tanto univariadas como
## multivariadas (pronósticos en modelos ARIMA, VAR y mucho más).
## Adicionalmente, fable hace parte de una colección de paquetes que se conoce como "tidyverts" que está diseñado 
## para trabajar de manera práctica series de tiempo usando el entorno de trabajo del "tidyverse"
## Algunos links de interés relacionados con el paquete fable son: 
### Link de referencia con las funciones disponibles en fable: https://fable.tidyverts.org/reference/index.html
### Como emplear el comando ARIMA de fable: https://otexts.com/fpp3/arima-r.html
### Para instalación y generalidades del paquete: https://fable.tidyverts.org/index.html
### Documentación sobre el conjunto de paquetes del tidyverts: https://tidyverts.org/
### Libro oficial del autor del paquete para el manejo de series de tiempo en R: https://otexts.com/fpp3/index.html
### página de CRAN para el paquete fable: https://cran.r-project.org/web/packages/fable/index.html

# Acá emplearemos una novedosa metodología basada en el paquete fable. 
# Lo primero que hay que tener en cuenta para emplear el paquete fable, es que ahora los objetos de series de tiempo
# tienen que ser *tsibble* para que las funciones del paquete puedan funcionar. 
# Esto no es una limitación, en la medida de que es muy fácil transformar de objetos ts a tsibble de la siguiente 
# manera: 

l.indprod_tsibble = as_tsibble(l.indprod)
# Nota: La conversión entre objetos ts a tsibble y viceversa es muy sencilla, pero en estos momentos no hay forma de 
#       hacer la conversión con objetos xts.

# Revisar el tipo de objeto y el contenido del tsibble que se acaba de crear:
class(l.indprod_tsibble) # indprod_tsibble es un dataframe adaptado para trabajar con series de tiempo 
glimpse(l.indprod_tsibble)

# Como se puede observar, tsibble es un tipo de dataframe diseñado o adaptado para trabajar con series de tiempo
# Una de las ventajas de emplear tsibble radica en que, al ser un dataframe, permite trabajar con varias series de tiempo
# al mismo tiempo. 
# Otra razón por la que es conveniente emplear tsibble es que al ser un tibble (tibble son los dataframes que se emplean
# en los paquetes del tidyverts) permite emplear con facilidad toda las metodologías y funcionalidades de los paquetes del
# tidyverts como lo son: ggplot2, dplyr, tidyr, lubridate y demás.

#Los objetos tipos tsibble también permiten graficar la serie de tiempo y analizar su FAC y FACP
x11()
l.indprod_tsibble %>%
  gg_tsdisplay(value, plot_type='partial')

# Así como realizar la gráfica y la FAC y la FACP de la primera diferencia de la serie
x11()
l.indprod_tsibble %>%
  gg_tsdisplay(difference(value), plot_type='partial')

# Para identiciar el modelo ARIMA usando criterios de información se empela la función ARIMA del paquete fable.
# La identificación se puede hacer usando AIC o BIC como criterios de información. 
# Existen dor formas en las que ARIMA puede identificar el mejor modelo usando criterios de inforamción: 
## 1. stepwise (método default): metodología simplificado para encontrar los parámetros p,d,q
##    empleando el algoritmo Hyndman-Khandakar.
## 2. search: realiza una busqueda más exhuastiva del mejor modelo ARIMA dada la series de tiempo.

# Nota: La función pivot_longer que se emplea para mostrar los resultados es del paquete tidyr por si quieren
# revisar en que consiste (https://tidyr.tidyverse.org/reference/pivot_longer.html)

# Usando el criterio de información AIC
l.indprod_fit_aic <- l.indprod_tsibble %>%
  model(stepwise = ARIMA(value, ic = "aic"),
        search = ARIMA(value, ic = "aic", stepwise=FALSE))

l.indprod_fit_aic %>% pivot_longer(everything(), names_to = "Model name",
                         values_to = "Orders")

# Por criterio de información AIC: 
## El método stepwise me recomienda: ARIMA(1,1,0)(1,0,2)[4] (Es decir, un modelo SARIMA que modela la parte estacional)
## El método search me recomienda: ARIMA(4,1,2) (Es decir, un modelo ARIMA sin parte estacional) 

# Usando el criterio de información BIC
l.indprod_fit_bic <- l.indprod_tsibble %>%
  model(stepwise = ARIMA(value, ic = "bic"),
        search = ARIMA(value, ic = "bic", stepwise=FALSE))

l.indprod_fit_bic %>% pivot_longer(everything(), names_to = "Model name",
                                   values_to = "Orders")

# Por criterio de información BIC: 
## El método stepwise me recomienda: ARIMA(1,1,0)(2,0,0)[4] (Es decir, un modelo SARIMA que modela la parte estacional)
## El método search me recomienda: ARIMA(1,1,0)(2,0,0)[4] (Es decir, un modelo SARIMA que modela la parte estacional)

# Nota: En nuestro caso en particular, se tomo una series desestacionalizada de la FRED, por lo que no hay necesidad de modelar
#       un modelo que modele la parte estacional. 

# 
# Nota: Para explorar más sobre como estimar y realizar una metodología Box Jenkins empleando el paquete fable
#       revisar: https://otexts.com/fpp3/arima-r.html
#

# 
# Ojo: Muy importante!
#
# En la práctica, lo mejor es usar tanto el método manual como el automático (en particular, el método ARIMA de fable)
# Basarse exclusivamente en el modelo auto.arima o ARIMA generalmente conduce a 
# conlusiones erroneas, por lo que no basta solo usar el modelo auto.arima.
# El método manual siempre debe realizarse y debe ser la principal guía,
# El auto.arima o ARIMA de fable debe verse como un método de identificación complementario. 
# En la práctica, es usual de que a veces los dos procedimientos difieran en los ordenes p y q del modelo ARIMA
# por lo que se recomienda más guiarse por los resultados del procedimiento manual. 

# Ojo: Es muy importante a la hora de seleccionar un modelo ARIMA para explicar 
# el proceso generador de datos usar tanto 
#la gráfica de la serie, las ACF y la PACF y los Criterios de Información.

# Nota: Generalmente los modelos del criterio de información BIC son más parsiomoniosos que el AIC. Si bien, esto es 
#       conveniente en el sentido que hay que estimar pocos parámetros, puede que si el modelo resulta ser demasiado 
#       parsiomonioso no pueda modelar correctamente la estructura de correlacion en la serie por lo que los residuales
#       no se compartan como ruido blanco. En este caso, es necesario emplear el modelo sugerido por el AIC u otro modelo
#       menos parsimonioso. Es muy importante, que en la validación de supuestos los residuales del modelo ARIMA se comporten
#       como ruido blanco, en particular, es muy importante que se satisfaga el supuesto de no correlación serial en 
#       los residuales. Si este supuesto no se satisface, entonces hay estrucutras de correlación en la serie que no se 
#       están modelando apropiadamente y por ende hay que nuevamente seleccionar un nuevo modelo ARIMA. 

#---- PASO 2- ESTIMACIÓN indprod ----

##
# Una vez se ha identificado la serie, se procede a estimar el modelo ARIMA. 
# Debe tomarse en cuenta que incluir muy pocos rezagos puede llevar a que los 
# residuales del modelo no se comporten como un Ruido Blanco y que incluir 
# muchos parámetros puede llevar a que nuestro modelo no sea parsimonioso; se 
# pierdan muchos grados de libertad y se pierda eficiencia.
##

# Existen 3 métodos de estimación para la función arima: (por default los 3 comandos utilizan CSS)
## ML: Máxima verosimilitud (más preciso y usualmente la mejor opción para bases pequeñas donde se pueda encontrar una solución analítica)
## CSS: (más veloz generalmente. se usa como aproximación o para bases de datos grandisimas)
## CSS-ML: Una combinación de ambas

### ML puede no converger
### CSS puede no hacer estimaciones lo suficientemente precisas y arrojar error 

# Para más información ver: 
# https://stats.stackexchange.com/questions/209730/fitting-methods-in-arima#:~:text=CSS%20sets%20the%20initial%20observations,an%20estimate%20of%20these%20values.


# Nota: Para hacer la estimación de un modelo arima se pueden utilizar 3 funciones distintas: 
## arima: función del paquete stats que es la más usual emplear y sirve para hacer predicciones con la función forecast del paquete forecast
## Arima: función del paquete forecast que es básicamente un wrapper de la función arima
## sarima: función del paquete astsa que permite estimar modelos sarima.

# ojo: tanto la función Arima como la función sarima están construidas sobre la función arima()
     # Cualquier de las 3 funciones para estimar un ARIMA permiten estimar modelos SARIMA
     # Es decir, son capaces de modelar la componente estacional de una serie.

# Nota: Cómo ya se indicó previamente, para mayor precisión en las estimaciones
      # y para evitar algunos inconvenientes que a veces ocurren al utilizar 
      # el método CSS la estimación del arima se hará mediante Máxima Verosimilitud.

# Para la serie de tiempo emplearemos los resultados del criterio de información BIC. No obstante, en caso de 
# no satisfacerse los supuestos de ruido blanco de los residuales del modelo, se deberían emplear los modelos seleccionados
# por AIC o escoger modelos ARIMA con p y q más altos (e.g. usando los segundos o terceros modelos que minimicen el BIC o el AIC). 

#Adicionalmente, vamos a priorizar los modelos donde el paquete arima haga él mismo la diferencia. 
#Por consiguiente, el orden de las estimaciones del modelo será: primero, arima(1,1,0) determinado por el BIC de manera manual. 
#Si no se cumplen los supuestos de ruido blanco, se procederá con el modelo arima(3,1,5) dado por el AIC. Finalmente, 
#tendremos el modelo dado por el paquete fable, el cual es un arima(4,1,2) para el criterio de información AIC. 

# 1. Utilizando el modelo en donde la función arima hace la diferenciación (por tanto se emplea la serie l.indprod)
#con los grados obtenidos por el BIC. 
arima_1.1.0_lindprod = arima(l.indprod, order = c(1,1,0), include.mean = T, method = "ML"); summary(arima_1.1.0_lindprod) # modelamiento ARIMA (1,1,0)

# 2. Utilizando el modelo en donde la función arima hace la diferenciación (por tanto se emplea la serie l.indprod)
#con los grados obtenidos por el AIC. 
arima_3.1.5_lindprod = arima(l.indprod, order = c(3,1,5), include.mean = T, method = "ML"); summary(arima_3.1.5_lindprod) # modelamiento ARIMA (3,1,5)

# 3. Modelo sugerido por la función fable con el criterio de información AIC. 
arima_4.1.2_lindprod = arima(l.indprod, order = c(4,1,2), include.mean = T, method = "ML"); summary(arima_4.1.2_lindprod) # modelamiento ARIMA (4,1,2)

# A manera pedagógica, vamos a estimar el modelo donde la función arima *NO* hace la diferenciación (por tanto se emplea la serie dl.indprod)
#para la graficación del pronóstico.
arima_2.0.4_dlindprod = arima(dl.indprod, order = c(2,0,4), include.mean = T, method = "ML"); summary(arima_2.0.4_dlindprod) # modelamiento ARIMA (2,0,4)

# Uso de stargazer.
## Generación de tabla para exportar los resultados de los modelos ARIMA estimados
## El paquete stargazer permite exportar las tablas ya sea a latex o a word (text)
## Para revisar como emplear el paquete stargazer
## revisar: https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf
stargazer(arima_1.1.0_lindprod, arima_3.1.5_lindprod, 
          column.labels=c("ARIMA(1,1,0)", "ARIMA(3,1,5)"),keep.stat=c("n","rsq"), 
          type = "text", style = "aer")
# type: es para especificar el tipo de tabla "latex" para latex y "text" para word
# style: para especificar el estilo de la tabla. "aer" para usar el estilo de la American Economic Review, 
#        "qje" para usar el estilo de Quaterly Journal of Economics

# Nota: En la praćtica no nos interesa mucho la significancia de los coeficientes del modelo ARIMA. Cuando se hace
#       un modelo ARIMA, lo que más nos interesa son los pronósticos a corto plazo que pueda generar el modelo 

# Para realizar directamente la estimación del modelo ARIMA utilizando el paquete fable
# es posible hacerlo empleando nuevamente la función ARIMA pero está vez especificando el valor de p, d y q
# en lugar de que el comando lo seleccione automáticamente. 

# Modelo arima(1,1,0): 

arima_1.1.0_lindprod_fable  = as_tsibble(l.indprod) %>%
  model(arima110 = ARIMA(value ~ pdq(1,1,0) + PDQ(0, 0, 0)))

glimpse(arima_1.1.0_lindprod_fable)

#Modelo arima(3,1,5)

arima_3.1.5_lindprod_fable  = as_tsibble(l.indprod) %>%
  model(arima315 = ARIMA(value ~ 1 + pdq(3,1,5) + PDQ(0, 0, 0)))

glimpse(arima_3.1.5_lindprod_fable)

#Modelo arima(4,1,2)

arima_4.1.2_lindprod_fable  = as_tsibble(l.indprod) %>%
  model(arima412 = ARIMA(value ~ pdq(4,1,2) + PDQ(0, 0, 0)))

glimpse(arima_4.1.2_lindprod_fable)

# Ojo: es importante indicar en el modelo ARIMA que no vamos a modelar compontente estacional en la serie, 
#      i.e. es importante especifciar que PDQ(0, 0, 0), para que el comando ARIMA nos estime un ARIMA puro
#      y no un modelo SARIMA con componente estacional

# Esto va a ser necesario, para poder hacer el pronóstico del modelo ARIMA empleando el paquete fable
# Lo recomendable es hacer el pronóstico usando el paquete fable por las ventajas que se veran más adelante. 
# Nota: Tengan presente que entre lo más importante de un modelo ARIMA, es realizar pronósticos futuros a corto plazo
#       con el modelo

# Para estimar un modelo SARIMA (i.e. un modelo ARIMA en donde también se modela la componente de estacionalidad 
# del proceso), no se especifica las componentes PDQ y se deja que el software las escoga)

sarima_1.1.0_lindprod_fable  = as_tsibble(l.indprod) %>%
  model(arima110 = ARIMA(value ~ pdq(1,1,0)))

glimpse(sarima_1.1.0_lindprod_fable)

# En nuestro ejemplo en particular *no* es necesaria generar un modelo SARIMA dado que la serie ya está desestacionalizada
# por lo que de acá en adelante vamos a trabajar con el modelo: arima_1.1.0_lindprod_fable (que no modela parte estacional)

#---- PASO 3- VALIDACIÓN indprod ----

# El supuesto más importante que se debe validar en un modelo ARIMA es que los residuales estimados 
# se comporten como un ruido blanco. Es decir, que la media de los residuales sea cero, la varianza 
# constante y la covarianza sea cero.

# Vamos a realizar el análisis de residuales para cada modelo: 

#
# 3.1 arima_1.1.0_lindprod (propuesto por el BIC) ----
#

# Se generan los residuales del modelo arima_1.1.0_lindprod
res_arima_1.1.0_lindprod = residuals(arima_1.1.0_lindprod)

## ACF y PACF para el modelo con d = 1
ggAcf(res_arima_1.1.0_lindprod, lwd=2) + ggtitle("ACF residuales") + theme_light()
ggPacf(res_arima_1.1.0_lindprod, lwd=2) + ggtitle("ACF residuales al cuadrado") + theme_light() # la ACF y PACF parece indicar que hay correlación serial en los residuales del modelo

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.indprod)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(res_arima_1.1.0_lindprod, lag=lags.test, type = c("Box-Pierce")) #Rechazo H0, no se cumple el supuesto. 
Box.test(res_arima_1.1.0_lindprod, lag=20, type='Box-Pierce') #Rechazo H0, no se cumple el supuesto. 
Box.test(res_arima_1.1.0_lindprod, lag=30, type='Box-Pierce') #Rechazo H0, no se cumple el supuesto.
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(res_arima_1.1.0_lindprod, lag=lags.test, type = c("Ljung-Box")) #Rechazo H0, no se cumple el supuesto.
Box.test(res_arima_1.1.0_lindprod , lag=20, type='Ljung-Box') #Rechazo H0, no se cumple el supuesto.
Box.test(res_arima_1.1.0_lindprod, lag=30, type='Ljung-Box') #Rechazo H0, no se cumple el supuesto.

# Nota: Como se rechaza la H0 para los dos test anteriores significa que el modelo arima escogido para modelar la serie l.indprod
      # No alcanza a capturar toda la estructura de correlación en la serie y eso se refleja en la presencia de correlación en los residuales.
      # La solución seria agregar más residuos autoregresivos o algunas componente de media móvil. No obstante, otra alternativa es utilizar
      # el modelo arima seleccionado por el AIC o el segundo mejor modelo escogido por el BIC. En todo caso, es necesario volver a estimar 
      # un modelo arima con coeficientes diferentes dado que es importante que no haya presencia notoria de correlación serial en los residuales del modelo arima estimado.

# 2. Pruebas de Heterocedasticidad en los residuales

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch_lindprod <-arch.test(arima_1.1.0_lindprod, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

# Como ya se mencionó, la correlación serial de los residuales al cuadrado del modelo ARIMA es una proxy de heterocedasticidad, por lo
# que una FAC y FACP de los residuales al cuadrado es una buena forma de establecer si hay heterocedasticidad o no en dichos residuales. 
# Dado que el comando anterior a veces no da tanta claridad sobre la severidad de la heterocedasticidad en la serie, siempre se recomienda
# realizar la ACF y PACF de los residuales del modelo ARIMA al cuadrado para verificar la presencia y la severidad de la heterocedasticidad 

#Vamos a graficar la ACF y PACF de ñps residuales al cuadrado del modelo arima_1.1.0_lindprod
lags=24
x11()
par(mfrow=c(1,2))
acf(res_arima_1.1.0_lindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF residuales al cuadrado') 
pacf(res_arima_1.1.0_lindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF residuales al cuadrado')

# Versíón ggplot:
x11()
ggAcf(res_arima_1.1.0_lindprod^2,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF residuales al cuadrado") + theme_light()
x11()
ggPacf(res_arima_1.1.0_lindprod^2,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF residuales al cuadrado") + theme_light()

# Nota: Las gráficas de la ACF y PACF de los residuales al cuadrado parecen corroborar que la serie presenta 
# heterocedasticidad. Se recomienda utilizar modelos menos parsimoniosos como el que propone el AIC. En caso, de que
# persista la heterocedasticidad, una forma de tratarla es empleando modelos garch 

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
qqPlot(res_arima_1.1.0_lindprod) #presencia de colas pesadas que impiden el cumplimiento del supuesto. 

#Prueba Jarque-Bera
jarque.bera.test(res_arima_1.1.0_lindprod) #Se rechaza H0, no hay normalidad. 

# Nota: Es muy usual en la práctica, que al modelar series de tiempo los residuales de la serie no satisfagan el 
#       supuesto de normalidad. Esto puede generar dificultad, en la medida de que la inferencia estadística convencional
#       no es correcta a pesar de que los pronósticos puntuales sí son correctas (sin importar si los residuales se 
#       comportan normal o no). No obstante, como nos interesa realizar pronósticos con intervalos de confianza
#       es necesario emplear una distribución que se acomode lo mejor posible a los residuales del modelo. Esto se puede
#       lograr empleando bootstrapping, que es una forma de simular la distribución de una muestra de datos realizando
#       un remuestreo con reemplazo de éstos. La función forecast del paquete forecast no permite hacer bootsrapping
#       mientras que la función forecast del paquete fable sí lo permite. 

# Dada la correlación serial que se observa en los residuales del modelo arima_1.1.0_lindprod se concluye que el modelo seleccionado
# por el criterio BIC no es adecuado dado que no captura toda la estructura de correlación en la serie. Por tanto, se procede a 
# varlidar los supuestos del modelo arima_3.1.5_lindprod propuesto por el modelo AIC y el modelo arima_4.1.2_lindprod propuesto por la
# metodología automática de fable. 

#
# 3.2 arima_3.1.5_lindprod (propuesto por el AIC) ----
#

# Se generan los residuales del modelo arima_3.1.5_dlindprod
res_arima_3.1.5_lindprod = residuals(arima_3.1.5_lindprod)

## ACF y PACF para el modelo con d = 1
ggAcf(res_arima_3.1.5_lindprod, lwd=2) + ggtitle("ACF residuales") + theme_light()
ggPacf(res_arima_3.1.5_lindprod, lwd=2) + ggtitle("PACF residuales al cuadrado") + theme_light() # la ACF y PACF parece indicar que no hay correlación serial en los residuales del modelo

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.indprod)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(res_arima_3.1.5_lindprod,lag=lags.test, type = c("Box-Pierce")) #No Rechazo H0, se cumple el supuesto. 
Box.test(res_arima_3.1.5_lindprod, lag=20, type='Box-Pierce') #No Rechazo H0, se cumple el supuesto. 
Box.test(res_arima_3.1.5_lindprod, lag=30, type='Box-Pierce') #No Rechazo H0, se cumple el supuesto.
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(res_arima_3.1.5_lindprod, type = c("Ljung-Box"), lag=lags.test) #No Rechazo H0, se cumple el supuesto.
Box.test(res_arima_3.1.5_lindprod, lag=20, type='Ljung-Box') #No Rechazo H0, se cumple el supuesto.
Box.test(res_arima_3.1.5_lindprod, lag=30, type='Ljung-Box') #No Rechazo H0, se cumple el supuesto.

#Ambas pruebas nos confirman que no existe correlación de los errores independientemente de la cantidad de lags que se establezcan. 

# 2. Pruebas de Heterocedasticidad en los residuales

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch_dlindprod <-arch.test(arima_3.1.5_lindprod, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza, por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie.

#Vamos a graficar la ACF y PACF de los residuales al cuadrado del modelo arima_3.1.5_lindprod
lags=24
x11()
par(mfrow=c(1,2))
acf(res_arima_3.1.5_lindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF residuales al cuadrado') 
pacf(res_arima_3.1.5_lindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF residuales al cuadrado')

# Versíón ggplot:
ggAcf(res_arima_3.1.5_lindprod^2,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF residuales al cuadrado") + theme_light()
ggPacf(res_arima_3.1.5_lindprod^2,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF residuales al cuadrado") + theme_light()

# Nota: Las gráficas de la ACF y PACF de los residuales al cuadrado parecen corroborar que la serie presenta 
# heterocedasticidad. 

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
qqPlot(res_arima_3.1.5_lindprod) #presencia de colas pesadas que impiden el cumplimiento del supuesto. 

#Prueba Jarque-Bera
jarque.bera.test(res_arima_3.1.5_lindprod) #Se rechaza H0, no hay normalidad. 

#
# 3.2 arima_4.1.2_lindprod (propuesto por el paquete fable) ----
#

# Se generan los residuales del modelo arima_4.1.2_lindprod
res_arima_4.1.2_lindprod = residuals(arima_4.1.2_lindprod)

## ACF y PACF para el modelo con d = 1
ggAcf(res_arima_4.1.2_lindprod, lwd=2) + ggtitle("ACF residuales") + theme_light()
ggPacf(res_arima_4.1.2_lindprod, lwd=2) + ggtitle("ACF residuales al cuadrado") + theme_light() # la ACF y PACF parece indicar que no hay correlación serial en los residuales del modelo

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.indprod)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(res_arima_4.1.2_lindprod,lag=lags.test, type = c("Box-Pierce")) #No Rechazo H0, se cumple el supuesto. 
Box.test(res_arima_4.1.2_lindprod, lag=20, type='Box-Pierce') #No Rechazo H0, se cumple el supuesto. 
Box.test(res_arima_4.1.2_lindprod, lag=30, type='Box-Pierce') #No Rechazo H0, se cumple el supuesto.
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(res_arima_4.1.2_lindprod, type = c("Ljung-Box"), lag=lags.test) #No Rechazo H0, se cumple el supuesto.
Box.test(res_arima_4.1.2_lindprod, lag=20, type='Ljung-Box') #No Rechazo H0, se cumple el supuesto.
Box.test(res_arima_4.1.2_lindprod, lag=30, type='Ljung-Box') #No Rechazo H0, se cumple el supuesto.

#Ambas pruebas nos confirman que no existe correlación de los errores independientemente de la cantidad de lags que se establezcan. 

# 2. Pruebas de Heterocedasticidad en los residuales

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch_dlindprod <-arch.test(arima_4.1.2_lindprod, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza, por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie.
#Sin embargo, en el caso del test de Portmanteau, los valores arrojados son mayores a los obtenidos en los anteriores modelos,
#lo que sugiere que la heterocedasticidad está más controlada. 

#Vamos a graficar la ACF y PACF de los residuales al cuadrado del modelo arima_4.1.2_lindprod
lags=24
x11()
par(mfrow=c(1,2))
acf(res_arima_4.1.2_lindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF residuales al cuadrado') 
pacf(res_arima_4.1.2_lindprod^2,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF residuales al cuadrado')

# Versíón ggplot:
ggAcf(res_arima_4.1.2_lindprod^2,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF residuales al cuadrado") + theme_light()
ggPacf(res_arima_4.1.2_lindprod^2,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF residuales al cuadrado") + theme_light()

# Nota: Las gráficas de la ACF y PACF de los residuales al cuadrado parecen corroborar que la serie presenta 
# heterocedasticidad. 

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
qqPlot(res_arima_4.1.2_lindprod) #presencia de colas pesadas que impiden el cumplimiento del supuesto. 

#Prueba Jarque-Bera
jarque.bera.test(res_arima_4.1.2_lindprod) #Se rechaza H0, no hay normalidad. 

# Finalmente, para que el proceso sea estacionario es necesario que las raíces del polínomio autoregresivo se 
# encuentren por fuera del círculo unitario y para que sea invertible es necesario que las raíces del polinomio de media
# móvil se encuentren por encuentren por fuera del círculo unitario. 
# El paquete fable permite verificar si lo anterior se cumple o no en el modelo ARIMA estimado mediante el paquete fable. 
# No obstante, es importante destacar que fable calcular es el inverso de las raíces del polinomio característico por lo
# que dichos inversos raíces se deben encontrar por dentro en lugar de fuer del círculo unitario (al ser los inversos de
# las raíces y no las raíces en si mismo las que se calculan). Para el modelo ARIMA arima_1.1.0_lindprod_fable estimado
# por fable se tiene que: 

x11()
gg_arma(arima_1.1.0_lindprod_fable)

# El modelo ARIMA puro solo tiene una raíz unitaria en su componente AR dado que el polinomio asociado a su componente AR
# es de grado 1 (recuerden que estimamos un ARIMA(1,1,0))

# Ahora bien, para el modelo ARIMA(3,1,5) seleccionado por el criterio de información AIC, se observa también que todas las
# inversas de las raíces del polinomio del AR y del MA se encuentran dentro del círculo unitario. Hay 3 raíces en el
# polinomio característico asociado a la componente AR y 5 raíces en el polinomio característico 
# asociada a la componente MA:  

x11()
gg_arma(arima_3.1.5_lindprod_fable)

# Finalmente, para el modelo ARIMA(4,1,2) seleccionado por el paquete fable, se observa también que todas las
# inversas de las raíces del polinomio del AR y del MA se encuentran dentro del círculo unitario. Hay 4 raíces en el
# polinomio característico asociado a la componente AR y 2 raíces en el polinomio característico 
# asociada a la componente MA: 

x11()
gg_arma(arima_4.1.2_lindprod_fable)

# Como pueden observar, los 3 modelos son: arima_1.1.0_lindprod_fable, arima_1.1.0_lindprod_fable y arima_4.1.2_lindprod_fable
# son estacionarios e invertibles por la magnitud(módulo) de sus raíces unitarias. 

# Acá aprovechamos para mostrarles que cuando se trabaja con un modelo SARIMA pueden aparecer más raíces unitarias 
# asociadas a la componente estacional del proceso que se modela (Se pueden observar que en el modelo SARIMA aparecen
# más raíces unitarias en comparación al ARIMA puro, estas raíces adicionales surgen de la parte estacional de la serie)

gg_arma(sarima_1.1.0_lindprod_fable)

# Nota: Reuerden que para nuestro modelo estamos trabajando exclusivamente con el modelo arima_1.1.0_lindprod_fable
#       el modelo ARIMA puro dado que la serie está desestacionalizada y omitimos el modelamiento del componente 
#       estacional de la serie

#---- PASO 4- PRONÓSTICO indprod ----

# Dado que el modelo arima(1,1,0) no cumple el supuesto más importante de no correlación serial de los errores, 
# en oposición con los modelos arima(3,1,5) y arima(4,1,2), solamente vamos a hacer los pronósticos
# con base en estos dos últimos modelos (recuerden que la razón principal para utilizar un modelo ARIMA es para
# generar pronósticos de corto plazo). 

# Nota: Tengan en cuenta que si bien el presente Script es bastante extenso (porque busca mostrarles todas las herramientas
#       y todo lo que tienen que tener en cuenta a la hora de estimar un modelo ARIMA en R), a la hora de presentar
#       sus talleres solo tienen que escoger un modelo que sea adecuado modelar por la metodología Box Jenkins y con el 
#       cual puedan hacer pronósticos y en lo posible que satisfaga al menos el supuesto de no correlación serial de los residuales

# 4.1 Pronóstico futuros ----

# Nota: Los intervalos de confianza que aparecen en los pronósticos que no emplean bootstrapping no son buenos 
#       dado que no se cumple el supuesto de normalidad en los residuales.
#       Por tanto, para hacer buenos pronósticos con intervalos de confianza correctos es necesario emplear bootstrapping
#       Esto es posible empleando la función forecast del paquete fable, que permite hacer pronósticos con bootrapping. 
#       Es importante resaltar que para usar dicho comando se necesita proveer un objeto estimado por el comando 
#       ARIMA del paquete fable. En nuestro caso, dicho objeto se estimó y es: arima_1.1.0_lindprod_fable

# Utilizando el comando forecast del paquete fable:

# Pronóstico 10 pasos adelantes, valores numéricos - modelo(3,1,5)
forescast_arima_3.1.5_lindprod = arima_3.1.5_lindprod_fable %>% 
  forecast(h = 10, bootstrap = TRUE, times = 10000); forescast_arima_3.1.5_lindprod

# Nota: A la hora de hacer simulaciones, ya sea de Monte Carlo o bootrapping se recomienda realizar al menos 10.000 simulaciones
#       Esto va a demorar un tiempo (al menos entre 1 a 2 minutos). Los tiempos difieren dependiendo del proesador y memoría RAM 
#       del PC que estén empleando.

# Nota: Si bien la simulación por bootstrapping es la metodología más empleada a la hora de trabajar con series que no tienen un comportamiento normal
#       se resalta que un bootstrapping convencional no es valido en caso de que la serie además de presentar no normalidad presente heterocedasticidad,
#       como es el caso de nuestra serie. En este caso, se recomienda emplear modelos de volatilidad condicional (AKA como GARCH) en donde se estimen dichos modelos
#       aumiendo que los errores siguen una distribución diferente a la normal (como una t con pocos grados de libertad, que suele tener colas más pesadas 
#       que una normal, i.e. es una distribución leptocúrtica). Lo anterior, se puede realizar en R empleando el paquete *rugarch* que es un paquete muy 
#       versatil no solo para estimar modelos GARCH sino también estimarlos con distribuciones que permiten modelar errores no necesariamente normales. 
#       No obstante, para efectos prácticos del curso de econometría II, como los modelos GARCH están por fuera de la temática del curso, 
#       pueden emplear la función forecast de fable para tratar problemas de no normalidad independiente de que haya heterocesdasticidad o no en los errores. 

# Adicionalmente, utilizando el comando hilo() que se encuentra en el paquete fable es posible mostrar 
# tanto el valor de los pronósticos puntuales como los intervalos de predicción a diferentes niveles. 
# level = c(80, 90, 95). Para encontrar los intervalos de predicción al 80, 95 y 95 %
pronósticos_3.1.5_lindprod = forescast_arima_3.1.5_lindprod %>% 
  hilo(level = c(80, 90, 95)); pronósticos_3.1.5_lindprod

View(pronósticos_3.1.5_lindprod)

# Gráfica pronóstico 10 pasos adelante
x11()
forescast_arima_3.1.5_lindprod %>% 
  autoplot(l.indprod_tsibble) + ggtitle("Pronósticos 10 pasos adelante Indice de Producción Industrial ") + ylab("Indice de Producción Industrial") + xlab("trimestres") + theme_light()

# Pronóstico 10 pasos adelantes, valores numéricos - modelo(4,1,2)
forescast_arima_4.1.2_lindprod = arima_4.1.2_lindprod_fable %>% 
  forecast(h = 10, bootstrap = TRUE, times = 10000); forescast_arima_4.1.2_lindprod

pronósticos_4.1.2_lindprod = forescast_arima_4.1.2_lindprod %>% 
  hilo(level = c(80, 90, 95)); pronósticos_4.1.2_lindprod

View(pronósticos_4.1.2_lindprod)

# Gráfica pronóstico 10 pasos adelante
x11()
forescast_arima_4.1.2_lindprod %>% 
  autoplot(l.indprod_tsibble) + ggtitle("Pronósticos 10 pasos adelante Indice de Producción Industrial ") + ylab("Indice de Producción Industrial") + xlab("trimestres") + theme_light()

#Gráficamente, es visible que los pronósticos entre uno y otro modelo no varían de manera considerable. 

# 4.2 Ahora vamos a ver el ajuste dentro de muestra ----

#Recordando la anotación hecha en el paso n°2 de Estimación, vamos a ver el ajuste dentro de la muestra del modelo
#diferenciado manualmente. 

## Para la series l.indprod - modelo arima(3,1,5)
fit_1 <- l.indprod - residuals(arima_3.1.5_lindprod)
## Para la series dl.indprod
fit_0 <- dl.indprod - residuals(arima_2.0.4_dlindprod)

#Predicción sobre la muestra
#Modelo l.indprod - modelo arima(3,1,5)
x11()
plot.ts(l.indprod,type="l",main="log(indprod) ajustada VS log(indprod) observada",lwd=2)
points(fit_1,col="blue",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","blue"),lty=1,lwd=2)

#Modelo dl.indprod - arima(2,0,4)
x11()
plot.ts(dl.indprod,type="l",main="Tasa de crecimiento ajustada VS Tasa de crecimeinto observada",lwd=2)
points(fit_0,col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","darkgreen"),lty=1,lwd=2)

# Como pueden observar el ajuste en muestra es bastante bueno, lo que muestra la capacidad y potencia de los modelos ARIMA
# para modelar series de tiempo univariadas, así como su versatilidad. 

#
# Notas finales
#

# Como pudieron notar los modelos ARMA son modelos que están diseñados para modelar series estacinoarias en covarianza.
## Eso implica que uno debería utilizar un modelo ARMA cuando las series se comportan con media constante, varianza constante 
## y autocorrelación que depende solo de la distancia temporal entre las observaciones.
## Para corroborar que una serie es estacionario se podría verificar que los residuales del modelo en efecto se comporten como
## Ruido blanco. 

#
# Notas sobre volatilidad
#

# Como lo muestra la gráfica: 
plot(diff(log(indprod_xts)), main = "Tasa de crecimiento IPI trimestre de Estados Unidos (1960-2012) (Index 2012 = 100)", ylab  = "tasa de crecimiento IPI")

# La tasa de crecimeiento del IPI pareciera presentar clusters de volatilidad, es decir, no hay una varianza constante a lo largo de la serie
# Sino que pareciera que en determinados periodos de tiempo hay un tipo de volatilidad y en otro periodo de tiempo dicha volatilidad cambia.

# El anterior comportamiento es muy común en muchas series de tiempo. 
# En particular, dicho comportamiento de presencia de varios clusters diferentes de volatilidad en una misma serie tiende a ser común 
# en series de tiempo financieras. 

# Dado que los modelos ARMA están diseñados para series con varianza constante se requiere de otro tipo de modelos capaces de capturar esos
# esos clusters de volatilidad. Para ello, se emplean modelos capaces de modelar la varianza variable en el tiempo del proceso generador de datos
# Dichos modelos son: 
### ARCH: Extensión del modelo AR para modelar series de tiempo con clusters de volatilidad
### GARCH: Extensión del modelo ARMA para modelar serie de tiempo con clusters de volatilidad (en la práctica se emplean más porque genera modelos más parsimoniosos)

# En R el paquete más comúnmente empleado para modelos GARCH univariados es *rugarch* y para modelos GARCH multivariados es *rmgarch*

