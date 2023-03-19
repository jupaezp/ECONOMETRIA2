### UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS ###
### ECONOMETRIA II - 2023-I ###
### SESIÓN #1 MONITORIA : INTRODUCCIÓN A R ###

remove(list = ls()) # Codigo para limpiar el Environment 

# 1. Introducción a R. ----

# ¿Qué es R? 
# Es un lenguaje interpretado que sirve para hacer análisis estadístico.

# Otras características de R:
# Lenguaje que permite trabajar con objetos (obviamente, no tan avanzado como Python pero igual permite la manipulación de objetos)
# Lenguaje con fuertes capacidades de programación funcional 
# (que en su gran mayoría es el paradigma que domina a la hora de hacer análisis estadístico)

# ¿Por qué R es tan demandado/popular hoy en día?
# Es un lenguaje moderno de finales de los 80s y comienzos de los 90s (a diferencia de stata)
# Es código libre (eso implica muchas cosas, como el poder estar en constante aprendizaje de las actualizaciones del lenguaje)
# Los mejores y principales estadísticos del mundo (academia, sector público, banca central, sector privado)
# todos usan R en su gran mayoría
## fácil de encontrar documentación
## fácil de mirar directamente el código de los comandos (cosa que no permite e.g. matlab)
## lenguaje altamente modular (muchas librerías, muy versatil)
## Ser libre permite que las primeras librerías de algún tema novedoso se implementen en R
## Hace que haya una comunidad vibrante y abierta que se ayuda y se compemente entre ellos
## (e.g. stackoverflow, cross-validated)

# ¿Qué cosas estadística permite hacer R?
# De todo, O sea literal...
# De todo.

# Quieren hacer estadística descriptiva simple en R?
# Lo pueden hacer

# Quieren hacer estadística bayesiana en R?
# Lo pueden hacer

# Quieren hacer modelos estado espacio y filtros de Kalman en R?
# Lo pueden hacer

# Quieren sumar dos números? 
# Lo pueden hacer

# Quieren calcular la volatilidad condicional de un activo financiero? 
# Lo pueden hacer

# Quieren estimar por GMM un modelo de datos panel con variables instrumentales? 
# Lo pueden hacer

# Quieren aplicar un modelo de diferencias en diferencias para ver si la teoría de salario mínimo neoclásico funciona? 
# Lo pueden hacer

# Quieren hacer un RDD para evaluar familias en acción? 
# Lo pueden hacer

# Quieren hacer una simulación de Monte Carlo en R?
# Lo pueden hacer y de hecho hoy lo vamos a hacer...

# Es decir, con R pueden hacer: 
## Macroeconomtría
## Microeconometría
## Econometría financiera
## Econometría Bayesiana
## Evaluación de impacto

# Quieren hacer algún análisis estadístico?
# Háganlo en R...

# 2. Creación de objetos compuestos ----

#Crear vectores: c()
Codigos = c(10,25,6,8,90,102); Codigos
x = 2; x
Colores = c("Rojo", "Azul", "Morado"); Colores

#Crear un vector secuencia 1 a 1.
Dias = 0:100; Dias

#Crear un vector secuencia que avance cada j unidades: seq(a,b,j)
Medicion = seq(1,20, 0.5); Medicion

#Crear un vector donde los objetos se repiten n veces: rep(objeto,n)
Numerico = rep(1,30) ; Numerico

Categorico = rep("mujer", 10); Categorico

Notas = c(1,1.2,1.6,4.5,4.7,3.6,3.9,2.5,5,5,3.2,1.9,0.5,
          0.6,0.5,3.5,0.2,0.5,4,4,4,4,4.6,5,5,5,4.6,4.8); Notas

Dummy = c(rep(0,5), rep(1,10), rep(0,5)); Dummy

#Creación de matrices: matrix(a:b,nrow = 4, byrow =TRUE) o matrix(a:b,nrow = 4, byrow = FALSE)

MAT1=matrix(1:10,nrow = 5,byrow = TRUE);MAT1 
MAT2=matrix(10:19,ncol=5, nrow = 2, byrow = FALSE);MAT2

#Creación de matrices a partir de vectores: cbind(a,b) o rbind(a,b)
a = c(1,3,5,7,9,7,10,6,5,7,9)
b = c(2,4,6,8,2,3,11,1,2,5,6)
MAT3 = cbind(a,b); MAT3
MAT4 = rbind(a,b); MAT4 

# 3. Operaciones entre vectores ----

#Número de objetos en el vector: length(vector)
length(a)
length(Colores)

#Tipo de objeto en el vector: class(vector)
class(b)
class(Colores)

# clases de vectores

vect_char = c("primero", "segundo", "tercero"); vect_char
vect_num = c(1, 2, 3); vect_num
vect_logic = c(TRUE, FALSE, F, T); vect_logic

# transformar un vector de un tipo a otro tipo 
## por ejemplo transformar un vector de tipo character en un vector de tipo factor
vect_fact = as.factor(vect_char); vect_fact
class(vect_fact)

#Estadistica descriptiva y dispersión del vector
summary(Notas) #Sesgada o con cola hacia la izquierda

#Elementos únicos del vector: unique()
unique(Notas)

#Ordenar elementos de menor a mayor: sort()
sort(Notas)

#Posición ordenada de menor a mayor
length(Notas)
order(Notas)

#Ordenar elementos de mayor a menor: sort(,decreasing=TRUE)
sort(Notas,decreasing = TRUE)       

#Seleccionar ciertos elementos del vector por posición 
a[2];a[3];a[4]
a[c(1,5,5)]

#Eliminar ciertos elementos del vector por posición
a[-c(1,5)]  
a = a[-c(1,5)] 
a
a = c(1,4,5,7,9,7,10,6,5,7,9)

# 4. Operaciones entre matrices ----

#Sumar una constante j a la matriz
MAT1 + 10

#Suma de matrices
MAT3 + t(MAT4)

#Multiplicar una constante j a la matriz
MAT3*10

#Matriz Transpuesta: t() 
t(MAT1) 

#Matriz identidad: diag()
diag(10)

#Matriz diagonal: diag()
Mat_diag = diag(1:10); Mat_diag

#Producto de matrices
dim(MAT3) #conocer las dimensiones de una matriz: 11x2
dim(MAT4) #2x11
MAT3%*%MAT4

#Inversa de una matriz: solve()
solve(Mat_diag)

#Determinante de la matriz: det()
det(Mat_diag)

# Que pasa con la inversa de una matriz no cuadrada: solve()
#solve(MAT2)

#Que pasa con el determinante de una matriz no cuadrada: det()
#det(MAT1)

#Mostrar el elemento ij de la matriz: MAT[i,j]
MAT2[2,1]
MAT2[1,2]

#Mostrar los elementos de la fila i: MAT[i,]
MAT3[1,]

#Mostrar los elmentos de la columna j: MAT[,j]
MAT3[,1]

#Agregar nombres a las filas y columnas de una matriz
rownames(MAT1) = c("a","b","c","d","e")
colnames(MAT1) = c("f","g")
MAT1

# 5. Operaciones lógicas ----

#Elementos mayores o iguales a j en el vector
a>=4

#Elementos menores o iguales a j en el vector
a<=4

#Elementos iguales a j en el vector
a==4

#Elementos diferentes a i en el vector
b!=6

#Dos afirmaciones verdaderas: "&"
a>=1 & b==3 # Recuerden que & es un "y" matemático

#Al menos una es cierta: "|"
a>=1 | b==3 # Recuerden que | es un "o" matemático

#Negación: "!"
!a < 0 #a no es menor que cero

# 6.Crear funciones en R ----

#Crear una función definiendo los argumentos y la expresión
sumar = function(x,y,z,j=3){
  w = x + y + z + j
  return(w)
}

#Evaluar la función para ciertos valores de los argumentos
sumar(2,3,10)   # dejando el argumento j fijado por el valor default
sumar(2,3,10,1) # dandole un valor al argumento j 

# 7. Estructuras de control y ciclos en R ----

# Ciclo while (Ciclo más general)

i = 0 # contador 
while (i<5){
  print(i)
  i = i + 1    # condición de actualización
}

i # Cuál fue el valor del contador después del ciclo while

# ciclo for (Es el ciclo más importante porque es el ciclo más comunmente empleado)

for (j in 1:10){
  print(j)
}

for (j in a){
  print(j)
}

suma = 0
valores = 1:100
for (numero in valores){
  print(numero)
  suma = numero + suma
}

suma
# esctructura de flujo: if-else (condicionales)

n1 = 5
n2 = 6

# Nota %% es el operador modulo: 
if (n2 %% 2 == 0){
  print("Es un número par")
}else{
  print("Es un número impar")
}

# 8. Manejo de ggplot2, dplyr y tidyr ----

# Importar conjunto de paquetes del tidyverse (incluye ggplot y dplyr)
library(tidyverse) # Libreria más importante en todo R (criterio personal del monitor)
library(readxl) # Libreria pra leer achivos excel
library(haven) # Libreria para leer archivos .dta de stata

# 8.1 Paquete dplyr ----
# dplyr es el paquete para manipular bases de datos en R

# mtcars: base de datos sobre características de carros antiguos
?mtcars

# Nota: Lo primero que siempre se debe hacer es visualizar la base de datos
#       Siempreeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee!

#Existen varias formas para ello:
print(mtcars) #Como su palabra lo dice, imprime el dataset en la consola
View(mtcars) #Lanza una nueva pestaña 
glimpse(mtcars) # visualizar una base de datos con el tipo de objeto 
#(Recordar que toda base de datos se trabaja como un objeto data.frame o tibble)

# Recuerden la estructura de una base de datos: 
## columnas: Cada columna representa una variable diferente
## filas: Cada fila representa una observación 
## (es decir la info. de cada variable para una determinada observación, ya sea casa, individuo y demás)


## Manipulación de base de datos usando dplyr ##

# Selección de unas variables de interés *SELECT*
mtcars_selected = mtcars %>% 
    select(mpg, cyl, disp); mtcars_selected

#Ordenar de forma descendente mpg
mtcars_selected %>%
  arrange(desc(mpg))

# Filtrar bases de datos a partir de los valores de una variable 
mtcars_selected %>% 
  filter(cyl > 4) #podemos usar cualquier operador numérico

# Creación de variables a partir de variables antiguas
manip1 = mtcars %>%  
  mutate(mult = mpg * cyl, sqr_mpg = mpg^2); manip1 # Nota: ctrl + shift + m para generar el simbolo: %>% (Intentelo!)

# manip1 = mutate(mtcars, mult = mpg * cyl, sqr_mpg = mpg^2)

# Para sacar estadística descriptiva de una base de datos
manip3 = mtcars %>% 
  group_by(cyl) %>% 
  summarize(n  = n()); manip3

## Otro ejemplo análogo_ Condados de EE.UU ## 

#Importamos la base de datos 
counties = read_rds(file.choose())

# Visualización de la base de datos
View(counties)
glimpse(counties)

## Seleccionamos ciertas columnas de interés
counties_selected = counties %>%
  select(state, county, population, 
         private_work, public_work, self_employed)

## Ordenar de manera descendente el trabajo público
counties_selected %>%
  arrange(desc(public_work))

##Vamos a seleccionar otras variables 
counties_selected2 = counties %>%
  select(state, county, population)

## Filtramos por los condados con una población superior a un millón
counties_selected2 %>%
  filter(population > 1000000)

## Filtramos por los condados en 
#el estado de California con una población superior a un millón
counties_selected2 %>%
  filter(state == "California",
         population > 1000000)
#resultado: hay 9 condados en California con una población superior a un millón

#Seleccionamos de nuevo unas columnas en específico
counties_selected3 = counties %>%
  select(state, county, population, men, women)

#Calculamos la proporción de mujeres como una fracción entre el número de mujeres /población
counties_selected3 %>%
  mutate(proportion_women = women / population)

#A manera de resumen de los 4 verbos más importantes:
#Select, mutate, filter y arrange
counties %>%
  # Seleccionamos 5 columnas
  select(state, county, population, men, women) %>%
  # Añadimos la proporción de mujeres
  mutate(proportion_women = women / population) %>%
  # Filtramos por una población de al menos 10k habitantes
  filter(population >= 10000) %>% 
  # Ordemanos de manera descendente por la proporción de mujeres 
  arrange(desc(proportion_women))

##Otros 2 verbos importantes: Summarize y group_by

#De nuevo, seleccionamos las variables que necesitamos
counties_selected4 = counties %>%
  select(state, county, population, income, unemployment,land_area)

# Agrupamos para encontrar la población mín., máx. desempleo y el ingreso promedio
counties_selected4 %>%
  summarize(min_population = min(population),
            max_unemployment = max(unemployment),
            average_income = mean(income))

#¿A cuáles condados corresponden los 2 primeros valores?
counties_selected4 %>% 
  filter(population == "85")
  #filter(unemployment == "29.4")

#Agrupamos por (group by) estado para encontrar el área total y la población
data_grap1 = counties_selected4 %>%
  group_by(state) %>%
  summarize(total_area = sum(land_area),
            total_population = sum(population)) %>% 
  
  #Si queremos saber la densidad (en cantidades de personas por metro^2)
  mutate(density = total_population / total_area) %>%
  arrange(desc(density)); data_grap1

# 8.2 Paquete tidyr ----
library(tidyr)

# iris: base de datos sobre 3 tipos diferentes de flores 
view(iris)
glimpse(iris)

# Formato Long
iris_longer = pivot_longer(iris, cols = 1:4, names_to = "Plant_characteristics", values_to = "Value")

glimpse(iris_longer)

# table2: número de casos de tuberculosis en algunos paises
view(table2)
glimpse(table2)

# Formato wider
table2_wider = pivot_wider(table2, names_from = type,values_from = count)

glimpse(table2_wider)
# Como pueden observar, el paquete tidyr 
# Efectivamente permite modificar las dimensiones de las bases de datos

# 8.3 Paquete ggplot2 ----

# grafica de ggplot2 (Observar que las gráficas se estructuran por layers)
grafica = mtcars %>% 
  ggplot(aes(x = mpg, y = hp)) +
  geom_point(aes(color = factor(cyl))) +
  geom_smooth(method='lm', formula= y~x, se = F) +
  theme_classic()

grafica

## Gráfica para el segundo ejemplo de condados ##
# Gráfica donde tengamos el income pc en el eje y y en el eje x la proporción de mujeres 

#summarize de todos los estados 

#Tomamos el ejemplos anterior 
data_graph2 = counties %>% 
  select(state, county, population, women, income_per_cap, public_work) %>%
  mutate(public_workers = public_work * population / 100) %>% 
  group_by(state) %>% 
  summarize(mean_income = mean(income_per_cap), 
            mean_propwork = mean(public_workers),
            mean_pop = mean(population)) 

graph2= data_graph2 %>% 
  ggplot(aes(x = mean_income, y = mean_propwork, size=mean_pop)) +
  geom_point(aes(color = factor(state))) +
  theme_bw(); graph2

#Otra forma más detallada 
graph3= data_graph2 %>% 
  ggplot(aes(x = mean_income, y = mean_propwork)) +
  geom_point() +
  facet_wrap(~ state); graph3

# 9. Simulación de Monte Carlo (Aplicación práctica de todo lo aprendido en monitoria) ----

# Simulación de Monte Carlo Enders: suma de dos dados jutstos (ejemplo pag. 204) ----

# La idea es simular la probabilidad de obtener un determinado valor
# dado la suma de los resultados de dos dados justos

# función que premite realizar la simulación de Monte Carlo
monte_carlo = function(rep){
  # set_number_dado simular el lanzamiento de un dado
  set_number_dado = function(random){
    number = 0
    if (random <= 1/6){
      number = 1
    }else if(1/6 < random &  random <= 2/6){
      number = 2
    }else if(2/6 < random & random <= 3/6){
      number = 3
    }else if(3/6 < random & random <= 4/6){
      number = 4
    }else if(4/6 < random & random <= 5/6){
      number = 5
    }else{
      number = 6
    }
    return(number)
  }
  # vector que almacena los resultados de la simulación de monte carlo
  vect = c()
  # simula rep veces el lanzamiento de dos dados y sumo sus resultados
  for (number in 1:rep){
    # genero los dos lanzamientos de los dos datos
    random1 = set_number_dado(runif(1)) # la distribución que se escogió para hacer la simulación fue una distribución uniforme (para garantizar tener un dado justo)
    random2 = set_number_dado(runif(1))
    vect = append(vect, random1 + random2) 
  }
  df = as_tibble(vect) %>% 
    mutate(value = as.factor(value))
  return(as_tibble(vect))
}

# Se realiza la simulación de Monte Carlo
prueba = monte_carlo(100000)# se repite 100000 veces el lanzamiento de los dos dados para garantizar alcanzar la distribución teórica

# Histograma que muestra que se alcanzan la distribución teórica propuesta por Enders
# en el ejemplo de la pag. 204 
graph = prueba %>% 
  ggplot(aes(x = value)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme_light(); graph

# El historgrama representa una distribución empírica igualita a la distribución teórica discreta para la suma del resultado de los dos dados

# 10. Bonus: Bootstrapping simple/sencillito ----

set.seed(9821) # La semilla permite reproducibilidad del código 

#### Bootstrapping Enders ####

# La simulación por bootstrapping es parecida a una simulación de Monte Carlo 
# excepto por una diferencia esencial: 
## En una simulación de Monte Carlo, uno general las variables aleatorias de una
## distribución data (teórica), como lo puede ser una distribución normal. 
## Por otro lado, en una simulación por bootstrapping las variables aleatorias se
## generan de la distribución muestral observada. 

## La simulación por bootstrapping usa "the plug-in-principle", en el que 
## la distribución observada de las variables aleatorias es la mejor estimación
## de su verdadera distribución. 

# La simulación por bootstrap fue propuesta por Efron(1979). 
# La idea principal de Efron es que la muestra de datos observada, es una muestra
# aleatoria de tamaño T proveniente de la verdadera distribución generando 
# los datos

# En escenia, la distibución empirica de los datos es la mejor estimación 
# de la verdadera distribución de los datos. 

# La función de distribución empírica se define como la distribución discreta
# que coloca una probabilidad de 1/T en cada uno de los datos observados. Es
# dicha distribución empírica (y no una distribución teórica predefinida) que
# se usa para generar las varaibles aleatorias

# Una muestra obtenida por bootstrapping es una muetra aleatoria de tamaño T
# sacada con reemplazo de los datos observados que pone una probabilidad de
# 1/T en cada uno de los datos observados. 

# Nota: Muestra con reemplazo y muestra sin reemplazo

## Una muestra que se construye con reemplazo es aquella en donde luego de sacar
## un elemento este se puede volver a considerar para futuros elecciones de 
## la muestra que se está generando. Es decir, que una observación se haya 
## seleccionado no impide que dicha observación no pueda volver a salir en el futuro
## Inclusive, una observación que ya haya sido seleccionada tiene la misma 
## probabilidad de salir que una observación que no haya sido seleccionada
## en selecciones futuras. Por tanto, la selección de observaciones es 
## totalmente independiente dado que no depende de que observación haya
## salido en el pasado

## Un muestra que se construye sin reemplazo es (completar más tarde...)

# Ejemplo 1: Bootstrapping convencional (iid) programado de manera manual ----

# Se tienen la una muestra observada con los siguientes 10 valores 
vect = c(0.8, 3.5, 0.5, 1.7, 7, 0.6, 1.3, 2, 1.8, -0.5)

# Características del vector
mean(vect)
sd(vect)

# Función que me genera muestreos basados en bootstrapping para el vector vect
manual_boots = function(num_boots, vect){
  # num_boots: número total de muestras generadas por bootstrapping 
  # vect: vector del ques se generará el remuestreo con reemplazo
  matriz = matrix(nrow = num_boots, ncol = length(vect))
  # Cada fila de la matriz es una muestra generada por bootstrapping
  for (rep in 1:num_boots){
    # la función sample me permite seleccionar elementos del vector vect con la misma probabilidad. 
    # Replace = TRUE, para obetner una muestra con reemplazo
    matriz[rep, ] = sample(vect, size = length(vect), replace = TRUE) 
  }
  return(matriz)
}

#Genero la simulación por bootstrapping
matriz_boot = manual_boots(100, vect)
