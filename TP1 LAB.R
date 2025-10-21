##### TP LABORATORIO PARA EL ANÁLISIS DE DATOS ECONÓMICOS Y FINANCIEROS ####
#### Tomas Schvartzman, Lucas Vyhñak, Santiago Sarkissian ####

set.seed(912)

#Cargamos las librerías a utilizar
library(readr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)


################EJERCICIO DE ANÁLISIS DE DATOS################

#importamos el dataset
expo <- read_delim("/Users/tommy/Downloads/expo_agregado_202509.lst",delim = "'",
                   trim_ws = TRUE)

view(expo)

#Le cambiamos los nombres a las columnas
expo <- expo %>%
  rename(tipo = `\fT`,fecha = FECHA_,aduana = ADU,codigo_cm = POS_NCM,
         destino = PAI,medio_de_transporte = M,unidad = UN,kilos_netos = PESO_NETO_KILOS,
         monto_fob_dolares = MONTO_FOB_DOLAR,cantidad_declaraciones = CANT_DECLARACIONES,
         cantidad_unidad = CANT_UNIDAD_ESTADISTICA,)

head(expo,20)

#Arreglamos los datos. En el dataset hay filas con guiones y vacías, así que las sacamos.
glimpse(expo)

#Las filas numéricas están como texto(chr), así que las convertimos a número.
expo <- expo %>%
  filter(tipo %in% c("E", "e"),            
         # Nos quedamos sólo con exportaciones y excluimos cualquier otra cosa
         !is.na(monto_fob_dolares)) %>%        
  # Eliminamos filas vacías en esa columna
  mutate(across(matches("monto|kilos|precio|cantidad"), as.numeric))
# Las columnas de números que se cargaron como texto las convertimos en númeors para poder operar

view(expo)

#Ahora tenemos el dataset impio para poder hacer y responder preguntas

#PREGUNTA 1: ¿Cuáles fueron lo 10 países a los que más se exportó en septiembre de 2025?

#Como los países están por código, tengo que cargar el archivo del indec para saber el nombre

paises <- read_excel("/Users/tommy/Downloads/codigo_paises.xls", skip = 1)

view(paises)

#Unimos cada código con su país
expo <- expo %>%
  left_join(paises, by = c("destino" = "CÓDIGO INDEC"))

view(expo)

#Ahora respondemos la pregunta con una tabla, y eliminamos los que no son países

expo %>% filter(!is.na(`NOMBRE DE USO COMÚN`),
                !`NOMBRE DE USO COMÚN` %in% c("América", "Asia", "África", "Europa", "Oceanía",
                                              "Última actualización: octubre de 2015.")) %>%
  group_by(`NOMBRE DE USO COMÚN`) %>% summarise(valor_total=sum(monto_fob_dolares, na.rm = TRUE)/1000000)%>%
  arrange(desc(valor_total)) %>% head(10)

#PREGUNTA 2: ¿Cuáles fueron los productos (según código ncm) que más se exportaron en septiembre de 2025?

expo %>% filter(!is.na(codigo_cm)) %>%           
  # Eliminamos las filas sin código NCM
  group_by(codigo_cm) %>%
  # Agrupamos por producto según su código ncm
  summarise(valor_total = sum(monto_fob_dolares, na.rm = TRUE)/ 1000000) %>% arrange(desc(valor_total)) %>%  
  # Ordenamos de mayor a menor
  head(10)

#PREGUNTA 3: ¿Existe relación entre cantidad importada y monto FOB?

#Primero, creamos una tabla nueva con los datos relevantes:

expo_new <- expo %>%
  select(fecha, codigo_cm, kilos_netos, monto_fob_dolares)
view(expo_new)

#Ahora hacemos un gráfico que explique lo que pide la pregunta.

expo %>%
  ggplot(aes(x = kilos_netos, y = monto_fob_dolares)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", linewidth = 1.5) +
  scale_x_log10() + scale_y_log10() +
  labs(
    title = "Relación entre peso neto exportado y monto FOB",
    x = "Kilos netos (log)",
    y = "Monto FOB en dólares (log)"
  ) +
  theme_minimal()


################EJERCICIO DE ANÁLISIS ECONOMÉTRICO################

#cargo el dataset y lo nombro gapminder

gapminder <- read_csv("/Users/tommy/Downloads/gapminder.csv")
view(gapminder)

### INCISO 1 ###

#filtramos los datos que necesitamos de argentina (años y ingreso per cápita), y armamos una tabla:
ingreso_argentina <- gapminder %>% filter(country == "Argentina") %>% select(year, income_per_person)
view(ingreso_argentina)

#creamos el gráfico:
ggplot(ingreso_argentina, aes(x = year, y = income_per_person)) + 
  geom_line(color = "red", linewidth = 1) +
  labs(
    title = "Ingreso per cápita en Argentina a lo largo de los años",
    x = "Año", 
    y = "Ingreso per cápita"
  )

### INCISO 2 ###

#Primero lo que hago es separar los datos entre el entrenamiento (train) y lo que testeo (test).
#Son los ultimos 10 años para testear, y el resto para entrenamiento
ingreso_arg_train <- head(ingreso_argentina, -10)
ingreso_arg_test <- tail(ingreso_argentina, 10)
view(ingreso_arg_train)
view(ingreso_arg_test)

#ahora hay que nombrar las variables independientes y dependientes de nuestros modelos
y_train <- ingreso_arg_train$income_per_person
x_train <- ingreso_arg_train$year
y_test <- ingreso_arg_test$income_per_person
x_test <- ingreso_arg_test$year

#Ahora si, estimamos los modelos para los datos de train en el orden solicitado, y pedimos resumen con los datos relevantes:

mod_lineal <- lm(y_train ~ x_train)
summary(mod_lineal)

mod_polin2 <- lm(y_train ~ poly(x_train, 2, raw = TRUE))
summary(mod_polin2)

mod_polin10 <- lm(y_train ~ poly(x_train, 10, raw = TRUE))
summary(mod_polin10)

#Usamos la raíz del error cuadrático medio (RMSE) para medir el desempeño del test. El mejor modelo es el que menor RMSE tenga

rmse <- function(obs, pred) sqrt(mean((obs-pred)^2))
rmse_test <- c(
  Lineal = rmse(y_test, predict(mod_lineal, newdata = data.frame(x_train = x_test))),
  Grado2 = rmse(y_test, predict(mod_polin2, newdata = data.frame(x_train = x_test))),
  Grado10 = rmse(y_test, predict(mod_polin10, newdata = data.frame(x_train = x_test)))
)

rmse_test

#Ahora graficamos los modelos de train, e incuimos puntos de los datos, tanto de train como de test

ggplot() +
  geom_point(data = ingreso_arg_train, aes(x = year, y = income_per_person),
             color = "blue", size = 2) + 
  geom_point(data = ingreso_arg_test, aes(x = year, y = income_per_person),
             color = "red", size = 2) +
  geom_line(aes(x = x_train, y = fitted(mod_lineal)), color = "black", linewidth = 1) +
  geom_line(aes(x = x_train, y = fitted(mod_polin2)), color = "orange", linewidth = 1) +
  geom_line(aes(x = x_train, y = fitted(mod_polin10)), color = "green", linewidth = 1)+
  labs(
    title = "Modelos que estiman el ingreso por persona en Argentina",
    subtitle = "Azul = Train, Rojo = Test",
    x = "Año", y = "Income per person",
    caption = "Negro = lineal; Naranja = grado 2; Verde: grado 10"
  )+
  theme_minimal()

### INCISO 3 ###
# (a) #

#Utilizamos los países del mercosur, de la siguiente manera:
mercosur <- c("Argentina", "Bolivia", "Chile", "Paraguay", "Uruguay")

#filtramos los datos que necesitamos:
sudamerica <- gapminder %>%
  filter(country %in% mercosur) %>%
  select(country, year, income_per_person)
view(sudamerica)

#convertimos la tabla a formato ancho, y ponemos una columna por país
ingresos_sudamerica <- sudamerica %>%
  pivot_wider(names_from = country, values_from = income_per_person)

view(ingresos_sudamerica)

#Ahora si, calculamos la matriz de correlación entre los ingresos
corr_ingresos_sudam <- cor(ingresos_sudamerica %>% select(-year))
corr_ingresos_sudam

# (b) #

#Primero, calculamos las variaciones porcentuales anuales
crecimiento_sudam <- ingresos_sudamerica %>%
  arrange(year) %>%
  mutate(across(-year, ~ (./lag(.) - 1)*100)) %>%
  drop_na()

#Ahora armamos la matriz de correlaciones
corr_crecimiento_sudam <- cor(select(crecimiento_sudam, -year))
corr_crecimiento_sudam

### INCISO 5 ###
#Usaremos el año 2005 para toda la parte 2

#filtramos el año para los datos que buscamos
expectativa_vida_2005 <- gapminder %>%
  filter(year == 2005) %>%
  select(country, life_expectancy, life_expectancy_female)
view(expectativa_vida_2005)

#Ahora creamos el gráfico
ggplot(expectativa_vida_2005, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(color = "blue") +
  labs(
    title = "Expectativa de vida general vs femenina en el año 2005",
    x = "Expectativa de vida femenina",
    y = "Expectativa de vida general"
  )+
  theme_minimal()

### INCISO 6 ###

#estimamos la regresión lineal simple
exp_vida_modelo_simple <- lm(life_expectancy ~ life_expectancy_female, data = expectativa_vida_2005)

summary(exp_vida_modelo_simple)

### INCISO 7 ###

#Para realizar el test de t, vamos a crear una variable de diferencia que nos lo facilita.
#Primero, convertimos las variables de expectativa de vida a numéricas.

expectativa_vida_2005 <- expectativa_vida_2005 %>%
  mutate(
    life_expectancy = as.numeric(life_expectancy),
    life_expectancy_female = as.numeric(life_expectancy_female)
  )

dif_exp_vida <- expectativa_vida_2005$life_expectancy_female - expectativa_vida_2005$life_expectancy

#Realizamos el test unilateral con las siguientes hipotesis:
# H0 = media(dif_exp_vida) = 0
# H1 = media(dif_exp_vida) > 0

t.test(dif_exp_vida, alternative = "greater", mu = 0)

### INCISO 8 ###

#Creamos una tabla nueva con los datos necesarios
vida_ingreso_2005 <- gapminder %>%
  filter(year == 2005) %>%
  select(country, life_expectancy, life_expectancy_female, income_per_person)

#verificamos, como antes, que las columnas sean numericas
vida_ingreso_2005 <- vida_ingreso_2005 %>%
  mutate(
    life_expectancy = as.numeric(life_expectancy),
    life_expectancy_female = as.numeric(life_expectancy_female),
    income_per_person = as.numeric(income_per_person)
  )
view(vida_ingreso_2005)

#Estimamos la regresión múltiple de la expectativa de vida sobre la expectativa de vida
#femenina y el ingreso per cápita
mod_mult_exp <- lm(life_expectancy ~ life_expectancy_female + income_per_person,
                   data = vida_ingreso_2005)

summary(mod_mult_exp)

### INCISO 9 ###

#Para regresar un nuevo modelo de life_expectancy, elegimos las variables children_per_woman,
#income_per_person, y population. Los elegimos porque son variables tanto demográficas como económicas,
#y pueden explicar la expectativa de vida general de las personas.
#Creamos primero una nueva tabla con los datos que necesitamos.

gapminder_new <- gapminder %>%
  filter(year == 2005) %>%
  select(country, life_expectancy, children_per_woman, income_per_person, population)
view(gapminder_new)

mod_expect_new <- lm(life_expectancy ~ children_per_woman + income_per_person + population, 
                     data = gapminder_new)
summary(mod_expect_new)


################EJERCICIO DE SIMULACIÓN################

#Inciso 1
simular_ingreso <- function(n, k) {
  rchisq(n, df = k)
}


set.seed(123)
Yk3 <- simular_ingreso(10000, 3)
Yk7 <- simular_ingreso(10000, 7)
Yk25 <- simular_ingreso(10000, 25)

par(mfrow = c(1, 3))

hist(Yk3, freq = FALSE, col = "lightpink", main = "Chi-cuadrado(3)", xlab = "Y")
curve(dchisq(x, df = 3), add = TRUE, col = "red", lwd = 2)

hist(Yk7, freq = FALSE, col = "lightblue", main = "Chi-cuadrado(7)", xlab = "Y")
curve(dchisq(x, df = 7), add = TRUE, col = "orange", lwd = 2)

hist(Yk25, freq = FALSE, col = "lightgrey", main = "Chi-cuadrado(25)", xlab = "Y")
curve(dchisq(x, df = 25), add = TRUE, col = "cyan", lwd = 2)

#Inciso 2
demanda_CD <- function(Y, p1, p2, alpha1, alpha2) {
  X1 <- (alpha1 * Y) / p1
  X2 <- (alpha2 * Y) / p2
  U <- ((X1^alpha1)*(X2^alpha2))
  data.frame(Y, X1, X2, U)
}

#Inciso 3
n=10000
k=7
P1=2
P2=3
alpha1 = 0.6
alpha2 = 0.4

ejemplo1 <- demanda_CD(Yk7, P1, P2, alpha1, alpha2)
# View(ejemplo1)

media_X1 <- mean(ejemplo1$X1)
media_X2 <- mean(ejemplo1$X2)
media_U <- mean(ejemplo1$U)

Q1_X1 <- quantile(ejemplo1$X1, 0.25)
Q2_X1 <- quantile(ejemplo1$X1, 0.50)
Q3_X1 <- quantile(ejemplo1$X1, 0.75)

Q1_X2 <- quantile(ejemplo1$X2, 0.25)
Q2_X2 <- quantile(ejemplo1$X2, 0.50)
Q3_X2 <- quantile(ejemplo1$X2, 0.75)

Q1_U <- quantile(ejemplo1$U, 0.25)
Q2_U <- quantile(ejemplo1$U, 0.50)
Q3_U <- quantile(ejemplo1$U, 0.75)

#Mostramos los resultados

media_X1
media_X2
media_U

Q1_X1
Q2_X1
Q3_X1

Q1_X2
Q2_X2
Q3_X2

Q1_U
Q2_U
Q3_U

#Graficamos

par(mfrow = c(1, 3))

hist(ejemplo1$X1, freq = FALSE,
     main = "Distribucion X1",
     xlab = "X1",
     col = "grey")
curve(dchisq(x * P1 / alpha1, df = k) * (P1 / alpha1), add = TRUE, col = "red", lwd = 2)

hist(ejemplo1$X2, freq = FALSE,
     main = "Distribucion X2",
     xlab = "X2",
     col = "violet")
curve(dchisq(x * P2 / alpha2, df = k)* (P2 / alpha2), add = TRUE, col = "red", lwd = 2)

hist(ejemplo1$U, freq = FALSE,
     main = "Distribucion U",
     xlab = "U",
     col = "blue")
lines(density(ejemplo1$U), col = "red", lwd = 2)

par(mfrow = c(1, 1))

#Inciso 4

#Creamos el codigo con un "c" generico donde despues lo definimos 
# y obtenemos el resultado

# prob_bajo_consumo1 <- mean(ejemplo1$X1 < c)
# prob_bajo_consumo2 <- mean(ejemplo1$X2 < c)

#Definimos
c=1
prob_bajo_consumo1 <- mean(ejemplo1$X1 < c)
prob_bajo_consumo2 <- mean(ejemplo1$X2 < c)

prob_bajo_consumo1
prob_bajo_consumo2

#Inciso 5

P1_b <- 1.2 * P1

demanda_CD_b <- function(Y, P1, P2, alpha1, alpha2) {
  X1_b <- (alpha1 * Y) / P1
  X2_b <- (alpha2 * Y) / P2
  U <- ((X1_b^alpha1)*(X2_b^alpha2))
  data.frame(Y, X1_b, X2_b, U)
}

ejemplo2 <- demanda_CD_b(Yk7, P1_b, P2, alpha1, alpha2)
# View(ejemplo2)

media_X1_b <- mean(ejemplo2$X1_b)

#Comparamos las distribuciones antes y despues del shock para X1

mean(ejemplo1$X1)
mean(ejemplo2$X1_b)

par(mfrow = c(1, 2)) 

hist(ejemplo1$X1, freq = FALSE,
     main = "Antes del aumento de P1",
     xlab = "X1", xlim = c(0,10),ylim = c(0, 0.6),
     col = "grey")
curve(dchisq(x * P1 / alpha1, df = k) * (P1 / alpha1), add = TRUE, col = "red", lwd = 2)
abline(v = mean(ejemplo1$X1), col = "blue", lwd = 2, lty = 2)


hist(ejemplo2$X1_b, freq = FALSE,
     main = "Despues del aumento de P1",
     xlab = "X1_b", xlim = c(0,10), ylim = c(0, 0.6),
     col = "grey")
curve(dchisq(x * P1_b / alpha1, df = k) * (P1_b / alpha1), add = TRUE, col = "purple", lwd = 2)
abline(v = mean(ejemplo2$X1_b), col = "blue", lwd = 2, lty = 2)

#La linea azul es la media de cada X1 (antes y despues del shock), ademas de ser un punto de 
# referencia claro para diferenciar ambos graficos.

par(mfrow = c(1,1))

#Inciso 6

d_pre  <- density(ejemplo1$X1)
d_post <- density(ejemplo2$X1_b)

plot(d_pre, col = "red", lwd = 2, main = "Distribución de X1 pre y post shock",
     xlab = "x1", ylim = c(0, max(d_pre$y, d_post$y)))
lines(d_post, col = "blue", lwd = 2)

mean(ejemplo1$U)
mean(ejemplo2$U)

#Inciso 7

# Parámetros
a <- 2
b <- 5

# Simulaciones
Y <- simular_ingreso(n, k)
alpha1 <- rbeta(n, a, b)
alpha2 <- 1 - alpha1

ejemplo1_heterogeneidad <- demanda_CD(Y, P1, P2, alpha1, alpha2)

# View(ejemplo1_heterogeneidad)

#-------Inciso 7, inciso 3
#Agregamos _H para denotar Heterogeneidad

media_X1_H <- mean(ejemplo1_heterogeneidad$X1)
media_X2_H <- mean(ejemplo1_heterogeneidad$X2)
media_U_H <- mean(ejemplo1_heterogeneidad$U)

Q1_X1_H <- quantile(ejemplo1_heterogeneidad$X1, 0.25)
Q2_X1_H <- quantile(ejemplo1_heterogeneidad$X1, 0.50)
Q3_X1_H <- quantile(ejemplo1_heterogeneidad$X1, 0.75)

Q1_X2_H <- quantile(ejemplo1_heterogeneidad$X2, 0.25)
Q2_X2_H <- quantile(ejemplo1_heterogeneidad$X2, 0.50)
Q3_X2_H <- quantile(ejemplo1_heterogeneidad$X2, 0.75)

Q1_U_H <- quantile(ejemplo1_heterogeneidad$U, 0.25)
Q2_U_H <- quantile(ejemplo1_heterogeneidad$U, 0.50)
Q3_U_H <- quantile(ejemplo1_heterogeneidad$U, 0.75)

#Graficamos

par(mfrow = c(1, 3))

hist(ejemplo1_heterogeneidad$X1, freq = FALSE,
     main = "Distribucion X1 Het.",
     xlab = "X1",
     col = "grey")
lines(density(ejemplo1_heterogeneidad$X1), col = "red", lwd = 2)

hist(ejemplo1_heterogeneidad$X2, freq = FALSE,
     main = "Distribucion X2 Het.",
     xlab = "X2",
     col = "violet")
lines(density(ejemplo1_heterogeneidad$X2), col = "red", lwd = 2)

hist(ejemplo1_heterogeneidad$U, freq = FALSE,
     main = "Distribucion U Het.",
     xlab = "U",
     col = "blue")
lines(density(ejemplo1_heterogeneidad$U), col = "red", lwd = 2)

par(mfrow = c(1, 1))

#-------Inciso 7, inciso 4

#Creamos el codigo con un "c" generico donde despues lo definimos 
# y obtenemos el resultado

# prob_bajo_consumo1 <- mean(ejemplo1_heterogeneidad$X1 < c)
# prob_bajo_consumo2 <- mean(ejemplo1_heterogeneidad$X2 < c)

#Definimos
c=1
prob_bajo_consumo1_H <- mean(ejemplo1_heterogeneidad$X1 < c)
prob_bajo_consumo2_H <- mean(ejemplo1_heterogeneidad$X2 < c)

prob_bajo_consumo1_H
prob_bajo_consumo2_H

#-------Inciso 7, inciso 5

P1_b <- 1.2 * P1

ejemplo2_heterogeneidad <- demanda_CD_b(Y, P1_b, P2, alpha1, alpha2)
# View(ejemplo2)

media_X1_b_H <- mean(ejemplo2_heterogeneidad$X1_b)


#Comparamos las distribuciones antes y despues del shock para X1

mean(ejemplo1_heterogeneidad$X1)
mean(ejemplo2_heterogeneidad$X1_b)

par(mfrow = c(1, 2)) 

hist(ejemplo1_heterogeneidad$X1, freq = FALSE,
     main = "Heterogeneidad antes del aumento de P1",
     xlab = "X1", xlim = c(0,10),ylim = c(0, 0.9),
     col = "grey")
lines(density(ejemplo1_heterogeneidad$X1), col = "red", lwd = 2)
abline(v = mean(ejemplo1_heterogeneidad$X1), col = "blue", lwd = 2, lty = 2)

hist(ejemplo2_heterogeneidad$X1_b, freq = FALSE,
     main = "Heterogeneidad despues del aumento de P1",
     xlab = "X1_b", xlim = c(0,10), ylim = c(0, 0.9),
     col = "grey")
lines(density(ejemplo2_heterogeneidad$X1_b), col = "purple", lwd = 2)
abline(v = mean(ejemplo2_heterogeneidad$X1_b), col = "blue", lwd = 2, lty = 2)

#La linea azul es la media de cada X1 (antes y despues del shock), ademas de ser un punto de 
# referencia claro para diferenciar ambos graficos.

par(mfrow = c(1,1))

#-------Inciso 7, inciso 6

d_pre_H  <- density(ejemplo1_heterogeneidad$X1)
d_post_H <- density(ejemplo2_heterogeneidad$X1_b)

plot(d_pre_H, col = "red", lwd = 2, main = "Distrib. de X1 pre y post shock con heterogeneidad",
     xlab = "x1", ylim = c(0, max(d_pre_H$y, d_post_H$y)))
lines(d_post_H, col = "blue", lwd = 2)

media_U_pre_H <- mean(ejemplo1_heterogeneidad$U)
media_U_post_H <- mean(ejemplo2_heterogeneidad$U)

media_U_pre_H
media_U_post_H
