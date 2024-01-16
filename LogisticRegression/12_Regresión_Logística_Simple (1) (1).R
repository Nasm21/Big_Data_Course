#La Regresión Logística Simple, desarrollada por 
#David Cox en 1958, es un método de regresión que permite 
#estimar la probabilidad de una variable cualitativa binaria 
#en función de una variable cuantitativa. Una de las principales 
#aplicaciones de la regresión logística es la de clasificación 
#binaria, en el que las observaciones se clasifican en un grupo 
#u otro dependiendo del valor que tome la variable empleada 
#como predictor. Por ejemplo, clasificar a un individuo 
#desconocido como hombre o mujer en función del tamaño de la 
#mandíbula.

library(tidyverse)

library(ISLR) #Contiene datasets del libro 
#"An Introduction to Statistical Learning with 
#Applications in R"
library(dplyr)
data("Default") #Un conjunto de datos simulado que contiene 
#información sobre diez mil clientes. El objetivo aquí es 
#predecir qué clientes incumplirán con su deuda de tarjeta
#de crédito.

#default: Un factor con niveles No y Sí que indica si el 
#cliente incumplió con su deuda.
#student: Un factor con niveles No y Sí que indica si el 
#cliente es estudiante.
#balance: El saldo promedio que el cliente tiene pendiente 
#en su tarjeta de crédito después de realizar su pago mensual.
#income: Ingresos del cliente.

head(Default)

# Se recodifican los niveles No, Yes a 1 y 0
Default <- Default %>%
         select(default, balance) %>%
         mutate(default = recode(default,
                                 "No"  = 0,
                                 "Yes" = 1))
head(Default)

# Ajuste de un modelo lineal por mínimos cuadrados.
modelo_lineal <- lm(default ~ balance, data = Default)
# Variable dependiente o Y= default
# Variabe independiente o X = balance

# Representación gráfica del modelo.
ggplot(data = Default, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) 

#Incluyendo la recta del modelo lineal
ggplot(data = Default, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  geom_smooth(method = "lm", color = "gray20", se = FALSE) +
  theme_bw()  +
  labs(title = "Regresión lineal por mínimos cuadrados",
       y = "Probabilidad default") +
  theme(legend.position = "none")

#Realizar una predicción para alguien con balance=10000
predict(object = modelo_lineal, 
        newdata = data.frame(balance = 10000))

#Para evitar estos problemas la regresión logística transforma 
#el valor devuelto por la regresión lineal (β0+β1X) empleando 
#una función cuyo resultado está siempre comprendido entre 
#0 y 1. Utilizando la función sigmoide.


#Para valores de x muy grandes positivos, el valor de e^−x
#es aproximadamente 0 por lo que el valor de la función 
#sigmoide es 1. Para valores de x muy grandes negativos, 
#el valor e^−x tiende a infinito por lo que el valor de la 
#función sigmoide es 0.

# Ajuste de un modelo logístico.
modelo_logistico <- glm(default ~ balance, 
                        data = Default, 
                        family = "binomial")
#Variable de respuesta=default
#Variable predictora=balance
#binomial porque tenemos una variable de respuesta con dos opciones

#names(modelo_logistico)

# Representación gráfica del modelo.
ggplot(data = Default, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo_logistico,
                                          newdata = data.frame(balance = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

# Con geom_smooth se puede obtener el gráfico directamente.
ggplot(data = Default, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE) +
  theme_bw() +
  theme(legend.position = "none")

#Identificar la ecuación
#log(p/1-p)=β0+β1X
summary <- summary(modelo_logistico)
summary
Intercepto <- summary$coefficients[1,1];Intercepto
Pendiente <- summary$coefficients[2,1];Pendiente



ggplot(data = Default, aes(x = balance, y = default)) +
  ggtitle("Grupo 3")+
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE) +
  theme_bw() +
  theme(legend.position = "none")+
  geom_text(aes(label = paste("ln(p/(1-p)) =", 
                              round(Pendiente, 2), 
                              "x +", 
                              round(Intercepto, 2)),
                x = 900, y = 1.05),color = "black",size = 4)+
  geom_text(aes(label = "Juan Franco\n Natalia Santamaria\n Luis Giron \n Nilson Mossos",
                x = 900, y = 0.3),color = "black",size = 4)
#ln(p/1-p)= -10.65 + 0.005499x

#Tarea para hoy Incluir la ecuación en el gráfico con su nombre

#Test de Wald para coeficientes individuales
#Significancia de la variable independiente
#Ho:La variable NO es significativa.
#Ha:La variable es significativa.
#summary
#como el p-valor de los coeficientes es menor que el nivel
#de significancia 0.05 se rechaza la hipótesis nula. Es decir,
#la variable es significativa.

#ln(p/1-p)= -10.65 + 0.005499x
#Analizar los coeficientes
modelo_logistico$coefficients
exp(modelo_logistico$coefficients)
exp(Pendiente)
exp(Intercepto)

#odd Ratio de la variable independiente es 1.005514
#Por cada unidad que aumenta la variable balance el odds
#de que se presente el evento aumenta 1.005514 veces.

#Calcular las probabilidades
head(modelo_logistico$fitted.values)

head(round(modelo_logistico$fitted.values,0),10)
#Probabilidad de que ocurra el evento 1 y así sucesivamente

#Clasificar según la probabilidad
#Elegir un punto de corte
Default$pred <- as.numeric(modelo_logistico$fitted.values>=0.5)

df <- data.frame(modelo_logistico$fitted.values,Default$pred)

# Crear un nuevo conjunto de datos para hacer predicciones 
#(balance es la variable independiente)

nuevos_datos <- data.frame(balance = c(100, 500, 
                                       1000, 1500,
                                       2000, 2500))

# Realizar predicciones con el modelo logístico
predicciones <- predict(modelo_logistico, 
                        newdata = nuevos_datos, 
                        type = "response")
#type = "response" se especifica para obtener las 
#probabilidades predichas.

# Mostrar las predicciones
print(predicciones)

#Clasificar según un umbral
umbral <- 0.5
clases_predichas <- ifelse(predicciones >= umbral, "Yes", "No")
print(clases_predichas)

#consultar y agregar al script la Matriz de confusión

Default$default
Default$pred

install.packages("caret")
library("caret")

tp<-sum(Default$default==1 & Default$pred==1)
tn<-sum(Default$default==0 & Default$pred==0)
fp<-sum(Default$default==0 & Default$pred==1)
fn<-sum(Default$default==1 & Default$pred==0)
c(tp,tn,fp,fn)

matriz <- confusionMatrix(as.factor(Default$default),as.factor(Default$pred))
matriz






