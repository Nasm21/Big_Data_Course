library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
#Gráfico de dispersión
murders %>% ggplot(aes(x = log10(population/10^6), 
                       y = log10(total),color=region,shape=region)) + 
  geom_point(show.legend = FALSE)+ xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")+ 
  facet_wrap(~ region, nrow = 2)
murders %>%
  count(region)
#Coeficiente de correlación de Pearson 
murders %>%
  group_by(region) %>%
  summarise(cor((log10(population/10^6)),
                log10(total),
                method = "pearson"))

#Modelo de regresión lineal
modelo <- murders %>%
  #filter(region == "Northeast") %>%
  lm(log10(total) ~ log10((population/10^6)),data= .)    

summary <- summary(modelo);summary

modelo$coefficients
##Coeficientes
Pendiente <- modelo$coefficients[2];Pendiente
Intercepto <- modelo$coefficients[1];Intercepto

# ¿Cómo interpretar los coeficientes?
# ¿El modelo es significativo?
# ¿Cómo es el ajuste del modelo?

murders %>%
  #filter(region == "Northeast") %>% 
  ggplot(aes(x = log10(population/10^6),
             y = log10(total))) + 
  geom_point(show.legend = FALSE)+
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in regions in 2010") + 
  geom_smooth(method = "lm", se = FALSE, color = "green") + 
  geom_smooth(method = "loess")+
  theme_light() +
  geom_text(aes(label = paste("y =", 
                              round(Pendiente, 2), 
                              "x +", 
                              round(Intercepto, 2)) ,
            x = 0.1, y = 2.8) ,color = "black",size = 5)

#Examen de residuales
ei=residuals(modelo);ei #muestra la discrepancia entre 
#los valores predichos y los valores observados reales.
#Un residuo positivo indica que la observación es mayor 
#que la predicción, mientras que un residuo negativo 
#indica lo contrario.
pred=fitted(modelo);pred #muestra qué valores predice 
#el modelo para cada observación

real <- murders %>%
  #filter(region == "Northeast") %>%
  summarize(y=log10(total),x=log10((population/10^6))) %>%
  arrange(x);real

#Validación del modelo

#1.Los errores tienen media de cero--------------------------#
#Hipótesis
#Ho = Los errores tienen media de cero
#Ha = Los errores NO tienen media de cero

#Prueba t para la media de ei
t.test(ei)
#Como el p-valor es mayor que el nivel de significancia 0.05, 
#no se rechaza la hipótesis nula

qplot(pred,ei,xlab = "Valores ajustados",
      ylab = "Residuales",ylim = c(-2,2))





#2. Los errores son normales--------------------------------#
#Hipótesis
#Ho = Los errores se distribuyen de forma normal
#Ha = Los errores NO se distribuyen de forma normal

#QQplot es una herramienta gráfica utilizada para evaluar si una 
#muestra de datos se distribuye de manera aproximada a una 
#distribución teórica específica, como la distribución 
#normal.

ggplot() +
  geom_qq(aes(sample = ei)) +
  geom_abline(intercept = mean(ei), slope = sd(ei), 
              color = "red", linetype = "dashed") +
  labs(title = "QQ Plot de Residuos", 
       x = "Cuantiles teóricos", y = "Cuantiles observados")

# si n<30 se usa la Prueba de Shapiro Wilk 
# si n>=30 se usa la Prueba de Anderson Darling

library(nortest)
ad.test(ei)

#shapiro.test(ei)

#Como el p-valor es mayor que el nivel de significancia 0.05, 
#no se rechaza la hipótesis nula

#3.Los errores son homocedásticos, la homocedasticidad sugiere 
#que la variabilidad de los errores es aproximadamente 
#constante en todos los niveles de la variable independiente.
#Hipótesis
#Ho = Los errores son homocedásticos
#Ha = Los errores NO son homocedásticos
# Prueba de Breusch Pagan 
# Varianza es constante
library(lmtest)
bptest(modelo)

#Como el p-valor es mayor que el nivel de significancia 0.05, 
#no se rechaza la hipótesis nula

#4.Los errores son independientes
#Hipótesis
#Ho = Los errores son independientes entre sí
#Ha = Los errores están correlacionados


#Prueba de Durbin Watson
dwtest(modelo,alternative = "two.sided") #greater o less
#Como el p-valor es mayor que el nivel de significancia 0.05, 
#no se rechaza la hipótesis nula

#Como los supuestos son válidos, el modelo se puede usar 
#para realizar predicciones

#Realizar una predicción para population = 20000000

pred1 <- predict(object = modelo, 
        newdata = data.frame(population=19378102))

total_pred=(10^(pred1));round(total_pred,0)

#Matriz de correlación
# Selecciona solo las variables numéricas
numeric_vars <- murders[, sapply(murders, is.numeric)]

# Calcula la matriz de correlación
correlation_matrix <- cor(numeric_vars);correlation_matrix

#Otra opción
library(corrplot)
corrplot(correlation_matrix, method = "number", tl.col = "black")


