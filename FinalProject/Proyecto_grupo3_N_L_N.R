library(dplyr)
library(readr)
library(ggthemes)
library(ggplot2)
library(cowplot)
library(forcats)
library(dslabs)
data_acv = read.csv("Avocado.csv")
# FUENTE = https://www.kaggle.com/datasets/alanluo418/avocado-prices-20152019
data_acv$calculada_Venta= data_acv$AveragePrice*data_acv$Total.Volume
numeric_val = data_acv[, sapply(data_acv, is.numeric)]
# Calcula la matriz de correlación
cor_matrix = cor(numeric_val);cor_matrix
max(data_acv$Total.Bags)

#Otra opción
library(corrplot)
corrplot(cor_matrix, method = "number", tl.col = "black")

#clasificacion de los aguacates 
#Aguacate Hass pequeño/mediano (~3-5 oz de aguacate) | Plu 4046 Aguacate
#Aguacate Hass grande (~8-10 oz de aguacate) | Plu 4225 Aguacate
#Aguacate Hass extra grande (~10-15 oz de aguacate) | #4770 Aguacate

#analisis exploratorio
#diagrama de barras por tipo de aguacate
data_acv %>%
  count(type) %>%
  mutate(proportion = n) %>%
  ggplot(aes(type, proportion,fill = type)) + 
  ggtitle("Diagrama de barras ")+
  geom_bar(stat = "identity")+
  geom_text(aes(label= proportion), color="black",  #
            position=position_dodge(0.9), 
            vjust=1.6, 
            size=4.0
  ) + 
  facet_wrap(~"Variable tipo de aguacate") +
  geom_smooth()
#analisis de volumen por año
data_acv %>%
  select(Total.Volume,year) %>%
  filter(year %in% c("2015","2016","2017","2018","2019"))%>%
  group_by(year) %>%
  summarise(sum(Total.Volume)) %>%
  ggplot(aes(year, `sum(Total.Volume)` ,fill = year)) + 
  ggtitle("Analisis De Volumen Por Año ")+
  geom_bar(stat = "identity")+
  geom_text(aes(label= `sum(Total.Volume)`), color="red",  #
            position=position_dodge(0.9), 
            vjust=1.6, 
            size=4.0
  ) + 
  facet_wrap(~"Historico de volumen por año") +
  geom_smooth()
#analisis de precio promedio por año
data_acv %>%
  select(AveragePrice,year) %>%
  filter(year %in% c("2015","2016","2017","2018","2019"))%>%
  group_by(year) %>%
  summarise(round(mean(AveragePrice),2)) %>%
  ggplot(aes(year, `round(mean(AveragePrice), 2)` ,fill = year)) + 
  ggtitle("Analisis De Precio promedio Por Año ")+
  geom_bar(stat = "identity")+
  geom_text(aes(label= `round(mean(AveragePrice), 2)`), color="white",  #
            position=position_dodge(0.9), 
            vjust=1.6, 
            size=8.0
  ) + 
  facet_wrap(~"Historico de Precio Promedio por año") +
  geom_smooth()
# correlacion de precio promedio / volumen vendido
data_acv %>% 
  ggplot(aes(x =AveragePrice , y = Total.Volume/10^5 )) + #valor en cientos de miles
  geom_point()+
  xlab(" Precio promedio por aguacate") + 
  ylab("Cantidad / Volumen de aguacates comprados del 2015 - 2019 ") +
  ggtitle("Relacion Precio/ Volumen")

# historico de bolsas pequeñas vendidas por año
data_acv %>%
  select(Small.Bags,year) %>%
  filter(year %in% c("2015","2016","2017","2018","2019")) %>%
  group_by(year) %>%
  summarise(sum(Small.Bags))%>%
  ggplot(aes(year, `sum(Small.Bags)` ,fill = year)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(label= `sum(Small.Bags)`), color="orange",  #
            position=position_dodge(0.9), 
            vjust=1.6, 
            size=4.0
  ) + 
  facet_wrap(~"HISTORICO BOLSAS PEQUEÑAS VENDIDAS / AÑO") +
  coord_flip()

# historico de bolsas grandes vendidas por año

data_acv %>%
  select(Large.Bags,year) %>%
  filter(year %in% c("2015","2016","2017","2018","2019")) %>%
  group_by(year) %>%
  summarise(sum(Large.Bags))%>%
  ggplot(aes(year, `sum(Large.Bags)` ,fill = year)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(label= `sum(Large.Bags)`), color="orange",  #
            position=position_dodge(0.9), 
            vjust=1.6, 
            size=4.0
  ) + 
  facet_wrap(~"HISTORICO BOLSAS GRANDES VENDIDAS / AÑO") +
  coord_flip()

# historico de bolsas extragrandes vendidas por año

data_acv %>%
  select(XLarge.Bags,year) %>%
  filter(year %in% c("2015","2016","2017","2018","2019")) %>%
  group_by(year) %>%
  summarise(sum(XLarge.Bags))%>%
  ggplot(aes(year, `sum(XLarge.Bags)` ,fill = year)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(label= `sum(XLarge.Bags)`), color="orange",  #
            position=position_dodge(0.9), 
            vjust=1.6, 
            size=4.0
  ) + 
  facet_wrap(~"HISTORICO BOLSAS EXTRAGRANDES VENDIDAS / AÑO") +
  coord_flip()

#Coeficiente de correlación de Pearson 
data_acv %>%
  group_by(region) %>%
  summarise(cor((log10(Total.Volume/10^6)),
                log10(calculada_Venta),
                method = "pearson"))

#Modelo de regresión lineal
modeloah <- data_acv %>%
  lm(log10(calculada_Venta) ~ log10((Total.Volume/10^6)),data= .)    

summary <- summary(modeloah);summary

modeloah$coefficients
##Coeficientes
Pendiente <- modeloah$coefficients[2];Pendiente
Intercepto <- modeloah$coefficients[1];Intercepto

data_acv %>%
  ggplot(aes(x = log10(Total.Volume/10^6),
             y = log10(calculada_Venta))) + 
  geom_point(show.legend = FALSE)+
  xlab("volumen in millions (log scale)") + 
  ylab("Total venta de aguacates (log scale)") +
  ggtitle("cantidades vendidas todos los estados 2015-2019") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  theme_light() +
  geom_text(aes(label = paste("y =", 
                              round(Pendiente, 2), 
                              "x +", 
                              round(Intercepto, 2)),
                x = 0.1, y = 2.8),color = "green",size = 8) 
#Examen de residuales
ei=residuals(modeloah);ei #muestra la discrepancia entre 
#los valores predichos y los valores observados reales.
#Un residuo positivo indica que la observación es mayor 
#que la predicción, mientras que un residuo negativo 
#indica lo contrario.
pred=fitted(modeloah);pred #muestra qué valores predice 
#el modelo para cada observación
realah = data_acv %>%
  summarize(y=log10(calculada_Venta),x=log10((Total.Volume/10^6))) %>%
  arrange(x);realah
#Prueba t para la media de ei
#install.packages("nortest")
library(nortest)

t.test(ei)
#Como el p-valor es mayor que el nivel de significancia 0.05, 
#no se rechaza la hipótesis nula
qplot(pred,ei,xlab = "Valores ajustados",
      ylab = "Residuales",ylim = c(-1,1))

#2. Los errores son normales
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
ad.test(ei)

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
bptest(modeloah)

#Como el p-valor es mayor que el nivel de significancia 0.05, 
#no se rechaza la hipótesis nula

#Prueba de Durbin Watson
dwtest(modeloah,alternative = "two.sided") #greater o less
#Como el p-valor es mayor que el nivel de significancia 0.05, 
#no se rechaza la hipótesis nula

pred1 <- predict(object = modeloah, 
                 newdata = data.frame(Total.Volume=2.971596))

total_pred=(10^(pred1));round(total_pred,0)

