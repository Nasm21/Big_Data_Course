
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)

#grafico de dispersion
murders %>% ggplot(aes(x=log10(population/10^6),
                       y=log10(total),color=region, shape=region)) + 
  geom_point(show.legend = FALSE)+xlab("population in millions(log scale)")+
  ylab("total number of murders(log scale)")+
  ggtittle("US Gun Murders in 2010")+
  facet_wrap(~ region, nrow=2)
#coeficiente de correlacion de pearson
murders %>%
  group_by(region)%>%
  summarise(cor((log10(population/10^6)),
            log10(total),
            method="pearson"))

#modelo de regresion lineal

modelo <- murders %>%
  filter(region == "Northeas") %>%
  lm(log10(total) ~ log10((population/10^6)), data= .)

summary <-summary(modelo); summary

modelo$coefficients
#coeficientes
Pendiente<-modelo$coefficients[2]; Pendiente
Intercepto<-modelo$coefficients[1]; Intercepto

#cómo podemos interpretar los coeficientes? el modelo es significativo
#como es el ajuste del modelo

modelo <- murders %>%
  filter(region == "Northeas") %>%






