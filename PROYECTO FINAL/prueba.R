library(dplyr)
library(readr)
library(ggthemes)
library(ggplot2)
library(cowplot)
library(forcats)
library(dslabs)
library(corrplot)
library(nortest)

#Carga de la base de datos
data_avo <- read.csv("avocado.csv")

data_avo$calculada_Venta= data_avo$AveragePrice*data_avo$Total.Volume

#Coeficiente de correlación de Pearson 
#data_avo<-data_avo %>% select(-Date)

data_avo<-data_avo %>% filter(data_avo$region=="Boston",data_avo$region!="TotalUS",data_avo$region!="Total U.S.",data_avo$Total.Volume<=4500, data_avo$Total.Volume>=100)
#data_avo$region=="Boston"

data_avo %>%
  group_by(region) %>%
  summarise(cor((log10(Total.Volume/10^5)),
                log10(calculada_Venta),
                method = "pearson"))

modeloah <- data_avo %>%
  lm(log10(calculada_Venta) ~ log10((Total.Volume/10^5)),data= .)  



modeloah$coefficients

##Coeficientes
Pendiente <- modeloah$coefficients[2];
Intercepto <- modeloah$coefficients[1];
Pendiente
Intercepto


data_avo %>%
  ggplot(aes(x = log10(Total.Volume/10^5),
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
                x = -2, y = 3.1),color = "green",size = 8) 

ei=residuals(modeloah);ei 

pred=fitted(modeloah);pred

realah = data_avo %>%
  summarize(y=log10(calculada_Venta),x=log10((Total.Volume/10^5))) %>%
  arrange(x);realah1

realah2 = data_avo %>%
  summarize(y=calculada_Venta,x=Total.Volume/10^5) %>%
  arrange(x);realah

t.test(ei)

qplot(pred,ei,xlab = "Valores ajustados",
      ylab = "Residuales",ylim = c(-1,1))

ggplot() +
  geom_qq(aes(sample = ei)) +
  geom_abline(intercept = mean(ei), slope = sd(ei), 
              color = "red", linetype = "dashed") +
  labs(title = "QQ Plot de Residuos", 
       x = "Cuantiles teóricos", y = "Cuantiles observados")

ad.test(ei)


library(lmtest)
bptest(modeloah)

dwtest(modeloah,alternative = "two.sided") #greater o less

pred1 <- predict(object = modeloah, 
                 newdata = data.frame(Total.Volume=3.690442))

total_pred=(10^(pred1));round(total_pred,0)

