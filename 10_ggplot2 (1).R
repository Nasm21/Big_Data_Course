library(dplyr)
library(ggplot2)
library(dslabs)

#https://r-graph-gallery.com/ggplot2-package.html
#Funciona por capas, siendo las más importantes:
#data = dataset
#Aesthetic mapping = Propiedades visuales del gráfico aes()
#Geometry = geom_X where X is the name of the geometry. 
#Some examples include geom_point, geom_bar, and geom_histogram.
#Facets = Permite dividir tu gráfico en subgráficos basados en una 
#o más variables
#Statistics = se utilizan para resumir los datos antes de graficarlos.
#En muchos casos, ggplot2 automáticamente elige una estadística 
#apropiada
#Coordinates = Especifica el sistema de coordenadas utilizado en el gráfico.
#Theme = Controla la apariencia general del gráfico, como los colores, 
#tamaños de fuente, etc

data("murders")
ggplot(data = murders)
ggplot(data=murders,aes(x = population/10^6, y = total))+
  geom_point()

murders %>% ggplot(aes(x = population/10^6, y = total)) + 
  geom_point()

murders %>% ggplot() + 
  geom_point(aes(x = population/10^6, y = total))

#graficos de dispersion x4
murders %>% ggplot(aes(x = population/10^6, 
                       y = total,color=region,shape=region)) + 
  geom_point(show.legend = FALSE)+xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")+ 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") + 
  facet_wrap(~ region, nrow = 2)

#dispersion mejorado.. parece regresion lineal
murders %>% ggplot(aes(x = population/10^6, 
                       y = total,color=region,shape=region)) + 
  geom_point(show.legend = FALSE)+xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")+ 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") + 
  facet_wrap(~ region, nrow = 2)+ 
  geom_smooth(show.legend = FALSE)

#dispersion de diferentes variables
p<- murders %>% ggplot(aes(x = population/10^6, 
                           y = total,color=region)) + 
  geom_point(aes(shape=region))+xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")+ 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") 
p

library(ggthemes)
p + theme_economist()

#Diagrama de barras 
murders %>%
  count(region) %>%
  mutate(porcentaje = (n/sum(n))*100) %>%
  ggplot(aes(region, porcentaje,fill = region)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = sprintf(
    "%.1f%%", porcentaje)),position = position_stack(vjust = 0.8))

#Diagrama de barras ordenado de mayor a menor
 murders %>%
  count(region) %>%
  mutate(proportion = n/sum(n)) %>%
   ggplot(aes(x=reorder(region, -n), proportion,fill = region)) + 
   geom_bar(stat = "identity")
 
 #Diagrama de barras ordenado de menor a mayor
 murders %>%
   count(region) %>%
   mutate(proportion = n/sum(n)) %>%
   ggplot(aes(x=reorder(region, n), proportion,fill = region)) + 
   geom_bar(stat = "identity")
 
#Diagrama circular
murders %>%
  count(region) %>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x="", proportion,fill = region)) + 
  geom_bar(stat = "identity",width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(
    scales::percent(proportion))),position = position_stack(vjust = 0.5)) 

#Boxplot
murders %>% select(region,rate) %>% 
  filter(region=="South") %>% 
  ggplot(aes(region,rate,fill=region)) + geom_boxplot()

murders %>% ggplot(aes(region,rate,fill=region)) + 
  geom_boxplot()

# Facetas
murders %>%
  count(region) %>%
  mutate(proportion = n/sum(n)) %>% 
  ggplot(aes(region, proportion,fill = region)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ region, nrow = 2)

#Varios gráficos

x <- log10(murders$population)
y <- murders$total

p1 <- data.frame(x = x, y = y) %>% 
  ggplot(aes(x,y)) +
  geom_point()
p2 <- data.frame(x = x) %>%
  ggplot(aes(x = x)) +
  geom_histogram()

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
