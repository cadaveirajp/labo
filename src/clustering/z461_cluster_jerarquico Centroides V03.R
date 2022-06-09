


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")
# Using tidyverse::
library(tidyverse)

#Aqui se debe poner la carpeta de la computadora local
setwd("/Users/jpc/Documents/00 Data Science Big data Carreer AI/01 Universidad Austral 2022/01_LabImp01/")    #Establezco el Working Directory


#leo el dataset
dataset  <- fread( "./datasets/ST4620/cluster_de_bajas.txt", stringsAsFactors= TRUE)
dataset12  <- fread( "./datasets/ST4620/cluster_de_bajas_12meses.txt", stringsAsFactors= TRUE)

dataset6 <- dataset12 %>%
  filter(pos < 6)


dataset6$cluster2 <- as.factor(dataset6$cluster2)
dataset6$foto_mes <- as.factor(dataset6$foto_mes)
dataset6$pos2 <- as.factor(dataset6$pos)

# dataset6$pos3 <- as.numeric(dataset6$pos)

#convert pos3 as characther date
dataset6$pos3 <- as.character(dataset6$pos)

dataset6[pos2 == 1, pos3 := "2021-10-01"]
dataset6[pos2 == 2, pos3 := "2021-09-01"]
dataset6[pos2 == 3, pos3 := "2021-08-01"]
dataset6[pos2 == 4, pos3 := "2021-07-01"]
dataset6[pos2 == 5, pos3 := "2021-06-01"]

dataset6$pos3 <- as.Date(dataset6$pos3)










dataset6$pos3 [1-6]


# dataset6 %>% 
#  mutate(pos4 = case_when(pos3 == 1~ 2021-10-01,
#                          pos3 == 2~ 2021-09-01,
 #                         pos3 == 3~ 2021-08-01,
   #                       pos3 == 4~ 2021-07-01,
  #                        pos3 == 5~ 2021-06-01,))


#dataset6$pos4 [1]


#dataset6 %>% 
 # mutate(pos3 = if_else(pos2 == 1, "2021-10-01",
#               if_else(pos2 == 2, "2021-09-01", 
#               if_else(pos2 == 3, "2021-08-01",  
#               if_else(pos2 == 4, "2021-07-01", "2021-06-01"))))) %>% 
 # as_tibble()

#ISOdate(year = 2021, month = dataset6$pos, day = 1)

# as.Date(ISOdate(year = yr, month = mo, day = day))




# dataset6$posdate <- as.Date(as.character(dataset6$posdate),          # as.Date & as.character functions
#                   format = "%Y%m%d")
  






# animated graphic

library(gganimate)
library(gapminder)
library(gifski)

### time series 

# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

# Time series


# my date set
#library(plyr)
#group.means<-ddply(dataset6 ,c("cluster2","ctrx_quarter"),summarise,mean=mean(ctrx_quarter))
#group.means

X10 <- aggregate(x=dataset6$ctrx_quarter,
          by=list(dataset6$cluster2,dataset6$pos3),
          FUN=mean)
names(X10) <- c("cluster2" ,"ctrx_quarter")


X1 <- dataset[  , mean(ctrx_quarter),  cluster2, ]  #media de la variable  ctrx_quarter
names(X1) <- c("cluster2" ,"ctrx_quarter")

# tapply(dataset6,cycle(dataset6),mean)


# Usual area chart
p <- dataset6 %>%
  ggplot( aes(x=pos3, y=mcuentas_saldo)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Cuentas saldo ($)") +
  theme_ipsum()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p






# Using Small multiple DEnsity plot


library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~cut) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

ggplot(data=dataset6, aes(x=mcuentas_saldo, group=cluster2, fill=cluster2)) +
  geom_density(adjust=20) +
  theme_ipsum() +
  facet_wrap(~cluster2) +
  theme(
    legend.position="none",
    panel.spacing = unit(1, "lines"),
    axis.ticks.x=element_blank()
  )

# Grafino animado, no funciona



Grafico <- ggplot(data = dataset6) +
  aes(x = ctrx_quarter, y = mtarjeta_visa_consumo, colour = pos2) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  facet_wrap(vars(pos3)) +
  labs(title = 'Pos: {pos3}', 
       x = 'ctrx_quarter', 
       y = 'mtarjeta_visa_consumo') +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  transition_time(pos3) + #Variable de Transición
  ease_aes('linear') #Tipo de Transición

Grafico
#Genero la animación a partir del ggplot
Animacion <- animate(Grafico, 
                     renderer = gifski_renderer(), 
                     nframes = 4) #Cantidad de fotogramas

#Guardo la animación en un archivo formato gif
anim_save(filename = "Gapminder.gif",
          animation = Animacion)

## GG plot animado version 2
 
# Libraries
library(ggplot2)
library(dplyr)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Most basic bubble plot
ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.7)

ggplot(dataset, aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.7)



# grafico base
ggplot(dataset6) +
  aes(x = pos, fill = factor(cluster2)) +
  geom_density(alpha = 0.3) +
  labs(title = "Gráfico de densidad", 
       subtitle = "Millaje en ciudad según número de cilindros",
       caption = "Fuente: datos mpg",
       x = "Foto Mes", y = "Densidad",
       fill = "Segemento") +
  theme(legend.position = "bottom")


dataset6 %>% 
  ggplot(aes(x = pos , y=mtarjeta_visa_consumo, fill = factor(cluster2))) +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2) +
  xlab("Month")+ 
  facet_wrap(~pos,ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dataset6, aes(pos, ctrx_quarter)) +
  geom_point() +
  facet_wrap(vars(cluster2))

ggplot(dataset6, aes(pos, mpayroll)) +
  geom_point() +
  facet_wrap(vars(cluster2))
  


X1 <- dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
names(X1) <- c("cluster2" ,"ctrx_quarter")

X2<- dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
names(X2) <- c("cluster2" ,"mtarjeta_visa_consumo")

X3<-dataset[  , mean(mcuentas_saldo),  cluster2 ]
names(X3) <- c("cluster2" ,"mcuentas_saldo")
X4<-dataset[  , mean(chomebanking_trx),  cluster2 ]
names(X4) <- c("cluster2" ,"chomebanking_trx")

# merge two data frames by ID
#everything <-cbind(X1, X2)
total <- merge(X1, X2, by ="cluster2")



#put all data frames into list
X_list <- list(X1, X2, X3, X4)      

#merge all data frames together
X_list %>% reduce(full_join, by='cluster2')



