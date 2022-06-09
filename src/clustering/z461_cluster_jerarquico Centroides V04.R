


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

###

#Calculo de Medianas ctrx_quarter

X1 <- aggregate(x=dataset6$ctrx_quarter,
          by=list(dataset6$cluster2,dataset6$pos3),
          FUN=mean)
names(X1) <- c("cluster2" , "pos3" ,"ctrx_quarter")


#Calculo de Medianas mtarjeta_visa_consumo
X2 <- aggregate(x=dataset6$mtarjeta_visa_consumo,
                 by=list(dataset6$cluster2,dataset6$pos3),
                 FUN=mean)
names(X2) <- c("cluster2" , "pos3" ,"mtarjeta_visa_consumo")

# agregar columna de tablas

X10 <- cbind(X1, X2$mtarjeta_visa_consumo)
names(X10) <- c("cluster2" , "pos3" ,"ctrx_quarter", "mtarjeta_visa_consumo")


X10$cluster2 <- as.numeric(X10$cluster2)

X101 <- X10 %>%
  filter(cluster2 < 2)

p <- X101 %>%
  ggplot( aes(x=pos3, y=ctrx_quarter)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("ctrx_quarter ($)") +
  theme_ipsum()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p


p2 <- X101 %>%
  ggplot( aes(x=pos3, y=mtarjeta_visa_consumo)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("mtarjeta_visa_consumo ($)") +
  theme_ipsum()

# Turn it interactive with ggplotly
p2 <- ggplotly(p2)
p2



# Libraries
library(ggplot2)
library(dplyr)

# Most basic bubble plot
# Libraries
library(ggplot2)
library(babynames) # provide the dataset: a dataframe called babynames
library(dplyr)
library(hrbrthemes)
library(viridis)

# Keep only 3 names
don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")

# Plot
X1 %>%
  ggplot( aes(x=pos3, y=mtarjeta_visa_consumo, group=cluster2, color=cluster2)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born")

X1

######## No funciona


# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)




#=============================================

## animado sabado

X10$cluster2 <- as.factor(X10$cluster2)

# Usual area chart

p <- X101 %>%
  ggplot(aes(x=pos3, y=ctrx_quarter)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("ctrx_quarter ($)") +
  theme_ipsum()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p


##########################################33333





Grafico <- ggplot(data = X10) +
  aes(x = ctrx_quarter, y = pos3, colour = cluster2) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  facet_wrap(vars(pos3)) +
  labs(title = 'Pos: {pos3}', 
       x = 'ctrx_quarter', 
       y = 'cluster2') +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  transition_time(pos3) + #Variable de Transición
  ease_aes('linear') #Tipo de Transición

Grafico
#Genero la animación a partir del ggplot
Animacion <- animate(Grafico, 
                     renderer = gifski_renderer(), 
                     nframes = 4) #Cantidad de fotogramas



# Using Small multiple DEnsity plot


library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)




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

ggplot(X10, aes(x=pos3, y=mtarjeta_visa_consumo, size = cluster2)) +
  geom_point(alpha=0.7)



# grafico base
ggplot(X10) +
  aes(x = pos3, fill = factor(cluster2)) +
  geom_density(alpha = 0.1) +
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

##install.packages("data.table")         # Install & load data.table package
library("data.table")
Merge_All_Means$cant_cluster <- setDT(dataset)[ , .N, keyby = cluster2]    # Using data.table package

#### Count cluster
##  dplyr::count(dataset, cluster2) 
# merge two data frames by ID
#everything <-cbind(X1, X2)
### total <- merge(X1, X2, by ="cluster2")


## old way to do it
#put all data frames into list
##X_list <- list(X1, X2, X3, X4)      

#merge all data frames together
#X_list %>% reduce(full_join, by='cluster2')

### new way to combine all in oe data frame
##setDT(dataset)[ , .N, keyby = cluster2]
cantidad <- dplyr::count(dataset, cluster2) 
Merge_All_Means <- list(X1, X2, X3, X4,cantidad) %>% reduce(inner_join, by = "cluster2")








