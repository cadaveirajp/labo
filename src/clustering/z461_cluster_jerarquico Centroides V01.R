


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

#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
#campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_trx",
#                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
#                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_trx", "Visa_msaldopesos",
#                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
#                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
#                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
#                    "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
#                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
#                     "mpagomiscuentas")



dataset$cluster2 <- as.factor(dataset$cluster2)
dataset$foto_mes <- as.factor(dataset$foto_mes)

# grafico base
ggplot(dataset12) +
  aes(x = pos, fill = factor(cluster2)) +
  geom_density(alpha = 0.3) +
  labs(title = "Gráfico de densidad", 
       subtitle = "Millaje en ciudad según número de cilindros",
       caption = "Fuente: datos mpg",
       x = "Foto Mes", y = "Densidad",
       fill = "Segemento") +
  theme(legend.position = "bottom")

# grafico base
ggplot(dataset12) +
  aes(x = pos, fill = factor(cluster2)) +
  geom_density(alpha = 0.8) +
  labs(title = "Gráfico de densidad", 
       subtitle = "Millaje en ciudad según número de cilindros",
       caption = "Fuente: datos mpg",
       x = "Posición", y = "Densidad",
       fill = "Segemento") +
  theme(legend.position = "bottom")


dataset12 %>% 
  ggplot(aes(x = pos , y=mtarjeta_visa_consumo, fill = factor(cluster2))) +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2) +
  xlab("Year")+ 
  facet_wrap(~foto_mes,ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(gganimate)

library(gapminder)
library(gifski)

ggplot(dataset12, aes(pos, ctrx_quarter)) +
  geom_point() +
  facet_wrap(vars(cluster2))

ggplot(dataset12, aes(pos, mpayroll)) +
  geom_point() +
  facet_wrap(vars(cluster2))
  
ggplot(dataset12, aes(pos, ctarjeta_visa_trx)) +
  geom_point() +
  facet_wrap(vars(cluster2))


ggplot(dataset12, aes(pos, mtarjeta_visa_consumo)) +
  geom_point() +
  facet_wrap(vars(cluster2))
     labs(title = 'Pos: {frame_time}', 
     x = 'ctrx_quarter', 
     y = 'mtarjeta_visa_consumo') +
       
ggplot(dataset12, aes(pos, mtarjeta_visa_consumo)) +
  geom_point() +
  facet_wrap(vars(cluster2))
     labs(title = 'Pos: {frame_time}', 
     x = 'ctrx_quarter', 
     y = 'mtarjeta_visa_consumo') +
       
     
       
       
         
ggplot(dataset12, aes(pos, mpayroll)) +
     geom_point() +
     facet_wrap(vars(cluster2))
     labs(title = 'Pos: {frame_time}', 
          x = 'Cluster', 
          y = 'mpayroll') +      
       
       
       

summary(mpg)
mpg
Grafico <- ggplot(data = dataset12) +
  aes(x = ctrx_quarter, y = mtarjeta_visa_consumo, colour = pos) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  facet_wrap(vars(pos)) +
  labs(title = 'Pos: {frame_time}', 
       x = 'ctrx_quarter', 
       y = 'mtarjeta_visa_consumo') +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  transition_time(pos) + #Variable de Transición
  ease_aes('linear') #Tipo de Transición

Grafico
#Genero la animación a partir del ggplot
Animacion <- animate(Grafico, 
                     renderer = gifski_renderer(), 
                     nframes = 10) #Cantidad de fotogramas

#Guardo la animación en un archivo formato gif
anim_save(filename = "Gapminder.gif",
          animation = Animacion)


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



