


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
library(data.table)



# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)



#Aqui se debe poner la carpeta de la computadora local
setwd("/Users/jpc/Documents/00 Data Science Big data Carreer AI/01 Universidad Austral 2022/01_LabImp01/")    #Establezco el Working Directory


#leo el dataset foto mes
dataset  <- fread( "./datasets/ST4610/cluster_de_bajas.txt", stringsAsFactors= TRUE)


# boxplot

# library
library(ggplot2)

#  boxplot  mrentabilidad 
dataset$cluster2 <- as.factor(dataset$cluster2)

ggplot(dataset, aes(x=cluster2, y=mrentabilidad, fill=cluster2)) + 
  geom_boxplot()

# grouped boxplot mrentabilidad_annual

ggplot(dataset, aes(x=cluster2, y=mrentabilidad_annual, fill=cluster2)) + 
  geom_boxplot()


# Solo que tiene rentabilidad positiva
##### Solo los que tienen rentabilidad menor a 0

datasetPositivos <- dataset %>%
  filter(mrentabilidad_annual >= 0)


ggplot(datasetPositivos, aes(x=cluster2, y=mrentabilidad_annual, fill=cluster2)) + 
  geom_boxplot()

ggplot(datasetPositivos, aes(x=cluster2, y=mrentabilidad, fill=cluster2)) + 
  geom_boxplot()



##### Solo los que tienen rentabilidad anual y mensual menor a 0

datasetNegativos <- dataset %>%
  filter(mrentabilidad_annual < 0)
###, mrentabilidad >= 0
ggplot(datasetNegativos, aes(x=cluster2, y=mrentabilidad_annual, fill=cluster2)) + 
  geom_boxplot()


####calculos de media de los positivos



X1 <- datasetPositivos[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
names(X1) <- c("cluster2" ,"ctrx_quarter")

X101<-datasetPositivos[  , mean(cmobile_app_trx),  cluster2 ]
names(X101) <- c("cluster2" ,"cmobile_app_trx")

X2<- datasetPositivos[  , mean(mtarjeta_visa_consumo),  cluster2 ]
names(X2) <- c("cluster2" ,"mtarjeta_visa_consumo")

X21<- datasetPositivos[  , mean(mtarjeta_visa_descuentos),  cluster2 ]
names(X21) <- c("cluster2" ,"mtarjeta_visa_descuentos")

X23<- datasetPositivos[  , mean(mtarjeta_master_consumo),  cluster2 ]
names(X23) <- c("cluster2" ,"mtarjeta_master_consumo")

X3<-datasetPositivos[  , mean(mcuentas_saldo),  cluster2 ]
names(X3) <- c("cluster2" ,"mcuentas_saldo")

X31<-datasetPositivos[  , mean(mplazo_fijo_pesos),  cluster2 ]
names(X31) <- c("cluster2" ,"mplazo_fijo_pesos")

X32<-datasetPositivos[  , mean(mplazo_fijo_dolares),  cluster2 ]
names(X32) <- c("cluster2" ,"mplazo_fijo_dolares")


X4<-datasetPositivos[  , mean(chomebanking_trx),  cluster2 ]
names(X4) <- c("cluster2" ,"chomebanking_trx")

X5<-datasetPositivos[  , mean(mprestamos_personales),  cluster2 ]
names(X5) <- c("cluster2" ,"mprestamos_personales")

X51<-datasetPositivos[  , mean(mprestamos_hipotecarios),  cluster2 ]
names(X51) <- c("cluster2" ,"mprestamos_hipotecarios")



X6<-datasetPositivos[  , mean(mtransferencias_recibidas),  cluster2 ]
names(X6) <- c("cluster2" ,"mtransferencias_recibidas")


X7<-datasetPositivos[  , mean(mpayroll),  cluster2 ]
names(X7) <- c("cluster2" ,"mpayroll")

X8<-datasetPositivos[  , mean(mcomisiones),  cluster2 ]
names(X8) <- c("cluster2" ,"mcomisiones")

X9<-datasetPositivos[  , mean(mrentabilidad),  cluster2 ]
names(X9) <- c("cluster2" ,"mrentabilidad")

X10<-datasetPositivos[  , mean(mrentabilidad_annual),  cluster2 ]
names(X10) <- c("cluster2" ,"mrentabilidad_annual")

X11<-datasetPositivos[  , mean(mactivos_margen),  cluster2 ]
names(X11) <- c("cluster2" ,"mactivos_margen")

X12<-datasetPositivos[  , mean(mpasivos_margen),  cluster2 ]
names(X12) <- c("cluster2" ,"mpasivos_margen")

X200<-datasetPositivos[  , mean(cliente_edad),  cluster2 ]
names(X200) <- c("cluster2" ,"cliente_edad")

X201<-datasetPositivos[  , mean(cliente_antiguedad),  cluster2 ]
names(X201) <- c("cluster2" ,"cliente_antiguedad")


###  mcaja_ahorro_dolares



#### Count cluster
cantidad <- dplyr::count(datasetPositivos, cluster2) 

### new way to combine all in oe data frame
##setDT(dataset)[ , .N, keyby = cluster2]
Merge_All_Means <- list(cantidad, X1, X101, X2, X21, X23, X3, X31, X32, X4, X5, X51, X6, X7, X8, X9, X10, X11, X12, X200, X201) %>% reduce(inner_join, by = "cluster2")

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( Merge_All_Means,
        file= "Merge_All_Means_positivos.txt",
        sep= "\t" )




#### Count cluster

# merge two data frames by ID
#everything <-cbind(X1, X2)
### total <- merge(X1, X2, by ="cluster2")


## old way to do it
#put all data frames into list
##X_list <- list(X1, X2, X3, X4)      

#merge all data frames together
#X_list %>% reduce(full_join, by='cluster2')





##### bubble Graphic

# Libraries
library(ggplot2)
library(dplyr)

# mis datos

datasetNegativos %>%
  arrange(desc(ctrx_quarter)) %>%
  mutate(cluster2 = factor(cluster2)) %>%
  ggplot(aes(x=mcuentas_saldo, y=mrentabilidad_annual, size = ctrx_quarter, color=cluster2)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1, 5), name="ctrx_quarter")



# Grafico Rentabilidad negativva -   prestamos_personales

datasetNegativos %>%
  arrange(desc(mprestamos_personales)) %>%
  mutate(cluster2 = factor(cluster2)) %>%
  ggplot(aes(x=mcuentas_saldo, y=mrentabilidad_annual, size = mprestamos_personales, color=cluster2)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1, 10), name="mprestamos_personales")

# Grafico Rentabilidad negativva -   prestamos_personales

datasetNegativos %>%
  arrange(desc(mprestamos_hipotecarios)) %>%
  mutate(cluster2 = factor(cluster2)) %>%
  ggplot(aes(x=mcuentas_saldo, y=mrentabilidad_annual, size = mprestamos_hipotecarios, color=cluster2)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1, 10), name="mprestamos_hipotecarios")

# Grafico Rentabilidad negativva  mprestamos_prendarios


datasetNegativos %>%
  arrange(desc(mprestamos_prendarios)) %>%
  mutate(cluster2 = factor(cluster2)) %>%
  ggplot(aes(x=mcuentas_saldo, y=mrentabilidad_annual, size = mprestamos_prendarios, color=cluster2)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1, 10), name="mprestamos_prendarios")





