#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

#Aqui se debe poner la carpeta de la computadora local
setwd("/Users/jpc/Documents/00 Data Science Big data Carreer AI/01 Universidad Austral 2022/01_LabImp01/")    #Establezco el Working Directory


#leo el dataset
dataset  <- fread( "./datasets/exp_ST4610_cluster_de_bajas.txt", stringsAsFactors= TRUE)


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





#primero, creo la carpeta donde van los resultados
#dir.create( "./exp/", showWarnings= FALSE )
#dir.create( "./exp/ST4610", showWarnings= FALSE )
# setwd( "~/buckets/b1/exp/ST4610" )


#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset$cluster2 <- as.factor(dataset$cluster2)

dataset %>% 
  ggplot(aes(x=cluster2,y=mtarjeta_visa_consumo, fill=mcuentas_saldo)) +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2) +
  xlab("Year")+ 
  facet_wrap(~continent,ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




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

# Using tidyverse::
library(tidyverse)

#put all data frames into list
X_list <- list(X1, X2, X3, X4)      

#merge all data frames together
X_list %>% reduce(full_join, by='cluster2')



