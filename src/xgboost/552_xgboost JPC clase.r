# XGBoost  sabor tradicional ,  cambiando algunos de los parametros

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("xgboost")

#Aqui se debe poner la carpeta de la computadora local
setwd("/Users/jpc/Documents/00 Data Science Big data Carreer AI/01 Universidad Austral 2022/01_LabImp01/")  #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita XGBoost
dtrain  <- xgb.DMatrix( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- xgb.train( data= dtrain,
                      param= list( objective=       "binary:logistic",
                                   max_depth=           4,
                                   min_child_weight=   20 ),
                      nrounds= 40
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion )  ) #genero la salida


dir.create( "./labo/",  showWarnings = FALSE ) 
setwd("/Users/jpc/Documents/00 Data Science Big data Carreer AI/01 Universidad Austral 2022/01_LabImp01/labo/") 
dir.create( "./exp/", showWarnings = FALSE )
setwd("/Users/jpc/Documents/00 Data Science Big data Carreer AI/01 Universidad Austral 2022/01_LabImp01/labo/exp/") 
dir.create( "./KA2552/", showWarnings = FALSE )
setwd("/Users/jpc/Documents/00 Data Science Big data Carreer AI/01 Universidad Austral 2022/01_LabImp01/labo/exp/KA2552/") 
archivo_salida  <- "./KA_552_001_probabilidad.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )
