library("data.table", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("xtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("arules", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("ggrepel", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("caret", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("imputeTS", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library(ROCR)
library("stargazer", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")

setwd("d:/Users_info/ALBANPD/My Documents/Bases")

load("d:/Users_info/ALBANPD/My Documents/Bases/Diciembre.Rdata")
load("d:/Users_info/ALBANPD/My Documents/Bases/ClientesActivos.Sep.Dic.Mar.Jul.RData")
load("d:/Users_info/ALBANPD/My Documents/Bases/Autorizacion 07-15 12-15.Rdata")
rm(BaseClientes20150930)
rm(BaseClientes20160331)
rm(BaseClientes20160731)

BaseClientes20151231$Identificacion<-as.character(BaseClientes20151231$Identificacion)
BaseClientes20151231[which(nchar(BaseClientes20151231$Identificacion)==9),]$Identificacion<-as.character(paste0(0,BaseClientes20151231[which(nchar(BaseClientes20151231$Identificacion)==9),]$Identificacion))  
BaseClientes20151231$Identificacion<-as.factor(BaseClientes20151231$Identificacion)


aux<-subset(BaseClientes20151231,select=c("Identificacion",
                                        "IdCuentaTarjeta",
                                        "Segmento_Riesgo",
                                        "RFM"
                                        ))   

Diciembre<-left_join(Diciembre,aux,by=c("Identificacion","IdCuentaTarjeta"))

Reporte47<-read.table("Reporte-RIEG-INCO-00047.txt",
                                    header=TRUE,
                                    sep="\t",
                                    na.strings = "NA")
c<-(sapply(Reporte47, function(x) class(x)[[1]]))       #lista de las clases de las variables

Reporte47<-read.table("Reporte-RIEG-INCO-00047.txt",header=TRUE,sep="\t",na.strings = "NA", colClasses =c(
  "factor",
  "integer",
  "factor",
  "factor",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "integer",
  "integer",
  "factor",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "factor",
  "numeric",
  "numeric",
  "factor",
  "numeric",
  "integer",
  "factor",
  "integer",
  "numeric",
  "factor",
  "factor",
  "factor",
  "numeric",
  "numeric",
  "numeric",
  "integer",
  "factor",
  "factor",
  "factor",
  "numeric",
  "factor",
  "integer"
))

Diciembre<-Diciembre[,-which(names(Diciembre) %in% c("TipoCliente"))]
#####################################33
odbcChannel <-odbcConnect("Tarjeta") #Databases

sqlTables(odbcChannel, tableType = "TABLE")

sqlColumns(odbcChannel, "Autorizacion")



Autorizacion <- sqlExecute(odbcChannel, "SELECT * FROM Autorizacion WHERE FechaConciliacion >= '2015-07-01' and FechaConciliacion <='2016-01-01'", fetch = TRUE)

##########################################3
aux<-Autorizacion %>% group_by(IdCuentaTarjeta) %>% mutate(Autorizaciones6M = n())

aux<-subset(aux,select=c("IdCuentaTarjeta","Autorizaciones6M"))
aux<-as.data.frame(aux)
aux<-subset(aux,!duplicated(IdCuentaTarjeta))
Diciembre<-left_join(Diciembre,aux,by=c("IdCuentaTarjeta"))
Diciembre$Autorizaciones6M<-na.replace(Diciembre$Autorizaciones6M, 0)
#################Numero De seguro#############################
aux<-subset(Autorizacion,TipoConsumo=="TRJSEGURO")  #TRJCONSUMO
aux<-aux %>% group_by(IdCuentaTarjeta) %>% mutate(ConsumosSeguro6M = n())
aux<-subset(aux,!duplicated(IdCuentaTarjeta))
aux<-subset(aux,select=c("IdCuentaTarjeta","ConsumosSeguro6M"))


Diciembre<-left_join(Diciembre,aux,by=c("IdCuentaTarjeta"))

Diciembre$ConsumosSeguro6M<-na.replace(Diciembre$ConsumosSeguro6M, 0)
######################Numero de Consumos###########################
aux<-subset(Autorizacion, TipoConsumo=="TRJCONSUMO")

aux<-Autorizacion %>% group_by(IdCuentaTarjeta) %>% mutate(Consumos6M= n())
aux<-as.data.frame(aux)
aux<-subset(aux,select=c("IdCuentaTarjeta","Consumos6M"))
aux<-subset(aux,!duplicated(IdCuentaTarjeta))

Diciembre<-left_join(Diciembre,aux,by=c("IdCuentaTarjeta"))
Diciembre$Consumos6M<-na.replace(Diciembre$Consumos6M,0)

#################### Consumos Seguros / Autorizaciones #################
Diciembre["SegurossobreConsumos"]<-0
Diciembre$SegurossobreConsumos<-Diciembre$ConsumosSeguro6M/(Diciembre$Consumos6M+1)
##############################Tipo Clientes############################
Diciembre["TipoCliente"]<-"NA"
Diciembre[which(Diciembre$CupoUtilizado==0 & Diciembre$Autorizaciones6M==0),]$TipoCliente<-"Inactivo"
Diciembre[which(Diciembre$TipoCliente=="NA"),]$TipoCliente<-"Activo"

##########################Segmento RFM###################################
Diciembre["SegmentoRFM"]<-"NA"
Diciembre[which(Diciembre$RFM==1 | Diciembre$RFM==2 | Diciembre$RFM==3),]$SegmentoRFM<-"Bajo"
Diciembre[which(Diciembre$RFM==4 | Diciembre$RFM==5 | Diciembre$RFM==6),]$SegmentoRFM<-"Medio"
Diciembre[which(Diciembre$RFM==7 | Diciembre$RFM==8 | Diciembre$RFM==9 | Diciembre$RFM==10),]$SegmentoRFM<-"Alto"
Diciembre[which(is.na(Diciembre$RFM)),]$SegmentoRFM<-"No Asignado"

#############################Segmento TDC################################
Diciembre$SegmentoTDC<-"NA"
Diciembre[which(Diciembre$Edad.Cuenta.Tc<=6),]$SegmentoTDC<-"0.Nuevo 6m"

Diciembre[which(Diciembre$Segmento_Riesgo=="1. Riesgo bajo" & Diciembre$SegmentoRFM=="Alto"),]$SegmentoTDC<-"1.Vip"

Diciembre[which(Diciembre$Segmento_Riesgo=="1. Riesgo bajo" & Diciembre$SegmentoRFM=="Medio"),]$SegmentoTDC<-"2.Preferente"
Diciembre[which(Diciembre$Segmento_Riesgo=="2. Riesgo medio" & Diciembre$SegmentoRFM=="Alto"),]$SegmentoTDC<-"2.Preferente"


Diciembre[which(Diciembre$Segmento_Riesgo=="1. Riesgo bajo" & Diciembre$SegmentoRFM=="Bajo"),]$SegmentoTDC<-"3.Movilizacion A"
Diciembre[which(Diciembre$Segmento_Riesgo=="1. Riesgo bajo" & Diciembre$SegmentoRFM=="No Asignado"),]$SegmentoTDC<-"3.Movilizacion A"
Diciembre[which(Diciembre$Segmento_Riesgo=="2. Riesgo medio" & Diciembre$SegmentoRFM=="Medio"),]$SegmentoTDC<-"3.Movilizacion A"
Diciembre[which(Diciembre$Segmento_Riesgo=="2. Riesgo medio" & Diciembre$SegmentoRFM=="Bajo"),]$SegmentoTDC<-"3.Movilizacion A"
Diciembre[which(Diciembre$Segmento_Riesgo=="2. Riesgo medio" & Diciembre$SegmentoRFM=="No Asignado"),]$SegmentoTDC<-"3.Movilizacion A"


Diciembre[which(Diciembre$Segmento_Riesgo=="3. Riesgo medalt" & Diciembre$SegmentoRFM=="Alto"),]$SegmentoTDC<-"4.Movilizacion B"


Diciembre[which(Diciembre$Segmento_Riesgo=="3. Riesgo medalt" & Diciembre$SegmentoRFM=="Medio"),]$SegmentoTDC<-"5.Normalizacion A"
Diciembre[which(Diciembre$Segmento_Riesgo=="3. Riesgo medalt" & Diciembre$SegmentoRFM=="Bajo"),]$SegmentoTDC<-"5.Normalizacion A"
Diciembre[which(Diciembre$Segmento_Riesgo=="3. Riesgo medalt" & Diciembre$SegmentoRFM=="No Asignado"),]$SegmentoTDC<-"5.Normalizacion A"
Diciembre[which(Diciembre$Segmento_Riesgo=="4. Riesgo alto" & Diciembre$SegmentoRFM=="Alto"),]$SegmentoTDC<-"5.Normalizacion A"


Diciembre[which(Diciembre$Segmento_Riesgo=="4. Riesgo alto" & Diciembre$SegmentoRFM=="Medio"),]$SegmentoTDC<-"6.Normalizacion B"
Diciembre[which(Diciembre$Segmento_Riesgo=="4. Riesgo alto" & Diciembre$SegmentoRFM=="Bajo"),]$SegmentoTDC<-"6.Normalizacion B"
Diciembre[which(Diciembre$Segmento_Riesgo=="4. Riesgo alto" & Diciembre$SegmentoRFM=="No Asignado"),]$SegmentoTDC<-"6.Normalizacion B"


Diciembre[which(Diciembre$SegmentoTDC=="5.Normalizacion A" & Diciembre$CupoUtilizado<100),]$SegmentoTDC<-"0.Nuevo +6m"
Diciembre[which(Diciembre$Segmento_Riesgo=="6.Normalizacion B" & Diciembre$CupoUtilizado<100),]$SegmentoTDC<-"0.Nuevo +6m"


Diciembre[which(Diciembre$Segmento_Riesgo=="5. No Asignado"),]$SegmentoTDC<-"7.Inactivo"

Diciembre[which(Diciembre$SegmentoTDC=="NA"),]$SegmentoTDC<-"8.No Asignado"

table(Diciembre$SegmentoTDC)
####################################################################################
########################Regresion#######################################
Activo<-subset(Diciembre,TipoCliente=="Activo")
Inactivo<-subset(Diciembre,TipoCliente=="Inactivo")

Diciembre<-Activo
#####################Conjunto de Entrenamiento###############################
TrainD <- createDataPartition(Diciembre$IdCuentaTarjeta, p=0.7, list=FALSE)
trainingD <- Diciembre[ TrainD, ]
testingD <- Diciembre[ -TrainD, ]
trainingD %>% count(Objetivo)
testingD %>% count(Objetivo)

####################Balancear el conjunto de entrenamiento#################################

muestra<-sample_n(subset(trainingD, Objetivo==0), 80321,replace = FALSE)  #Activos
#muestra<-sample_n(subset(trainingD, Objetivo==0), 2935,replace = FALSE)  #Activos

muestra["filtro"]<-1
muestra<-subset(muestra, select = c("IdCuentaTarjeta","filtro"))
Buenos<-subset(trainingD, Objetivo==0) %>%left_join(muestra,by=c("IdCuentaTarjeta" = "IdCuentaTarjeta"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-247]
########################Malos####################
n<-4
#n<-21  #Activos
Malos<-do.call("rbind", replicate(n, subset(trainingD, Objetivo==1), simplify = FALSE))

BalanceadaD<-rbind(Buenos,Malos)
BalanceadaD %>% count(Objetivo)


####################Arboles###########################
################Arbol#############################
fit = rpart(Objetivo~ 
              CupoUtilizado,
            method="class",
            data=BalanceadaD)


rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)


########################Variable provincia#############################
table(Diciembre$ProvinciaDomicilio)
Diciembre["Provincia"]<-"NA"
Diciembre[which(Diciembre$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


Diciembre[which(Diciembre$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="CA—AR"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

Diciembre[which(Diciembre$Provincia=="NA"),]$Provincia<-0




Diciembre["Afinidad"]<-"NA"
Diciembre[which(Diciembre$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - CUOTA FACIL"),]$Afinidad<-1
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - GOLD"),]$Afinidad<-1

Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - BLACK"),]$Afinidad<-0
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - PLATINUM"),]$Afinidad<-0
Diciembre[which(Diciembre$Afinidad=="NA"),]$Afinidad<-0


View(table(Diciembre$Afinidad,Diciembre$Objetivo))

Diciembre["TieneSuperAvance12M"]<-"NA"
Diciembre[which(Diciembre$NumeroConsumosSuperAvance12M>0),]$TieneSuperAvance12M<-1
Diciembre[which(Diciembre$NumeroConsumosSuperAvance12M==0),]$TieneSuperAvance12M<-0
table(Diciembre$TieneSuperAvance12M)


Diciembre["CodEstadoCivil"]<-"NA"
Diciembre[which(Diciembre$EstadoCivil=="CASADO"),]$CodEstadoCivil<-1
Diciembre[which(Diciembre$EstadoCivil=="DIVORCIADO"),]$CodEstadoCivil<-1


Diciembre[which(Diciembre$EstadoCivil=="SOLTERO"),]$CodEstadoCivil<-0
Diciembre[which(Diciembre$EstadoCivil=="UNI”N LIBRE"),]$CodEstadoCivil<-0
Diciembre[which(Diciembre$EstadoCivil=="VIUDO"),]$CodEstadoCivil<-0

Diciembre[which(Diciembre$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1


Diciembre["TieneBlack"]<-0
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - BLACK"),]$TieneBlack<-1

Diciembre["TieneGold"]<-0
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - GOLD"),]$TieneGold<-1


Diciembre["TienePlatinum"]<-0
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - PLATINUM"),]$TienePlatinum<-1



Diciembre["SectorComercio"]<-0
Diciembre[which(Diciembre$Sector=="COMERCIO"),]$SectorComercio<-1

Diciembre["SectorDependencia"]<-0
Diciembre[which(Diciembre$Sector=="DEPENDENCIA"),]$SectorDependencia<-1

Diciembre["SectorPersonal"]<-0
Diciembre[which(Diciembre$Sector=="PERSONAL"),]$SectorPersonal<-1

Diciembre["SectorProduccion"]<-0
Diciembre[which(Diciembre$Sector=="PRODUCCION"),]$SectorProduccion<-1

Diciembre["SectorServicios"]<-0
Diciembre[which(Diciembre$Sector=="SERVICIOS"),]$SectorServicios<-1


Diciembre["CodEstadoCivil"]<-1
Diciembre[which(Diciembre$EstadoCivil=="SOLTERO"),]$CodEstadoCivil<-0
Diciembre[which(Diciembre$EstadoCivil=="UNI”N LIBRE"),]$CodEstadoCivil<-0
Diciembre[which(Diciembre$EstadoCivil=="VIUDO"),]$CodEstadoCivil<-0

Diciembre["Instruccion"]<-1
Diciembre[which(Diciembre$NivelInstruccion=="FORMACION INTERMEDIA"),]$Instruccion<-0
Diciembre[which(Diciembre$NivelInstruccion=="SECUNDARIA"),]$Instruccion<-0
Diciembre[which(Diciembre$NivelInstruccion=="UNIVERSIDAD"),]$Instruccion<-0

Diciembre["HastaPrimaria"]<-0
Diciembre[which(Diciembre$NivelInstruccion=="PRIMARIA"),]$HastaPrimaria<-1

Diciembre["HastaUniversidad"]<-0
Diciembre[which(Diciembre$NivelInstruccion=="UNIVERSIDAD"),]$HastaUniversidad<-1

Diciembre["Consume2omasAvances12M"]<-0
Diciembre[which(Diciembre$NumeroConsumosAvance12M<2.5),]$Consume2omasAvances12M<-1

Diciembre["CupoUsadoAvancesHasta"]<-0
Diciembre[which(Diciembre$`Cupo.Ut.Avances/Cupo.Aprobado`<0.0054),]$CupoUsadoAvancesHasta<-1

Diciembre["CodT6CupoUtilizado"]<-1
Diciembre[which(Diciembre$T6.CupoUtilizado>=(-0.44)),]$CodT6CupoUtilizado<-0


Diciembre["TieneMenos300dSinConsumo"]<-1
Diciembre[which(Diciembre$Dias.sin.consumo>300),]$TieneMenos300dSinConsumo<-0



Diciembre["CodCupoUtilizado"]<-0
Diciembre[which(Diciembre$CupoUtilizado<439),]$CodCupoUtilizado<-1



Diciembre["CodPromCupoUtilizado3M"]<-0
Diciembre[which(Diciembre$Prom.CupoUtilizado3<661),]$CodPromCupoUtilizado3M<-1

Diciembre["NormCupoUtilizado"]<-as.numeric(scale(Diciembre$CupoUtilizado))
Diciembre["NormPromCupoUtilizado3M"]<-as.numeric(scale(Diciembre$Prom.CupoUtilizado3))


Diciembre["NormPromCupoUtili3MxTipoClienteActivo"]<-0
Diciembre[which(Diciembre$TipoCliente=="ACTIVO"),]$NormPromCupoUtili3MxTipoClienteActivo<-Diciembre[which(Diciembre$TipoCliente=="ACTIVO"),]$NormPromCupoUtilizado3M


Diciembre["NormPromCupoUtili3MxTipoClienteInactivo"]<-0
Diciembre[which(Diciembre$TipoCliente=="INACTIVO"),]$NormPromCupoUtili3MxTipoClienteInactivo<-Diciembre[which(Diciembre$TipoCliente=="INACTIVO"),]$NormPromCupoUtilizado3M


Diciembre["TieneMas300DiasSinConsumo"]<-0
Diciembre[which(Diciembre$Dias.sin.consumo>300),]$TieneMas300DiasSinConsumo<-1


Diciembre["TieneMas920DiasSinConsumo"]<-1
Diciembre[which(Diciembre$Dias.sin.consumo<946),]$TieneMas920DiasSinConsumo<-0


Diciembre["TieneMenos920DiasSinConsumo"]<-1
Diciembre[which(Diciembre$Dias.sin.consumo>920),]$TieneMenos920DiasSinConsumo<-0

Diciembre["TieneMenos150DiasSinConsumo"]<-1
Diciembre[which(Diciembre$Dias.sin.consumo>150),]$TieneMenos150DiasSinConsumo<-0

Diciembre["Menos8Autorizaciones6M"]<-0
Diciembre[which(Diciembre$Autorizacion12M<8.5),]$Menos8Autorizaciones6M<-1

Diciembre["codCupoUti3MXDiasSinCon"]<-0
Diciembre$codCupoUti3MXDiasSinCon<-Diciembre$TieneMenos920DiasSinConsumo*Diciembre$CodPromCupoUtilizado3M
table(Diciembre$codCupoUti3MXDiasSinCon)

Diciembre["CupoUtilizadoAvanceIguala0"]<-0
Diciembre[which(Diciembre$CupoUtilizadoAvance==0),]$CupoUtilizadoAvanceIguala0<-1

Diciembre["SujetoInteres"]<-0
Diciembre[which(Diciembre$SegmentoTDC=="1.Vip" 
                | Diciembre$SegmentoTDC=="2.Preferente"
                | Diciembre$SegmentoTDC=="3.Movilizacion A" ),]$SujetoInteres<-1

Diciembre["CodCupoUtilizado"]<-0
Diciembre[which(Diciembre$CupoUtilizado<412),]$CodCupoUtilizado<-1

#######################Activos##########################################

modA <- glm(Objetivo~ 
              NormCupoUtilizado+
              #PagosVencidos+
              Otra_Tarjeta+
            #STotalFamiliar+
              SoloSeguro
             +Afinidad
            +Provincia
            
            +CodEstadoCivil
            #+SectorPersonal
            #+SectorComercio
            #+SectorDependencia
            #+SectorServicios
            #+SectorProduccion
            #+TieneGold
            #+TieneBlack
            #+TienePlatinum
            
            #+Numero_tarjetasT6
            +CodT6CupoUtilizado
            
            #+CargasFamiliares
            
             #+CupoUsadoAvancesHasta
            #+Consume2omasAvances12M
            +CupoUtilizadoAvanceIguala0
            #+Instruccion
            #+HastaPrimaria
            #+HastaUniversidad
            #+TieneMenos300dSinConsumo
            #+TieneMenos300dSinConsumo
            #+TieneMenos150DiasSinConsumo
            #+TieneMenos920DiasSinConsumo
            +Menos8Autorizaciones6M
           # +ConsumosSeguro6M
            , data=BalanceadaD,
            family="binomial"(link = "logit"))

summary(modA)


fitted.results <- predict(modA,newdata=testingD,type='response')
pr <- prediction(fitted.results, testingD$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
str(fitted.results)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
######################Gini######################
2*auc-1


#######################K-S########################################
fitted.results <- predict(modA,newdata=testingD,type='response')
aux<-cbind(testingD,fitted.results)

sample1<-subset(aux,Objetivo==1)$fitted.results
sample2<-subset(aux,Objetivo==0)$fitted.results

cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2,na.rm = TRUE), max(sample1, sample2,na.rm = TRUE), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 
y1-y0


# png(file = "c:/temp/ks.png", width = 1024, height = 768, type="cairo-png")
ggplot(aux, aes(x = fitted.results, group = Objetivo, color = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 14) +
  theme(legend.position ="top") +
  xlab("Score") +
  ylab("DistribuciÛn acumulada") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=3) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=3) +
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  ggtitle("Prueba K-S") +
  annotate("text", x = .75, y = .25, 
           label = paste("K-S =", round(y1-y0, 3))) +
  theme(legend.title=element_blank())

#######################Curva Roc######################################
fitted.results <- predict(modA,newdata=testingD,type='response')


aux<-cbind(testingD,fitted.results)
aux$Objetivo<-as.numeric(aux$Objetivo)
aux[which(aux$Objetivo==1),]$Objetivo<-0
aux[which(aux$Objetivo==2),]$Objetivo<-1
head(aux$Objetivo)


basicplot <- ggplot(aux, aes(d = Objetivo, m = fitted.results)) +  geom_roc(labels = FALSE)



basicplot+  
  style_roc(guide = TRUE, xlab = "1- Tasa de Falsos Desertores", 
            ylab = "Tasa de Verdaderos Desertores", theme = theme_bw)+
  #theme_bw(base_size = 14)+
  labs(title="Curva ROC") +
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =",round(calc_auc(basicplot)$AUC, 3))) 

###############Matriz de confuci√≥n##############################
fitted.results <- predict(modA,newdata=testingD,type='response')
fitted.results <- ifelse(fitted.results> 0.5,1,0)
confusionMatrix(testingD$Objetivo, fitted.results)

table(testing$Objetivo, fitted.results)
#####################
library("ResourceSelection", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

hoslem.test(modA$y, fitted(modA), g=10)
#######################Desviaci√≥n estandar#############################

##############################NagelkerkeR2 ######################
library("fmsb", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

NagelkerkeR2(modA)

########################R2########################################
library("pscl", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("BaylorEdPsych", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
PseudoR2(modA)
pR2(modA)
############################McFadden's R squared in R##################

nullmod <- glm(Objetivo~1,, data=Balanceada, 
               family="binomial"(link = "logit"))
1-logLik(mod1)/logLik(nullmod)

############################desviacion estandar de residuos#######################
fitted.results <- predict(mod1,newdata=DicAbr2,type='response')
sd(fitted.results)

######################VIF#####################
vif(modA)
sqrt(vif(modA))
####################Deciles###########################
fitted.results <- predict(modA,newdata=Diciembre,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

Diciembre["Score"]<-predict(modA,newdata=Diciembre,type='response')
Diciembre["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
Diciembre[which(Diciembre$Score <= quan[2]),]$Decil<-"P1"
Diciembre[which(Diciembre$Score <=quan[3] & Diciembre$Score > quan[2]),]$Decil<-"P2"
Diciembre[which(Diciembre$Score <=quan[4] & Diciembre$Score > quan[3]),]$Decil<-"P3"
Diciembre[which(Diciembre$Score <=quan[5] & Diciembre$Score > quan[4]),]$Decil<-"P4"
Diciembre[which(Diciembre$Score <=quan[6] & Diciembre$Score > quan[5]),]$Decil<-"P5"
Diciembre[which(Diciembre$Score <=quan[7] & Diciembre$Score > quan[6]),]$Decil<-"P6"
Diciembre[which(Diciembre$Score <=quan[8] & Diciembre$Score > quan[7]),]$Decil<-"P7"
Diciembre[which(Diciembre$Score <=quan[9] & Diciembre$Score > quan[8]),]$Decil<-"P8"
Diciembre[which(Diciembre$Score <=quan[10] & Diciembre$Score > quan[9]),]$Decil<-"P9"
Diciembre[which(Diciembre$Score >=quan[10]),]$Decil<-"P10"

View(table(Diciembre$Decil,Diciembre$Objetivo))


##############################

aux <- subset(Diciembre,select=c("IdCuentaTarjeta","Objetivo",
                                 "CupoUtilizado",
                                 "CodCupoUtilizado",
              "NormCupoUtilizado",
              "Otra_Tarjeta",
              "STotalFamiliar",
              "SoloSeguro",
            "Afinidad",
            "Provincia",
            "CodEstadoCivil",
            "CupoUtilizadoAvanceIguala0",
            "Menos8Autorizaciones6M",
            "SegmentoTDC",
            "SujetoInteres",
            "Score",
            "Decil"))
write.csv(aux,file = "ClientesActivos.csv")


aux<-subset(Diciembre,Afinidad==1 | SoloSeguro=="SI")
aux<-subset(aux,Decil=="P10" | Decil=="P9")
table(aux$Objetivo)
aux<-subset(Diciembre,Afinidad==1 & SoloSeguro==1)
            