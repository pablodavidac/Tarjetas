library("data.table", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("RODBC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("xtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("arules", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("ggrepel", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("caret", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("ROCR", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("stargazer", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("scales", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("gtable", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("randomForest", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("reshape2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("arm", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("bit64", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("magrittr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("tree", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("party", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("rpart.plot", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plotROC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("DescTools", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
setwd("d:/Users_info/ALBANPD/My Documents/Bases")


Base20151231<-read.table("Base 2015 12 31.csv",
                         header=TRUE,
                         sep=",",
                         na.strings = "NA", 
                         colClasses =c(
                           "factor",
                           "integer",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "numeric",
                           "integer",
                           "integer",
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
                           "factor",
                           "factor",
                           "integer",
                           "numeric",
                           "integer",
                           "numeric",
                           "numeric",
                           "numeric",
                           "factor",
                           "integer",
                           "integer",
                           "integer",
                           "integer",
                           "integer",
                           "integer",
                           "factor",
                           "factor",
                           "numeric",
                           "numeric",
                           "integer",
                           "integer",
                           "integer",
                           "factor",
                           "logical",
                           "logical",
                           "factor",
                           "factor")
)

Base20160430<-read.table("Base 2016 04 30.csv",
                         header=TRUE,
                         sep=",",
                         na.strings = "NA",
                         colClasses =c(
"factor",
"integer",
"numeric",
"numeric",
"numeric",
"integer",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"integer",
"integer",
"factor",
"factor",
"factor",
"factor",
"numeric",
"factor",
"factor",
"factor",
"factor",
"factor",
"numeric",
"integer",
"numeric",
"numeric",
"integer",
"numeric",
"factor",
"integer",
"integer",
"integer",
"integer",
"integer",
"factor",
"factor",
"numeric",
"numeric",
"integer",
"integer",
"integer",
"factor",
"factor",
"integer",
"integer",
"factor",
"integer",
"factor",
"integer",
"factor",
"factor",
"factor",
"integer",
"factor",
"numeric",
"factor",
"numeric",
"numeric"))

clientesDic<-subset(Base20151231,EstadoCuentaTarjeta=="ACTIVA" | EstadoCuentaTarjeta=="APROBADA" )
clientesDic<-subset(clientesDic,select=c("IdCuentaTarjeta","Identificacion"))

clientesAbr<-subset(Base20160430, CuentaTarjetaMonitor==1)
clientesAbr<-subset(clientesAbr,EstadoCuentaTarjeta=="ACTIVA" | EstadoCuentaTarjeta=="APROBADA")
clientesAbr<-subset(clientesAbr, select=c("Identificacion","IdCuentaTarjeta"))

Desertores<-read.csv("DesertoresEne-Ago.csv",
                       header=TRUE,
                       sep=",",
                       na.strings = "NA"
                       )
#########################################################################
R42<-R42[,-which(names(R42) %in% c("FechaActivacionCuentaTarjeta",
                                   #"FechaConciliacionUltimoConsumo",
                                   "PropietariaCartera"))]
Abril<-inner_join(clientesAbr,R42,by=c("Identificacion","IdCuentaTarjeta"))
Abril$Identificacion<-as.factor(Abril$Identificacion)

                                           
SaldosAbril<-SaldosAbril[,-which(names(SaldosAbril) %in% c("EstadoCuentaTarjeta",
                                                           "EstadoTarjeta"
                                                           #"FechaActivacionCuentaTarjeta",
                                                           #"FechaConciliacionUltimoConsumo"
                                                           ))]
Abril<-inner_join(Abril,SaldosAbril,by=c("Identificacion",
                                         "IdCuentaTarjeta"
                                         ))
Abril$Identificacion<-as.factor(Abril$Identificacion)



##########################Diciembre###############3
R43<-R43[,-which(names(R43) %in% c("FechaActivacionCuentaTarjeta",
                                   #"FechaConciliacionUltimoConsumo",
                                   "PropietariaCartera"))]
Diciembre<-inner_join(clientesDic,R43,by=c("Identificacion","IdCuentaTarjeta"))
Diciembre$Identificacion<-as.factor(Diciembre$Identificacion)
SaldosDiciembre<-SaldosDiciembre[,-which(names(SaldosDiciembre) %in% c("EstadoCuentaTarjeta",
                                                                   "EstadoTarjeta"
                                                                   #"FechaActivacionCuentaTarjeta",
                                                                   #"FechaConciliacionUltimoConsumo"
                                                                   ))]
Diciembre<-inner_join(Diciembre,SaldosDiciembre,by=c("Identificacion","IdCuentaTarjeta"))
Diciembre$Identificacion<-as.factor(Diciembre$Identificacion)


############################Calcular Edades############################

######################Para Diciembre###################################


########################Edad cliente######################
d2<-Diciembre$FechaNacimiento
d3<- substr(d2, 0, 10)
#yy<-substr(d3,7,10)
yy<-substr(d3,0,4)
#mm<-substr(d3,4,5)
mm<-substr(d3,6,7)
#dd<-substr(d3,0,2)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2016", "04","30"))
n<-length(Diciembre$FechaNacimiento)
Diciembre["Edad.Cliente"] <- NA
Diciembre$Edad.Cliente<-age_years(d5,rep(as.Date(ISOdate("2015", "12","31")),n))
age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age}

#######################################Edad de la cuenta tarjeta###########################
d2<-Diciembre$FechaActivacionCuentaTarjeta
d3<- substr(d2, 0, 10)
yy<-substr(d3,0,4)
mm<-substr(d3,6,7)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2015", "12","31"))
n<-length(Diciembre$FechaActivacionCuentaTarjeta)
Diciembre["Edad.Cuenta.Tc"] <- "NA"
Diciembre$Edad.Cuenta.Tc<-round((as.Date(ISOdate("2015", "12","31"))-as.Date(d5))/30, digits = 0)
Diciembre$Edad.Cuenta.Tc<-as.numeric(Diciembre$Edad.Cuenta.Tc)
#####################################Tiempo del último consumo#######################
d2<-Diciembre$FechaConciliacionUltimoConsumo
d3<- substr(d2, 0, 10)
yy<-substr(d3,0,4)
mm<-substr(d3,6,7)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2015", "12","31"))
n<-length(Diciembre$FechaConciliacionUltimoConsumo)
Diciembre["Dias.sin.consumo"] <- NA
Diciembre$Dias.sin.consumo<-round((as.Date(ISOdate("2015", "12","31"))-as.Date(d5)), digits = 0)
Diciembre$Meses.sin.consumo<-round((as.Date(ISOdate("2015", "12","31"))-as.Date(d5))/30, digits = 0)
Diciembre$Meses.sin.consumo<-as.numeric(Diciembre$Meses.sin.consumo)
Diciembre$Edad.Cuenta.Tc<-as.numeric(Diciembre$Edad.Cuenta.Tc)
Diciembre$Dias.sin.consumo<-as.numeric(Diciembre$Dias.sin.consumo)


########################################Asalariado########################
#View(subset(Base3,select = c("FuenteIngreso","Sueldo20160131")))
Diciembre["Asalariado"]<-"0"
Diciembre$Asalariado[Diciembre$FuenteIngreso=="SALARIO"]<-"1"
########################Numero_tarjetasDic15##################
Diciembre$Numero_tarjetas[is.na(Diciembre$Numero_tarjetas)]<-0
########################Numero_tarjetasSep15##################
Diciembre$Numero_tarjetasT3[is.na(Diciembre$Numero_tarjetasT3)]<-0
########################Numero_tarjetasJun15##################
Diciembre$Numero_tarjetasT6[is.na(Diciembre$Numero_tarjetasT6)]<-0
##########################NuevaTC##############################33
Diciembre["NuevaTc3M"]<-Diciembre$Numero_tarjetas-Diciembre$Numero_tarjetasT3
Diciembre["NuevaTc6M"]<-Diciembre$Numero_tarjetas-Diciembre$Numero_tarjetasT6

Diciembre["NormCupoUtilizado"]<-as.numeric(scale(Diciembre$CupoUtilizado))
Diciembre["NormPromCupoUtilizado3M"]<-as.numeric(scale(Diciembre$Prom.CupoUtilizado3))


##############################Para Abril###################################



########################Edad cliente######################
d2<-Abril$FechaNacimiento
d3<- substr(d2, 0, 10)
#yy<-substr(d3,7,10)
yy<-substr(d3,0,4)
#mm<-substr(d3,4,5)
mm<-substr(d3,6,7)
#dd<-substr(d3,0,2)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2016", "04","30"))
n<-length(Abril$FechaNacimiento)
Abril["Edad.Cliente"] <- NA
Abril$Edad.Cliente<-age_years(d5,rep(as.Date(ISOdate("2016", "04","30")),n))
age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age}
Abril$Edad.Cliente<-as.numeric(Abril$Edad.Cliente)
#######################################Edad de la cuenta tarjeta###########################
d2<-as.Date(Abril$FechaActivacionCuentaTarjeta)
d3<- substr(d2, 0, 10)
yy<-substr(d3,0,4)
mm<-substr(d3,6,7)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2015", "12","31"))
n<-length(Abril$FechaActivacionCuentaTarjeta)
Abril["Edad.Cuenta.Tc"] <- "NA"
Abril$Edad.Cuenta.Tc<-round((as.Date(ISOdate("2016", "04","30"))-as.Date(d5))/30, digits = 0)
#####################################Tiempo del último consumo#######################
d2<-Abril$FechaConciliacionUltimoConsumo
d3<- substr(d2, 0, 10)
yy<-substr(d3,0,4)
mm<-substr(d3,6,7)
dd<-substr(d3,9,10)
d5<-ISOdate(yy, mm, dd)
d5<-as.Date(d5)
#as.Date(ISOdate("2015", "12","31"))
n<-length(Abril$FechaConciliacionUltimoConsumo)
Abril["Dias.sin.consumo"] <- NA
Abril$Dias.sin.consumo<-round((as.Date(ISOdate("2016", "04","30"))-as.Date(d5)), digits = 0)
Abril$Meses.sin.consumo<-round((as.Date(ISOdate("2016", "04","30"))-as.Date(d5))/30, digits = 0)
Abril$Meses.sin.consumo<-as.numeric(Abril$Meses.sin.consumo)
Abril$Edad.Cuenta.Tc<-as.numeric(Abril$Edad.Cuenta.Tc)
Abril$Dias.sin.consumo<-as.numeric(Abril$Dias.sin.consumo)


########################################Asalariado########################
#View(subset(Base3,select = c("FuenteIngreso","Sueldo20160131")))
Abril["Asalariado"]<-"0"
Abril$Asalariado[Abril$FuenteIngreso=="SALARIO"]<-"1"
########################Numero_tarjetasDic15##################
Abril$Numero_tarjetas[is.na(Abril$Numero_tarjetas)]<-0
########################Numero_tarjetasSep15##################
Abril$Numero_tarjetasT3[is.na(Abril$Numero_tarjetasT3)]<-0
########################Numero_tarjetasJun15##################
Abril$Numero_tarjetasT6[is.na(Abril$Numero_tarjetasT6)]<-0
##########################NuevaTC##############################33
Abril["NuevaTc3M"]<-Abril$Numero_tarjetas-Abril$Numero_tarjetasT3
Abril["NuevaTc6M"]<-Abril$Numero_tarjetas-Abril$Numero_tarjetasT6

##############################################################
Abril["NormCupoUtilizado"]<-as.numeric(scale(Abril$CupoUtilizado))
Abril["NormPromCupoUtilizado3M"]<-as.numeric(scale(Abril$Prom.CupoUtilizado3))



####################Desertores de Abril y Diciembre####################

DesertoresDic<-subset(Diciembre,Objetivo==1)
DesertoresAbr<-subset(Abril,Objetivo==1)

########################No desertores ###########################
NoDesertoresDic<-subset(Diciembre,is.na(Mesdesercion))

NoDesertoresAbr<-subset(Abril,Objetivo==0)
#table(NoDesertoresAbr$EstadoCuentaTarjeta,NoDesertoresAbr$EstadoTarjeta)
#NoDesertoresAbr<-subset(NoDesertoresAbr,!duplicated(Identificacion))

################################Cruzar Abril y Diciembre los no desertores###############
NAbr<-anti_join(NoDesertoresAbr,NoDesertoresDic, by=c("Identificacion","IdCuentaTarjeta"))
NDic<-anti_join(NoDesertoresDic,NoDesertoresAbr, by=c("Identificacion","IdCuentaTarjeta"))
NN<-anti_join(NoDesertoresDic,NDic, by=c("Identificacion","IdCuentaTarjeta"))
NN2<-anti_join(NoDesertoresAbr,NAbr, by=c("Identificacion","IdCuentaTarjeta"))




NDic<-subset(NDic,select=c("Identificacion","IdCuentaTarjeta"))
NDic<-inner_join(NDic,Base20160430,by=c("Identificacion","IdCuentaTarjeta"))
Aux<-subset(NDic,EstadoCuentaTarjeta!="BLOQUEADA X CASTIGO")
Aux<-subset(Aux,Estadotarjeta!="BLOQUEADA X CASTIGO" )
Aux<-subset(Aux,EstadoCuentaTarjeta!="ANULADA")
Aux<-subset(Aux,EstadoCuentaTarjeta!="CANCELADA")


#aux<-subset(NDic,Estadotarjeta=="CANCELADA CLIENTE")



Aux<-subset(Aux,select=c("Identificacion","IdCuentaTarjeta"))
NDic<-anti_join(NoDesertoresDic,NoDesertoresAbr, by=c("Identificacion","IdCuentaTarjeta"))

#####################Poner todas la base a sus respectivos cortes####################
DesertoresDicMayAgo<-subset(Diciembre,Mesdesercion>4)
NDic2<-inner_join(Aux,Diciembre,by=c("Identificacion","IdCuentaTarjeta"))
NN<-subset(NN,select=c("Identificacion","IdCuentaTarjeta"))
NDGrupoDic<-sample_n(NN,91824,replace = FALSE)
NDGrupoAbr<-anti_join(NN,NDGrupoDic,by=c("Identificacion","IdCuentaTarjeta"))
NDGrupoDic<-inner_join(NDGrupoDic,Diciembre,by=c("Identificacion","IdCuentaTarjeta"))
NDGrupoAbr<-inner_join(NDGrupoAbr,Abril,by=c("Identificacion","IdCuentaTarjeta"))
NAbr<-subset(NAbr,select=c("Identificacion","IdCuentaTarjeta"))
NAbr<-inner_join(NAbr,Abril,select=c("Identificacion","IdCuentaTarjeta"))

DicAbr<-rbind(DesertoresDicMayAgo,NDic2,NDGrupoDic,DesertoresDic,NDGrupoAbr,NAbr,DesertoresAbr)





######################Variables montos###########################

summary(DicAbr$MontoSeguro)

DicAbr[which(is.na(DicAbr$MontoSeguro)),]$MontoSeguro<-0
NROW(subset(DicAbr,MontoSeguro==0))


summary(DicAbr$MontoSinSeguro)
DicAbr[which(is.na(DicAbr$MontoSinSeguro)),]$MontoSinSeguro<-0
NROW(subset(DicAbr,MontoSinSeguro==0))


summary(DicAbr$MontoConSeguro)
DicAbr[which(is.na(DicAbr$MontoConSeguro)),]$MontoConSeguro<-0
NROW(subset(DicAbr,MontoConSeguro==0))


DicAbr$MontoSeguro<-DicAbr$MontoConSeguro-DicAbr$MontoSinSeguro
summary(DicAbr$MontoSeguro)


aux<-subset(DicAbr,MontoConSeguro>MontoSinSeguro & MontoSinSeguro==0)
aux<-subset(DicAbr,MontoSeguro>0  & MontoSinSeguro==0)




DicAbr["SoloConsumioSeguro1M"]<-"NO"
DicAbr[which(DicAbr$MontoSeguro>0 & DicAbr$MontoSinSeguro==0),]$SoloConsumioSeguro1M<-"SI"
View(table(DicAbr$SoloConsumioSeguro1M,DicAbr$Objetivo))

DicAbr["Salario"]<-DicAbr$Sueldo/366
DicAbr["PromConsumos6M"]<-(DicAbr$NumeroConsumos6M/6)
DicAbr["PromConsumos3M"]<-(DicAbr$NumeroConsumos3M/3)
DicAbr["PromMaxDiasSinconsumo3M"]<-(DicAbr$Max_Dias_sin_consumos_3M/3)
DicAbr["PromMaxDiasSinconsumo6M"]<-(DicAbr$Max_Dias_sin_consumos_6M/6)
###################################################################

########################Variable provincia#############################
table(DicAbr$ProvinciaDomicilio)
DicAbr["Provincia"]<-"NA"
DicAbr[which(DicAbr$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
DicAbr[which(DicAbr$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
DicAbr[which(DicAbr$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
DicAbr[which(DicAbr$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
DicAbr[which(DicAbr$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
DicAbr[which(DicAbr$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
DicAbr[which(DicAbr$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
DicAbr[which(DicAbr$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


DicAbr[which(DicAbr$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
DicAbr[which(DicAbr$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

DicAbr[which(DicAbr$Provincia=="NA"),]$Provincia<-0




DicAbr["Afinidad"]<-"NA"
DicAbr[which(DicAbr$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
DicAbr[which(DicAbr$AfinidadTarjeta=="TARJETA ALIA - CUOTA FACIL"),]$Afinidad<-1
DicAbr[which(DicAbr$AfinidadTarjeta=="TARJETA ALIA - GOLD"),]$Afinidad<-1

DicAbr[which(DicAbr$AfinidadTarjeta=="TARJETA ALIA - BLACK"),]$Afinidad<-0
DicAbr[which(DicAbr$AfinidadTarjeta=="TARJETA ALIA - PLATINUM"),]$Afinidad<-0
DicAbr[which(DicAbr$Afinidad=="NA"),]$Afinidad<-0


View(table(DicAbr$Afinidad,DicAbr$Objetivo))

DicAbr["TieneSuperAvance12M"]<-"NA"
DicAbr[which(DicAbr$NumeroConsumosSuperAvance12M>0),]$TieneSuperAvance12M<-1
DicAbr[which(DicAbr$NumeroConsumosSuperAvance12M==0),]$TieneSuperAvance12M<-0
table(DicAbr$TieneSuperAvance12M)


DicAbr["CodEstadoCivil"]<-"NA"
DicAbr[which(DicAbr$EstadoCivil=="CASADO"),]$CodEstadoCivil<-1
DicAbr[which(DicAbr$EstadoCivil=="DIVORCIADO"),]$CodEstadoCivil<-1


DicAbr[which(DicAbr$EstadoCivil=="SOLTERO"),]$CodEstadoCivil<-0
DicAbr[which(DicAbr$EstadoCivil=="UNIÓN LIBRE"),]$CodEstadoCivil<-0
DicAbr[which(DicAbr$EstadoCivil=="VIUDO"),]$CodEstadoCivil<-0

DicAbr[which(DicAbr$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1


DicAbr["TieneBlack"]<-0
DicAbr[which(DicAbr$AfinidadTarjeta=="TARJETA ALIA - BLACK"),]$TieneBlack<-1

DicAbr["TieneGold"]<-0
DicAbr[which(DicAbr$AfinidadTarjeta=="TARJETA ALIA - GOLD"),]$TieneGold<-1


DicAbr["TienePlatinum"]<-0
DicAbr[which(DicAbr$AfinidadTarjeta=="TARJETA ALIA - PLATINUM"),]$TienePlatinum<-1



DicAbr["SectorComercio"]<-0
DicAbr[which(DicAbr$Sector=="COMERCIO"),]$SectorComercio<-1

DicAbr["SectorDependencia"]<-0
DicAbr[which(DicAbr$Sector=="DEPENDENCIA"),]$SectorDependencia<-1

DicAbr["SectorPersonal"]<-0
DicAbr[which(DicAbr$Sector=="PERSONAL"),]$SectorPersonal<-1

DicAbr["SectorProduccion"]<-0
DicAbr[which(DicAbr$Sector=="PRODUCCION"),]$SectorProduccion<-1

DicAbr["SectorServicios"]<-0
DicAbr[which(DicAbr$Sector=="SERVICIOS"),]$SectorServicios<-1


DicAbr["CodEstadoCivil"]<-1
DicAbr[which(DicAbr$EstadoCivil=="SOLTERO"),]$CodEstadoCivil<-0
DicAbr[which(DicAbr$EstadoCivil=="UNIÓN LIBRE"),]$CodEstadoCivil<-0
DicAbr[which(DicAbr$EstadoCivil=="VIUDO"),]$CodEstadoCivil<-0

DicAbr["Instruccion"]<-1
DicAbr[which(DicAbr$NivelInstruccion=="FORMACION INTERMEDIA"),]$Instruccion<-0
DicAbr[which(DicAbr$NivelInstruccion=="SECUNDARIA"),]$Instruccion<-0
DicAbr[which(DicAbr$NivelInstruccion=="UNIVERSIDAD"),]$Instruccion<-0

DicAbr["HastaPrimaria"]<-0
DicAbr[which(DicAbr$NivelInstruccion=="PRIMARIA"),]$HastaPrimaria<-1

DicAbr["HastaUniversidad"]<-0
DicAbr[which(DicAbr$NivelInstruccion=="UNIVERSIDAD"),]$HastaUniversidad<-1

DicAbr["Consume2omasAvances12M"]<-1
DicAbr[which(DicAbr$NumeroConsumosAvance12M>1),]$Consume2omasAvances12M<-0

DicAbr["CupoUsadoAvancesHasta"]<-0
DicAbr[which(DicAbr$`Cupo.Ut.Avances/Cupo.Aprobado`<0.041),]$CupoUsadoAvancesHasta<-1

DicAbr["CodT6CupoUtilizado"]<-1
DicAbr[which(DicAbr$T6.CupoUtilizado>(-0.35)),]$CodT6CupoUtilizado<-0


DicAbr["TieneMenos300dSinConsumo"]<-1
DicAbr[which(DicAbr$Dias.sin.consumo>300),]$TieneMenos300dSinConsumo<-0



DicAbr["CodCupoUtilizado"]<-0
DicAbr[which(DicAbr$CupoUtilizado<439),]$CodCupoUtilizado<-1



DicAbr["CodPromCupoUtilizado3M"]<-0
DicAbr[which(DicAbr$Prom.CupoUtilizado3<542),]$CodPromCupoUtilizado3M<-1




DicAbr["NormPromCupoUtili3MxTipoClienteActivo"]<-0
DicAbr[which(DicAbr$TipoCliente=="ACTIVO"),]$NormPromCupoUtili3MxTipoClienteActivo<-DicAbr[which(DicAbr$TipoCliente=="ACTIVO"),]$NormPromCupoUtilizado3M


DicAbr["NormPromCupoUtili3MxTipoClienteInactivo"]<-0
DicAbr[which(DicAbr$TipoCliente=="INACTIVO"),]$NormPromCupoUtili3MxTipoClienteInactivo<-DicAbr[which(DicAbr$TipoCliente=="INACTIVO"),]$NormPromCupoUtilizado3M


DicAbr["TieneMas300DiasSinConsumo"]<-0
DicAbr[which(DicAbr$Dias.sin.consumo>300),]$TieneMas300DiasSinConsumo<-1


DicAbr["TieneMas920DiasSinConsumo"]<-1
DicAbr[which(DicAbr$Dias.sin.consumo<920),]$TieneMas920DiasSinConsumo<-0


DicAbr["TieneMenos920DiasSinConsumo"]<-1
DicAbr[which(DicAbr$Dias.sin.consumo>920),]$TieneMenos920DiasSinConsumo<-0

DicAbr["TieneMenos150DiasSinConsumo"]<-1
DicAbr[which(DicAbr$Dias.sin.consumo>150),]$TieneMenos150DiasSinConsumo<-0

table(DicAbr$NormPromCupoUtilizado3M)
(table(DicAbr$CodT6CupoUtilizado,DicAbr$Objetivo))
table(DicAbr$AfinidadTarjeta)
###########################Cruzes##################################
aux<-subset(DicAbr,is.na(T36.CupoUtilizado))

aux<-subset(DicAbr,is.na(T3.CupoUtilizado) & CupoUtilizado>0)

aux<-subset(Abril,is.na(T6.CupoUtilizado) & CupoUtilizado>0)

aux<-subset(aux,select=c("IdCuentaTarjeta","CupoUtilizado","T3.CupoUtilizado"))

aux<-subset(aux,CupoUtilizado>0)

View(Reporte25[which(Reporte25$IdCuentaTarjeta=="610782"),])


aux<-subset(DicAbr,is.na(Dias.sin.consumo))
hist(aux$CupoUtilizadoAvance)
table(aux$CupoUtilizado)


NROW(aux[which(!is.na(aux$FechaConciliacionUltimoConsumo)),]$FechaConciliacionUltimoConsumo)

table(aux$TipoCliente)

table(aux$CupoUtilizadoAvance)
#################################################################################

summary(DicAbr$NumeroConsumosAvance3M)
DicAbr[which(is.na(DicAbr$NumeroConsumosAvance12M)),]$NumeroConsumosAvance12M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumosAvance6M)),]$NumeroConsumosAvance6M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumosAvance3M)),]$NumeroConsumosAvance3M<-0

summary(DicAbr$NumeroConsumosSuperAvance3M)
DicAbr[which(is.na(DicAbr$NumeroConsumosSuperAvance12M)),]$NumeroConsumosSuperAvance12M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumosSuperAvance6M)),]$NumeroConsumosSuperAvance6M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumosSuperAvance3M)),]$NumeroConsumosSuperAvance3M<-0


summary(DicAbr$NumeroConsumoConInteres6M)
DicAbr[which(is.na(DicAbr$NumeroConsumoConInteres12M)),]$NumeroConsumoConInteres12M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumoConInteres6M)),]$NumeroConsumoConInteres6M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumoConInteres3M)),]$NumeroConsumoConInteres3M<-0


summary(DicAbr$NumeroConsumoSinInteres6M)
DicAbr[which(is.na(DicAbr$NumeroConsumoSinInteres12M)),]$NumeroConsumoSinInteres12M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumoSinInteres6M)),]$NumeroConsumoSinInteres6M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumoSinInteres3M)),]$NumeroConsumoSinInteres3M<-0


summary(DicAbr$NumeroConsumoCorriente6M)
DicAbr[which(is.na(DicAbr$NumeroConsumoCorriente12M)),]$NumeroConsumoCorriente12M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumoCorriente6M)),]$NumeroConsumoCorriente6M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumoCorriente3M)),]$NumeroConsumoCorriente3M<-0


summary(DicAbr$NumeroConsumos3M)
DicAbr[which(is.na(DicAbr$NumeroConsumos12M)),]$NumeroConsumos12M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumos6M)),]$NumeroConsumos6M<-0
DicAbr[which(is.na(DicAbr$NumeroConsumos3M)),]$NumeroConsumos3M<-0



summary(DicAbr$MedioPagoPOS12M)
DicAbr[which(is.na(DicAbr$MedioPagoVoucher12M)),]$MedioPagoVoucher12M<-0
DicAbr[which(is.na(DicAbr$MedioPagoPOS12M)),]$MedioPagoPOS12M<-0



summary(DicAbr$T3.CupoUtilizado)
NROW(DicAbr[which(is.na(DicAbr$T3.CupoUtilizado)),])
NROW(DicAbr[which(is.infinite(DicAbr$T3.CupoUtilizado)),])
DicAbr[which(is.na(DicAbr$T3.CupoUtilizado)),]$T3.CupoUtilizado=(-1)
DicAbr[which(is.infinite(DicAbr$T3.CupoUtilizado)),]$T3.CupoUtilizado<-(DicAbr[which(is.infinite(DicAbr$T3.CupoUtilizado)),]$CupoUtilizado-1)




NROW(DicAbr[which(is.na(DicAbr$T6.CupoUtilizado)),])
NROW(DicAbr[which(is.infinite(DicAbr$T6.CupoUtilizado)),])
DicAbr[which(is.na(DicAbr$T6.CupoUtilizado)),]$T6.CupoUtilizado=(-1)
DicAbr[which(is.infinite(DicAbr$T6.CupoUtilizado)),]$T6.CupoUtilizado<-(DicAbr[which(is.infinite(DicAbr$T6.CupoUtilizado)),]$CupoUtilizado-1)



NROW(DicAbr[which(is.na(DicAbr$T36.CupoUtilizado)),])
NROW(DicAbr[which(is.infinite(DicAbr$T36.CupoUtilizado)),])
DicAbr[which(is.na(DicAbr$T36.CupoUtilizado)),]$T36.CupoUtilizado=(-1)
DicAbr[which(is.infinite(DicAbr$T36.CupoUtilizado)),]$T36.CupoUtilizado<-(DicAbr[which(is.infinite(DicAbr$T36.CupoUtilizado)),]$CupoUtilizado-1)







NROW(DicAbr[which(is.na(DicAbr$`Cupo.Utilizado/Cupo.Aprobado`)),])
NROW(DicAbr[which(is.infinite(DicAbr$`Cupo.Utilizado/Cupo.Aprobado`)),])
DicAbr[which(is.na(DicAbr$`Cupo.Utilizado/Cupo.Aprobado`)),]$`Cupo.Utilizado/Cupo.Aprobado`<-DicAbr[which(is.na(DicAbr$`Cupo.Utilizado/Cupo.Aprobado`)),]$CupoUtilizado
#DicAbr[which(is.infinite(DicAbr$`Cupo.Utilizado/Cupo.Aprobado`)),]$`Cupo.Utilizado/Cupo.Aprobado`<-(DicAbr[which(is.infinite(DicAbr$T36.CupoUtilizado)),]$CupoUtilizado-1)



NROW(DicAbr[which(is.na(DicAbr$`Cupo.Ut.Avances/Cupo.Aprobado`)),])
NROW(DicAbr[which(is.infinite(DicAbr$`Cupo.Ut.Avances/Cupo.Aprobado`)),])
DicAbr[which(is.na(DicAbr$`Cupo.Ut.Avances/Cupo.Aprobado`)),]$`Cupo.Ut.Avances/Cupo.Aprobado`<-DicAbr[which(is.na(DicAbr$`Cupo.Ut.Avances/Cupo.Aprobado`)),]$CupoUtilizadoAvance

summary(DicAbr$Dias.sin.consumo)
NROW(DicAbr[which(is.na(DicAbr$Dias.sin.consumo)),])
NROW(DicAbr[which(is.infinite(DicAbr$Dias.sin.consumo)),])

DicAbr[which(is.na(DicAbr$Dias.sin.consumo)),]$Dias.sin.consumo<-(as.numeric(DicAbr[which(is.na(DicAbr$Dias.sin.consumo)),]$Edad.Cuenta.Tc)*30)


NROW(DicAbr[which(is.na(DicAbr$Edad.Cliente)),])
DicAbr[which(is.na(DicAbr$Edad.Cliente)),]$Edad.Cliente<-mean(DicAbr$Edad.Cliente,na.rm=TRUE)


NROW(DicAbr[which(is.na(DicAbr$CargasFamiliares)),])
DicAbr[which(is.na(DicAbr$CargasFamiliares)),]$CargasFamiliares<-1

NROW(DicAbr[which(is.na(DicAbr$Edad.Cuenta.Tc)),])
DicAbr[which(is.na(DicAbr$Edad.Cuenta.Tc)),]$Edad.Cuenta.Tc<-mean(DicAbr$Edad.Cuenta.Tc,na.rm=TRUE)




######################################################################


#################Conjunto de entrenamiento y prueba######################################
Train <- createDataPartition(DicAbr$IdCuentaTarjeta, p=0.7, list=FALSE)
training <- DicAbr[ Train, ]
testing <- DicAbr[ -Train, ]
training %>% count(Objetivo)
testing %>% count(Objetivo)

####################Balancear el conjunto de entrenamiento#################################
muestra<-sample_n(subset(training, Objetivo==0),741,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("IdCuentaTarjeta","filtro"))
Buenos<-subset(training, Objetivo==0) %>%left_join(muestra,by=c("IdCuentaTarjeta" = "IdCuentaTarjeta"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-241]
########################Malos####################
n<-24
Malos<-do.call("rbind", replicate(n, subset(training, Objetivo==1), simplify = FALSE))

Balanceada<-rbind(Buenos,Malos)
Balanceada %>% count(Objetivo)
####################################################################################


########################Regresión ###############################################

mod1 <- bayesglm(Objetivo~ 
                   Max_Dias_sin_consumos_3M+
                   NormCupoUtilizado+
                   #`Cupo.Utilizado/Cupo.Aprobado`+
                   #Dias.sin.consumo+
                   MesesRecencia+
                   Prom.DiasMora6+
                   Numero_tarjetas+
                   Otra_Tarjeta+
                   NumeroConsumos6M+
                   NumeroConsumoSinInteres6M+
                   #NumeroConsumos12M+
                   NumeroConsumoCorriente12M+
                   SVidaDesgravamen+
                   STotalFamiliar+
                   #MedioPagoVoucher12M+
                   Edad.Cuenta.Tc+
                   #Max_Dias_sin_avances_3M+
                   #Sueldo+
                   SoloSeguro+
                   TipoCliente
                   #SoloConsumioSeguro1M
                 , data=Balanceada, family="binomial"(link = "logit"))

summary(mod1)



mod1 <- glm(as.factor(Objetivo) ~ 
              NormCupoUtilizado+
              `CupoUtilizado/Cupo.Apro.Normal`+
              T6.CupoUtilizado+
              Prom.DiasMora3+
              NumeroConsumoSinInteres6M+
              Max_Dias_sin_consumos_3M+
              Numero_tarjetas+
              #Edad.Cliente+
              #Edad.Cuenta.Tc
            SoloSeguro+
            TipoCliente
            ,data=Balanceada, family="binomial"(link = "logit"))


mod1 <-glm(Objetivo~ 
                   #CupoUtilizado+
                   #CupoAprobado+
                   #Prom.CupoUtilizado3+
                   TipoCliente+
                   #CupoUtilxSoloSeg+
                   
                   NormCupoUtilizado+
                  Prom.DiasMora6+
                   `Cupo.Utilizado/Cupo.Aprobado`+
                   #`Cupo.Ut.Avances/Cupo.Aprobado`+
                   T6.CupoUtilizado+
                   #Prom.DiasMora3+
                   #NumeroConsumoSinInteres6M+
                   Max_Dias_sin_consumos_3M+
                   Numero_tarjetas+
                   #Edad.Cliente+
                   #Edad.Cuenta.Tc+
                   NumeroConsumosAvance12M+
                   NumeroConsumoCorriente3M+
                   #NumeroConsumoConInteres3M+
                   #CargasFamiliares+
                   #Sueldo+
                   # "Sector",
                   MesesRecencia+
                   #MontoSeguro+
                   SoloPlan+
                  SoloSeguro+
                   #SoloPlanSeguro+
                   Otra_Tarjeta
                 ,data=Balanceada, family="binomial"(link = "logit"))



mod1 <- glm(Objetivo~ 
              #Max_Dias_sin_consumos_3M+
              NormCupoUtilizado+
              #`Cupo.Utilizado/Cupo.Aprobado`+
              #Dias.sin.consumo+
              MesesRecencia+
              #Prom.DiasMora6+
              #Numero_tarjetas+
              Otra_Tarjeta+
              #NumeroConsumos6M+
              #NumeroConsumoSinInteres6M+
              #NumeroConsumoConInteres6M+
              #NumeroConsumoCorriente12M+
              SVidaDesgravamen+
              STotalFamiliar+
              #Edad.Cuenta.Tc+
             Salario+
              SoloSeguro+
               #SoloPlan+
               SoloPlanSeguro+ 
              TipoCliente
         #+MedioPagoPOS12M
          #+Max_Dias_sin_avances_3M
          #+NumeroConsumos12M
         +PromMaxDiasSinconsumo3M
         #+PromConsumos3M
          #+MontoSeguro
          #+Edad.Cliente
          #Afinidad
            #SoloConsumioSeguro1M
            , data=Balanceada, 
            family="binomial"(link = "probit"))


summary(mod1)



View(varImp(mod1))


fitted.results <- predict(mod1,newdata=testing,type='response')
pr <- prediction(fitted.results, testing$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
str(fitted.results)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

fitted.results <- predict(mod1,newdata=testing,type='response')
aux<-testing$Objetivo-fitted.results
ks.test(testing$Objetivo, fitted.results)

#Hosmer-Lemeshow 
library(MKmisc)
HLgof.test(fit = fitted.results, obs = testing$Objetivo)

library("MLmetrics", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
Gini(fitted.results, testing$Objetivo)
#KS_Stat(fitted.results, as.numeric(testing$Objetivo))

###############Matriz de confución##############################
fitted.results <- predict(mod1,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results> 0.5,1,0)
confusionMatrix(testing$Objetivo, fitted.results)

table(testing$Objetivo, fitted.results)

####################Deciles###########################
fitted.results <- predict(mod1,newdata=DicAbr,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

DicAbr["Score"]<-predict(mod1,newdata=DicAbr,type='response')
DicAbr["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
DicAbr[which(DicAbr$Score < quan[2]),]$Decil<-"P1"
DicAbr[which(DicAbr$Score <quan[3] & DicAbr$Score >= quan[2]),]$Decil<-"P2"
DicAbr[which(DicAbr$Score <quan[4] & DicAbr$Score >= quan[3]),]$Decil<-"P3"
DicAbr[which(DicAbr$Score <quan[5] & DicAbr$Score >= quan[4]),]$Decil<-"P4"
DicAbr[which(DicAbr$Score <quan[6] & DicAbr$Score >= quan[5]),]$Decil<-"P5"
DicAbr[which(DicAbr$Score <quan[7] & DicAbr$Score >= quan[6]),]$Decil<-"P6"
DicAbr[which(DicAbr$Score <quan[8] & DicAbr$Score >= quan[7]),]$Decil<-"P7"
DicAbr[which(DicAbr$Score <quan[9] & DicAbr$Score >= quan[8]),]$Decil<-"P8"
DicAbr[which(DicAbr$Score <quan[10] & DicAbr$Score >= quan[9]),]$Decil<-"P9"
DicAbr[which(DicAbr$Score >=quan[10]),]$Decil<-"P10"

View(table(DicAbr$Decil,DicAbr$Objetivo))





####################Excluir datos atípicos##################
DicAbr3<-DicAbr
q<-quantile(DicAbr$NormCupoUtilizado)
q<-as.vector(q)
IQR=q[4]-q[2]
(uppWhi<-q[4]+1.5*IQR)
DicAbr3[which(DicAbr3$NormCupoUtilizado>uppWhi),]$NormCupoUtilizado<-uppWhi

q<-quantile(DicAbr$NumeroConsumos3M)
q<-as.vector(q)
IQR=q[4]-q[2]
(uppWhi<-q[4]+1.5*IQR)
DicAbr3[which(DicAbr3$NumeroConsumos3M>uppWhi),]$NumeroConsumos3M<-uppWhi



##############Balanceo de base por muestreo 1 ####################################

table(DicAbr$Objetivo)

MuestraBuenos<-sample_n(subset(DicAbr3, Objetivo==0),8137,replace = FALSE)
Malos<-subset(DicAbr,Objetivo==1)

BalanceadaC<-rbind(MuestraBuenos,Malos)
#######################################################################################

mod1 <- glm(Objetivo~ 
              #Max_Dias_sin_consumos_3M+
              NormCupoUtilizado+
              #`Cupo.Utilizado/Cupo.Aprobado`+
              #Dias.sin.consumo+
              MesesRecencia+
              #Prom.DiasMora6+
              Numero_tarjetas+
              Otra_Tarjeta+
              #NumeroConsumos6M+
              #NumeroConsumoSinInteres6M+
              #NumeroConsumoConInteres6M+
              #NumeroConsumoCorriente12M+
              SVidaDesgravamen+
              STotalFamiliar+
              Salario+
              SoloSeguro+
              #SoloPlanSeguro+ 
              TipoCliente
            +PromMaxDiasSinconsumo3M
          , data=BalanceadaC, 
            family="binomial"(link = "logit"))


summary(mod1)


################Codificaccion de P1 - P10 ###########################
fitted.results <- predict(mod1,newdata=DicAbr2,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

DicAbr2["Score"]<-predict(mod1,newdata=DicAbr2,type='response')

DicAbr2[which(DicAbr2$Score <= quan[2]),]$Decil<-"P1"
DicAbr2[which(DicAbr2$Score <=quan[3] & DicAbr2$Score > quan[2]),]$Decil<-"P2"
DicAbr2[which(DicAbr2$Score <=quan[4] & DicAbr2$Score > quan[3]),]$Decil<-"P3"
DicAbr2[which(DicAbr2$Score <=quan[5] & DicAbr2$Score > quan[4]),]$Decil<-"P4"
DicAbr2[which(DicAbr2$Score <=quan[6] & DicAbr2$Score > quan[5]),]$Decil<-"P5"
DicAbr2[which(DicAbr2$Score <=quan[7] & DicAbr2$Score > quan[6]),]$Decil<-"P6"
DicAbr2[which(DicAbr2$Score <=quan[8] & DicAbr2$Score > quan[7]),]$Decil<-"P7"
DicAbr2[which(DicAbr2$Score <=quan[9] & DicAbr2$Score > quan[8]),]$Decil<-"P8"
DicAbr2[which(DicAbr2$Score <=quan[10] & DicAbr2$Score > quan[9]),]$Decil<-"P9"
DicAbr2[which(DicAbr2$Score >=quan[10]),]$Decil<-"P10"

View(table(DicAbr2$Decil,DicAbr2$Objetivo))



############################Arbol de desicion######################
fit = rpart(Objetivo~ 
              #CupoUtilizado+
              #CupoAprobado+
              #CupoUtilizadoNormal+
              #Prom.CupoUtilizado3+
              
              NormCupoUtilizado+
              TipoCliente+
              Prom.DiasMora6+
              `Cupo.Utilizado/Cupo.Aprobado`+
              #`Cupo.Ut.Avances/Cupo.Aprobado`+
              T6.CupoUtilizado+
              #Prom.DiasMora3+
              #NumeroConsumoSinInteres6M+
              #Max_Dias_sin_consumos_3M+
              Numero_tarjetas+
              #Edad.Cliente+
              #Edad.Cuenta.Tc+
              SoloSeguro+
              #NumeroConsumosAvance12M+
              #NumeroConsumoCorriente3M+
              #NumeroConsumoConInteres3M+
              #CargasFamiliares+
              #Sueldo+
              # "Sector",
              MesesRecencia+
              Dias.sin.consumo+
              SoloPlan+
              #SoloPlanSeguro+
              Otra_Tarjeta,
            method="class",
            control=rpart.control(minsplit=30, cp=0.01),
            data=Balanceada)


rpart.plot(fit)

rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)


############################
