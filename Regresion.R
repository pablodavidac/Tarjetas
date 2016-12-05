

Aux<-subset(DicAbr,select=c(
  "NormCupoUtilizado",
    "PagosVencidos",
    "Otra_Tarjeta",
    "SVidaDesgravamen",
    "STotalFamiliar",
    "SoloSeguro",
    "TipoCliente",
  "PromMaxDiasSinconsumo3M",
  "Provincia",
  "T6.CupoUtilizado",
  "MontoSeguro",
  "CodEstadoCivil",
  "CodEstadoCivil",
"Numero_tarjetasT6",
"TieneGold",
"TieneBlack",
"TienePlatinum"
))
Aux$Otra_Tarjeta<-gsub("SI",1,Aux$Otra_Tarjeta)
Aux$Otra_Tarjeta<-gsub("NO",0,Aux$Otra_Tarjeta)
Aux$Otra_Tarjeta<-as.numeric(Aux$Otra_Tarjeta)

Aux$SVidaDesgravamen<-gsub("SI",1,Aux$SVidaDesgravamen)
Aux$SVidaDesgravamen<-gsub("NO",0,Aux$SVidaDesgravamen)
Aux$SVidaDesgravamen<-as.numeric(Aux$SVidaDesgravamen)

Aux$STotalFamiliar<-gsub("SI",1,Aux$STotalFamiliar)
Aux$STotalFamiliar<-gsub("NO",0,Aux$STotalFamiliar)
Aux$STotalFamiliar<-as.numeric(Aux$STotalFamiliar)


Aux$SoloSeguro<-gsub("SI",1,Aux$SoloSeguro)
Aux$SoloSeguro<-gsub("NO",0,Aux$SoloSeguro)
Aux$SoloSeguro<-as.numeric(Aux$SoloSeguro)


Aux$TipoCliente<-gsub("ACTIVO",1,Aux$TipoCliente)
Aux$TipoCliente<-gsub("IN1",0,Aux$TipoCliente)
Aux$TipoCliente<-as.numeric(Aux$TipoCliente)


Aux$Provincia<-as.numeric(Aux$Provincia)

Aux$CodEstadoCivil<-as.numeric(Aux$CodEstadoCivil)

Aux<-Aux[,-13]
table(Aux$TipoCliente)


write.csv(Aux,file="BaseScoring.csv")

mod1 <- glm(Objetivo~ 
              NormCupoUtilizado+
              PagosVencidos+
              Otra_Tarjeta+
              Numero_tarjetasT6+
              SVidaDesgravamen+
              STotalFamiliar+
              SoloSeguro+
              TipoCliente
            +PromMaxDiasSinconsumo3M
            #+Afinidad
            +Provincia
            
            #+`Cupo.Utilizado/Cupo.Aprobado`
            #+T3.CupoUtilizado
            +T6.CupoUtilizado
            #+`Cupo.Ut.Avances/Cupo.Aprobado`
            #+NumeroConsumosAvance3M
            #+Dias.sin.consumo
            #+Prom.DiasMora3
            #+NumeroConsumoConInteres3M
            #+NumeroConsumoSinInteres3M
            #+NumeroConsumosSuperAvance12M
           # +TieneSuperAvance12M
            #+MontoSeguro
            #+EstadoCivil
            #+NivelInstruccion
            #+Sector
            #+Edad.Cliente
            +CargasFamiliares
            +CodEstadoCivil
            +Numero_tarjetasT6
           
           
           +TieneGold
           +TieneBlack
           +TienePlatinum
           
           +SectorComercio
           +SectorPersonal
           +SectorProduccion
           +SectorDependencia
           +Instruccion
           +MesesRecencia
           #+Dias.sin.consumo
            , data=Balanceada,
            family="binomial"(link = "logit"))




mod1 <- glm(Objetivo~ 
             # NormCupoUtilizado+
              PagosVencidos+
              Otra_Tarjeta+
              SVidaDesgravamen+
              STotalFamiliar+
              SoloSeguro+
              TipoCliente
            #+PromMaxDiasSinconsumo3M
            #+Afinidad
            +Provincia
            
            #+`Cupo.Utilizado/Cupo.Aprobado`
            #+T3.CupoUtilizado
            #+T6.CupoUtilizado
            #+`Cupo.Ut.Avances/Cupo.Aprobado`
            #+NumeroConsumosAvance3M
            #+Dias.sin.consumo
            #+Prom.DiasMora3
            #+NumeroConsumoConInteres3M
            #+NumeroConsumoSinInteres3M
            #+NumeroConsumosSuperAvance12M
             #+TieneSuperAvance12M
            #+MontoSeguro
            #+EstadoCivil
            #+NivelInstruccion
            #+Sector
            #+Edad.Cliente
            #+CargasFamiliares
            +CodEstadoCivil
            #+SectorPersonal
            #+SectorComercio
            #+SectorDependencia
            #+SectorServicios
            #+SectorProduccion
            +TieneGold
            +TieneBlack
           +TienePlatinum

           
            +CodT6CupoUtilizado
           
          # +CargasFamiliares
           
           +CupoUsadoAvancesHasta
           #+Consume2omasAvances12M
           #+Instruccion
           #+HastaPrimaria
           #+HastaUniversidad
           +TieneMenos300dSinConsumo
            , data=Balanceada,
            family="binomial"(link = "logit"))

summary(mod1)
table(DicAbr$Sector,DicAbr$Objetivo)
#######################AUC###################################

  fitted.results <- predict(mod1,newdata=testing,type='response')
  pr <- prediction(fitted.results, testing$Objetivo)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf)
  str(fitted.results)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  auc
######################Gini######################
2*auc-1
  

#######################K-S########################################
fitted.results <- predict(mod1,newdata=testing,type='response')
aux<-cbind(testing,fitted.results)

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

###############Matriz de confución##############################
fitted.results <- predict(mod1,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results> 0.5,1,0)
confusionMatrix(testing$Objetivo, fitted.results)

table(testing$Objetivo, fitted.results)

#######################Desviación estandar#############################

##############################NagelkerkeR2 ######################
library("fmsb", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

NagelkerkeR2(mod1)

############################McFadden's R squared in R##################

nullmod <- glm(Objetivo~1,, data=Balanceada, 
               family="binomial"(link = "logit"))
1-logLik(mod1)/logLik(nullmod)

############################desviacion estandar de residuos#######################
fitted.results <- predict(mod1,newdata=DicAbr2,type='response')
sd(fitted.results)



####################Deciles###########################
fitted.results <- predict(mod1,newdata=DicAbr,type='response')
View(quan<-quantile(fitted.results, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)

DicAbr["Score"]<-predict(mod1,newdata=DicAbr,type='response')
DicAbr["Decil"]<-"NA"

################Codificaccion de P1 - P10 ###########################
DicAbr[which(DicAbr$Score <= quan[2]),]$Decil<-"P1"
DicAbr[which(DicAbr$Score <=quan[3] & DicAbr$Score > quan[2]),]$Decil<-"P2"
DicAbr[which(DicAbr$Score <=quan[4] & DicAbr$Score > quan[3]),]$Decil<-"P3"
DicAbr[which(DicAbr$Score <=quan[5] & DicAbr$Score > quan[4]),]$Decil<-"P4"
DicAbr[which(DicAbr$Score <=quan[6] & DicAbr$Score > quan[5]),]$Decil<-"P5"
DicAbr[which(DicAbr$Score <=quan[7] & DicAbr$Score > quan[6]),]$Decil<-"P6"
DicAbr[which(DicAbr$Score <=quan[8] & DicAbr$Score > quan[7]),]$Decil<-"P7"
DicAbr[which(DicAbr$Score <=quan[9] & DicAbr$Score > quan[8]),]$Decil<-"P8"
DicAbr[which(DicAbr$Score <=quan[10] & DicAbr$Score > quan[9]),]$Decil<-"P9"
DicAbr[which(DicAbr$Score >=quan[10]),]$Decil<-"P10"

View(table(DicAbr$Decil,DicAbr$Objetivo))

##########################################################################

#################################Modelo balanceado sin datos atípicos###############
DicAbr2<-DicAbr

####################Reemplazar datos atípicos##############################################
q<-quantile(DicAbr$NormCupoUtilizado)
q<-as.vector(q)
IQR=q[4]-q[2]
(uppWhi<-q[4]+1.5*IQR)
DicAbr2[which(DicAbr2$NormCupoUtilizado>uppWhi),]$NormCupoUtilizado<-uppWhi

q<-quantile(DicAbr$NumeroConsumos3M)
q<-as.vector(q)
IQR=q[4]-q[2]
(uppWhi<-q[4]+1.5*IQR)
DicAbr2[which(DicAbr2$NumeroConsumos3M>uppWhi),]$NumeroConsumos3M<-uppWhi

q<-quantile(DicAbr$Salario)
q<-as.vector(q)
IQR=q[4]-q[2]
(uppWhi<-q[4]+1.5*IQR)
DicAbr2[which(DicAbr2$Salario>uppWhi),]$Salario<-uppWhi

DicAbr2[which(DicAbr2$PagosVencidos>5),]$PagosVencidos<-5

#################Conjunto de entrenamiento y prueba######################################
#Train <- createDataPartition(DicAbr$IdCuentaTarjeta, p=0.7, list=FALSE)
training2 <- DicAbr2[ Train, ]
#testing <- DicAbr[ -Train, ]
#training %>% count(Objetivo)
#testing %>% count(Objetivo)

####################Balancear el conjunto de entrenamiento#################################
muestra<-sample_n(subset(training2, Objetivo==0),741,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("IdCuentaTarjeta","filtro"))
Buenos<-subset(training2, Objetivo==0) %>%left_join(muestra,by=c("IdCuentaTarjeta" = "IdCuentaTarjeta"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-215]
########################Malos####################
n<-24
Malos<-do.call("rbind", replicate(n, subset(training, Objetivo==1), simplify = FALSE))

Balanceada2<-rbind(Buenos,Malos)
Balanceada2 %>% count(Objetivo)


#######################Muestreo de Base#######################################
Buenos<-sample_n(subset(training2, Objetivo==0),8137,replace = FALSE)
Malos<-subset(DicAbr2,Objetivo==1)

BalanceadaM<-rbind(Buenos,Malos)


########################No desertores en P1##########################

Buenos<-sample_n(subset(training, Objetivo==0 & Decil=="P1"),8137,replace = FALSE)
Malos<-subset(DicAbr,Objetivo==1)

BalanceadaC<-rbind(Buenos,Malos)

fitted.results <- predict(mod1,newdata=DicAbr,type='response')
Aux<-cbind(DicAbr,fitted.results)

fitted.results <- ifelse(fitted.results> 0.5,1,0)
table(Aux$Objetivo, Aux$fitted.results)


qplot(fitted.results, data = Aux, geom = "density",
      color = Objetivo, linetype = Objetivo)
