####################Histogramas de variables##########################
summary(DicAbr$CupoUtilizado)

Aux<-subset(Base,Objetivo==0 & is.finite(Base$Edad.Cuenta.Tc))


#######################Cupo Utilizado################################

######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$CupoUtilizado
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 5000, by = 100), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores Cupo Utilizado") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  xlim(0,5000)+
  scale_x_continuous(breaks=seq(0,5000,by=500))+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$CupoUtilizado
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 5000, by = 100), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores Cupo Utilizado") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  xlim(0,5000)+
  scale_x_continuous(breaks=seq(0,5000,by=500))+
  theme_bw()


################Dsitribuci贸n acumulada##################33

ggplot(Aux, aes(CupoUtilizado)) + stat_ecdf(geom = "step")


df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
                 g = gl(2, 100))

ggplot(DicAbr, aes(x, colour = g)) + stat_ecdf()



ggplot(DicAbr, aes(x = CupoUtilizado)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado", x="D贸lares", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,12000,by=1000))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################


#######################Promedio Dias en mora################################

summary(DicAbr$Prom.DiasMora3)
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$Prom.DiasMora3
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 30, by = 1), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores Promedio d铆as en mora 3 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  xlim(0,5000)+
  scale_x_continuous(breaks=seq(0,30,by=2))+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Prom.DiasMora3
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 30, by = 1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores Promedio d铆as en mora 3 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  xlim(0,5000)+
  scale_x_continuous(breaks=seq(0,30,by=2))+
  theme_bw()


################Dsitribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = Prom.DiasMora3)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Promedio d铆as en mora 3 meses", x="D铆as", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,180,by=10))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################




#######################N煤mero de consumos avances 12 meses################################

summary(DicAbr$NumeroConsumosAvance12M)
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$NumeroConsumosAvance12M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 20, by = 1), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - N煤mero de avances en 12 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="N煤mero de avances", y="Frecuencia") + 
  xlim(0,20)+
  scale_x_continuous(breaks=seq(0,20,by=2))+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$NumeroConsumosAvance12M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 20, by = 1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - N煤mero de avances en 12 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="N煤mero de avances", y="Frecuencia") + 
  xlim(0,20)+
  scale_x_continuous(breaks=seq(0,20,by=2))+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = NumeroConsumosAvance12M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="N煤mero de avances en 12 meses", x="N煤mero de avances", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,70,by=10))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################



#######################N煤mero de consumos Superavances 12 meses################################

summary(DicAbr$NumeroConsumosSuperAvance12M)
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$NumeroConsumosSuperAvance12M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 7, by = 1), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - N煤mero de consumos Superavances 12 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="N煤mero de Superavances", y="Frecuencia") + 
  xlim(0,7)+
  scale_x_continuous(breaks=seq(0,7,by=2))+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$NumeroConsumosSuperAvance12M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 7, by = 1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - N煤mero de consumos Superavances 12 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="N煤mero de Superavances", y="Frecuencia") + 
  xlim(0,7)+
  scale_x_continuous(breaks=seq(0,7,by=2))+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = NumeroConsumosSuperAvance12M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="N煤mero de consumos Superavances 12 meses", x="N煤mero de avances", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,7,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################



Aux<-subset(DicAbr,is.na(NumeroConsumoConInteres12M))

#######################N煤mero de consumos con intereses 12 meses################################

summary(DicAbr$NumeroConsumoConInteres12M)
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$NumeroConsumoConInteres12M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 70, by = 5), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Consumos con interes 12 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Consumos", y="Frecuencia") + 
  xlim(0,7)+
  scale_x_continuous(breaks=seq(0,70,by=5))+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$NumeroConsumoConInteres12M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 70, by = 5), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Consumos con interes 12 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="consumos", y="Frecuencia") + 
  xlim(0,7)+
  scale_x_continuous(breaks=seq(0,70,by=5))+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = NumeroConsumoConInteres12M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Consumos con interes 12 meses", x="Consumos", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,220,by=20))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################


#######################N煤mero de consumos corrientes 3 meses################################

summary(DicAbr$NumeroConsumoCorriente3M)
aux<-subset(DicAbr,is.infinite(NumeroConsumoCorriente3M))

######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$NumeroConsumoCorriente3M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 100, by = 1), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Consumos corrientes en 3 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Consumos", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,100,by=1),limits=c(0,100))+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$NumeroConsumoCorriente3M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 100, by = 1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Consumos corrientes en 3 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="consumos", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,100,by=5))+
  xlim(0,20)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = NumeroConsumoCorriente12M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Consumos corrientes en 12 meses", x="Consumos", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,100,by=1),limits=c(0,20))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################

#######################N煤mero de consumos sin interes 6 meses################################

summary(DicAbr$NumeroConsumoSinInteres6M)
aux<-subset(DicAbr,is.infinite(NumeroConsumoSinInteres6M))

######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$NumeroConsumoSinInteres6M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 30, by = 1), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Consumos sin intereses en 6 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Consumos", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,30,by=1))+
  xlim(0,30)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$NumeroConsumoSinInteres6M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 30, by = 1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Consumos sin intereses en 6 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="consumos", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,30,by=1))+
  xlim(0,30)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = NumeroConsumoSinInteres6M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Consumos sin intereses en 6 meses", x="Consumos", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,30,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(0,30)+
  theme_bw()

################################################################


#######################N煤mero Cupo Utilizado t/t-3################################

summary(DicAbr$T3.CupoUtilizado)
auxInf<-subset(DicAbr,is.infinite(T3.CupoUtilizado))
auxNa<-subset(DicAbr,is.na(T3.CupoUtilizado))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$T3.CupoUtilizado
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 10, by = 0.5), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Cupo Utilizado t/t-3") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(-1,10,by=0.5))+
  xlim(-1,10)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$T3.CupoUtilizado
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 10, by = 0.5), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Cupo Utilizado t/t-3") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(-1,10,by=0.5))+
  xlim(-1,10)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = T3.CupoUtilizado)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado t/t-3", x="", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(-1,10,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(-1,10)+
  theme_bw()

################################################################


#######################N煤mero Cupo Utilizado t/t-6################################

summary(DicAbr$T6.CupoUtilizado)
auxInf<-subset(DicAbr,is.infinite(T6.CupoUtilizado))
auxNa<-subset(DicAbr,is.na(T6.CupoUtilizado))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$T6.CupoUtilizado
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 10, by = 0.5), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Cupo Utilizado t/t-6") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(-1,10,by=0.5))+
  xlim(-1,10)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$T6.CupoUtilizado
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 10, by = 0.5), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Cupo Utilizado t/t-6") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(-1,10,by=0.5))+
  xlim(-1,10)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = T6.CupoUtilizado)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado t/t-6", x="", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(-1,10,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(-1,10)+
  theme_bw()

################################################################



#######################N煤mero Cupo Utilizado t-3/t-6################################

summary(DicAbr$T36.CupoUtilizado)
auxInf<-subset(DicAbr,is.infinite(T36.CupoUtilizado))
auxNa<-subset(DicAbr,is.na(T36.CupoUtilizado))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$T36.CupoUtilizado
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 10, by = 0.5), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Cupo Utilizado t-3/t-6") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(-1,10,by=0.5))+
  xlim(-1,10)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$T36.CupoUtilizado
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 10, by = 0.5), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Cupo Utilizado t-3/t-6") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(-1,10,by=0.5))+
  xlim(-1,10)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = T36.CupoUtilizado)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado t-3/t-6", x="", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(-1,10,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(-1,10)+
  theme_bw()

################################################################


#######################Cupo Utilizado Normal################################
summary(DicAbr$CupoUtilizadoNormal)
auxInf<-subset(DicAbr,is.infinite(CupoUtilizadoNormal))
auxNa<-subset(DicAbr,is.na(CupoUtilizadoNormal))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$CupoUtilizadoNormal
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 5000, by = 100), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Cupo Utilizado Normal") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,5000,by=500))+
  xlim(0,5000)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$CupoUtilizadoNormal
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 5000, by = 100), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Cupo Utilizado Normal") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,5000,by=500))+
  xlim(0,5000)+
  theme_bw()


################Dsitribuci贸n acumulada##################33

ggplot(DicAbr, aes(x = CupoUtilizadoNormal)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado Normal", x="D贸lares", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,12000,by=2000))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################


#######################Promedio Cupo Utilizado 3 meses################################
summary(DicAbr$Prom.CupoUtilizado3)
auxInf<-subset(DicAbr,is.infinite(Prom.CupoUtilizado3))
auxNa<-subset(DicAbr,is.na(Prom.CupoUtilizado3))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$Prom.CupoUtilizado3
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 5000, by = 100), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Promedio Cupo Utilizado 3 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,5000,by=500))+
  xlim(0,5000)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Prom.CupoUtilizado3
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 5000, by = 100), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Promedio Cupo Utilizado 3 meses") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,5000,by=500))+
  xlim(0,5000)+
  theme_bw()


################Dsitribuci贸n acumulada##################33

ggplot(DicAbr, aes(x = Prom.CupoUtilizado3)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Promedio Cupo Utilizado 3 meses", x="D贸lares", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,12000,by=2000))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################




#######################Cupo.Utilizado/Cupo.Aprobado################################

summary(DicAbr$`Cupo.Utilizado/Cupo.Aprobado`)
auxInf<-subset(DicAbr,is.infinite(`Cupo.Utilizado/Cupo.Aprobado`))
auxNa<-subset(DicAbr,is.na(`Cupo.Utilizado/Cupo.Aprobado`))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$`Cupo.Utilizado/Cupo.Aprobado`
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 1, by = 0.1), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Cupo Utilizado/Cupo Aprobado") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  xlim(0,1)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$`Cupo.Utilizado/Cupo.Aprobado`
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 1, by = 0.1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Cupo Utilizado/Cupo Aprobado") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  xlim(0,1)+
  theme_bw()


################Distribuci贸n acumulada##################33
summary(DicAbr$`Cupo.Utilizado/Cupo.Aprobado`)

ggplot(DicAbr, aes(x = `Cupo.Utilizado/Cupo.Aprobado`)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado/Cupo Aprobado", x="", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,1,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(0,1)+
  theme_bw()

################################################################



#######################Cupo.Utilizado Avances/Cupo.Aprobado################################

summary(DicAbr$`Cupo.Ut.Avances/Cupo.Aprobado`)
auxInf<-subset(DicAbr,is.infinite(`Cupo.Ut.Avances/Cupo.Aprobado`))
auxNa<-subset(DicAbr,is.na(`Cupo.Ut.Avances/Cupo.Aprobado`))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$`Cupo.Ut.Avances/Cupo.Aprobado`
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 1, by = 0.1), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Cupo Utilizado Avances/Cupo Aprobado") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  xlim(0,1)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$`Cupo.Ut.Avances/Cupo.Aprobado`
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(min(aux,na.rm = TRUE), 1, by = 0.1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Cupo Utilizado Avances/Cupo Aprobado") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  xlim(0,1)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = `Cupo.Ut.Avances/Cupo.Aprobado`)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado Avances/Cupo Aprobado", x="", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,1,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(0,1)+
  theme_bw()

################################################################



#######################M谩ximo d铆as sin consumo en 3 meses################################

summary(DicAbr$Max_Dias_sin_consumos_3M)
auxInf<-subset(DicAbr,is.infinite(Max_Dias_sin_consumos_3M))
auxNa<-subset(DicAbr,is.na(Max_Dias_sin_consumos_3M))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$Max_Dias_sin_consumos_3M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 100, by =10 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - M谩ximo d铆as sin consumo en 3 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D铆as", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,100,by=10))+
  xlim(30,100)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Max_Dias_sin_consumos_3M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 100, by = 10), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - M谩ximo d铆as sin consumo en 3 meses") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D铆as", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,100,by=10))+
  xlim(30,100)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = Max_Dias_sin_consumos_3M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="M醲imo d韆s sin consumo en 3 meses", x="D韆s", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(30,90,by=10),limits = c(30,90))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################


######################D铆as sin consumo ################################

summary(DicAbr$Dias.sin.consumo)
auxInf<-subset(DicAbr,is.infinite(Dias.sin.consumo))
auxNa<-subset(DicAbr,is.na(Dias.sin.consumo))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$Dias.sin.consumo
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 6424, by =250 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Dias desde el 煤ltimo consumo") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D铆as", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,6424,by=500))+
  #xlim(30,100)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Dias.sin.consumo
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 6424, by = 250), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Dias desde el 煤ltimo consumo") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D铆as", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,6424,by=500))+
  #xlim(0,6424)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = Dias.sin.consumo)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Dias desde el 煤ltimo consumo", x="D铆as", y="Densidad acumulada") +
 # scale_x_continuous(breaks=seq(0,6424,by=10),limits=c(0,100))+
  #guide_legend(label.position="bottom")+
  theme_bw()

################################################################



######################Edad de la TC################################

summary(DicAbr$Edad.Cuenta.Tc)
auxInf<-subset(DicAbr,is.infinite(Edad.Cuenta.Tc))
auxNa<-subset(DicAbr,is.na(Edad.Cuenta.Tc))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$Edad.Cuenta.Tc
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 220, by =10 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Edad de la TC") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Meses", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,220,by=20))+
  #xlim(30,100)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Dias.sin.consumo
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 220, by = 10), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Edad de la TC") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Meses", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,220,by=20))+
  #xlim(0,6424)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = Edad.Cuenta.Tc)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Edad de la TC", x="Meses", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,90,by=5),limits = c(0,90))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  theme_bw()

################################################################


######################Edad Cliente###############################

summary(DicAbr$Edad.Cliente)
auxInf<-subset(DicAbr,is.infinite(Edad.Cliente))
auxNa<-subset(DicAbr,is.na(Edad.Cliente))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$Edad.Cliente
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(20, 90, by =5 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Edad de la TC") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="A帽os", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(20,90,by=5))+
  #xlim(30,100)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Edad.Cliente
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(20, 90, by = 5), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Edad de la TC") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="A帽os", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(20,90,by=5))+
  #xlim(0,6424)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = Edad.Cliente)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Edad del cliente", x="A駉s", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(20,90,by=5))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(20,90)+
  theme_bw()

################################################################
######################Numero de consumos###############################

summary(DicAbr$NumeroConsumos6M)
auxInf<-subset(DicAbr,is.infinite(Edad.Cliente))
auxNa<-subset(DicAbr,is.na(Edad.Cliente))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$NumeroConsumos6M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(20, 90, by =5 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Edad de la TC") +
  #labs(title="NumeroConsumos6M - Desertores") +
  labs(x="A帽os", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(20,90,by=5))+
  #xlim(30,100)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$NumeroConsumos6M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(20, 90, by = 5), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Edad de la TC") +
  #labs(title="NumeroConsumos6M - Desertores") +
  labs(x="A帽os", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(20,90,by=5))+
  #xlim(0,6424)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = PromConsumos3M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Numero Consumos en 6 Meses", x="Consumos", y="Densidad acumulada") +
  scale_x_continuous(breaks=seq(0,65,by=5), limits = c(0,60))+
  theme_bw()

################################################################


######################Instituciones financieras###############################

summary(DicAbr$Numero_tarjetas)
auxInf<-subset(DicAbr,is.infinite(Numero_tarjetas))
auxNa<-subset(DicAbr,is.na(Numero_tarjetas))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$Numero_tarjetas
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 9, by =1 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Instituciones financieras") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Instituciones financieras", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,9,by=1))+
  #xlim(30,100)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Numero_tarjetas
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 9, by = 1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Instituciones financieras") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Instituciones financieras", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,9,by=1))+
  #xlim(0,6424)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = Numero_tarjetas)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Instituciones financieras", x="Instituciones financieras", y="Distribuci贸n acumulada") +
  scale_x_continuous(breaks=seq(0,9,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(0,9)+
  theme_bw()

################################################################

######################Cargas familiares###############################

summary(DicAbr$Max_Dias_sin_consumos_3M)
auxInf<-subset(DicAbr,is.infinite(Max_Dias_sin_consumos_3M))
auxNa<-subset(DicAbr,is.na(Max_Dias_sin_consumos_3M))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$CargasFamiliares
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 23, by =1 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Cargas familiares") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Cargas", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,23,by=1))+
  # xlim(0,10)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Numero_tarjetas
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 23, by = 1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Cargas familiares") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Cargas", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,23,by=1))+
  #xlim(0,6424)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = Numero_tarjetas)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cargas familiares", x="Cargas", y="Distribuci贸n acumulada") +
  scale_x_continuous(breaks=seq(0,15,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(0,15)+
  theme_bw()

################################################################


######################Cargas familiares###############################

summary(DicAbr$STotalDif)
auxInf<-subset(DicAbr,is.infinite(Max_Dias_sin_consumos_3M))
auxNa<-subset(DicAbr,is.na(Max_Dias_sin_consumos_3M))
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$STotalDif
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 1, by =1 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Cargas familiares") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Cargas", y="Frecuencia") + 
  
  # scale_x_continuous(breaks=seq(0,23,by=1))+
  # xlim(0,10)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Numero_tarjetas
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 23, by = 1), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Cargas familiares") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="Cargas", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,23,by=1))+
  #xlim(0,6424)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = Numero_tarjetas)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cargas familiares", x="Cargas", y="Distribuci贸n acumulada") +
  scale_x_continuous(breaks=seq(0,15,by=1))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(0,15)+
  theme_bw()

################################################################





######################Monto consumo solo seguro mes corte###############################
summary(DicAbr$MontoSeguro)

######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$MontoSeguro
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 240, by =10 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Monto solo seguro mes de corte") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,240,by=10))+
  xlim(0,150)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$MontoSeguro
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 240, by = 10), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Monto solo seguro mes de corte") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,240,by=10))+
  xlim(0,150)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = MontoSeguro)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Monto solo seguro mes de corte", x="D贸lares", y="Distribuci贸n acumulada") +
  scale_x_continuous(breaks=seq(0,240,by=10))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(0,240)+
  theme_bw()
############################################################3



######################Monto consumo sin seguro mes corte###############################
summary(DicAbr$MontoSinSeguro)

######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$MontoSinSeguro
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 5200, by =200 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Monto sin seguro mes de corte") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,5190,by=200))+
  xlim(0,5190)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$MontoSinSeguro
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 5190, by = 200), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Monto sin seguro mes de corte") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,5190,by=200))+
  xlim(0,5190)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x = MontoSinSeguro)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Monto sin seguro mes de corte", x="D贸lares", y="Distribuci贸n acumulada") +
  scale_x_continuous(breaks=seq(0,5200,by=200))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  xlim(0,5200)+
  theme_bw()
############################################################3



######################Monto consumo con seguro mes corte###############################
summary(DicAbr$MontoConSeguro)

######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$MontoConSeguro
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 5200, by =200 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Monto con seguro mes de corte") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,5190,by=200))+
  xlim(0,5190)+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$MontoConSeguro
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 5190, by = 200), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Monto con seguro mes de corte") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  scale_x_continuous(breaks=seq(0,5190,by=200))+
  xlim(0,5190)+
  theme_bw()


################Distribuci贸n acumulada##################33


ggplot(DicAbr, aes(x =Edad.Cliente)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Edad Cliente", x="A駉s", y="Distribuci髇 acumulada") +
 # scale_x_continuous(breaks=seq(0,5200,by=200))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  #xlim(0,5200)+
  theme_bw()
############################################################3


##############Salario#####################

################Distribuci贸n acumulada##################33
summary(DicAbr$Salario)

ggplot(DicAbr, aes(x = Salario)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Salario", x="Salrios m韓imos", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()


##############
summary(DicAbr$Max_Dias_sin_consumos_6M)

ggplot(DicAbr, aes(x = Max_Dias_sin_consumos_6M)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Max_Dias_sin_consumos_6M", x="D韆s sin consumo", y="Densidad acumulada") +
  
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,10))+
  theme_bw()



######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==0)
aux<-Aux$Max_Dias_sin_consumos_6M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 240, by =10 ), 
                 col="black", 
                 #fill="blue", 
                 fill="orange",
                 alpha = .2) + 
  labs(title="No desertores - Max_Dias_sin_consumos_6M") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  #scale_x_continuous(breaks=seq(0,240,by=10))+
  theme_bw()

##################33Desertores###########################
######################No desertores#######################################
Aux<-subset(DicAbr,Objetivo==1)
aux<-Aux$Max_Dias_sin_consumos_6M
ggplot(data=Aux, aes(aux)) + 
  geom_histogram(breaks=seq(0, 240, by = 10), 
                 col="black", 
                 fill="blue", 
                 #fill="orange",
                 alpha = .2) + 
  labs(title="Desertores - Max_Dias_sin_consumos_6M") +
  #labs(title="Edad de la TC - Desertores") +
  labs(x="D贸lares", y="Frecuencia") + 
  
  #scale_x_continuous(breaks=seq(0,240,by=10))+
  theme_bw()


################################################################





ggplot(aux, aes(x = fitted.results)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Kolmogorov-Smirnov", x="Score", y="Distribuci髇 acumulada") +
  #scale_x_continuous(breaks=seq(0,5200,by=200))+
  #guide_legend(label.position="bottom")+
  #theme(legend.position="bottom")+
  # xlim(0,5200)+
  theme_bw()
################KS - prueba##################33
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plotROC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")


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

# png(file = "c:/temp/ks.png", width = 1024, height = 768, type="cairo-png")
ggplot(aux, aes(x = fitted.results, group = Objetivo, color = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 14) +
  theme(legend.position ="top") +
  xlab("Score") +
  ylab("Distribuci髇 acumulada") +
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
  fitted.results <- predict(mod1,newdata=testing,type='response')
  
  
  aux<-cbind(testing,fitted.results)
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

basicplot
#######################################

aux$Objetivo<-as.numeric(aux$Objetivo)

basicplot

basicplot + 
  style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("Themes and annotations") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1))




####################

mod1 <- glm(Objetivo~ 
                   Max_Dias_sin_consumos_3M+
                   NormCupoUtilizado+
                   `Cupo.Utilizado/Cupo.Aprobado`+
                   #Dias.sin.consumo+
                   MesesRecencia+
                   #Prom.DiasMora6+
                   Numero_tarjetas+
                   Otra_Tarjeta+
                   #NumeroConsumos6M+
                   NumeroConsumoSinInteres6M+
                   #NumeroConsumos12M+
                   NumeroConsumoCorriente12M+
                   SVidaDesgravamen+
                   STotalFamiliar+
                   MedioPagoVoucher12M+
                   Edad.Cuenta.Tc+
                   #Max_Dias_sin_avances_3M+
                 # Sueldo+
                   SoloSeguro+
                   TipoCliente
                   #MontoSeguro
                 #SoloConsumioSeguro1M
                 , data=Balanceada, 
            family="binomial"(link = "logit"))
           # family=poisson(link=log))
            
#################################Curvas de lorenzt################
library("ineq", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
Distr1 <- Lc(fitted.results, plot =T)

# create data.frame from LC
p <- Distr1[1]
L <- Distr1[2]
Distr1_df <- data.frame(p,L)


# plot
ggplot(data=Distr1_df) +
  geom_point(aes(x=p, y=L)) +
  geom_line(aes(x=p, y=L)) +
  scale_x_continuous(name="Cumulative share of X", limits=c(0,1)) + 
  scale_y_continuous(name="Cumulative share of Y", limits=c(0,1)) +
  geom_abline()+
  theme(legend.title=element_blank())


################################################
aux<-subset(DicAbr,Decil=="P1")
fitted.results <- predict(mod1,newdata=aux,type='response')
pr <- prediction(fitted.results, aux$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

library("MLmetrics", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

Gini(fitted.results, aux$Objetivo)
####################

################Pruebas de independencia###########################
Lambda(table(DicAbr$Numero_tarjetas,DicAbr$Otra_Tarjeta))
chisq.test(table(DicAbr$Numero_tarjetas,DicAbr$Otra_Tarjeta))

Lambda(table(DicAbr$SVidaDesgravamen,DicAbr$STotalFamiliar))
chisq.test(table(DicAbr$SVidaDesgravamen,DicAbr$STotalFamiliar))

Lambda(table(DicAbr$SoloSeguro,DicAbr$SoloPlan))
chisq.test(table(DicAbr$SoloSeguro,DicAbr$SoloPlan))


Lambda(table(DicAbr$SoloPlanSeguro,DicAbr$TipoCliente))
chisq.test(table(DicAbr$SoloPlanSeguro,DicAbr$TipoCliente))


Lambda(table(DicAbr$STotalFamiliar,DicAbr$Otra_Tarjeta))
chisq.test(table(DicAbr$STotalFamiliar,DicAbr$Otra_Tarjeta))

Lambda(table(DicAbr$STotalFamiliar,DicAbr$SoloPlan))
chisq.test(table(DicAbr$STotalFamiliar,DicAbr$SoloPlan))

Lambda(table(DicAbr$STotalFamiliar,DicAbr$TipoCliente))
a<-chisq.test(table(DicAbr$STotalFamiliar,DicAbr$TipoCliente))

Lambda(table(DicAbr$Numero_tarjetas,DicAbr$SVidaDesgravamen))
a<-chisq.test(table(DicAbr$Numero_tarjetas,DicAbr$SVidaDesgravamen))

Lambda(table(DicAbr$Numero_tarjetas,DicAbr$SoloPlan))
a<-chisq.test(table(DicAbr$Numero_tarjetas,DicAbr$SoloPlan))


Lambda(table(DicAbr$Numero_tarjetas,DicAbr$TipoCliente))
a<-chisq.test(table(DicAbr$Numero_tarjetas,DicAbr$TipoCliente))




Lambda(table(DicAbr$SoloSeguro,DicAbr$Otra_Tarjeta))
a<-chisq.test(table(DicAbr$SoloSeguro,DicAbr$Otra_Tarjeta))


Lambda(table(DicAbr$SoloSeguro,DicAbr$SVidaDesgravamen))
a<-chisq.test(table(DicAbr$SoloSeguro,DicAbr$SVidaDesgravamen))

Lambda(table(DicAbr$SoloSeguro,DicAbr$TipoCliente))
a<-chisq.test(table(DicAbr$SoloSeguro,DicAbr$TipoCliente))


Lambda(table(DicAbr$SoloPlanSeguro,DicAbr$Otra_Tarjeta))
a<-chisq.test(table(DicAbr$SoloPlanSeguro,DicAbr$Otra_Tarjeta))


Lambda(table(DicAbr$SoloPlanSeguro,DicAbr$SVidaDesgravamen))
a<-chisq.test(table(DicAbr$SoloPlanSeguro,DicAbr$SVidaDesgravamen))


Lambda(table(DicAbr$SoloPlanSeguro,DicAbr$SoloPlan))
a<-chisq.test(table(DicAbr$SoloPlanSeguro,DicAbr$SoloPlan))


Lambda(table(DicAbr$Otra_Tarjeta,DicAbr$SVidaDesgravamen))
a<-chisq.test(table(DicAbr$Otra_Tarjeta,DicAbr$SVidaDesgravamen))

Lambda(table(DicAbr$Otra_Tarjeta,DicAbr$SoloPlan))
a<-chisq.test(table(DicAbr$Otra_Tarjeta,DicAbr$SoloPlan))


Lambda(table(DicAbr$Otra_Tarjeta,DicAbr$TipoCliente))
a<-chisq.test(table(DicAbr$Otra_Tarjeta,DicAbr$TipoCliente))


Lambda(table(DicAbr$SVidaDesgravamen,DicAbr$SoloPlan))
a<-chisq.test(table(DicAbr$SVidaDesgravamen,DicAbr$SoloPlan))


Lambda(table(DicAbr$SVidaDesgravamen,DicAbr$TipoCliente))
a<-chisq.test(table(DicAbr$SVidaDesgravamen,DicAbr$TipoCliente))

Lambda(table(DicAbr$SoloPlan,DicAbr$TipoCliente))
a<-chisq.test(table(DicAbr$SoloPlan,DicAbr$TipoCliente))


Lambda(table(DicAbr$TieneBlack,DicAbr$TieneGold))
a<-chisq.test(table(DicAbr$TieneBlack,DicAbr$TieneGold))

Lambda(table(DicAbr$TieneBlack,DicAbr$TienePlatinum))
a<-chisq.test(table(DicAbr$TieneBlack,DicAbr$TienePlatinum))

Lambda(table(DicAbr$TieneGold,DicAbr$TienePlatinum))
a<-chisq.test(table(DicAbr$TieneGold,DicAbr$TienePlatinum))

a<-as.numeric(a$statistic)
(a/202699)^0.5  #Phi
(a/(a+202699))^0.5  # C de pearson

###########################Boxplot########################

install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")

set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), rating = c(rnorm(200),rnorm(200, mean=.8)))


fill <- "#4271AE"
line <- "#1F3552"

ggplot(DicAbr, aes(y=CupoUtilizado,x="NormCupoUtilizado")) + 
  geom_boxplot(fill = fill, colour = line) +
  scale_y_continuous(breaks=seq(0,12000,by=1000))+
  stat_boxplot(geom ='errorbar') + 
  theme_bw(base_size = 14)+
  coord_flip()

#################Cupo Utilizado#####################3
ggplot(BalanceadaM, aes(y=CupoUtilizado,x="CupoUtilizado")) + 
geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(fill = fill, colour = line,width=0.18,)+
  scale_y_continuous(breaks=seq(0,12000,by=1000))+
  stat_boxplot(geom ='errorbar') + 
  theme_bw(base_size = 14)+
  xlab("") +
  ylab("D髄ares")+
  coord_flip()

#################Normalido Cupo Utilizado#####################3
summary(DicAbr$NormCupoUtilizado)
ggplot(Balanceada, aes(y=NormCupoUtilizado,x="NormCupoUtilizado")) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(fill = fill, colour = line,width=0.18,)+
  scale_y_continuous(breaks=seq(0,11.5,by=0.5))+
  stat_boxplot(geom ='errorbar') + 
  theme_bw(base_size = 14)+
  xlab("") +
  ylab("D髄ares")+
  coord_flip()

#################Meses Recencia#####################3
summary(DicAbr$MesesRecencia)
ggplot(DicAbr, aes(y=MesesRecencia,x="MesesRecencia")) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(fill = fill, colour = line,width=0.1,)+
  #scale_y_continuous(breaks=seq(0,11.5,by=0.5))+
  stat_boxplot(geom ='errorbar') + 
  theme_bw(base_size = 14)+
  xlab("") +
  ylab("")+
  coord_flip()

#################PromMaxDiasSinconsumo3M#####################3
summary(DicAbr$PromMaxDiasSinconsumo3M)
ggplot(DicAbr, aes(y=PromMaxDiasSinconsumo3M,x="Max D韆s Sin consumo 3 Meses")) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(fill = fill, colour = line,width=0.1)+
  #scale_y_continuous(breaks=seq(0,11.5,by=0.5))+
  stat_boxplot(geom ='errorbar') + 
  theme_bw(base_size = 14)+
  xlab("") +
  ylab("D韆s")+
  coord_flip()

#################Salario#####################3
summary(DicAbr$Salario)
ggplot(BalanceadaM, aes(y=Salario,x="Salario")) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(fill = fill, colour = line,width=0.1)+
  #scale_y_continuous(breaks=seq(0,11.5,by=0.5))+
  stat_boxplot(geom ='errorbar') + 
  theme_bw(base_size = 14)+
  xlab("") +
  #scale_y_continuous(breaks=seq(0,100,by=5))+
  ylab("N鷐ero de salarios b醩icos")+
  coord_flip()



#################Normalido Cupo Utilizado#####################3
summary(BalanceadaC$NormCupoUtilizado)
ggplot(DicAbr2, aes(y=NormCupoUtilizado,x="NormCupoUtilizado")) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(fill = fill, colour = line,width=0.18,)+
  scale_y_continuous(breaks=seq(0,11.5,by=0.5))+
  stat_boxplot(geom ='errorbar') + 
  theme_bw(base_size = 14)+
  xlab("") +
  ylab("D髄ares")+
  coord_flip()


#################Pagos vencidos#####################3
summary(BalanceadaC$PagosVencidos)
ggplot(Balanceada, aes(y=PagosVencidos,x="PagosVencidos")) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(fill = fill, colour = line,width=0.18,)+
  scale_y_continuous(breaks=seq(0,10,by=1))+
  stat_boxplot(geom ='errorbar') + 
  theme_bw(base_size = 14)+
  xlab("") +
  ylab("PagosVencidos")+
  coord_flip()
############################################################3


fit = rpart(Objetivo~ 
              Dias.sin.consumo,
              method="class",
            #control=rpart.control(minsplit=30, cp=0.01),
            data=Balanceada)


rpart.plot(fit)

rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)


