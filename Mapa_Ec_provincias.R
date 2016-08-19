library(rgeos)
library(maptools)
#library(gpclib)  # may be needed, may not be
library("plyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
#Mapa modificado islas
library("ggmap", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
setwd("~/R")
pibxprov <- read.csv2("por.provincias.csv",sep = ",",header = TRUE,dec = ".")
setwd("~/Cobertura/ECU_adm")
mapa_shp_swe <- readShapePoly("ECU_adm1")
mapa_map_swe <- fortify(mapa_shp_swe)

Encoding(levels(mapa_shp_swe@data$NAME_0)) <- "latin1"
gpclibPermit()

espPols <- unionSpatialPolygons(mapa_shp_swe, mapa_shp_swe@data$ID_0)

pais <- mapa_shp_swe[c(1:8,10:24),1]
gal<- mapa_shp_swe[9,1]

#dy <- bbox(pais)[2,1] - 1.11*bbox(gal)[2,1]
#dx <- 0.99*bbox(pais)[1,2] - bbox(gal)[1,2]

dy <- bbox(pais)[2,1] - 4.1*bbox(gal)[2,1]
dx <- 1.07*bbox(pais)[1,2] - bbox(gal)[1,2]


New.Gal <- elide(gal, shift=c(dx, dy))
bbgal <- bbox(gal)

ecu <- rbind(pais,New.Gal)
IDs2 <- sapply(ecu@polygons, function(x)x@ID)
IDs <- (c(1:8,10:24,9))
## cambiar dependiendo de bases 
idx <- match(IDs, mapa_shp_swe@data$ID_1)

dat2add <- data.frame(Provincias = pibxprov$Provincia,
                      HASC_1 = pibxprov$HASC_1,
                      PIB=pibxprov$POBRES)[idx, ]

row.names(dat2add) <- IDs2
ecu_pib <- SpatialPolygonsDataFrame(ecu, dat2add)

ecu_p <- fortify(ecu_pib, region = "HASC_1")


###Mapa con texto
library("scales", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")

library("Cairo", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("dplyr", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
library("rgdal", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")
colnames(ecu_p)[6]<-"id"
pibxprov$id<-pibxprov$HASC_1
plotData <- left_join(ecu_p, pibxprov)
setwd("~/R")
pibxprov <- read.csv2("por.provincias.csv",sep = ",",header = TRUE,dec = ".")
p<-ggplot() +
  geom_map(data = pibxprov, aes(map_id = HASC_1, fill = POBRES.Porcentaje ,label=pibxprov$Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow3", size = 0.5)+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  #scale_fill_distiller(palette = "Greens", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
  scale_fill_gradientn(colours = c("snow", "cyan2", "cyan4"))+
  guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  #geom_text(data = pibxprov, aes(x = clong, y = clat, label = as.character(paste(pibxprov$Nom,": ",round(pibxprov$POBRES.Porcentaje*100, digits = 2),"%")),fontface=2),size = 2.9)+
  labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
#labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
p  
#ggsave(p, file = "map1.png", width = 10, height = 9.2, type = "cairo-png")

#####################Carga de datos#############################
setwd("~/R")
Share <- read.csv2("Share.csv",sep = ",",header = TRUE,dec = ".")

#############################geom_label_repel###########################################
library("ggrepel", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")

p<-ggplot() +
  geom_map(data = Share, aes(map_id = HASC_1, fill = Solidario ,label=Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow3", size = 0.5)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  #scale_fill_distiller(palette = "Greens",labels = percent,breaks = pretty_breaks(n = 5), values = c(1,0))+
  #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
 scale_fill_gradientn(colours = c("snow", "cyan", "cyan4"),labels = percent,values = rescale(c(0,0.05,0.11)),breaks = pretty_breaks(n = 5))+
  #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  geom_label_repel(data = Share,segment.size = 0.55,
                   aes(clong, clat, fill = Solidario,label = as.character(paste(Nombre,":",Solidario*100,"%"))),
                   fontface = 'bold', color = 'darkblue',size = 3.3,
                   #gold4,tan1,slategray4,turquoise4,blueviolet
                   box.padding = unit(0.25, "lines"),
                   force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
   ggtitle("Participación del Solidario en \n Tarjetas Principales")+
  theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold.italic"))+
  labs(fill = "Participación")
#labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
p 
ggsave(p, file = "TCx1000.png", width = 6.8, height = 5.2, type = "cairo-png")
######################Solidario#############################

p<-ggplot() +
  geom_map(data = Share, aes(map_id = HASC_1, fill = Solidario ,label=Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow3", size = 0.5)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  #scale_fill_distiller(palette = "Greens",labels = percent,breaks = pretty_breaks(n = 5), values = c(1,0))+
  #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
  scale_fill_gradientn(colours = c("snow", "cyan", "cyan4"),labels = percent,values = rescale(c(0,0.05,0.11)),breaks = pretty_breaks(n = 5))+
  #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  geom_label_repel(data = Share,segment.size = 0.55,
                   aes(clong, clat, fill = Solidario,label = as.character(paste(Nombre,":",Solidario*100,"%"))),
                   fontface = 'bold', color = 'darkblue',size = 3.3,
                   #gold4,tan1,slategray4,turquoise4,blueviolet
                   box.padding = unit(0.25, "lines"),
                   force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
  ggtitle("Participación del Solidario en \n Tarjetas Principales")+
  theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold"))+
  labs(fill = "Participación")
#labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
p 
###########################Tarjetas Percápita################################
  
  p<-ggplot() +
    geom_map(data = Share, aes(map_id = HASC_1, fill = Tarjetaspercapita1000 ,label=Provincia),map = ecu_p) +
    #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
    geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow3", size = 0.5)+
    guides(fill = guide_legend(reverse = TRUE))+
    coord_map() +
    expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
    #"YlOrBr"
    scale_fill_distiller(palette = "Blues",breaks = pretty_breaks(n = 5), values = c(1,0))+
    #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
    #scale_fill_gradientn(colours = c("snow", "cyan", "cyan4"),labels = percent,values = rescale(c(0,0.05,0.11)),breaks = pretty_breaks(n = 5))+
    #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
    theme_nothing(legend = TRUE) +
    ylim(-5.2, 1.5)+
    geom_label_repel(data = Share,segment.size = 0.55,
                     aes(clong, clat, fill = Tarjetaspercapita1000,label = as.character(paste(Nombre,":",Tarjetaspercapita1000))),
                     fontface = 'bold', color = 'chocolate',size = 3.3,
                     #gold4,tan1,slategray4,turquoise4,blueviolet
                     box.padding = unit(0.25, "lines"),
                     force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
    ggtitle("Tarjetas de crédito principales \n por cada 1000 personas")+
    theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold"))+
    labs(fill = "TC por 1000 personas")
  #labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
  p 


##########################Saldo Promedio################################

p<-ggplot() +
  geom_map(data = Share, aes(map_id = HASC_1, fill = SaldoPromedio ,label=Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow1", size = 0.5)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  scale_fill_distiller(palette = "Greens",breaks = pretty_breaks(n = 5), values = c(1,0))+
  #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
  #scale_fill_gradientn(colours = c("snow", "cyan", "cyan4"),labels = percent,values = rescale(c(0,0.05,0.11)),breaks = pretty_breaks(n = 5))+
  #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  geom_label_repel(data = Share,segment.size = 0.55,
                   aes(clong, clat, fill = SaldoPromedio,label = as.character(paste(Nombre,":$",SaldoPromedio))),
                   fontface = 'bold', color = 'blue4',size = 3.3,
                   #gold4,tan1,slategray4,turquoise4,blueviolet
                   box.padding = unit(0.25, "lines"),
                   force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
  ggtitle("Saldo Promedio en Tarjetas Crédito \n (Enero - Mayo del 2016)")+
  theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold"))+
  labs(fill = "Dólares")
#labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
p 

##########################Morosidad/Saldo total################################

p<-ggplot() +
  geom_map(data = Share, aes(map_id = HASC_1, fill = Mora.SaldoTotal ,label=Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow1", size = 0.5)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  scale_fill_distiller(palette = "Reds", labels = percent,breaks = pretty_breaks(n = 5), values = c(1,0))+
  #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
  #scale_fill_gradientn(colours = c("snow", "cyan", "cyan4"),labels = percent,values = rescale(c(0,0.05,0.11)),breaks = pretty_breaks(n = 5))+
  #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  geom_label_repel(data = Share,segment.size = 0.55,
                   aes(clong, clat, fill = Mora.SaldoTotal,label = as.character(paste(Nombre,":",round(Mora.SaldoTotal, digits = 3)*100,"%"))),
                   fontface = 'bold', color = 'darkblue',size = 3.3,
                   #gold4,tan1,slategray4,turquoise4,blueviolet
                   box.padding = unit(0.25, "lines"),
                   force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
  ggtitle("Saldo en Mora sobre Saldo Total en Tarjetas Crédito \n (Enero - Mayo del 2016)")+
  theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold"))+
  labs(fill = "Mora/Saldo Total")
#labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
p 

##########################Diferido/Saldo total################################

p<-ggplot() +
  geom_map(data = Share, aes(map_id = HASC_1, fill = Diferido.SaldoTotal ,label=Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow1", size = 0.5)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  scale_fill_distiller(palette = "Greens", labels = percent,breaks = pretty_breaks(n = 5), values = c(1,0))+
  #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
  #scale_fill_gradientn(colours = c("snow", "cyan", "cyan4"),labels = percent,values = rescale(c(0,0.05,0.11)),breaks = pretty_breaks(n = 5))+
  #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  geom_label_repel(data = Share,segment.size = 0.55,
                   aes(clong, clat, fill = Diferido.SaldoTotal,label = as.character(paste(Nombre,":",round(Diferido.SaldoTotal, digits = 3)*100,"%"))),
                   fontface = 'bold', color = 'darkmagenta',size = 3.3,
                   #gold4,tan1,slategray4,turquoise4,blueviolet
                   box.padding = unit(0.25, "lines"),
                   force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
  ggtitle("Saldo Diferido sobre Saldo Total en Tarjetas Crédito \n (Enero - Mayo del 2016)")+
  theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold"))+
  labs(fill = "Saldo Diferido/Saldo Total")
#labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
p 


##########################Pos percapita################################
setwd("~/R")
Pos <- read.csv2("PosMayo16.csv",sep = ",",header = TRUE,dec = ".")
p<-ggplot() +
  geom_map(data = Pos, aes(map_id = HASC_1, fill = TOTAL*1000/POBLACION ,label=Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow1", size = 0.5)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  scale_fill_distiller(palette = "Greys",breaks = pretty_breaks(n = 5), values = c(1,0))+
  #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
  #scale_fill_gradientn(colours = c("snow", "cyan", "cyan4"),labels = percent,values = rescale(c(0,0.05,0.11)),breaks = pretty_breaks(n = 5))+
  #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  geom_label_repel(data = Pos,segment.size = 0.55,
                   aes(clong, clat, fill = TOTAL*1000/POBLACION,label = as.character(paste(Nombre,":",round(TOTAL*1000/POBLACION, digits = 1)))),
                   fontface = 'bold', color = 'brown2',size = 3.3,
                   #gold4,tan1,slategray4,turquoise4,blueviolet
                   box.padding = unit(0.25, "lines"),
                   force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
  ggtitle("Puntos de Venta para TC \n por cada 1000 personas")+
  theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold"))+
  labs(fill = "Puntos de venta \n por 1000 personas")
#labs(title = "Porcentaje de Pobres por \n Provincia",fill = "")
p 

##########################Datafast################################
setwd("~/R")
Pos <- read.csv2("PosMayo16.csv",sep = ",",header = TRUE,dec = ".")
p<-ggplot() +
  geom_map(data = Pos, aes(map_id = HASC_1, fill = DATAFAST*1000/POBLACION ,label=Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow1", size = 0.5)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  #scale_fill_distiller(palette = "Greys",breaks = pretty_breaks(n = 5), values = c(1,0))+
  #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
  scale_fill_gradientn(colours = c("azure2", "aquamarine2", "aquamarine4"),breaks = pretty_breaks(n = 5), values = rescale(c(0,4,10)))+
  #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  geom_label_repel(data = Pos,segment.size = 0.55,
                   aes(clong, clat, fill = DATAFAST*1000/POBLACION,label = as.character(paste(Nombre,":",round(DATAFAST*1000/POBLACION, digits = 1)))),
                   fontface = 'bold', color = 'brown2',size = 3.3,
                   #gold4,tan1,slategray4,turquoise4,blueviolet
                   box.padding = unit(0.25, "lines"),
                   force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
  ggtitle("Puntos de Venta Datafast \n por cada 1000 personas")+
  theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold"))+
  labs(fill = "Pos Datafast \n por 1000 personas")
p 
##########################MEDIANET################################
setwd("~/R")
Pos <- read.csv2("PosMayo16.csv",sep = ",",header = TRUE,dec = ".")
p<-ggplot() +
  geom_map(data = Pos, aes(map_id = HASC_1, fill = MEDIANET*1000/POBLACION ,label=Provincia),map = ecu_p) +
  #geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group),fill = "NA", color = "snow1", size = 0.5)+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_map() +
  expand_limits(x = ecu_p$long, y = ecu_p$lat) + 
  #"YlOrBr"
  #scale_fill_distiller(palette = "Greys",breaks = pretty_breaks(n = 5), values = c(1,0))+
  #scale_fill_distiller(palette = "YlGn", labels = percent,breaks = pretty_breaks(n = 10), values = c(1,0))+
  scale_fill_gradientn(colours = c("bisque1", "gold", "gold3"),breaks = pretty_breaks(n = 5), values = rescale(c(0,4,10)))+
  #guides(fill = guide_legend(title = "Porcentajes de Pobres",reverse = TRUE,title.position = "top"))+
  theme_nothing(legend = TRUE) +
  ylim(-5.2, 1.5)+
  geom_label_repel(data = Pos,segment.size = 0.55,
                   aes(clong, clat, fill = MEDIANET*1000/POBLACION,label = as.character(paste(Nombre,":",round(MEDIANET*1000/POBLACION, digits = 1)))),
                   fontface = 'bold', color = 'brown2',size = 3.3,
                   #gold4,tan1,slategray4,turquoise4,blueviolet
                   box.padding = unit(0.25, "lines"),
                   force =0,point.padding = unit(0.25, "lines"),show_guide  = F) +
  ggtitle("Puntos de Venta Medianet \n por cada 1000 personas")+
  theme(legend.position=  c(0.22,0.2),legend.justification=c(1,0), plot.title=element_text(size = rel(1.5), family = "Times",face="bold"))+
  labs(fill = "Pos Medianet \n por 1000 personas")
p 
