library("foreign", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")

file.choose()

"C:\\PathToFile\\MyDataFile.sav"

setwd("d:/Users_info/ALBANPD/My Documents/Bases")
dataset = read.spss("Solidario_FINAL_Final.sav", to.data.frame=TRUE,use.value.labels = FALSE)


dataset<-spss.get("Solidario_FINAL_Final.sav", lowernames=FALSE, datevars = NULL,
         use.value.labels = TRUE, to.data.frame = TRUE,
         force.single=TRUE,
         allow=NULL, charfactor=FALSE)

dataset = read.table("Solidario_FINAL_Final.sav", header = TRUE)


library("memisc", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.0/library")

data <- as.data.set(spss.system.file('Solidario_FINAL_Final.sav'))

View(data[,c(9973:10000)])


data$holdbrand01
data$q23



data$q23_inv_otro_slice01
d<-data[,c(10334:10639)]

table(data$loopq1_brand1_q2_1001)

q1b<-c("q1b01","q1b02","q1b03","q1b04","q1b05","q1b06","q1b07","q1b08","q1b09","q1b10",
          "q1b11","q1b12","q1b13","q1b14","q1b15","q1b16","q1b17","q1b18","q1b19")
#q1<-c("holdbrand01","holdbrand02","holdbrand03","holdbrand04","holdbrand05","holdbrand06","holdbrand07",
#      "holdbrand08","holdbrand09","holdbrand10","holdbrand11","holdbrand12","holdbrand13","holdbrand14",
#      "holdbrand15","holdbrand16","holdbrand17","holdbrand18","holdbrand19","holdbrand20")
q1<-c("q101","q102","q103","q104","q105","q106","q107","q108","q109","q110","q111",
      "q112","q113","q114","q115","q116","q117","q118","q119","q120")


q23_con<-c("q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           q23_con_brand1_slice01,
           ")


d<-data[q1]
x<-array(0,dim=c(769,20))
for (i in 20) {
  x<-rbind(x,d[,i])
  }

write.csv(d,file="q23_con.csv",row.names = FALSE)


q1<-data[q1]
write.csv(q1,file="q1.csv",row.names = FALSE)

q1b<-data[q1b]
write.csv(q1b,file="q1b.csv",row.names = FALSE)
