#Lectura del dataset
#http://sitiobigdata.com/index.php/2019/01/19/machine-learning-metrica-clasificacion-parte-3/
SGBPS<-read.csv("../dataset/SGBVM_Temporada.csv")

names(SGBPS)

#table(SGBVM$v207)#Estado actual del paciente
table(SGBPS$v214)#Tuvo secuelas o no

SGBPS[SGBPS$v214==2,]$v214<-1
SGBPS[SGBPS$v214==3,]$v214<-1
SGBPS[SGBPS$v214==4,]$v214<-1
SGBPS<-SGBPS[(SGBPS$v214==0) | (SGBPS$v214==1),]
SGBPS<-SGBPS[,(-184)]#v207 Estado actual del paciente


#Convertir la variable de salida en factor

SGBPS$subtipo<-as.factor(SGBPS$subtipo)
SGBPS$v5<-as.factor(SGBPS$v5)
SGBPS$v6<-as.factor(SGBPS$v6)
SGBPS$v21<-as.factor(SGBPS$v21)
SGBPS$v22<-as.factor(SGBPS$v22)
SGBPS$v23<-as.factor(SGBPS$v23)
SGBPS$v24<-as.factor(SGBPS$v24)
SGBPS$v25<-as.factor(SGBPS$v25)
SGBPS$v26<-as.factor(SGBPS$v26)
SGBPS$v27<-as.factor(SGBPS$v27)
SGBPS$v29<-as.factor(SGBPS$v29)
SGBPS$v30<-as.factor(SGBPS$v30)
SGBPS$v31<-as.factor(SGBPS$v31)
SGBPS$v32<-as.factor(SGBPS$v32)
SGBPS$v33<-as.factor(SGBPS$v33)
SGBPS$v37<-as.factor(SGBPS$v37)
SGBPS$v38<-as.factor(SGBPS$v38)
SGBPS$v214<-as.factor(SGBPS$v214)
SGBPS$Temporada<-as.factor(SGBPS$Temporada)
sapply(SGBPS[1,],class)
table(SGBPS$v214)



#Train-Test

#J48 v37
#library(rJava)
require(psych)
library(RWeka)
library(FSelector)
library(caret)
library(pROC)
library(randomForest)

semillas<-c(33833,90,2940,86203,123,789,67554,6094,47053,2098,819,8,70090,3293,110,70988,44550,110923,99099,16,19023,7007,504,222,32,3000,4593,10203,309485,909653)
#mode(semillas); length(semillas)

promBalancedAccuracy<-0
#mode(promBalancedAccuracy); length(promBalancedAccuracy)
#construimos una matris que contiene 30 filas y 8 columnas
resultados<-matrix(nrow=30,ncol=8)
colnames(resultados)<-(c("N° ejcuciones","Semilla","Accuracy","Sensitivity","Specificity","Balanced Accuracy","Kappa"," Under Curve" ))
#mode(resultados); length(resultados)

for(i in 1:30){
  cat(semillas[i],"\n")
  set.seed(semillas[i])
  inTrain = createDataPartition(SGBPS$v214,p = 2/3,list=FALSE)
  trainingSGBPS = SGBPS[ inTrain,]
  df <- data.frame(trainingSGBPS$v210,trainingSGBPS$v144,trainingSGBPS$v165,trainingSGBPS$v7,trainingSGBPS$v214)
  write.csv(df,"trainingC45.csv")
  testingSGBPS = SGBPS[-inTrain,]
  (treev28LCR = J48(as.simple.formula(names(trainingSGBPS), "v214"), data = trainingSGBPS))
  
#  reglas<-as.matrix(scan(text=.jcall(treev28LCR$classifier, "S", "toString") #,sep="\n", what="") )[-c(1:2), ,drop=FALSE]
 # print(reglas)
 # cat(reglas,"\n", file="C45CV_2.txt",append = T)

  resultado<-predict(treev28LCR,testingSGBPS, type="class")
  cm<-confusionMatrix(table(resultado, testingSGBPS$v214))
  BalancedAccuracy<-cm$byClass[11]
  cat(BalancedAccuracy,"\n")
  resultados[i,1]<-i
  resultados[i,2]<-semillas[i]
  resultados[i,3]<-cm$overall[1]
  resultados[i,4]<-cm$byClass[1]
  resultados[i,5]<-cm$byClass[2]
  resultados[i,6]<-cm$byClass[11]
  resultados[i,7]<-cm$overall[2]
  miROC<-roc(as.numeric(testingSGBPS$v214) ~ as.numeric(resultado))
  resultados[i,8]<-miROC$auc
  promBalancedAccuracy<-promBalancedAccuracy + BalancedAccuracy
}
cat(promBalancedAccuracy/30,"\n")
resultadosDF<-data.frame(resultados)
write.csv(resultadosDF,"J48_SGBPronostico_secuela_2.csv")

