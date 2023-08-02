library(wooldridge)
library(pROC)
library(rsq)
library(stargazer)
library(readstata13)
library(mfx)
library(ROCR)
library(readr)
library(caret)
library(aod)
library(car)
library(pscl)

#Establecemos el directorio
setwd("C:/Users/Kary/Desktop/MBA ORT/2022/Análisis de regresión/Obligatorio")
getwd()

#Cargamos la base de datos
base_datos<-read.csv('C:/Users/Kary/Desktop/MBA ORT/2022/Análisis de regresión/Obligatorio/base_Obligatorio_DEAN_2022.csv')
View(base_datos)

#Pregunta 1: Regresar la variable innova en las variables gtoinn, comp_prod, pol_ind e ing_tot
#modelo de probabilidad lineal
Innova_MPL <- lm(base_datos$innova~base_datos$gtoinn+base_datos$comp_prod+base_datos$pol_ind+base_datos$ing_tot)
summary(Innova_MPL)

#modelo logit
Innova_Logit <-glm(base_datos$innova~base_datos$gtoinn+base_datos$comp_prod+base_datos$pol_ind+base_datos$ing_tot,family=binomial(link="logit"),data=base_datos)
summary(Innova_Logit)

#modelo probit
Innova_Probit <-glm(base_datos$innova~base_datos$gtoinn+base_datos$comp_prod+base_datos$pol_ind+base_datos$ing_tot,family=binomial(link="probit"),data=base_datos)
summary(Innova_Probit)


#Pregunta 2: medidas de bondad de ajuste para cada uno de los modelos
#modelo de probabilidad lineal (R2 ajustado)
rsq(Innova_MPL, adj = TRUE) #R2 ajustado
summary(Innova_MPL)$adj.r.squared #R2 ajustado otra forma
rsq(Innova_MPL) #R2 

#modelo logit (AIC y Pseudo R2)
rsq.lr(Innova_Logit) #R2 basado en el Log likelihood ratio
DpR2(Innova_Logit) #PseudoR2
logLik(Innova_Logit) #El logaritmo de la Función de Verosimilitud
AIClogit<- 2*5-2*logLik(Innova_Logit) #Confirmamos el AIC para el modelo Logit
AIClogit
Resdev.logit<--2*logLik(Innova_Logit) #Residual deviance,medida de que tan lejos 
Resdev.logit


#modelo probit (AIC y Pseudo R2)
rsq.lr(Innova_Probit) #R2 basado en el Log likelihood ratio
pR2(Innova_Probit)#PseudoR2
logLik(Innova_Probit) #El logaritmo de la Función de Verosimilitud
AICprobit<- 2*5-2*logLik(Innova_Probit) #Confirmamos el AIC para el modelo Probit
AICprobit
Resdev.probit<--2*logLik(Innova_Probit) #Residual deviance,medida de que tan lejos 
Resdev.probit

#Bondad de ajuste todos los modelos juntos
stargazer(Innova_MPL, Innova_Logit, Innova_Probit, type="text", out="tabla_bondades.txt")


#Pregunta 4:  intervalos de confianza de los parámetros estimados al 95%. 
#intervalo de confianza al 95% para los modelos MPL
confint(Innova_MPL, level = 0.95)
confint(Innova_Logit, level=0.95)
confint(Innova_Probit, level=0.95)


#Pregunta 5: coeficientes estimados como cambios en la razón de posibilidades de éxito (cambios en los Odds Ratios) para el modelo logit. 
##Calculamos la probabilidad de éxito p de acuerdo al odds ratio en el modelo sin regresores:
modLoginnova = glm(innova~1, data=base_datos, family=binomial(link="logit"))
summary(modLoginnova)
probinnovar<-exp(modLoginnova$coefficient[1])/(1+exp(modLoginnova$coefficient[1]))
probinnovar 

#el resultado indica que, sin regresores, el intercepto es el log del odds ratio de que la empresa realice alguna actividad de innovación en la muestra
table(base_datos$innova)
ORenMuestra<-(1707/4268)/(1-(1707/4268))
ORenMuestra
LogORenMuestra<-log(ORenMuestra)
LogORenMuestra

#Obtenemos directamente los OddsRatios del modelo logit
cambioenORgtoinn <- exp(Innova_Logit$coefficients[2])
cambioenORgtoinn
cambioenORcomp_prod <- exp(Innova_Logit$coefficients[3])
cambioenORcomp_prod
cambioenORpol_ind <- exp(Innova_Logit$coefficients[4])
cambioenORpol_ind 
cambioenORing_tot <- exp(Innova_Logit$coefficients[5])
cambioenORing_tot 

##Con el comando logitor obtenemos directamente los OddsRatios del modelo logit
logitor(base_datos$innova~base_datos$gtoinn+base_datos$comp_prod+base_datos$pol_ind+base_datos$ing_tot, data=base_datos)


#Pregunta 6: Efectos marginales en el promedio (EPenP) y el efecto marginal promedio (EPP) para los modelos probit y logit. 
#Efectos marginales de todas las variables independientes en su promedio: EPenP
efmgEPenPlog<-logitmfx(innova ~ gtoinn+comp_prod+pol_ind+ing_tot, data=base_datos)
efmgEPenPlog$mfxest

efmgEPenPpro<-probitmfx(innova ~ gtoinn+comp_prod+pol_ind+ing_tot, data=base_datos)
efmgEPenPpro$mfxest

#Comparamos EpenP de los modelos Logit y Probit con las estimaciones de los parámetros del modelo de probabilidad lineal. 
coefMPL <- c(0.017902, -0.085533, 0.199630, 0.057772)
estmMPL <- matrix(coefMPL, nrow = 4, ncol = 1)
estmMPL
tablaefmgsEPenP<-cbind(efmgEPenPlog$mfxest[1:4],efmgEPenPpro$mfxest[1:4],estmMPL[1:4])
colnames(tablaefmgsEPenP) <- c("Modelo Logit","Modelo Probit","Modelo MPL")
rownames(tablaefmgsEPenP) <- c("gtoinn", "comp_prod", "pol_ind", "ing_tot")
tablaefmgsEPenP

#Efectos marginales en el promedio de todos los efectos marginales: EPP
efmgEPPlog<-logitmfx(innova ~ gtoinn+comp_prod+pol_ind+ing_tot, data=base_datos, atmean = FALSE)
efmgEPPlog$mfxest

efmgEPPpro<-probitmfx(innova ~ gtoinn+comp_prod+pol_ind+ing_tot, data=base_datos, atmean = FALSE)
efmgEPPpro$mfxest

#Para comparar con el modelo MPL es mejor comparar las interacciones entre los coeficientes estimados y los efectos parciales promedios EPP
coefMPL <- c(0.017902, -0.085533, 0.199630, 0.057772)
estmMPL <- matrix(coefMPL, nrow = 4, ncol = 1)
estmMPL
tablaefmgsEPP<-cbind(efmgEPPlog$mfxest[1:4],efmgEPPpro$mfxest[1:4],estmMPL[1:4])
colnames(tablaefmgsEPP) <- c("Modelo Logit","Modelo Probit","Modelo MPL")
rownames(tablaefmgsEPP) <- c("gtoinn", "comp_prod", "pol_ind", "ing_tot")
tablaefmgsEPP


#Pregunta 7: Matrices de contingencia para cada uno de los modelos con un umbral de 0.5
#utilizamos los coeficientes de los modelos para predecir los casos exitosos.
base_datos$predInnova_MPL = predict(Innova_MPL, newdata=base_datos, type="response")
base_datos$predInnova_Logit = predict(Innova_Logit, newdata=base_datos, type="response")
base_datos$predInnova_Probit = predict(Innova_Probit, newdata=base_datos, type="response")
innovaobserv<-factor(base_datos$innova, labels = c("No Innova", "Innova"))

#predicciones con umbral 0.5
InnovaclasifMPL<-factor(as.numeric(base_datos$predInnova_MPL>=0.5), labels = c("NO","SI"))
InnovaclasifLogit<-factor(as.numeric(base_datos$predInnova_Logit>=0.5), labels = c("NO","SI"))
InnovaclasifProbit<-factor(as.numeric(base_datos$predInnova_Probit>=0.5), labels = c("NO","SI"))

#construimos las matrices de contingencia y precisión para cada modelo
MPL<-table(InnovaclasifMPL, innovaobserv)
precisiónMPL<-(MPL[1,1]+MPL[2,2])/nrow(base_datos)
MPL
precisiónMPL

LOG<-table(InnovaclasifLogit, innovaobserv)
precisiónLOG<-(LOG[1,1]+LOG[2,2])/nrow(base_datos)
LOG
precisiónLOG

PRO<-table(InnovaclasifProbit, innovaobserv)
precisiónPRO<-(PRO[1,1]+PRO[2,2])/nrow(base_datos)
PRO
precisiónPRO


#Pregunta 8: Precisión general de los modelos, la tpr y la fpr para cada uno de los modelos. 
#sensibilidad:tpr = exitos correctamente clasificados/total exitos.
#1-especificidad:fpr = fracasos incorrectamente clasificados/total fracasos.

MPL
sensMPL<-(MPL[2,2]/(MPL[2,2]+MPL[1,2]))
fprMPL<-(MPL[2,1]/(MPL[2,1]+MPL[1,1]))
sensMPL
fprMPL
EspecMPL<-1-fprMPL
EspecMPL
Espec2MPL<-2049/2561
Espec2MPL

LOG
sensLOG<-(LOG[2,2]/(LOG[2,2]+LOG[1,2]))
fprLOG<-(LOG[2,1]/(LOG[2,1]+LOG[1,1]))
sensLOG
fprLOG
EspecLOG<-1-fprLOG
EspecLOG

PRO
sensPRO<-(PRO[2,2]/(PRO[2,2]+PRO[1,2]))
fprPRO<-(PRO[2,1]/(PRO[2,1]+PRO[1,1]))
sensPRO
fprPRO
EspecPRO<-1-fprPRO
EspecPRO


#Pregunta 9: Umbral que mejore la precisión del modelo. Con este umbral determinado, calcular la precisión, la tpr y la fpr para cada uno de los modelos.

rocMPL<-roc(base_datos$innova, base_datos$predInnova_MPL, plot = TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,print.auc=TRUE, show.thres=TRUE)
rocLogit<-roc(base_datos$innova, base_datos$predInnova_Logit, plot = TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,print.auc=TRUE, show.thres=TRUE)
rocProbit<-roc(base_datos$innova, base_datos$predInnova_Probit, plot = TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,print.auc=TRUE, show.thres=TRUE)

#comparamos con los resultados de la matriz de contigencia
coords(rocMPL, 0.5, ret = c("accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE)
coords(rocLogit, 0.5, ret = c("accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE)
coords(rocProbit, 0.5, ret = c("accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE)
#obtenemos el mejor umbral con el método youden y closest.topleft . calculamos la precisión, la tpr y la fpr para cada uno de los modelos.
coords(rocMPL, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity", "tpr", "fpr"), transpose = FALSE, best.method="youden")
coords(rocMPL, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="closest.topleft")

coords(rocLogit, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity", "tpr", "fpr"), transpose = FALSE, best.method="youden")
coords(rocLogit, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="closest.topleft")

coords(rocProbit, "best", ret=c("threshold", "accuracy", "sensitivity", "specificity", "tpr", "fpr"), transpose = FALSE, best.method="youden")
coords(rocProbit, "best", ret=c("threshold", "accuracy", "sensitivity", "1-specificity", "tpr", "fpr"), transpose = FALSE, best.method="closest.topleft")


#Pregunta 10: Graficamos la curva ROC y obtenemos el área bajo la curva para cada uno de los modelos. 

plot(rocMPL, col="blue", print.thres="best", print.thres.best.method="youden",print.auc=TRUE, print.thres.col="red", print.thres.pch=19)
plot(rocMPL, col="blue", print.thres=0.5, print.thres.col="red", print.thres.pch=19)
plot(rocMPL2, col="blue", print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19)

plot(rocLogit, col="blue", print.thres="best", print.thres.best.method="youden",print.auc=TRUE, print.thres.col="red", print.thres.pch=19)
plot(rocLogit, col="blue", print.thres=0.5, print.thres.col="red", print.thres.pch=19)
plot(rocLogit2, col="blue", print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19)

plot(rocProbit, col="blue", print.thres="best", print.thres.best.method="youden", print.auc=TRUE, print.thres.col="red", print.thres.pch=19)
plot(rocProbit, col="blue", print.thres=0.5, print.thres.col="red", print.thres.pch=19)
plot(rocProbit2, col="blue", print.thres="best", print.thres.best.method="closest.topleft", print.thres.col="black", print.thres.pch=19)

