#EJERCICIO 3
#1. DATOS
#datos descargados
analisisCOVID=read_excel("Analisis_COVID.xlsx")
view(analisisCOVID)

#conversión a factor
analisisCOVID$FIEBRE=as.factor(analisisCOVID$FIEBRE)
analisisCOVID$COVID=as.factor(analisisCOVID$COVID)
analisisCOVID$IGM=as.factor(analisisCOVID$IGM)
analisisCOVID$IGG=as.factor(analisisCOVID$IGG)
analisisCOVID$TEST=as.factor(analisisCOVID$TEST)
analisisCOVID$PCR=as.factor(analisisCOVID$PCR)
analisisCOVID$NEUMONIA=as.factor(analisisCOVID$NEUMONIA)

#3.ESTADISTICOS DESCRIPTIVOS
#medias de DIMERO D, PLAQUETAS, LEUCOCITOS
mean(analisisCOVID$DIMERO_D)
mean(analisisCOVID$PLAQUETAS)
mean(analisisCOVID$LEUCOCITOS)

#medianas de DIMERO D, PLAQUETAS, LEUCOCITOS
median(analisisCOVID$DIMERO_D)
median(analisisCOVID$PLAQUETAS)
median(analisisCOVID$LEUCOCITOS)

#cuasi-varianza de DIMERO D, PLAQUETAS, LEUCOCITOS
var(analisisCOVID$DIMERO_D)
var(analisisCOVID$PLAQUETAS)
var(analisisCOVID$LEUCOCITOS)

#cuasi-desviación típica de DIMERO D, PLAQUETAS, LEUCOCITOS
sd(analisisCOVID$DIMERO_D)
sd(analisisCOVID$PLAQUETAS)
sd(analisisCOVID$LEUCOCITOS)

#5.REPRESENTACION GRAFICA
par(mfrow=c(1, 3))

#Plaquetas: histograma
a=sort(analisisCOVID$PLAQUETAS)
hist(a, main="PLAQUETAS", 
        col=c("red", "orange", "green", "skyblue", "blue","grey" ))

#Linfocitos: histograma
b=sort(analisisCOVID$LINFOCITOS)
hist(b, main="LINFOCITOS", 
     col=c("yellow", "yellow", "pink", "purple", "blue", "skyblue", "green"))

#Neumonía: gráfico circular
analisisCOVID$NEUMONIA
table(analisisCOVID$NEUMONIA)
pie(table(analisisCOVID$NEUMONIA), 
    main="NEUMONÍA", col=c("orange", "skyblue"))

#media y mediana de linfocitos
mean(analisisCOVID$LINFOCITOS)
median(analisisCOVID$LINFOCITOS)

#EJERCICIO 4
#1.DIAGRAMA DE PARES
variable_numerica=analisisCOVID[, c(5, 6, 7, 8, 9, 11)]
view(variable_numerica)
pairs(variable_numerica)
pairs(variable_numerica, col= analisisCOVID$COVID)

#2.MATRIZ DE CORRELACIONES
par(mfrow=c(1, 2))
correlaciones<-cor(variable_numerica)
corrplot(correlaciones, method="number")
corrplot(correlaciones, method="color", type="upper")

#3.b.CORRELACIONES COVIDpositivo y COVIDnegativo
#correlaciones COVIDpositivo
v.numerica_COVID=analisisCOVID[, c(5, 6, 7, 8, 9, 11, 13)]
positivo=v.numerica_COVID[v.numerica_COVID$COVID=="SI",]
positivo6=positivo[, c(1, 2, 3, 4, 5, 6)]
Covid_positivo<-cor(positivo6)
corrplot(Covid_positivo, method="number")
corrplot(Covid_positivo, method="color", type="upper")

#correlaciones COVIDnegativo
negativo=v.numerica_COVID[v.numerica_COVID$COVID=="NO",]
negativo6=negativo[, c(1, 2, 3, 4, 5, 6)]
Covid_negativo<-cor(negativo6)
corrplot(Covid_negativo, method="number")
corrplot(Covid_negativo, method="color", type="upper")

#4.a.DIAGRAMA DISPERSION LINFOCITOS-DIMERO_D
par(mfrow=c(1, 1))
plot(analisisCOVID$LINFOCITOS, analisisCOVID$DIMERO_D, 
     col=analisisCOVID$COVID)

#4.c.DIAGRAMA CAJAS 
par(mfrow=c(1, 3))
#diagrama LINFOCITOS
boxplot(analisisCOVID$LINFOCITOS ~ analisisCOVID$COVID, analisisCOVID,
        col = c("red", "darkorange"))

#diagrama DIMERO_D
boxplot(analisisCOVID$DIMERO_D ~ analisisCOVID$COVID, analisisCOVID,
        col = c("yellow", "green"))

#diagrama HEMOGLOBINA
boxplot(analisisCOVID$HEMOGLOBINA ~ analisisCOVID$COVID, analisisCOVID,
        col = c("blue", "darkblue"))

#4.d.CORRELACIONES variables numéricas y COVID
v.numerica_COVID$COVID=as.numeric(v.numerica_COVID$COVID)
cor(v.numerica_COVID$LEUCOCITOS, v.numerica_COVID$COVID)
cor(v.numerica_COVID$LINFOCITOS, v.numerica_COVID$COVID)
cor(v.numerica_COVID$HEMOGLOBINA, v.numerica_COVID$COVID)
cor(v.numerica_COVID$PLAQUETAS, v.numerica_COVID$COVID)
cor(v.numerica_COVID$DIMERO_D, v.numerica_COVID$COVID)
cor(v.numerica_COVID$SAT, v.numerica_COVID$COVID)

#EJERCICIO 5
#1.MATRIZ DE CONFUSION
pcr_ordenado<-arrange(analisisCOVID, analisisCOVID$PCR)
view(pcr_ordenado)
pcr=pcr_ordenado[c(1:8, 18:25), ]
view(pcr)
pcr$PCR<-factor(pcr$PCR)
pcr$PCR
matriz<-table(pcr$IGM, pcr$PCR, dnn= c("estimado", "realidad"))
matriz

#sensibilidad
5/8
#especificidad
6/8

#2.FALSOS NEGATIVOS Y FALSOS POSITIVOS
#falso negativo clinica
3/8
#falso positivo clinica
2/8
#falso negativo instrucciones
3/20
#falso positivo instrucciones
2/50

#4.NIVEL MEDIO DIMERO D
SI=analisisCOVID[analisisCOVID$COVID=="SI",]
X=SI$DIMERO_D
NO=analisisCOVID[analisisCOVID$COVID=="NO",]
Y=NO$DIMERO_D
mean(X)-mean(Y)

#comprobar homocedasticidad
var.test(X, Y, conf.level = 0.99)

#t.test 
t.test(X, Y,
       alternative="greater",
       mu = 0, paired=FALSE, var.equal=TRUE,
       conf.level=0.99)

#5.NIVEL MEDIO HEMOGLOBINA
H=SI$HEMOGLOBINA
E=NO$HEMOGLOBINA
mean(H)-mean(E)

#comprobar homocedasticidad
var.test(H, E, conf.level = 0.99)

#t.test
t.test(H, E,
       alternative="greater",
       mu = 0, paired=FALSE, var.equal=TRUE,
       conf.level=0.99)

