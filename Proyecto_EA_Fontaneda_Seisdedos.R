# Carga de datos previa a los ejercicios.
load("coronavirus.Rdata")


# Las fechas donde se han registrado los datos pueden sernos útiles
fechas = unique(coronavirus$date)
# Seleccionamos los casos que cumplen las condiciones
contagios.esp=coronavirus$cases[coronavirus$type=="confirmed"
                                & coronavirus$Country.Region=="Spain"]
#Opciones gráficas
par(mfrow=c(1,2))
#Pintamos la curva de diarios
plot(fechas,contagios.esp,type = "b", lwd = 2, col = 2,
     main = "Contagiados diarios en Espana")
#Pintamos la curva de acumulados
contagios.esp.acc = cumsum(contagios.esp)
plot(fechas,contagios.esp.acc,type = "b", lwd = 2, col = 2,
     main = "Contagiados acumulados en Espana")


#Ejercicio 1:

#1.2.
#a)
fechas[15]
contagios.esp[which(fechas == "2020-03-23")]
#b)
which(fechas == "2020-03-03")
#c)
sum(contagios.esp[which(fechas == "2020-03-23"):which(fechas == "2020-03-27")])
#d)
fechas[which.max(contagios.esp)]
contagios.esp[which.max(contagios.esp)]
#e)
length(which(contagios.esp >=8000))
#f)
length(coronavirus$cases[coronavirus$Country.Region=="Spain"])


#1.3
#Dejamos solo las fechas a partir de las cuales hay más de 10 contagios.
fechascortadas=fechas[which(contagios.esp>10)[1]:length(fechas)]
contagioscortados=contagios.esp[which(contagios.esp>10)[1]:length(contagios.esp)]
contagioscortados.acc=contagios.esp.acc[which(contagios.esp>10)[1]:length(contagios.esp.acc)]
plot(fechascortadas,contagioscortados,type = "b", lwd = 2, col = 2,
     main = "Contagiados diarios en Espana")
plot(fechascortadas,contagioscortados.acc,type = "b", lwd = 2, col = 2,
     main = "Contagiados acumulados en Espana")


#1.4
recuperados.esp=coronavirus$cases[coronavirus$type=="recovered"
                                  & coronavirus$Country.Region=="Spain"]
recuperados.esp.acc=cumsum(recuperados.esp)
recuperadoscortados=recuperados.esp[which(contagios.esp>10)[1]:length(contagios.esp)]
recuperadoscortados.acc=recuperados.esp.acc[which(contagios.esp>10)[1]:length(contagios.esp)]
fallecidos.esp=coronavirus$cases[coronavirus$type=="death"
                                 & coronavirus$Country.Region=="Spain"]
fallecidos.esp.acc=cumsum(fallecidos.esp)
fallecidoscortados=fallecidos.esp[which(contagios.esp>10)[1]:length(contagios.esp)]
fallecidoscortados.acc=fallecidos.esp.acc[which(contagios.esp>10)[1]:length(contagios.esp)]
dev.off()
plot(fechascortadas,contagioscortados,type = "b", lwd = 2, col = 2,
     main = "Contagiados diarios en Espana")
lines(fechascortadas,recuperadoscortados,type = "b",lwd = 2, col = 3)
lines(fechascortadas,fallecidoscortados,type = "b",lwd = 2, col = 8)
plot(fechascortadas,contagioscortados.acc,type = "b", lwd = 2, col = 2,
     main = "Contagiados acumulados en Espana")
lines(fechascortadas,recuperadoscortados.acc,type = "b",lwd = 2, col = 3)
lines(fechascortadas,fallecidoscortados.acc,type = "b",lwd = 2, col = 8)


#1.7
mar7=which(fechas=="2020-03-07")
contagios.it=coronavirus$cases[coronavirus$type=="confirmed"
                                & coronavirus$Country.Region=="Italy"]
contagios.it.acc=cumsum(contagios.it)
contagios.us=coronavirus$cases[coronavirus$type=="confirmed"
                                & coronavirus$Country.Region=="US"]
contagios.us.acc=cumsum(contagios.us)
contagios.and=coronavirus$cases[coronavirus$type=="confirmed"
                                & coronavirus$Country.Region=="Andorra"]
contagios.and.acc=cumsum(contagios.and)
contagios.pr=coronavirus$cases[coronavirus$type=="confirmed"
                                & coronavirus$Country.Region=="Portugal"]
contagios.pr.acc=cumsum(contagios.pr)
plot(fechas[mar7:85],contagios.us[mar7:85],type = "b", lwd = 2, col = 2,
     main = "Contagiados diarios")
lines(fechas[mar7:85],contagios.it[mar7:85],type = "b",lwd = 2, col = 3)
lines(fechas[mar7:85],contagios.pr[mar7:85],type = "b",lwd = 2, col = 4)
lines(fechas[mar7:85],contagios.and[mar7:85],type = "b",lwd = 2, col = 6)
lines(fechas[mar7:85],contagios.esp[mar7:85],type = "b",lwd = 2, col = 7)
plot(fechas[mar7:85],contagios.us.acc[mar7:85],type = "b", lwd = 2, col = 2,
     main = "Contagiados diarios")
lines(fechas[mar7:85],contagios.it.acc[mar7:85],type = "b",lwd = 2, col = 3)
lines(fechas[mar7:85],contagios.pr.acc[mar7:85],type = "b",lwd = 2, col = 4)
lines(fechas[mar7:85],contagios.and.acc[mar7:85],type = "b",lwd = 2, col = 6)
lines(fechas[mar7:85],contagios.esp.acc[mar7:85],type = "b",lwd = 2, col = 7)


#1.9
habitantes.us=327352000
habitantes.it=60359546
habitantes.pr=10276617
habitantes.and=77006
habitantes.esp=46600396
plot(fechas[mar7:85],contagios.and.acc[mar7:85]/habitantes.and,type = "b", lwd = 2, col = 6,
     main = "Contagiados diarios")
lines(fechas[mar7:85],contagios.it.acc[mar7:85]/habitantes.it,type = "b",lwd = 2, col = 3)
lines(fechas[mar7:85],contagios.pr.acc[mar7:85]/habitantes.pr,type = "b",lwd = 2, col = 4)
lines(fechas[mar7:85],contagios.us.acc[mar7:85]/habitantes.us,type = "b",lwd = 2, col = 2)
lines(fechas[mar7:85],contagios.esp.acc[mar7:85]/habitantes.esp,type = "b",lwd = 2, col = 7)


#Ejercicio 2
install.packages("dplyr") #Si no está instalado
library(dplyr) #Cargamos la librería en memoria
contagios <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(Country.Region) %>%
  summarise(total = sum(cases)) %>%
  arrange(Country.Region)
fallecidos <- coronavirus %>%
  filter(type == "death") %>%
  group_by(Country.Region) %>%
  summarise(total = sum(cases)) %>%
  arrange(Country.Region)
recuperados <- coronavirus %>%
  filter(type == "recovered") %>%
  group_by(Country.Region) %>%
  summarise(total = sum(cases)) %>%
  arrange(Country.Region)
total <- data.frame(contagios,recuperados$total,fallecidos$total)
#Cambio de nombre de las variables, podeis elegir los que querais
colnames(total) <- c("pais", "contagios", "recuperados", "fallecidos")
#Quitamos los países con menos de 10 casos y el crucero
total.red <- subset(total, contagios >= 10 & recuperados >= 10
                    & fallecidos >= 10 & pais != "Diamond Princess")
row.names(total.red) = NULL #Reajustamos el numero de fila
install.packages("rworldmap") #Si no está instalado
library(rworldmap) #Cargamos la libreria en memoria
spdf <- joinCountryData2Map(total, joinCode="NAME", nameJoinColumn="pais")
mapCountryData(spdf, nameColumnToPlot="contagios", catMethod="quantiles")



#2.1
plot(total.red$fallecidos,total.red$contagios)
identify(total.red$fallecidos,total.red$contagios)


#2.2
plot(total.red$fallecidos[-c(33,43,74,82,83)],total.red$contagios[-c(33,43,74,82,83)])
pairs(total.red[-c(33,43,74,82,83),2:4])


#2.3
cor(total.red[-c(33,43,74,82,83),2:4])
plot(total.red$contagios[-c(33,43,74,82,83)],total.red$recuperados[-c(33,43,74,82,83)])
recta= lm(total.red$recuperados[-c(33,43,74,82,83)] ~ total.red$contagios[-c(33,43,74,82,83)])
abline(recta, col=2, lwd = 2)


#2.4
pairs(log(total.red[,2:4]))
cor(log(total.red[,2:4]))


#2.5
plot(log(total.red$contagios),log(total.red$fallecidos))
recta= lm(log(total.red$fallecidos) ~ log(total.red$contagios))
abline(recta, col=2, lwd = 2)
as.numeric(recta$coefficients[2])
contagios.recta=100000
fallecidos.recta=exp(recta$coefficients[2]*log(contagios.recta)+recta$coefficients[1])
as.numeric(fallecidos.recta)


#Ejercicio 3
#1. Datos
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


#3.Estadisticos descriptivos.
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


#5.Representación gráfica
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


#Ejercicio 4
#1.Diagrama de pares
variable_numerica=analisisCOVID[, c(5, 6, 7, 8, 9, 11)]
view(variable_numerica)
pairs(variable_numerica)
pairs(variable_numerica, col= analisisCOVID$COVID)


#2.Matriz de correlaciones
par(mfrow=c(1, 2))
correlaciones<-cor(variable_numerica)
corrplot(correlaciones, method="number")
corrplot(correlaciones, method="color", type="upper")


#3.b.Correlaciones COVIDpositivo y COVIDnegativo
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


#4.a.Diagrama de dispresión LINFOCITOS-DIMERO_D
par(mfrow=c(1, 1))
plot(analisisCOVID$LINFOCITOS, analisisCOVID$DIMERO_D, 
     col=analisisCOVID$COVID)


#4.c.Diagrama de cajas 
par(mfrow=c(1, 3))
#diagrama LINFOCITOS
boxplot(analisisCOVID$LINFOCITOS ~ analisisCOVID$COVID, analisisCOVID,
        col = c("red", "darkorange"))


#Diagrama DIMERO_D
boxplot(analisisCOVID$DIMERO_D ~ analisisCOVID$COVID, analisisCOVID,
        col = c("yellow", "green"))
 
#Diagrama HEMOGLOBINA
boxplot(analisisCOVID$HEMOGLOBINA ~ analisisCOVID$COVID, analisisCOVID,
        col = c("blue", "darkblue"))


#4.d.Correlaciones variables numéricas y COVID
v.numerica_COVID$COVID=as.numeric(v.numerica_COVID$COVID)
cor(v.numerica_COVID$LEUCOCITOS, v.numerica_COVID$COVID)
cor(v.numerica_COVID$LINFOCITOS, v.numerica_COVID$COVID)
cor(v.numerica_COVID$HEMOGLOBINA, v.numerica_COVID$COVID)
cor(v.numerica_COVID$PLAQUETAS, v.numerica_COVID$COVID)
cor(v.numerica_COVID$DIMERO_D, v.numerica_COVID$COVID)
cor(v.numerica_COVID$SAT, v.numerica_COVID$COVID)


#Ejercicio 5
#1.MATRIZ DE CONFUSION
pcr_ordenado<-arrange(analisisCOVID, analisisCOVID$PCR)
view(pcr_ordenado)
pcr=pcr_ordenado[c(1:8, 18:25), ]
view(pcr)
pcr$PCR<-factor(pcr$PCR)
pcr$PCR
matriz<-table(pcr$IGM, pcr$PCR, dnn= c("estimado", "realidad"))
matriz

#Sensibilidad
5/8
#Especificidad
6/8

#2.Falsos negativos y falsos positivos
#falso negativo clinica
3/8
#falso positivo clinica
2/8
#falso negativo instrucciones
3/20
#falso positivo instrucciones
2/50

#4.Nivel medio dímero D
SI=analisisCOVID[analisisCOVID$COVID=="SI",]
X=SI$DIMERO_D
NO=analisisCOVID[analisisCOVID$COVID=="NO",]
Y=NO$DIMERO_D
mean(X)-mean(Y)

#Comprobar homocedasticidad
var.test(X, Y, conf.level = 0.99)

#t.test 
t.test(X, Y,
       alternative="greater",
       mu = 0, paired=FALSE, var.equal=TRUE,
       conf.level=0.99)

#5.Nivel medio hemoglobina
H=SI$HEMOGLOBINA
E=NO$HEMOGLOBINA
mean(H)-mean(E)

#Comprobar homocedasticidad
var.test(H, E, conf.level = 0.99)

#t.test
t.test(H, E,
       alternative="greater",
       mu = 0, paired=FALSE, var.equal=TRUE,
       conf.level=0.99)


#Ejercicio opcional

#Importamos los datos y las librerias necesarias
load("coronavirus.Rdata")
library(gifski)
library(gganimate)
library(ggplot2)
library(tidyverse)


#Creamos un dataframe con los países y los datos que nos interesan.

corona.esp.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Spain",select=c(cases, date, Country.Region))
corona.esp.cum <- corona.esp.cum %>%
  mutate(cases = cumsum(cases))

corona.it.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Italy",select=c(cases, date, Country.Region))
corona.it.cum <- corona.it.cum %>%
  mutate(cases = cumsum(cases))

corona.us.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="US",select=c(cases, date, Country.Region))
corona.us.cum <- corona.us.cum %>%
  mutate(cases = cumsum(cases))

corona.an.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Andorra",select=c(cases, date, Country.Region))
corona.an.cum <- corona.an.cum %>%
  mutate(cases = cumsum(cases))

corona.por.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Portugal",select=c(cases, date, Country.Region))
corona.por.cum <- corona.por.cum %>%
  mutate(cases = cumsum(cases))

corona.kr.cum <- subset(coronavirus, coronavirus$type=="confirmed" & coronavirus$Country.Region=="Korea, South",select=c(cases, date, Country.Region))
corona.kr.cum <- corona.kr.cum %>%
  mutate(cases = cumsum(cases))

total.acc <- rbind(corona.esp.cum,corona.it.cum,corona.us.cum,corona.an.cum,corona.por.cum,corona.kr.cum)


#Hacemos los cambios entre las fechas más progresivos mediante dos pasadas con complete()
#Una con un paso de 1, y otra con un paso de 0.5, para dar un efecto más estético a la animación.

total.acc_smoother <- total.acc %>%
  group_by(Country.Region) %>%
  complete(date = full_seq(date, 1)) %>%
  mutate(cases = spline(x = date, y = cases, xout = date)$y) %>%
  group_by(date) %>%
  mutate(rank = min_rank(-cases) * 1) %>%
  ungroup()

  group_by(Country.Region) %>%
  complete(date = full_seq(date, .5)) %>%
  mutate(cases = spline(x = date, y = cases, xout = date)$y) %>%
  mutate(rank =      approx(x = date, y = cases,      xout = date)$y) %>%
  ungroup()  %>% 
  arrange(Country.Region,date)


#Función de Steve Jburr adaptada para nuestros datos (https://github.com/stevejburr/Bar-Chart-Race/blob/master/Final.R)

make_barchart_race <- function(data,x,y,
                               number=10,
                               title="",
                               caption="",
                               nframes=300,
                               fps=5,
                               end_pause=20){
  y <- rlang::enquo(y)
  x <- rlang::enquo(x)
  number <- rlang::enquo(number)
  data %>%
    group_by(date) %>%
    arrange(-!!y) %>%
    mutate(rank=row_number()) %>%
    filter(rank<=!!number) -> data
  data %>%
    ggplot(aes(x=-rank,y=!!y,fill=!!x, group=!!x)) +
    geom_tile(aes(y=!!y/2,height=!!y),width=0.9,show.legend = F)+
    geom_text(aes(label=!!x),
              hjust="right",
              colour="black",
              fontface="bold",
              nudge_y=-1000)+
    geom_text(aes(label=scales::comma(!!y)),
              hjust="left",
              nudge_y=2000,
              colour="grey30")+
    theme_minimal() +
    coord_flip(clip="off") +
    scale_x_discrete("") +
    scale_y_continuous("",labels=scales::comma)+
    theme(panel.grid.major.y=element_blank(),
          panel.grid.minor.x=element_blank(),
          plot.title= element_text(size=20,colour="grey50",face="bold"),
          plot.caption = element_text(colour="grey50"),
          plot.subtitle = element_text(size=20,colour="grey50",face="bold"),
          plot.margin = margin(1,1,1,2,"cm"),
          axis.text.y=element_blank())+
    transition_time(date) +
    labs(title=title,
         subtitle='{round(frame_time,0)}',
         caption=caption)-> p
  animate(p, nframes = nframes, fps = fps, end_pause = end_pause)
}

#Ejecutamos la función con los frames y velocidad que queremos y guardamos el .gif

make_barchart_race(total.acc_smoother,
                   Country.Region,
                   cases,
                   title="Evolución de los contagios totales\n de CoVid-19 en países seleccionados",
                   caption="",
                   nframes=1500,
                   fps=30,
                   end_pause=60)
anim_save("out.gif")
