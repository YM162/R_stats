layout(matrix(1))
hist(data$Conc_ppm,main="Concentración",xlab="Concentración",breaks=10)
summary(head(data$Conc_ppm),100)

boxplot(data$Conc_ppm ~ data$Estacion, col=rainbow(28))

N = sort(data$Conc_ppm)
length(N[N>0.93])

L = data$Long_cm


length(L[L>25.20])-length(L[L>33.3])

P = data$Peso_g
quantile(P,0.75)-quantile(P,0.25)


mean(S)
var(S)
sd(S)



D =c(5.50,5.57,5.42,5.61,5.53,5.47,4.88,5.62,5.63,4.07,5.29,5.34,5.26,5.44,5.46,5.55,5.34,5.30,5.36,5.79,5.75,5.29,5.10,5.86,5.58,5.27,5.85,5.65,5.39)
summary(D)
boxplot(D)




X = c(6,6,5,6,7,5,6,7,8,6,7,7,7,6,8,8,7,6,8,7)
Y = c(5,6,7,6,6,8,7,6,6,8,7,7,7,8,6,7,8,9,8,9)

Y.fit= lm(Y~X);Y.fit
plot(X,Y)
abline(Y.fit)

X.fit= lm(X~Y)
plot(Y,X)
abline(X.fit)
cor(X,Y)