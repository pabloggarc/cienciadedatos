(muestra = read.table("datos_planetas.txt"))

regresion = lm(D~R, data=muestra)

summary(regresion)

#Vemos si tenemos outliers

(res = summary(regresion)$residuals)

(sr = sqrt(sum(res^2)/4))

for (i in 1:length(res)) {
  if (res[i] > 3*sr) {
    print("el suceso ")
    print(res[i])
    print(" es un suceso anómalo o outlier")
  }
}


##################################

(muestra = read.table("datos_materiales.txt"))

regresion = lm(muestra$D~muestra$R)

summary(regresion)

#Vemos si tenemos outliers

(res = summary(regresion)$residuals)

(sr = sqrt(sum(res^2)/7))

for (i in 1:length(res)) {
  if (res[i] > 2*sr) {
    print("el suceso ")
    print(res[i])
    print(" es un suceso anómalo o outlier")
  }
}


