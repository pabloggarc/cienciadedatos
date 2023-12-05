(calificaciones = read.table("datos_notas.txt"))

(muestra = data.frame(calificaciones))

clasificacion = rpart(C.G~T+L+P, data=muestra, method="class", minsplit=1)
