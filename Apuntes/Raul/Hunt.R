calificaciones = read.table("hunt.txt")

(muestra = data.frame(calificaciones))

clasificacion = rpart(CG~T+L+P, data=muestra, method="class", minsplit=1)

clasificacion

