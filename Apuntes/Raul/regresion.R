muestra = read.table("planetas.txt")

regresion = lm(D~R, data = muestra)

regresion

summary(regresion)

(res = summary(regresion)$residuals)

(rs = sqrt(sum(res^2)/4))

for (i in 1:length(res)) {
  if (res[i] > 3*rs) { 
    print("el suceso"); print(res[i]); print("es un suceso anómalo o outlier")
  }
}