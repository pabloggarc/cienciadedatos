source("aux_functions.R")

regression_line = function(x, y) {
  b = covariance(x, y) / variance(x)
  a = fcd_mean(y) - b * fcd_mean(x)
  matrix(c(a, b), ncol = 1)
}

ssr = function(p, x, y) {
  X = matrix(c(rep(1, times = len(x)), x), ncol = 2)
  y_hat = X %*% p
  sum((y_hat - rep(fcd_mean(y)))^2)
}

ssy = function(y) {
  sum((y - rep(fcd_mean(y)))^2)
}

r2 = function(sr, sy) {
  sr/sy
}

get_residuals = function(p, x, y) {
  X = matrix(c(rep(1, times = len(x)), x), ncol = 2)
  y_hat = X %*% p
  abs(y - y_hat)
}

standard_error = function(p, x, y) {
  sqrt(sum(get_residuals(p, x, y)^2) / len(x))
}

detect_outlier = function(p, x, y, d) {
  X = matrix(c(rep(1, times = len(x)), x), ncol = 2)
  y_hat = X %*% p
  abs(y - y_hat) > d * standard_error(p, x, y)
}

print_outliers = function(x, y, outliers) {
  for (i in 1:len(outliers)) {
    if (outliers[i]) {
      cat(paste0("Outlier: (", x[i], ", ", y[i], ")\n"))
    }
  }
}

fcd_regression = function(sample, d) {
  X = sample[, 1]
  Y = sample[, 2]
  
  param = regression_line(X, Y)
  r = r2(ssr(param, X, Y), ssy(Y))
  
  a = param[1, 1]
  b = param[2, 1]
  
  cat("Recta de regresión\n")
  cat("y =", round(b, 3), "x +", round(a, 3), "\n")
  cat("R2 =", round(r, 3), "\n")
  
  outliers = detect_outlier(param, X, Y, d)
  
  colors = rep("green", length(X))
  colors[outliers] = "red"
  plot(X, Y, col=colors, pch=21, bg=colors, main="Recta de regresión", xlab="Eje X", ylab="Eje Y")
  abline(a=a, b=b, col="blue")
  
  print_outliers(X, Y, outliers)
}


sample1 = read.xlsx("../Memoria/data/conj1.xlsx", colNames=FALSE)
fcd_regression(sample1, 2)

sample2 = read.xlsx("../Memoria/data/conj2.xlsx", colNames=FALSE)
fcd_regression(sample2, 1.5)

sample3 = read.xlsx("../Memoria/data/conj3.xlsx", colNames=FALSE)
fcd_regression(sample3, 2)

sample4 = read.xlsx("../Memoria/data/conj4.xlsx", colNames=FALSE)
fcd_regression(sample4, 33)