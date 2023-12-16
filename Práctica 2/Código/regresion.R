len = function(list) {
  count = 0
  for (element in list) {
    count = count + 1
  }
  count
}

fcd_mean = function(list) {
  add = 0
  for (i in 1:len(list)) {
    add = add + list[i]
  }
  add / len(list)
}

standard_dev = function(list) {
  mean = fcd_mean(list)
  n = len(list)
  add = 0
  for (i in 1:n) {
    add = add + ((list[i] - mean)^2)
  }
  sqrt(add/n)
}

variance = function(list) {
  dev = standard_dev(list)
  var = dev^2
  var
}

covariance = function(x, y) {
  if (len(x) != len(y)) {
    stop("X e Y deben tener la misma dimensión")
  } else {
    sum = 0
    for (i in 1:len(x)) {
      sum = sum + (x[i] * y[i])
    }
    (sum/len(x))-(fcd_mean(x)*fcd_mean(y))
  }
}

regression_line = function(x, y) {
  b = covariance(x, y) / variance(x)
  a = fcd_mean(y) - b * fcd_mean(x)
  matrix(c(a, b), ncol = 1)
}

ssr = function(p, x, y) {
  y_hat = c()
  for (xi in x) {
    Xi = matrix(c(1, xi), nrow = 1)
    y_hat = c(y_hat, Xi %*% p)
  }
  sum((y_hat - rep(fcd_mean(y)))^2)
}

ssy = function(y) {
  sum((y - rep(fcd_mean(y)))^2)
}

r2 = function(sr, sy) {
  sr/sy
}

fcd_regression = function(sample) {
  X = sample[, 1]
  Y = sample[, 2]
  
  param = regression_line(X, Y)
  r = r2(ssr(param, X, Y), ssy(Y))
  
  a = param[1, 1]
  b = param[2, 1]
  
  print(sprintf("y = %.3fx + %.3f", b, a))
  print(sprintf("R2 = %.3f", r))
  
  plot(X, Y, col="blue", main="Recta de regresión", xlab="Eje X", ylab="Eje Y")
  
  abline(a=a, b=b, col="red")
}


sample1 = read.xlsx("../Memoria/data/conj1.xlsx", colNames=FALSE)
fcd_regression(sample1)

sample2 = read.xlsx("../Memoria/data/conj2.xlsx", colNames=FALSE)
fcd_regression(sample2)

sample3 = read.xlsx("../Memoria/data/conj3.xlsx", colNames=FALSE)
fcd_regression(sample3)

sample4 = read.xlsx("../Memoria/data/conj4.xlsx", colNames=FALSE)
fcd_regression(sample4)