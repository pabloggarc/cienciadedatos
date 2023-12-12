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
  if(len(x) != len(y)){
    stop("X e Y deben tener la misma dimensión")
  }
  else{
    sum = 0
    for (i in 1:len(x)) {
      sum = sum + (x[i] * y[i])
    }
    (sum/len(x))-(fcd_mean(x)*fcd_mean(y))
  }
}

regression_line = function(x, y){
  b = covariance(x, y) / variance(x)
  a = fcd_mean(y) - b * fcd_mean(x)
  matrix(c(a, b), ncol = 1)
}

ssr = function(p, x, y){
  y_hat = c()
  for(xi in x){
    Xi = matrix(c(1, xi), nrow = 1)
    y_hat = c(y_hat, Xi %*% p)
  }
  sum((y_hat - rep(fcd_mean(y)))^2)
}

ssy = function(y){
  sum((y - rep(fcd_mean(y)))^2)
}

r2 = function(sr, sy){
  sr/sy
}

fcd_regression = function(data){
  X = data[, 1]
  Y = data[, 2]
  
  param = regression_line(X, Y)
  r = r2(ssr(param, X, Y), ssy(Y))
  
  print(paste0("y = ", param[2, 1], "x + ", param[1, 1]))
  print(paste0("R2 = ", r))
  
  #line = function(x){matrix(c(1, x), nrow = 1) %*% param}
  #Buscar la forma de plotear la funcion line
}

fcd_regression(data.frame(c(2.4, 6.1, 6.4, 3.4), c(5.4, 5.2, 5.5, 3.9)))