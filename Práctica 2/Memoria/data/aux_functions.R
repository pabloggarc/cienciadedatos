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

union = function(c1, c2) {
  if (len(c1) == 0) {
    c2
  } else if (is.element(c1[1], c2)) {
    union(c1[-1], c2)
  } else {
    union(c1[-1], append(c2, c1[1]))
  }
}

dif = function(c1, c2) {
  res = c()
  for (element in c1) {
    if (!(element %in% c2)) {
      res = c(res, element)
    }
  }
  res
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

euc_distance = function(p1, p2) {
  sqrt(((p1[1] - p2[1])^2) + ((p1[2] - p2[2])^2))
}

covariance = function(x, y) {
  if (len(x) != len(y)) {
    stop("X e Y deben tener la misma dimensión")
  }
  else {
    sum = x %*% y
    (sum/len(x))-(fcd_mean(x)*fcd_mean(y))
  }
}