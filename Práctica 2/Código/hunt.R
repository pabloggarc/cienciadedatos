len = function(list) {
  count = 0
  for (element in list) {
    count = count + 1
  }
  count
}

get_frec = function(data, criteria, value) {
  n = len(data)
  p = 0
  for (i in 1:n) {
    if (data[i, criteria] == value) {
      p = p + 1
    }
  }
  p = p / n
}

entropy = function(frec) {
  if (frec == 0 || frec == 1) {
    return(0)
  } else {
    return(-frec * log(frec, 2) - (1 - frec) * log(1 - frec, 2))
  }
}

error = function(frec) {
    return(1 - max(frec, 1 - frec))
}

gini = function(frec) {
    return(1 - frec^2 + (1 - frec)^2)
}

get_gain = function(data, criteria, classes, measure) {
  n = len(data)
  for (element in criteria) {
    frec = get_frec(data, classes, element)
    print(frec)
    switch(measure,
           "entropy" = entropy(frec),
           "error" = error(frec),
           "gini" = gini(frec))
  }
}

hunt = function(data, classes, criteria, measure) {
  
  gain = get_gain(data, criteria, classes, measure)
  
}

(data = read.xlsx("../data/calificaciones.xlsx"))

hunt(data, "C.G", c("T", "L", "P"), "entropy")



