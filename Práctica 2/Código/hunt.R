len = function(list) {
  count = 0
  for (element in list) {
    count = count + 1
  }
  count
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

get_frec = function(sample, col, criteria, elements, values) {
  n = 0
  frecs = setNames(data.frame(matrix(rep(0, times = length(values)), nrow = 1)), values)
  
  for (i in 1:len(sample[, col])) {
    if (sample[i, col] %in% elements) {
      
      frecs[1, sample[i, criteria]] = frecs[1, sample[i, criteria]] + 1
      n = n + 1
    }
  }
  frecs / n
}

entropy = function(frec) {
  acum = 0
  for (i in 1:len(frec)) {
    f = frec[1, i]
    if (f != 0 && f != 1) {
      acum = acum - f * log(f, 2)
    }
  }
  acum
}

error = function(frec) {
  return(1 - max(frec))
}

gini = function(frec) {
  acum = 0
  
  for (i in 1:len(frec)) {
    f = frec[1, i]
    acum = acum + f^2
    }
  1 - acum
}

get_impurity = function(sample, col, elements, criteria, equivalence_clases, measure) {
  if (is.null(elements)) {
    return(0)
  }
  frec = get_frec(sample, col, criteria, elements, equivalence_clases)
  switch(measure, "entropy" = entropy(frec), "error" = error(frec), "gini" = gini(frec))
}

get_gain = function(sample, left, right, criteria, right_element, col, measure) {
  n = len(sample)
  equivalence_clases = get_elements(sample, criteria)
  
  impurity_father = get_impurity(sample, col, union(left, right), criteria, equivalence_clases, measure)
  impurity_left = get_impurity(sample, col, left, criteria, equivalence_clases, measure)
  impurity_right = get_impurity(sample, col, right, criteria, equivalence_clases, measure)

  return(impurity_father - (len(left) / n * impurity_left + len(right) / n * impurity_right))
}

get_elements  = function(sample, col) {
  elements = c()
  for (element in sample[, col]) {
    if (!(element %in% elements)) {
      elements = c(elements, element)
    }
  }
  elements
}

create_classification = function(sample, col, right_element, criteria, measure) {

  left = c()
  
  for (i in 1:len(sample[, col])) {
    element = sample[i, col]
    
    if (!(element %in% left) && sample[i, criteria] != right_element) {
      left = unlist(append(left, element))
    }
  }
  right = dif(get_elements(sample, col), left)
  
  if(len(right) > 0){
    gain = get_gain(sample, left, right, criteria, right_element, col, measure)
  }
  else {
    gain = 0
  }
  
  data.frame(parent=col, right_element, left=I(list(left)), right=I(list(right)), gain)
}

hunt = function(sample, classes, criteria, measure) {
  
  final_clasification = data.frame()
  best_gain = -1
  best_clasification = NULL
  
  while (nrow(sample) > 0 && best_gain != 0) {
    best_gain = -1
    for (col in classes) {
      for (element in get_elements(sample, criteria)) {
        
        clasification = create_classification(sample, col, element, criteria, measure)
        
        actual_gain = clasification$gain
        
        if (actual_gain > best_gain) {
          best_gain = actual_gain
          best_clasification = clasification
        }
      }
    }
    
    if (best_gain != 0) {
      sample = subset(sample, sample[, best_clasification$parent] %in% unlist(best_clasification$left))
      final_clasification = rbind(final_clasification, best_clasification)
      rownames(sample) = NULL
    }
    print("11")
    print(sample)
    print("***")
    print(best_clasification)
    print("22")
  }
  print(final_clasification)
}

#(sample = read.xlsx("../Memoria/data/calificaciones.xlsx"))
(sample = read.xlsx("../Memoria/data/vehiculos.xlsx"))

a = hunt(sample, c("tCarnet", "nRuedas", "nPasajeros"), "tVehiculo", "error")
