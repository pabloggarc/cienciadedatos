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
  res = list()
  for (element in c1) {
    if (!(element %in% c2)) {
      res = append(res, element)
    }
  }
  res
}

get_frec = function(data, col, branch, criteria, values) {
  n = 0
  frecs = c()
  
  for () {
    
  }
  
  #lo de abajo esta mal, iterar en col y hacer un contador de las veces que aparece cada elemento de branch
  
  
  
    p = 0
    for (i in 1:n) {
      if (data[i, col] == value) {
        p = p + 1
      }
    }
    if (p==0 || n==0) {
      return(0)
    }
    frecs = append(frecs, p / n)
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

get_impurity = function(data, col, elements, criteria, equivalence_clases, measure) {
  frec = get_frec(data, classes, element)############
  switch(measure,
       "entropy" = entropy(frec),
       "error" = error(frec),
       "gini" = gini(frec))
}

get_gain = function(data, left, right, criteria, right_element, col, measure) {
  n = len(data)
  equivalence_clases = get_elements(criteria)
  impurity_father = get_impurity(data, col, union(left, right), criteria, equivalence_clases, measure)
  impurity_left = get_impurity(data, col, left, criteria, equivalence_clases, measure)
  impurity_right = get_impurity(data, col, right, criteria, equivalence_clases, measure)
  
  
  
}

get_elements  = function(data, col) {
  elements = c()
  for (element in data[, col]) {
    if (!(element %in% elements)) {
      elements = c(elements, element)
    }
  }
  elements
}

create_classification = function(data, col, right_element, criteria) {

  left = list()
  
  for (i in 1:len(data[, col])) {
    element = data[i, col]
    if (!(element %in% left) && data[i, criteria] != right_element) {
      left = append(left, element)
    }
  }
  right = dif(get_elements(data, col), left)
  
  gain = get_gain(data, criteria, classes, measure)
  
  data.frame(parent=col, right_element, left=I(list(left)), right=I(list(right))), gain)
}

hunt = function(data, classes, criteria, measure) {
  
  gain = -1
  
  for (col in classes) {
    for (element in get_elements(data, criteria)) {
      
      clasification = create_classification(data, col, element)
      
      actual_gain = get_gain(data, classes, criteria, measure)
      
      if (actual_gain > gain) {
        left = actual_left
        right = actual_right
        gain = actual_gain
      }
    }
    
  }
  
}

(data = read.xlsx("../data/calificaciones.xlsx"))

#hunt(data, c("T", "L", "P"), "C.G", "entropy")

create_classification(data, "T", "Ap", "C.G")

