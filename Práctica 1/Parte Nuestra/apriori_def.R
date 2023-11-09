len = function(l1) {
	count = 0
	for (element in l1) {
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

intersect = function(c1, c2) {
	if (len(c1) == 0) {
		c()
	} else if (is.element(c1[1], c2) ) {
		append(intersect(c1[-1], c2), c1[1])
	} else {
		intersect(c1[-1], c2)
	}
}

dif = function(c1, c2) {
	res = c()
	for (element in c1) {
		if (!(element %in% c2)) {
			res = append(res, element)
		}
	}
	res
}

count_appearance = function(table, elements) {
	count = 0
	for (i in 1:len(table[,1])) {
		acum = 1
		for (element in elements) {
			acum = (table[i,element]) & acum
		}
		count = count + acum
	}
	count
}

support = function(table, elements) {
	count_appearance(table, elements) / len(table[,1])
}

support_clasif = function(table, ocurrences, s) {
	valid_ocurrences = list()
	for (ocurrence in ocurrences) {
		support_oc = support(table, ocurrence)
		if (support_oc >= s) {
			valid_ocurrences = append(valid_ocurrences, list(ocurrence))
		}
	}
	valid_ocurrences	
}


binom = function(n, k) {
  temp = matrix(0, nrow = n + 1, ncol = k + 1)
  for (i in 0:n) {
	for (j in 0:min(i, k)) {
		if (j == 0 || j == i) {
			temp[i + 1, j + 1] = 1
		} 
		else {
			temp[i + 1, j + 1] = temp[i, j] + temp[i, j + 1]
		}
	}
  }
  temp[n + 1, k + 1]
}

equals = function(l1, l2) {
	n = len(l1)

	if (n != len(l2)) return(FALSE)
	if (n==0) return(TRUE)

	for (i in 1:n) if (l1[i] != l2[i]) return(FALSE)
	TRUE
}

cabeza = function(l, n) {
	if (n <= 0) return(NULL)
	head(l, n)
}

cola = function(l, n) {
	if (n <= 0) return(NULL)
	tail(l, n)
}

fk_1 = function(clasif) {
	comb = lapply(clasif, c)
	n = len(clasif)
	n_comb = n
	inicio = 1

	for (k in 2:n) {
		for (i in inicio:(n_comb-1)) {
			for (j in (i+1):n_comb) {

				cab1 = cabeza(comb[[i]], len(comb[[i]])-1)
				cab2 = cabeza(comb[[j]], len(comb[[j]])-1)
				
				col1 = cola(comb[[i]], 1)
				col2 = cola(comb[[j]], 1)

				if (equals(cab1, cab2) & !equals(col1, col2)) {
					new = c(cab1, col1, col2)
					comb = c(comb, list(new))
				}
			}
		}
		inicio = n_comb + 1
		n_comb = n_comb + binom(n, k)
	}
	cola(comb, n_comb-n)
}

confidence = function(table, left, right) {
	count_appearance(table, union(left, right)) / count_appearance(table, left)
}

get_asotiations = function(table, candidates) {
	asotiations = data.frame()
	final_asotiations = lapply(candidates, function(x){
		k = len(x)
		for (dim in 1:(k - 1)){
			left_sides = combn(x, m=dim, simplify=TRUE)
			for (col in 1:len(left_sides[1,])){
				left_side = left_sides[, col]
				right_side = dif(x, left_side)
				new_asoc = data.frame(left = I(list(left_side)), right = I(list(right_side)))
				asotiations <<- rbind(asotiations, new_asoc)
			}
		} 
	})
	asotiations
}

getElements = function(data) {
  elements = c()
  for (i in 1:len(data)) {
    elements = union(elements, data[[i]])
  }
  elements
}

getTable = function(data, elements) {
  nCol = len(elements)
  nRow = len(data)
  table = data.frame(matrix(0, ncol = nCol, nrow = nRow, dimnames = list(1:nRow, elements)))

  for (i in 1:nRow) {
    for (j in 1:len(data[[i]])) {
      table[i, data[[i]][j]] = 1
    }
  }
  table
}

print_asotiations = function(left_list, right_list){
	left_side = paste(left_list[[1]], collapse = ",")
	right_side = paste(right_list[[1]], collapse = ",")
	cat("{")
	cat(left_side)
	cat("} --> {")
	cat(right_side)
	cat("}\n")
}

ap_genrules = function(table, asoc, c) {
	asoc = cbind(asoc, data.frame(matrix(1, ncol = 1, nrow = len(asoc[,1]), dimnames = list(1:len(asoc[,1]), "valid"))))
	#valid_asoc = data.frame(left = list(), right = list())	# Lista de validos. Pasan confianza
	for (i in len(asoc[,"left"]):1) {		# Empieza por el mas largo
		A = asoc[i, "left"][[1]]		# Conjunto A (izquierdo) de la asociacion
		right = asoc[i, "right"][[1]]		# Lado derecho de la asociacion
		B = union(A, right)		# Conjunto B de la asociacion

		if(asoc[i, "valid"]) {
			conf = confidence(table, A, right)	
			if (conf < c) {
				# Lo que hay dentro de este for es buscar todos los subconjuntos A', calcular su lado derecho (B-A') y anadirlos a discard_iter
				for (j in 1:len(A)) {	# j son las "dimensiones" de A'
					A_primes = unlist(lapply(j, function(m) {combn(A, m=m, simplify=TRUE)}), recursive=FALSE) # Subconjuntos de A de dimension j
					for (k in seq(1, len(A_primes), by=j)) {	# Indice en el que empieza A' en la lista A_primes
						new_left = A_primes[k:(k+j-1)]
						new_right = dif(B, new_left)
						index = indexOf_asoc(asoc, new_left, new_right) #Indice de la asociación que se va a descartar
						asoc[index, "valid"] = 0
					}
				}
			}
		}
	}
	subset(asoc, asoc$valid == 1)
}

indexOf_asoc = function(df, left_side, right_side) {
	encontrado = FALSE
	i = 1
	while(!encontrado & i <= len(df[,1])){
		encontrado = equals(df[i, "left"][[1]], left_side) & equals(df[i, "right"][[1]], right_side)
		i = i + 1
	}
	i-1
}

apriori = function(data, s, c) {

  elements = getElements(data)

  table = getTable(data, elements)

  soporte_clasif = support_clasif(table, elements, s)
  
  # PARA CUANDO ESTÉN SOLUCIONADAS LAS ESTRUCTURAS DE DATOS
  combinations = fk_1(soporte_clasif) 
  valid_support = support_clasif(table, combinations, s)
  
  if (len(valid_support) > 0) {
    
    conf = get_asotiations(table, valid_support)
    valid_asoc = ap_genrules(table, conf, c)
    for (i in 1:len(valid_asoc[,1])){
		print_asotiations(valid_asoc[i,1], valid_asoc[i,2])
	}
    
  } else {
    print("No hay ninguna combinación que pase el soporte")
  }
}

data2 = list(c("Pan", "Agua", "Leche", "Naranjas"), c("Pan", "Agua", "Café", "Leche"), c("Pan", "Agua", "Leche"), c("Pan", "Café", "Leche"), c("Pan", "Agua"), c("Leche"))
data = list(c("X", "C", "N", "B"), c("X", "T", "B", "C"), c("N", "C", "X"), c("N", "T", "X", "B"), c("X", "C", "B"), c("N"), c("X", "B", "C"), c("T", "A"))

apriori(data2, 0.4, 0.9)


