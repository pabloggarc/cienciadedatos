
len = function(list) {
	count = 0
	for (element in list) {
		count = count + 1
	}
	count
}

generar_combinaciones = function(lista) {
  comb = list()

  for (longitud in 1:length(lista)) {
    combinaciones_longitud = combn(lista, longitud, simplify = FALSE)
    
    comb = c(comb, combinaciones_longitud)
  }
  
  tail(comb, len(comb)-len(lista))
}

factorial = function(n) {
	if (n <= 1) return(1)
	n * factorial(n-1)
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

apri_gen = function(clasif) {
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
		n_comb = n_comb + factorial(n) / (factorial(k) * factorial(n-k))
	}
	cola(comb, n_comb-n)
}

l = c("A", "B", "C", "D")

apri_gen(l)