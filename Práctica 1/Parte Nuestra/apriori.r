len = function(list){
	count = 0
	for (element in list){
		count = count + 1
	}
	count
}

union = function(c1, c2){
	if (len(c1) == 0){
		c2
	}
	else if (is.element(c1[1], c2)){
		union(c1[-1], c2)
	}
	else{
		union(c1[-1], append(c2, c1[1]))
	}
}

intersect = function(c1, c2){
	if (len(c1) == 0){
		c()
	}
	else if (is.element(c1[1], c2)){
		append(intersect(c1[-1], c2), c1[1])
	}
	else{
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

count_appearance = function(table, elements){
	count = 0
	for (i in 1:len(table[,1])){
		acum = 1
		for (element in elements){
			acum = (table[i,element]) & acum
		}
		count = count + acum
	}
	count
}

support = function(table, elements) {
	count_appearance(table, elements) / len(table[,1])
}

support_clasif = function(table, ocurrences, s){
	valid_ocurrences = c()
	for (ocurrence in ocurrences){
		support_oc = support(table, ocurrence)
		if (support_oc >= s){
			valid_ocurrences = append(valid_ocurrences, ocurrence)
		}
	}
	valid_ocurrences	
}

create_comb = function(table, clasif, s) {
    lista = c()
    dim = 2
    next_dim = TRUE
    while (dim <= len(clasif) & next_dim == TRUE) {
        next_dim = FALSE
        comb = unlist(lapply(dim, function(m) {combn(clasif, m=m, simplify=TRUE)}), recursive=FALSE)

        for (j in seq(1, len(comb), by=dim)) {
            add = c()
            for (k in j:(j+dim-1)) {
                add = append(add, comb[k])
            }
            if (support(table, add) >= s) {
                next_dim = TRUE
                lista = append(lista, list(add))
            }
        }
        dim = dim+1
    }
    lista
}

confidence = function(table, left, right) {
	count_appearance(table, union(left, right)) / count_appearance(table, left)
}

get_asotiations = function(table, comb, c) {
	kMax = len(comb[len(comb)][[1]])
	listLeft = list()
	listRight = list()
	
	for (i in 2:kMax) {
	
		split = Filter(function(x) length(x)==i, comb)
		
		for (j in 1:len(split)) {
			
			for (k in 1:(i-1)) {
			  
				leftSides = lapply(k, function(m) {combn(split[j][[1]], m=m, simplify=TRUE)})
				
				df = do.call(rbind.data.frame, leftSides)

				for (n in 1:len(df[1,])) {

					all = split[j][[1]]

					left = df[,n]

					right = dif(split[j][[1]], df[,n])

					if (confidence(table, left, right) >= c) {

						listLeft = append(listLeft , list(left))
						listRight = append(listRight , list(right))

					}
				}
			}
		}
	}
	asoc = data.frame(left = I(listLeft), right = I(listRight))
	asoc
}

apriori = function(table, elements, s, c) {
  soporte_clasif = support_clasif(table, elements, s)
  
  combinations = create_comb(table, soporte_clasif, s)
  
  if (len(combinations) > 0) {
    
    conf = get_asotiations(table, combinations, c)
    print(conf)
    
  } else {
    print("No hay ninguna combinación que pase el soporte")
  }
}

elements = c(c("P"), c("A") ,c("L"), c("C"), c("N"))
table = matrix(c(1,1,0,1,1, 1,1,1,1,0, 1,1,0,1,0, 1,0,1,1,0, 1,1,0,0,0, 0,0,0,1,0),6,5,byrow=TRUE,dimnames=list(c("suceso1","suceso2","suceso3","suceso4","suceso5","suceso6"),c("P","A","C","L","N")))

elements = c(c("A"), c("B"), c("C"), c("D"), c("E"))
table = matrix(c(0,0,0,0,1, 0,0,0,1,0, 0,1,1,0,1, 1,0,0,1,0, 0,0,0,0,1, 0,0,1,0,0), 6, 5, byrow=TRUE, dimnames=list(c("suceso1", "suceso2", "suceso3", "suceso4", "suceso5", "suceso6"), c("A", "B", "C", "D", "E")))  

apriori(table, elements, 0.5, 0.8)


















