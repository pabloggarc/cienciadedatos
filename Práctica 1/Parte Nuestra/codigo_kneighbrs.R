bubble = function(list, asc = TRUE){
	n = len(list)
	if(asc){
		for (i in 2:n){
			for (j in 1:(n-1)){
				if (list[j] > list[j+1]){
					temp = list[j]
					list[j] = list[j+1]
					list[j+1] = temp
				}
			}
		}
	}
	else {
		for (i in 2:n){
			for (j in 1:(n-1)){
				if (list[j] < list[j+1]){
					temp = list[j]
					list[j] = list[j+1]
					list[j+1] = temp
				}
			}
		}
	}
	list
}

len = function(list){
	count = 0
	for (element in list){
		count = count + 1
	}
	count
}







euc_distance = function(p1,p2){
	if(len(p1) == len(p2)){
		add = 0
		for(i in 1:len(p1)){
			add = add + ((p1[i] - p2[i])^2)
		}
		sqrt(add)
	} else {
		print("No se puede calcular la distancia euclídea")
	}
}


create_distance_matrix = function(df){
	empty_matrix = matrix(ncol = len(df[,1]), nrow = len(df[,1]))
	distances = data.frame(empty_matrix)
	for (i in 1:len(df[,1])){
		for (j in i:len(df[,1])){
			dist = euc_distance(df[i,], df[j,])
			distances[i,j] = dist
			distances[j,i] = dist
		}
	}
	distances			
}

detect_outliers = function(sample, distance_matrix, k, d, details){
	if(details){
		print("->PASO 3: IDENTIFICACIÓN DE LOS OUTLIERS")
	}	
	outliers = c()
	for (column in 1:len(distance_matrix[1,])){
		ordered_column = bubble(distance_matrix[column,])
		if (ordered_column[k+1] > d){
			outliers = append(outliers,column)
			if(details){
				cat("El punto", column, "(",paste(sample[column,], collapse = ","), ") es un outlier\n")
			}
		}
	}
	outliers
}

k_neighbors = function(sample, k, d, details = FALSE){
	if(details){
		print("->PASO 1: DETERMINACIÓN DE d Y k")
		cat("Grado de outlier: d =",d,"\n")
		cat("K-Vecino más próximo: k =",k,"\n\n")
	}
	d_matrix = create_distance_matrix(sample)
	if(details){
		print("->PASO 2: MATRIZ DE DISTANCIAS ENTRE PUNTOS:\n")
		print(d_matrix)
		cat("\n")
	}
	detect_outliers(sample, d_matrix, k, d, details)
}



muestra2 = data.frame("teoría" = c(4,4,5,1,5), "lab" = c(4,3,5,1,4))
muestra = data.frame("mujeres" = c(9,9,11,2,11), "hombres" = c(9,7,11,1,9))
k_neighbors(muestra,3,3.5,TRUE)
