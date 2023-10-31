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
	add = 0
	for(i in 1:len(p1)){
		add = add + ((p1[i] - p2[i])^2)
	}
	sqrt(add)
}


distance_matrix = function(df){
	empty_matrix = matrix(ncol = len(df[,1]), nrow = len(df[,1]))
	distances = data.frame(empty_matrix)
	for (i in 1:len(df[,1])){
		p_dists = c()
		for (j in i:len(df[,1])){
			dist = euc_distance(df[i,], df[j,])
			distances[i,j] = dist
			distances[j,i] = dist
		}
		distances = rbind(distances,p_dists)
	}
	distances			
}

detect_outliers = function(distance_matrix,k,n){
	outliers = c()
	for (column in 1:len(distance_matrix[1,])){
		ordered_column = bubble(distance_matrix[column,])
		if (ordered_column[k+1] > n){
			outliers = append(outliers,column)
		}
	}
	outliers
}

k_neighbors = function(sample, k, n){
	d_matrix = distance_matrix(sample)
	detect_outliers(d_matrix, k, n)
}



muestra = data.frame("teoría" = c(4,4,5,1,5), "lab" = c(4,3,5,1,4))
a = k_neighbors(muestra,3,2.5)
a
distancias = distance_matrix(muestra)
a=rbind(muestra,c(1,2))
a

(empty_matrix = matrix(ncol = 5, nrow = 5))
