 bubble_LOF = function(d_list, p_list){
	n = len(d_list)
	for (i in 2:n){
		for (j in 1:(n-1)){
			if (d_list[j] > d_list[j+1]){
				temp = d_list[j]
				d_list[j] = d_list[j+1]
				d_list[j+1] = temp

				temp = p_list[j]
				p_list[j] = p_list[j+1]
				p_list[j+1] = temp
			}
		}
	}
	data.frame("dis"=d_list, "poi"=p_list)
}

len = function(list){
	count = 0
	for (element in list){
		count = count + 1
	}
	count
}

manhattan_distance = function(p1, p2) {
	add = 0
	for(i in 1:len(p1)){
		add = add + abs(p1[i] - p2[i])
	}
	add
}

create_distance_matrix = function(df){
	empty_matrix = matrix(ncol = len(df[,1]), nrow = len(df[,1]))
	distances = data.frame(empty_matrix)
	for (i in 1:len(df[,1])){
		p_dists = c()
		for (j in i:len(df[,1])){
			dist = manhattan_distance(df[i,], df[j,])
			distances[i,j] = dist
			distances[j,i] = dist
		}
		distances = rbind(distances, p_dists)
	}
	distances
}

add_list = function(l) {
	add = 0
	for (i in 1:len(l)) {
		add = add + l[i]
	}
	add
}

get_n_set = function(df, k) {

	d_list = list()
	p_list = list()
	
	distance_matrix = create_distance_matrix(df)

	for (column in 1:len(distance_matrix[1,])){
		ordered_column = tail(bubble_LOF(distance_matrix[,column], 1:ncol(distance_matrix)), ncol(distance_matrix)-1)

		k_aux = k

		while(k_aux < nrow(ordered_column) & ordered_column[k_aux, "dis"] == ordered_column[k_aux + 1, "dis"]) {
			k_aux = k_aux + 1
		}

		d_list = append(d_list, list(head(ordered_column[,"dis"], k_aux)))
		p_list = append(p_list, list(head(ordered_column[,"poi"], k_aux)))
	}
	data.frame(dis = I(d_list), poi = I(p_list))
}

get_densities = function(n_set) {
	densities = c()
	for (i in 1:len(n_set[,1])) {
		densities = append(densities, len(n_set[i,1][[1]]) / add_list(n_set[i,1][[1]]))
	}
	densities
}

get_drm = function(n_set) {
	drms = c()
	for (i in 1:len(n_set[,1])) {
		add_densities = 0
	
		points = n_set[i, "poi"][[1]]
		print(points)
		for (j in 1:len(points)) {
			add_densities = add_densities + n_set[points[j], "densities"]
		}

		drm = n_set[i, "densities"] * len(points) / add_densities
		drms = append(drms, drm)
	}
	drms
}

lof = function(muestra, k) {
	n_set = get_n_set(muestra, k)

	densities = get_densities(n_set)
	n_set = cbind(n_set, densities)

	drms = get_drm(n_set)

	n_set = cbind(n_set, drms)
	n_set
}

muestra = data.frame("teoria" = c(4,4,5,1,5), "lab" = c(4,3,5,1,4))
muestra2 = data.frame("mujeres" = c(9,9,11,2,11), "hombres" = c(9,7,11,1,9))

(lof(muestra, 3))