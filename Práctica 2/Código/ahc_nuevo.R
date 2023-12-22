len = function(list) {
  count = 0
  for (element in list) {
    count = count + 1
  }
  count
}

euc_distance = function(p1, p2) {
  sqrt(((p1[1] - p2[1])^2) + ((p1[2] - p2[2])^2))
}

create_distance_matrix = function(df) {
  n = len(df[,1])
  empty_matrix = matrix(0, ncol = n, nrow = n)
  distances = data.frame(empty_matrix)
  for (i in 1:n) {
    for (j in 1:i) {
      distances[i, j] = euc_distance(df[i,], df[j,])
      if (i != j) distances[j, i] = NA
    }
  }
  distances
}

create_chosen_matrix = function(df) {
  n = len(df[,1])
  empty_matrix = matrix(0, ncol = n, nrow = n)
  chosen = data.frame(empty_matrix)
  for (i in 1:n) {
    for (j in 1:i) {
      chosen[j, i] = NA
    }
  }
  chosen
}

min_ahc = function(distances, chosen) {
  min_d = Inf
  min_c = c()
  for(i in 1:nrow(distances)) {
    for(j in 1:i) {
      if (distances[i, j] < min_d && distances[i, j] != 0 && chosen[i, j] == 0) {
        min_d = distances[i, j]
        min_c = c(i, j)
      }
    }
  }
  min_c
}

choose_distance = function(distances, chosen, distance){
  for(i in 1:nrow(distances)) {
    for(j in 1:i) {
      if (distances[i, j] == distance) {
        chosen[i, j] = 1
      }
    }
  }
  chosen
}

coltags_2_update = function(distances, chosen, distance){
  cols = c()
  for(i in 1:nrow(distances)) {
    for(j in 1:i) {
      if (distances[i, j] == distance) {
        cols = c(cols, i, j)
      }
    }
  }
  cols
}

fcd_ahc = function(data){
  
  dist_matrix = create_distance_matrix(data)
  chosen_matrix = create_chosen_matrix(data)
  first_dist_matrix = dist_matrix
  next_cluster = 1
  forest = sapply(colnames(dist_matrix), function(root){Node$new(root)})
  
  while(sum(chosen_matrix, na.rm = TRUE) != (len(dist_matrix) * (len(dist_matrix) - 1) / 2)){
    print(dist_matrix)
    print(chosen_matrix)
    print(forest)
    
    #Buscar y marcar distancia mínima
    min_index = min_ahc(dist_matrix, chosen_matrix)
    chosen_matrix = choose_distance(dist_matrix, chosen_matrix, dist_matrix[min_index[1], min_index[2]])
    
    #Se actualizan las etiquetas de la matriz
    new_tree_tag = Node$new(paste0("C", next_cluster))
    new_tree_tag$AddChildNode(forest[[min_index[1]]])
    new_tree_tag$AddChildNode(forest[[min_index[2]]])
    trees_2_update = sapply(Traverse(new_tree_tag, filterFun = isLeaf), function(x){as.integer(substring(x$name, 2))})
    for(i in 1:len(forest)){
      if(i %in% trees_2_update){
        forest[[i]] = new_tree_tag
      }
    }
    next_cluster = next_cluster + 1
    
    #Se actualiza la matriz de distancias (TODO)
    
    if(next_cluster == 5) break
  }
}
  

df_sample = data.frame(t(matrix(c(0.89, 2.94, 4.36, 5.21, 3.75, 1.12, 6.25, 3.14, 4.1, 1.8, 3.9, 4.27), 2, 6, dimnames=list(c("X","Y")))))
fcd_ahc(df_sample)