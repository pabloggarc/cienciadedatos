source("aux_functions.R")

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

update_distance_matrix = function(initial_dm, actual_dm, forest, new_cluster_points, criteria_name) {
  updated_cols = c(rep(0, times = len(forest)))
  updated_cols[new_cluster_points] = 1
  
  for (i in 1:len(forest)) {
    if (!updated_cols[i]) {
      tree_points = sapply(Traverse(forest[[i]], filterFun = isLeaf), function(x){as.integer(substring(x$name, 2))})
      new_distance = func_criteria(initial_dm, new_cluster_points, tree_points, criteria_name)
      
      for (i in new_cluster_points) {
        for (j in tree_points) {
          minor = min(i, j)
          mayor = max(i, j)
          actual_dm[mayor, minor] = new_distance
        }
      }
      updated_cols[tree_points] = 1
    }
  }
  actual_dm
}

func_criteria = function(initial_dm, new_cluster_points, tree_points, criteria_name) {
  distances = c()
  for (i in new_cluster_points) {
    for (j in tree_points) {
      minor = min(i, j)
      mayor = max(i, j)
      distances = c(distances, initial_dm[mayor, minor])
    }
  }
  criteria_name(distances)
}

calculate_cpcc = function(first_dist_matrix, dist_matrix) {
  x = unlist(first_dist_matrix)
  x = x[!is.na(x) & x != 0]
  
  y = unlist(dist_matrix)
  y = y[!is.na(y) & y != 0]
  
  sx = standard_dev(x)
  sy = standard_dev(y)
  sxy = covariance(x, y)
  
  sxy / (sx * sy)
}

fcd_ahc = function(data, criteria, details = FALSE) {
  
  dist_matrix = create_distance_matrix(data)
  chosen_matrix = create_chosen_matrix(data)
  first_dist_matrix = dist_matrix
  next_cluster = 1
  forest = sapply(colnames(dist_matrix), function(root){Node$new(root)})
  
  while(sum(chosen_matrix, na.rm = TRUE) != (len(dist_matrix) * (len(dist_matrix) - 1) / 2)){
    
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
    
    criteria_name = switch(criteria, "MIN" = min, "MAX" = max, "AVG" = fcd_mean)
    
    dist_matrix = update_distance_matrix(first_dist_matrix, dist_matrix, forest, trees_2_update, criteria_name)
    
    if (details) {
      cat("\nIteración", next_cluster - 1)
      cat("============\n")
      
      cat("\n\nMatriz de distancias\n")
      print(dist_matrix)
      
      cat("\n\nMatriz de distancias elegidas\n")
      print(chosen_matrix)
      
      cat("\n\nDendrogramas\n")
      print(forest)
    }
  }
  
  cat("\n\nMatriz cofenética\n")
  print(dist_matrix)
  
  cat("\nDendrograma final\n")
  print(forest[[1]])
  
  cpcc = calculate_cpcc(first_dist_matrix, dist_matrix)
  cat("\n\nCoeficiente de CPCC: ", cpcc)
  
  plot(forest[[1]])
  plot(as.dendrogram(forest[[1]]), center = TRUE, yaxt='n')
}

(sample = read.xlsx("../Memoria/data/datosKMeans.xlsx"))

fcd_ahc(sample, "MIN")





