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

fcd_min = function(distances, chosen) {
  print(distances)
  min_y = 2
  min_x = 1
  min_v = distances[2, 1]
  for (i in 2:len(distances[,1])) {
    for (j in 1:(i-1)) {
      if (!chosen[i, j] && distances[i, j] < min_v) {
        min_v = distances[i,j]
        min_y = i
        min_x = j
      }
    }
  }
  c(min_v, min_y, min_x)
}

create_distance_matrix = function(df) {
  n = len(df[,1])
  empty_matrix = matrix(0, ncol = n, nrow = n)
  distances = data.frame(empty_matrix)
  for (i in 1:n) {
    for (j in i:n) {
      distances[j, i] = euc_distance(df[i,], df[j,])
    }
  }
  distances
}

update_clusters = function(clusters, min_y, min_x) {
  new_clusters = clusters
  new_clusters[min_x] = paste0("C",iter, "{", clusters[min_x], ",", clusters[min_y], "}")
  new_clusters[min_y] = ""
  new_clusters
}

ahc = function(df) {
  n = len(df[,1])
  distances = create_distance_matrix(df)
  updated_distances = distances
  clusters = 1:n
  chosen = data.frame(matrix(0, ncol = n, nrow = n))
  iter = 1
  while(iter < n) {
    my_min = fcd_min(updated_distances, chosen)
    min_v = my_min[1]
    min_y = my_min[2]
    min_x = my_min[3]
    new_cluster = paste0("C",iter, "{", clusters[min_x], ",", clusters[min_y], "}")
    print(new_cluster)
    
    chosen[min_y, min_x] = 1
    
    clusters = update_clusters(clusters, min_y, min_x)
    
    iter = iter + 1
  }
}


df_sample = data.frame(t(matrix(c(0.89, 2.94, 4.36, 5.21, 3.75, 1.12, 6.25, 3.14, 4.1, 1.8, 3.9, 4.27), 2, 6, dimnames=list(c("X","Y")))))


ahc(df_sample)


# Primero, instala e importa el paquete data.tree
#install.packages("data.tree")
library(data.tree)

# Crea un nodo raíz
raiz <- Node$new("C")

# Agrega hijos al nodo raíz
raiz$AddChild("C4")
raiz$AddChild("1")

# Agrega un nieto al primer hijo
raiz$children[[1]]$AddChild("Nieto 1")
raiz$children[[1]]$AddChild("Nieto 1")

raiz$children[[[1]]]$AddChild("Nieto 3")

# Imprime el árbol
print(raiz)

