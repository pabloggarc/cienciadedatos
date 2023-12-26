source("aux_functions.R")

create_distance_matrix = function(points, centroids) {
  n_pts = nrow(points)
  n_cent = nrow(centroids)
  distances = data.frame(matrix(ncol = n_pts, nrow = n_cent))
  for (cent in 1:n_cent) {
    for (pt in 1:n_pts) {
      distances[cent, pt] = euc_distance(points[pt,], centroids[cent,])
    }
  }
  distances            
}

create_assigment_matrix = function(dist_matrix) {
  n_pts = ncol(dist_matrix)
  n_cent = nrow(dist_matrix)
  assig_matrix = data.frame(matrix(0, ncol = n_pts, nrow = n_cent))
  for (col in 1:n_pts) {
    minimum_centroid = 1
    for (cent in 2:n_cent) {
      if (dist_matrix[cent, col] < dist_matrix[minimum_centroid, col]) {
        minimum_centroid = cent
      }
    }
    assig_matrix[minimum_centroid, col] = 1
  }
  assig_matrix            
}

update_cent = function(df_sample, centroids, assig_matrix) {
  n_pts = ncol(assig_matrix)
  n_cent = nrow(assig_matrix)
  new_centroids = c()
  for (cent in 1:n_cent) {
    centroid_x = 0
    centroid_y = 0
    count = 0
    for (pt in 1:n_pts) {
      if (assig_matrix[cent, pt] == 1) {
        centroid_x = centroid_x + df_sample[pt, 1]
        centroid_y = centroid_y + df_sample[pt, 2]
        count = count + 1
      }
    }
    if (count == 0){
      new_centroids = append(append(new_centroids, centroids[cent,1]),centroids[cent,2])
    }
    else {
      new_centroids = append(append(new_centroids, centroid_x/count), centroid_y/count)
    }
  }
  data.frame(t(matrix(new_centroids, 2, n_cent, dimnames=list(c("X","Y")))))
}

printCentroids = function(df_sample, centroids, assig_matrix){
  n_cents = nrow(assig_matrix)
  n_pts = ncol(assig_matrix)
  cat("\n--RESULTADOS--\n")
  for (cent in 1:n_cents){
    cat(paste0("Centroide ", cent, " (", centroids[cent,1], ", ", centroids[cent,2],") \n"))
    for(pt in 1:n_pts){
      if (assig_matrix[cent,pt] == 1){
        cat(paste0("  - Punto ", pt, " (", df_sample[pt, 1], ", ", df_sample[pt,2], ")\n"))
      }
    }
    cat("\n")
  }
}

create_clust_vector = function(assig_matrix){
  n_pts = ncol(assig_matrix)
  n_cents = nrow(assig_matrix)
  clust_vector = c()
  for(pt in 1:n_pts){
    for(cent in 1:n_cents){
      if (assig_matrix[cent,pt] == 1){
        clust_vector = append(clust_vector, cent)
      }
    }
  }
  clust_vector
}

fcd_kmeans = function(df_sample, centroids, details = FALSE) {
  n_pts = nrow(df_sample)
  n_cent = nrow(centroids)
  
  prev_assig_matrix = data.frame(matrix(0, ncol = n_pts, nrow = n_cent))
  
  step = 1
  dist_matrix = create_distance_matrix(df_sample, centroids)
  assig_matrix = create_assigment_matrix(dist_matrix)
  if(details){
    cat("PASO", step, "\n------- \n")
    cat("Matriz de distancias \n")
    print(dist_matrix)
    cat("\nMatriz de asignaciones \n")
    print(assig_matrix)
    cat("\n")
  }
  
  while (!identical(prev_assig_matrix, assig_matrix)) {
    prev_assig_matrix = assig_matrix
    centroids = update_cent(df_sample, centroids, assig_matrix)
    dist_matrix = create_distance_matrix(df_sample, centroids)
    assig_matrix = create_assigment_matrix(dist_matrix)
    
    if(details) {
      cat("Los nuevos centroides son:\n")
      print(centroids)
      step = step + 1
      cat("\nPASO", step, "\n------- \n")
      cat("Matriz de distancias \n")
      print(dist_matrix)
      cat("\nMatriz de asignaciones \n")
      print(assig_matrix)
      cat("\n")
    }
  }
  printCentroids(df_sample, centroids, assig_matrix)
  cat("Vector de clusterización\n")
  create_clust_vector(assig_matrix)
}


(data = read.xlsx("../Memoria/data/datosKMeans.xlsx"))
(centroides = read.xlsx("../Memoria/data/centroides.xlsx"))

fcd_kmeans(data, centroides, TRUE)