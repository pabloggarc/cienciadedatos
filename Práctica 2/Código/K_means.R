len = function(list) {
  count = 0
  for (element in list) {
    count = count + 1
  }
  count
}

euc_distance = function(p1, p2) {
  if(len(p1) == len(p2)) {
    add = 0
    for(i in 1:len(p1)) {
      add = add + ((p1[i] - p2[i])^2)
    }
    sqrt(add)
  } else {
    print("No se puede calcular la distancia euclídea")
  }
}

create_distance_matrix = function(pts, cent) {
  n_pts = nrow(pts)
  n_cent = nrow(cent)
  distances = data.frame(matrix(ncol = n_pts, nrow = n_cent))
  for (i in 1:n_cent) {
    for (j in 1:n_pts) {
      distances[i, j] = euc_distance(pts[j,], cent[i,])
    }
  }
  distances            
}

create_assigment_matrix = function(df) {
  n_pts = ncol(df)
  n_cent = nrow(df)
  assigment = data.frame(0, matrix(ncol = n_pts, nrow = n_cent))
  for (col in 1:n_pts) {
    minimum_idx = 1
    for (cent in 2:n_cent) {
      if (df[cent, col] < df[minimum_idx, col]) {
        minimum_idx = cent
      }
    }
    assigment[minimum_idx, j] = 1
  }
  assigment            
}

fcd_kmeans = function(df_sample, centroids) {
  (mat = create_distance_matrix(df_sample, centroids))
}


df_sample = data.frame(t(matrix(c(3.5, 4.5, 0.75, 3.25, 0, 3, 1.75, 0.75, 3, 3.75, 3.75, 4.5, 1.25, 0.75, 0.25, 3, 3.5, 4.25, 1.5, 0.5, 1, 1, 3, 4, 0.5, 3, 2, 0.25, 0, 0.25), 2, 15, dimnames=list(c("velocidad","temperatura")))))
centroids = data.frame(t(matrix(c(1, 2, 2, 2, 3, 2), 2, 3, dimnames=list(c("X","Y")))))


fcd_kmeans(df_sample, centroids)
