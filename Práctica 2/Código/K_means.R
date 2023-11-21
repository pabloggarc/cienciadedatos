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

create_assigment_matrix = function(df) {
  n_pts = ncol(df)
  n_cent = nrow(df)
  assigment = data.frame(matrix(0, ncol = n_pts, nrow = n_cent))
  for (col in 1:n_pts) {
    minimum_idx = 1
    for (cent in 2:n_cent) {
      if (df[cent, col] < df[minimum_idx, col]) {
        minimum_idx = cent
      }
    }
    assigment[minimum_idx, col] = 1
  }
  assigment            
}

update_cent = function(df_sample, mat_assig) {
  n_pts = ncol(mat_assig)
  n_cent = nrow(mat_assig)
  centroid_mean = c()
  for (cent in 1:n_cent) {
    centroid_x = 0
    centroid_y = 0
    count = 0
    for (pt in 1:n_pts) {
      if (mat_assig[cent, pt] == 1) {
        centroid_x = centroid_x + df_sample[pt, 1]
        centroid_y = centroid_y + df_sample[pt, 2]
        count = count + 1
      }
    }
    centroid_mean = append(append(centroid_mean, centroid_x/count), centroid_y/count)
  }
  data.frame(t(matrix(centroid_mean, 2, n_cent, dimnames=list(c("X","Y")))))
}

fcd_kmeans = function(df_sample, centroids) {
  n_pts = nrow(df_sample)
  n_cent = nrow(centroids)
  
  prev_iter = data.frame(matrix(0, ncol = n_pts, nrow = n_cent))
  
  mat = create_distance_matrix(df_sample, centroids)
  mat_assig = create_assigment_matrix(mat)

  while (!identical(prev_iter, mat_assig)) {
    prev_iter = mat_assig
    centroids = update_cent(df_sample, mat_assig)
    mat = create_distance_matrix(df_sample, centroids)
    mat_assig = create_assigment_matrix(mat)
  }
  centroids
}


df_sample = data.frame(t(matrix(c(4, 4, 3, 5, 1, 2, 5, 5, 0, 1, 2, 2, 4, 5, 2, 1), 2, 8, dimnames=list(c("velocidad","temperatura")))))
centroids = data.frame(t(matrix(c(0, 1, 2, 2), 2, 2, dimnames=list(c("X","Y")))))

df_sample = data.frame(t(matrix(c(3.5, 4.5, 0.75, 3.25, 0, 3, 1.75, 0.75, 3, 3.75, 3.75, 4.5, 1.25, 0.75, 0.25, 3, 3.5, 4.25, 1.5, 0.5, 1, 1, 3, 4, 0.5, 3, 2, 0.25, 0, 0.25), 2, 15, dimnames=list(c("velocidad","temperatura")))))
centroids = data.frame(t(matrix(c(1, 2, 2, 2, 3, 2), 2, 3, dimnames=list(c("X","Y")))))

fcd_kmeans(df_sample, centroids)
