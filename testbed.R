di <- 100
multitude <- 4
d <- c(12, 13, 10, 9, 4, 25, 8, 26, 51, 7, 11, 32)

point_to_bin <- function(pnt, di, multitude = 2){
  
  # Find a plane defined by the two dimensions with smallest value:
  # this is the plane which defines the bin the point belongs.
  are_3_min <- vector("double", 3)
  which_min <- vector("integer", 3)
  ad <- abs(pnt)
  are_3_min[1] <- min(ad)
  which_min[1] <- which(ad == are_3_min[1])
  are_3_min[2] <- min(ad[-which_min[1]])
  which_min[2] <- which(ad == are_3_min[2])
  are_3_min[3] <- min(ad[-c(which_min[1], which_min[2])])
  which_min[3] <- which(ad == are_3_min[3])
  
  # Construct a vector with bin positions of the point (at which_min[1 and 2]) 
  # and the sign of the projection (at which_min[3]) flagged by 2 * multitude.
  res_d <- rep(0, length(pnt))
  res_d[which_min[1]] <- ceiling((sign(pnt[which_min[1]]) * are_3_min[1] / are_3_min[3]) * multitude)
  res_d[which_min[2]] <- ceiling((sign(pnt[which_min[1]]) * are_3_min[2] / are_3_min[3]) * multitude)
  res_d[which_min[3]] <- sign(pnt[which_min[3]]) * multitude * 2
  
  res_d
}

bin_data <- function(df, multitude = 2) {
  
  dimz <- ncol(df)
  np <- nrow(df)
  
  # Move point cloud to origin
  centr <- colMeans(df)
  cnt_df <-df - centr 
  
  min_df <- mapply(min, cnt_df)
  di <- max(c(-min_df,  mapply(max, cnt_df)))
  
  if(np < multitude * multitude * dimz * 5) {
    sprintf("Warning: Number of points (%d) is too small for the proposed multitude (%d)", np, multitude)
    multitude <- 2^floor(log2(sqrt(np / dimz / 5)))
    sprintf("multitude changed to %d", multitude)
  }
  
  if(multitude < 1) {
    #------------------------  STOP IF TOO FIEW POINTS  ------------------------#
    stop("Number of points is too small.\n use simple quickhull")
  } else {
    posit <- lapply(1:dimz, point_to_bin(x, di, multitude))
    df <- cbind(df, posit)
  }
  df
}
