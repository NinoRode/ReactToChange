library(microbenchmark)

# di <- 100
# multitude <- 4
# d <- c(12, 13, 10, 9, 4, 25, 8, 26, 51, 7, 11, 32)

pntz <- read.csv2("data/tek_onako.csv")


point_to_bin <- function(pnt, multitude = 2){
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
  res_d <- pnt - pnt
  # res_d <- rep(0, ncol(pnt))
  names(res_d) <- paste0("p", names(pnt))
  res_d[which_min[1]] <- sign(pnt[which_min[1]]) * ceiling(are_3_min[1] / are_3_min[3] * multitude)
  res_d[which_min[2]] <- sign(pnt[which_min[1]]) * ceiling(are_3_min[2] / are_3_min[3] * multitude)
  res_d[which_min[3]] <- sign(pnt[which_min[3]]) * Inf
  
  res_d
}

bin_data <- function(pntz, multitude = 2) {
  
  dimz <- ncol(pntz)
  np <- nrow(pntz)
  
  # Move point cloud to origin
  centr <- colMeans(pntz)
  cnt_pntz <-pntz - centr 
  
  min_pntz <- mapply(min, cnt_pntz)
  di <- max(c(-min_pntz,  mapply(max, cnt_pntz)))
  
  if(np < multitude * multitude * dimz * 5) {
    sprintf("Warning: Number of points (%d) is too small for the proposed multitude (%d)", np, multitude)
    multitude <- 2^floor(log2(sqrt(np / dimz / 5)))
    sprintf("multitude changed to %d", multitude)
  }
  
  if(multitude < 1) {
    #------------------------  STOP IF TOO FIEW POINTS  ------------------------#
    stop("Number of points is too small.\n use simple quickhull")
  } else {
    # Compute positions of points in bins
    p <- (lapply(1:np, function(x) point_to_bin(unlist(cnt_pntz[x, ]), multitude))) 
    posit <- as.data.frame(do.call(rbind, p))       # Convert list to data frame rows
    pntz <- cbind(pntz, posit)
  }
  pntz
}

cub <- bin_data(pntz, multitude = 4)

dimz <- ncol(pntz)
cube <- split(cub, cub[, (dimz + 1):(2 * dimz)], drop = TRUE)

x <- unlist(pntz[1, ])
microbenchmark(
  sqrt(sum(x*x)),
  sqrt(sum(x^2)),
  norm(x, "2"),
  max(abs(x))*(sum((abs(x)/max(abs(x)))^2))^(1/2),
  times = 5000)

microbenchmark(
  unlist(apply(cnt_pntz, 2, max)),
  sapply(cnt_pntz, max),
  vapply(cnt_pntz, max, double(1)),
  times = 50000)


