di <- 100
multitude <- 4
d <- c(12, 13, 10, 9, 4, 25, 8, 26, 51, 7, 11, 32)

#' Find a plane defined by the two dimensions with smallest value:
#' this is the plane which defines the bin the point belongs.

are_3_min <- vector("double", 3) # Načeloma je "double" pravi tip.
which_min <- vector("integer", 3)
ad <- abs(d)
are_3_min[1] <- min(ad)
which_min[1] <- which(ad == are_3_min[1])
are_3_min[2] <- min(ad[-which_min[1]])
which_min[2] <- which(ad == are_3_min[2])
are_3_min[3] <- min(ad[-c(which_min[1], which_min[2])])
which_min[3] <- which(ad == are_3_min[3])

# Construct a vector with bin positions of the point (at which_min[1 and 2]) 
# and the sign of the projection (at which_min[3]) flagged by 2 * multitude.
res_d <- rep(0, length(d))
res_d[which_min[1]] <- ceiling((sign(d[which_min[1]]) * are_3_min[1] / are_3_min[3]) * multitude)
res_d[which_min[2]] <- ceiling((sign(d[which_min[1]]) * are_3_min[2] / are_3_min[3]) * multitude)
res_d[which_min[3]] <- sign(d[which_min[3]]) * multitude * 2
