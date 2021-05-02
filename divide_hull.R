
multitude <- 8

pntz <- read.csv2("data/tek_onako.csv")
dimz <- ncol(pntz)
np <- nrow(pntz)

centr <- colMeans(pntz) #compute centroid
cnt_pntz <-pntz - centr # Move to origin

min_pntz <- mapply(min, cnt_pntz)
di <- max(c(- min_pntz,  mapply(max, cnt_pntz)))

if(np < multitude * multitude * dimz * 5) {
  sprintf("Warning: Number of points (%d) is too small for the proposed multitude (%d)", np, multitude)
  multitude <- 2^floor(log2(sqrt(np / dimz / 5)))
  sprintf("multitude changed to %d", multitude)
}

if(multitude < 2) {
  stop("Number of points is too small")
} else {
  prt <- di/multitude
  cube <- t(sapply(0:(2 * multitude), function(x) min_pntz + x * prt))
}
