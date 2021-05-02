
pnt7 <- read.csv2("data/tek_onako.csv")
multitude <- 8

centr <- colMeans(pnt7) #compute centroid
cnt_pnt7 <-pnt7 - centr # Move to origin

min_pnt7 <- mapply(min, cnt_pnt7)
di <- max(c(- min_pnt7,  mapply(max, cnt_pnt7)))

prt <- di/multitude
cube <- t(sapply(0:(2 * multitude), function(x) min_pnt7 + x * prt))

