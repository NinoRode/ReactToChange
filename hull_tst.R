decide <- function(skln) {
  nrm <- sqrt(sum(skln^2)) # sqrt(drop(i %*% i)) but faster
  delt <- which(nrm == min(nrm))
  skln <- skln[-delt, ]
  skln
}

find_level_max <- function(pntz, mask = NULL) {
  dimz <- ncol(pntz)
  if(is.null(mask)) mask <- rep(1, dimz)
  
  cmpr <- 1 : dimz * mask
  cmpr <- cmpr[cmpr != 0]
  np <- nrow(pntz)
  nc <- length(cmpr)
  
  maxs <- apply(pntz, 2, max) # find max for each dimension
  has_max <- t(vapply(1:np, function(i) {(maxs[cmpr] %in% pntz[i, cmpr])}, numeric(nc)))
  max_pos <- which(has_max == 1, arr.ind = TRUE)
  mask <- has_max[max_pos[which(duplicated(max_pos[ , "row"])), "row"], ] #Pozor, kaj če jih je več?!!
  num_max <- rowSums(has_max)
  if(any(num_max > 1)) {
    tmp <- pntz[num_max > 0, ]
    pntz[num_max > 1, ] <- 0
    addit <-  decide(find_level_max(pntz, mask))
    tmp <- rbind(tmp, addit)
    return(tmp)
  } else {
    return(pntz[num_max == 1, ])
  }
}

test2 <- matrix(c(2, 7, 3, 9, 4, 3, 5, 8, 6, 4, 6, 7, 6, 7.5, 7, 5, 7, 7, 8, 6, 9, 2), ncol = 2, byrow = TRUE)

p <- read.csv2("data/tek_onako.csv")
pntz <- scale(unique(p[, 1:4]), scale = FALSE)

positive <- function(x) all(x > 0)
pntz <- pntz[apply(pntz, 1, positive), ]
tst <- find_level_max(pntz)
