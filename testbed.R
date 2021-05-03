d <- c(12, 13, 10, 9, 4, 25, 8, 26, 51, 7, 11, 32)

two_min <- vector(typeof(d), 2) # Pozor: ta način potrebuje vektor kot input.
which_min <- vector("integer", 2)
ad <- abs(d)
two_min[1] <- min(ad)
which_min[1] <- which(ad == two_min[1])
two_min[2] <- min(ad[-which_min[1]])
which_min[2] <- which(ad == two_min[2])

