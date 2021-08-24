library(geometry)
library(matrixcalc)

hull_algo <- function(p_hull_pntz) {
  convhulln(p_hull_pntz, options = "Tv", output.options = NULL,
            return.non.triangulated.facets = FALSE)
}

drop_rows_one <- function(pntz, rows = NULL) {
  
  dim_num <- length(dim(pntz)) + 1
  if (dim_num == 1)
    max_row <- length(pntz)
  else
    max_row <- dim(pntz)[1]
  
  stopifnot("Too many dimensions: only 3-dim supported" = dim_num < 5)
  stopifnot("Rows do not exist" = max(rows) <= max_row, min(rows) > 0)
  
  switch(dim_num,
         pntz <- pntz[-rows],
         pntz <- pntz[-rows],
         pntz <- pntz[-rows, ], 
         pntz <- pntz[-rows, , ] 
  )
}

decide <- function(levl) {
  nrm <- sqrt(rowSums(levl^2)) # sqrt(drop(i %*% i)) but faster
  delt <- which.max(nrm)
  levl <- levl[delt, ]
}

change_position <- function(tmp_pntz, in_position) {
  rws <- nrow(tmp_pntz)
  ip <- matrix(rep(in_position, rws), nrow = rws, byrow = TRUE)
  tmp_pntz <- tmp_pntz * ip
  tmp_pntz
}

get_level <- function(pntz) {
 
  dimz <- ncol(pntz)
  np <- nrow(pntz)
  levl <- matrix(numeric(), nrow = 0, ncol = dimz)
  mask <- NULL
  
  while (nrow(levl) < dimz && np > 0) {
    
    print(nrow(levl)) ########################################################## debug
    #..........................................................................#
    # Najdi točke z največjo vednostjo ("zgornje") za vsako dimenzijo
    #..........................................................................#
    if(is.null(mask) || sum(mask) == 0) mask <- rep(1, dimz)
    
    cmpr <- 1 : dimz * mask
    
    cmpr <- cmpr[cmpr != 0]
    nc <- length(cmpr)
    
    maxs <- apply(pntz, 2, max) # find max for each dimension
    has_max <- t(vapply(1:np, function(i) {(maxs[cmpr] %in% pntz[i, cmpr])}, numeric(nc)))
    max_pos <- which(has_max == 1, arr.ind = TRUE)
    
    mask <- has_max[max_pos[which(duplicated(max_pos[ , "row"])), "row"], ] # Pozor, če jih je več postane matrika #####
    if (is.matrix(mask)) mask <- ifelse(colSums(mask) > 0, 1, 0)
    
    num_max <- rowSums(has_max)
    
    #..........................................................................#
    # Pripravi večkratne zgornje in jih očisti iz podatkov 
    #..........................................................................#
    if (any(num_max > 1)){
      drop <- which(num_max > 1)
      for_levl <- (pntz[drop, ])
      pntz <- drop_rows_one(pntz, drop)
      # num_max <- drop_rows_one(num_max, drop)
    } else {
      for_levl <- pntz[which(num_max == 1), ]
    }
    
    if (!is.matrix(for_levl)) for_levl <- t(as.matrix(for_levl))
    #..........................................................................#
    # Počisti dvojnike
    #..........................................................................#
    # dbl <- unlist(apply(for_levl, 2, function(x) {which(x == x[which(duplicated(x))])})) 
    # if(length(dbl) > 0){
    #   for_levl <- decide(for_levl[dbl, ])
    # }
    
    if (nrow(levl) + nrow(for_levl) > dimz && nrow(for_levl) > 1) {
      for_levl <- decide(for_levl)
    }
    levl <- rbind(levl, for_levl)
    levl <- levl[!duplicated(levl), ]
    if (!is.matrix(levl)) levl <- t(as.matrix(levl))
    if (nrow(levl) > dimz) {
      maxs <- apply(levl, 2, max) # find max for each dimension
      levl <- levl[vapply(1:nrow(levl), function(i) {any(maxs %in% pntz[i, ])}, numeric(1)), ]
 
    }
    
    dbl <- unlist(apply(levl, 2, function(x) {which(x == x[which(duplicated(x))])})) 
    if(length(dbl) > 0){
      levl <- decide(levl[dbl, ])
    }
    
    #..........................................................................#
    # Popravi vektor v matriko
    #..........................................................................#
    if (!is.matrix(pntz)) pntz <- t(as.matrix(pntz))
    
    #..........................................................................#
    np <- nrow(pntz)
    
  }
  
  
  levl # Procedura, ki kliče, mora preveriti, če je levl kompleten
}

find_points_over <- function(pntz, one_level) {
  collin_test <- qr(one_level)
  while (collin_test$rank < ncol(one_level)) {
  
  }
  
  pntz[is_it_same_side(pntz, one_level, other = TRUE), ]
}

is_it_same_side <- function(pntz, facet, eye = NULL, other = FALSE) {
  #' Determines if the points are on the same/other side of 
  #' the (hyperplane of the) facet 
  #' as the eye (or on the hyperplane).
  #' @param pntz: points to be tested
  #' @param eye: observers eye, origin of the view
  #' @param facet: data frame or matrix of points determining a hyperplane of the facet
  #' @param other: boolean determines if the same or other side is true
  
  if (!is.null(eye)) {
    pntz <- pntz - eye
    facet <- facet - eye
  }
  
  if (!is.matrix(pntz)) {
    if(is.vector(pntz)) {
      pntz <- as.matrix(t(pntz))
    } else {
      pntz <- as.matrix(pntz)}
  }
  
  if (!is.matrix(facet)) {
    facet <- as.matrix(facet)
  }
  
  f <- solve (facet, rep(1, ncol(facet)))
  
  pos <- vapply(1:nrow(pntz), function(x) 
  {round(t(pntz[x, ] - facet[1, ]) %*% f, 12)}, double(1))
  
  if(other) {
    return(ifelse(pos <= 0, FALSE, TRUE ))
  } else {
    return(ifelse(pos <= 0, TRUE, FALSE ))
  }
}

build_tower <- function(pntz, one_level) {
  
  # find_sky_line <- function(pntz, to_origin = FALSE) {
  #' Finds the sky line of the data
  
  if(!is.matrix(pntz)) {
    if(is.vector(pntz)) {
      pntz <- as.matrix(t(pntz))
    } else {
      pntz <- as.matrix(pntz)
    }
  }
  
  dimz <- ncol(pntz)
  np <- nrow(pntz)
  
  # if(to_origin) {
  #   colMin <- apply(pntz, 2, min)
  #   pntz <- sweep(pntz, 2, colMin)
  # }
  
  p_hull_pntz <- one_level
  colnames(p_hull_pntz) <- paste0("x", 1:dimz)
  
  while (np >= dimz) {
    
    pntz_over <- find_points_over(pntz, one_level)
    
    np <- nrow(pntz_over)
    if (is.null(np)) np <- 1
    
    if (np < dimz) {
      
      p_hull_pntz <- rbind(p_hull_pntz, pntz_over)
      
    } else {
      
      pntz <- pntz_over
      one_level <- get_level(pntz)
      p_hull_pntz <- rbind(p_hull_pntz, one_level) # put in p_hull_pntz
    }
  }
  
  return(p_hull_pntz)
}

build_p_hull <- function(pntz) {
  
  cnter <- colMeans(pntz)
  tmp_pntz <- scale(pntz, center = cnter, scale = FALSE)
  plot(tmp_pntz)
  
  first_max  <- get_level(tmp_pntz) # find points with maximum on each dimension
  mc <- max.col(first_max)
  first_max <- first_max[mc, ]
  # p_hull_pntz <- first_max
  
  tmp <- build_tower(tmp_pntz, first_max)
  p_hull_pntz <- tmp 
  points(tmp,pch = 19)
  
  tmp_pntz <- -tmp_pntz
  first_min  <- get_level(tmp_pntz) # find points with minimum on each dimension
  first_min <- first_min[max.col(first_min), ]
  # p_hull_pntz <- rbind(p_hull_pntz, -first_min)
  
  tmp <- build_tower(tmp_pntz, first_min)
  tmp <- -tmp
  p_hull_pntz <- rbind(p_hull_pntz, tmp) 
  points(tmp,pch = 19)
  
  tmp_pntz <- -tmp_pntz
  first_min <- -first_min
  
  #vse kombinacije
  for (x in 1:ncol(tmp_pntz)) {
    one_level <- rbind(first_max[x, ], first_min[-x, ])
    in_position <- sign(colMeans(one_level))
    tmp_pntz <- change_position(tmp_pntz, in_position) 
    one_level <- change_position(one_level, in_position) 
    tmp <-  build_tower(tmp_pntz, one_level)
    tmp <- change_position(tmp, in_position)  
    tmp_pntz <- change_position(tmp_pntz, in_position) 
    p_hull_pntz <- rbind(p_hull_pntz, tmp)
    points(tmp,pch = 19, col = x)
    
    one_level <- rbind(first_max[-x, ], first_min[x, ])
    in_position <- sign(colMeans(one_level))
    tmp_pntz <- change_position(tmp_pntz, in_position) 
    one_level <- change_position(one_level, in_position) 
    tmp <-  build_tower(tmp_pntz, one_level)
    tmp <- change_position(tmp, in_position) 
    tmp_pntz <- change_position(tmp_pntz, in_position) 
    p_hull_pntz <- rbind(p_hull_pntz, tmp)
    points(tmp,pch = 19, col = x)
  }
  
  p_hull_pntz <- unique(p_hull_pntz)
  p_hull_pntz <- p_hull_pntz + matrix(rep(cnter, nrow(p_hull_pntz)), nrow = nrow(p_hull_pntz), byrow = TRUE)
}

build_hull <- function(build_p_hull) {
  
  p_hull_pntz <- build_p_hull(pntz)
  tst <- hull_algo(p_hull_pntz)
  # x <- matrix(numeric(), nrow = 0, ncol = ncol(p_hull_pntz))
  # cntr <- 1:nrow(tst)
  # for (i in cntr) {
  #   x <- rbind(x, p_hull_pntz[tst[i, 1], ], p_hull_pntz[tst[i, 2], ])
  # }
  list(tst, p_hull_pntz)
}

# p <- as.data.frame(matrix(rnorm(10000), ncol = 100))
# pntz <- p[, 1:4]
pntz <- read.csv2("/home/nino/git/div_hull/data/test_data_prblm.csv", row.names = 1)

p_hull_pntz <- build_p_hull(pntz)
p_hull_pntz
sts <- (hull_algo(p_hull_pntz))
sts
sort(unique(as.vector(sts)))

tst <- (hull_algo(pntz))
tst

sort(unique(as.vector(tst)))
sort(unique(as.vector(sts)))
