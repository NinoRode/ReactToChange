library(geometry)

hull_algo <- function(p_hull_pntz) {
  convhulln(p_hull_pntz, options = "Tv", output.options = NULL,
            return.non.triangulated.facets = FALSE)
}


vec_norm <- function(i) sqrt(sum(i^2)) # vector norm: as sqrt(drop(i %*% i)), but faster

decide <- function(one_level) {
  nrm <- sqrt(rowSums(one_level^2)) # sqrt(drop(i %*% i)) but faster
  delt <- which.max(nrm)
  one_level <- one_level[delt, ]
  one_level
}

# drop_row <- function(..., rows = NULL) {
#   input_list <- list(...)
#   input_list
#   output_list <- lapply(input_list, length)
#   output_list
#   
# }


drop_rows_one <- function(x, rows = NULL) {
  
  dim_num <- length(dim(x)) + 1
  if (dim_num == 1)
    max_row <- length(x)
  else
    max_row <- dim(x)[1]
    
  
  stopifnot("Too many dimensions: only 3-dim supported" = dim_num < 5)
  stopifnot("Rows do not exist" = max(rows) <= max_row, min(rows) > 0)
  
  switch(dim_num,
         x <- x[-rows],
         x <- x[-rows],
         x <- x[-rows, ], 
         x <- x[-rows, , ] 
  )
}

# primer1 <- matrix(1:9, nrow = 3, byrow = TRUE)
# primer2 <- 1:3
# primer3 <- letters[1:3]
# primer4 <- array(1:27, rep(3, 3))
# primer5 <- array(1:81, rep(3, 4))
# 
# print(drop_rows_one(primer1, 2))
# print(drop_rows_one(primer2, 2))
# print(drop_rows_one(primer3, c(2, 3)))
# print(drop_rows_one(primer4, c(2, 3)))
# print(drop_rows_one(primer5, c(2, 3)))


find_level_max <- function(pntz, mask = NULL) {
  
  if (is.vector(pntz)) {
    pntz <- t(as.matrix(pntz)) 
  }
  if (nrow(pntz) == 0) {
    return() 
  }
  ######################################################################################################### to test  
  dimz <- ncol(pntz)
  if(is.null(mask)) mask <- rep(1, dimz)
  
  cmpr <- 1 : dimz * mask
  cmpr <- cmpr[cmpr != 0]
  np <- nrow(pntz)
  nc <- length(cmpr)
  maxs <- apply(pntz, 2, max) # find max for each dimension
  has_max <- t(vapply(1:np, function(i) {(maxs[cmpr] %in% pntz[i, cmpr])}, numeric(nc)))
  max_pos <- which(has_max == 1, arr.ind = TRUE)
  mask <- has_max[max_pos[which(duplicated(max_pos[ , "row"])), "row"], ] #Pozor, kaj če jih je več?!! ###########################
  if (is.matrix(mask)) mask <- ifelse(colSums(mask) > 0, 1, 0)
  
  num_max <- rowSums(has_max)
  if(any(num_max > 1)) {
    tmp <- pntz[num_max > 1, ]
    # pntz[num_max > 1, ] <- NA
    # pntz <- pntz[-which(num_max > 1), ]
    
    drop <- which(num_max > 1)
    pntz <- drop_rows_one(pntz, drop)
    num_max <- drop_rows_one(num_max, drop)
    
    addit <- find_level_max(pntz, mask)
    if(is.matrix(pntz) && length(pntz) > 0) { 
      addit <- decide(addit)
      num_max[num_max ==2] <- 0
      tmp <- rbind(tmp, addit, pntz[num_max == 1, ] )
    } else if (is.vector(pntz)) {
      tmp <- rbind(tmp, pntz) 
    }
    
  } else {
    tmp <- pntz[num_max == 1, ]
  }
  
  # for (i in 1:ncol(tmp)) {
  #   print(decide(tmp[duplicated(tmp[ , i]) | duplicated(tmp[ , i], fromLast = TRUE), ])) 
  # }
  
  if(is.matrix(tmp) && nrow(tmp) > ncol(tmp)){
    dblz <- unlist(sapply(1: ncol(tmp), function (i) which(duplicated(tmp[ , i]) | duplicated(tmp[ , i], fromLast = TRUE))))
    tmp <- rbind(tmp[-dblz, ], decide(tmp[dblz, ]))
  }
  
  return(tmp)
}

level_max <- function(pntz) {
  
  if(!is.matrix(pntz)) {
    if(is.vector(pntz)) {
      pntz <- as.matrix(t(pntz))
    } else {
      pntz <- as.matrix(pntz)
    }
  }
 
  one_level <- find_level_max(pntz)
  
  dbl <- unlist(apply(one_level, 2, function(x) {which(x == x[which(duplicated(x))])})) 
  if(length(dbl) > 0){
    include <- one_level[dbl, ]  
    
    skln_nrm <- apply(include, 1, vec_norm)
    one_level <-one_level[-dbl[which.min(skln_nrm)], ]
  }
  # max_max <- vapply(1:dimz, function(i) {
  #   max(skln_nrm[ which(one_level[, i] == max(one_level[, i]))])
  # }, double(1))
  # one_level <- one_level[vapply(skln_nrm, function (i) {i %in% max_max }, logical(1)), ]
  one_level
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
    
    p_hull_pntz <- data.frame(matrix(ncol = dimz, nrow = 0))
    colnames(p_hull_pntz) <- paste0("x", 1:dimz)
    
    while (np >= dimz) {
      
      p_hull_pntz <- rbind(p_hull_pntz, one_level) # put in p_hull_pntz
      
      pntz_over <- pntz[is_it_same_side(pntz, one_level, other = TRUE), ]
      np <- nrow(pntz_over)
      if (is.null(np)) np <- 1
      
      if (np < dimz) {
        
        p_hull_pntz <- rbind(p_hull_pntz, pntz_over)
      
      } else {
        
        pntz <- pntz_over
        one_level <- find_level_max(pntz)
        
      }
    }
    
    return(p_hull_pntz)
}

change_position <- function(tmp_pntz, in_position) {
  rws <- nrow(tmp_pntz)
  ip <- matrix(rep(in_position, rws), nrow = rws, byrow = TRUE)
  tmp_pntz <- tmp_pntz * ip
  tmp_pntz
}

build_p_hull <- function(pntz) {
  
  cnter <- colMeans(pntz)
  tmp_pntz <- scale(pntz, center = cnter, scale = FALSE)
  plot(tmp_pntz)
  
  first_max  <- level_max(tmp_pntz) # find points with maximum on each dimension
  mc <- max.col(first_max)
  first_max <- first_max[mc, ]
  # p_hull_pntz <- first_max
  
  tmp <- build_tower(tmp_pntz, first_max)
  p_hull_pntz <- tmp 
  points(tmp,pch = 19)
  
  tmp_pntz <- -tmp_pntz
  first_min  <- level_max(tmp_pntz) # find points with minimum on each dimension
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
    points(tmp,pch = 19)
    
    one_level <- rbind(first_max[-x, ], first_min[x, ])
    in_position <- sign(colMeans(one_level))
    tmp_pntz <- change_position(tmp_pntz, in_position) 
    one_level <- change_position(one_level, in_position) 
    tmp <-  build_tower(tmp_pntz, one_level)
    tmp <- change_position(tmp, in_position) 
    tmp_pntz <- change_position(tmp_pntz, in_position) 
    p_hull_pntz <- rbind(p_hull_pntz, tmp)
    points(tmp,pch = 19)
  }
  
  p_hull_pntz <- unique(p_hull_pntz) + matrix(rep(cnter, nrow = nrow(p_hull_pntz), byrow = TRUE))
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


# test2 <- matrix(c(2, 7, 3, 9, 4, 3, 5, 8, 6, 4, 6, 7, 6, 7.5, 7, 5, 7, 7, 8, 6, 9, 2), ncol = 2, byrow = TRUE)
# 
# p <- read.csv2("data/tek_onako.csv")

p <- data.frame("x1" = rnorm(100000), "x2" = rnorm(100000))
p <- as.data.frame(matrix(rnorm(10000), ncol = 100))
pntz <- p[, 1:5]
# 
# positive <- function(x) all(x > 0)
# pntz <- pntz[apply(pntz, 1, positive), ]

# pntz <- read.csv2("data/test_data2s.csv")
# pntz <- scale(pntz[1:20, ], scale = FALSE)

tst <- build_hull(pntz)
sts <- hull_algo(pntz)
 
plot(pntz)
for (i in 1:nrow(sts)) {
  x <- rbind(pntz[sts[i, 1], ], pntz[sts[i, 2], ])
  points(x,pch = 19)
}
# plot(pntz)
# # p <-(tst[[1]])
# ch <- unique(as.vector(tst[[1]]))
# p <- tst[[2]]
# rownames(p) <- NULL
# # for (i in ch)) {
#   
# points(as.matrix(p[ch, ]), pch = 19)


