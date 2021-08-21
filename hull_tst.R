library(geometry)

hull_algo <- function(p_hull_pntz) {
  convhulln(p_hull_pntz, options = "Tv", output.options = NULL,
            return.non.triangulated.facets = FALSE)
}


vec_norm <- function(i) sqrt(sum(i^2)) # vector norm: as sqrt(drop(i %*% i)), but faster

decide <- function(one_level) {
  nrm <- sqrt(sum(one_level^2)) # sqrt(drop(i %*% i)) but faster
  delt <- which(nrm == min(nrm))
  one_level <- one_level[-delt, ]
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


find_level_max <- function(pntz, mask = NULL, cntr = NULL) {
  dimz <- ncol(pntz)
  if(is.null(mask)) mask <- rep(1, dimz)
  
  cmpr <- 1 : dimz * mask
  cmpr <- cmpr[cmpr != 0]
  np <- nrow(pntz)
  nc <- length(cmpr)
  if(is.null(cntr)) cntr <- 0 ############################################################################## debug
  maxs <- apply(pntz, 2, max) # find max for each dimension
  has_max <- t(vapply(1:np, function(i) {(maxs[cmpr] %in% pntz[i, cmpr])}, numeric(nc)))
  max_pos <- which(has_max == 1, arr.ind = TRUE)
  mask <- has_max[max_pos[which(duplicated(max_pos[ , "row"])), "row"], ] #Pozor, kaj če jih je več?!!
  num_max <- rowSums(has_max)
  if(any(num_max > 1)) {
    cntr <- cntr + 1 ####################################################################################### debug
    print(cntr) ############################################################################################ debug
    tmp <- pntz[num_max > 1, ]
    # pntz[num_max > 1, ] <- NA
    # pntz <- pntz[-which(num_max > 1), ]
    
    drop <- which(num_max > 1)
    pntz <- drop_rows_one(pntz, drop)
    num_max <- drop_rows_one(num_max, drop)
    
    addit <- find_level_max(pntz, mask, cntr)
    addit <- decide(addit)
    num_max[num_max ==2] <- 0  ############################################################################# works
    tmp <- rbind(tmp, addit, pntz[num_max == 1, ] )

  } else {
    tmp <- pntz[num_max == 1, ]
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
  one_level
  # max_max <- vapply(1:dimz, function(i) {
  #   max(skln_nrm[ which(one_level[, i] == max(one_level[, i]))])
  # }, double(1))
  # one_level <- one_level[vapply(skln_nrm, function (i) {i %in% max_max }, logical(1)), ]
  
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
      
      if (np < dimz) {
        
        p_hull_pntz <- rbind(p_hull_pntz, pntz_over)
      
      } else {
        
        pntz <- pntz_over
        one_level <- find_level_max(pntz) ######################################### debug
        
      }
    }
    
    return(p_hull_pntz)
}

build_p_hull <- function(pntz) {
  
  cnter <- colMeans(pntz)
  pntz <- scale(pntz, center = cnter, scale = FALSE)
  
  first_max  <- level_max(pntz) # find points with maximum on each dimension
  p_hull_pntz <- first_max
  p_hull_pntz <- rbind(p_hull_pntz, build_tower(pntz, first_max)) 
  
  first_min  <- -level_max(-pntz) # find points with minimum on each dimension
  p_hull_pntz <- rbind(p_hull_pntz, first_min)
  p_hull_pntz <- rbind(p_hull_pntz, build_tower(pntz, first_min)) 
  
  #vse kombinacije
  for (x in 1:ncol(pntz)) {
    one_level <- rbind(first_max[x, ], first_min[-x, ])
    tst0 <-  build_tower(pntz, one_level)
    p_hull_pntz <- rbind(p_hull_pntz, tst0)
    # p_hull_pntz <- rbind(p_hull_pntz, build_tower(pntz, one_level))
    
    one_level <- rbind(first_max[-x, ], first_min[x, ])
    p_hull_pntz <- rbind(p_hull_pntz, build_tower(pntz, one_level))
  }
  
  p_hull_pntz <- p_hull_pntz + matrix(rep(cnter, nrow = nrow(p_hull_pntz), byrow = TRUE))
}

build_hull <- function(build_p_hull) {
  
  p_hull_pntz <- build_p_hull(pntz)
  plot(pntz)
  lines(p_hull_pntz)
  
  hull_algo(p_hull_pntz)
}


# test2 <- matrix(c(2, 7, 3, 9, 4, 3, 5, 8, 6, 4, 6, 7, 6, 7.5, 7, 5, 7, 7, 8, 6, 9, 2), ncol = 2, byrow = TRUE)
# 
# p <- read.csv2("data/tek_onako.csv")
# pntz <- scale(unique(p[, 1:4]), scale = FALSE)
# 
# positive <- function(x) all(x > 0)
# pntz <- pntz[apply(pntz, 1, positive), ]

pntz <- read.csv2("data/test_data2.csv")

# tst <- level_max(pntz)
# tst

tst <- build_hull(pntz)
sts <- hull_algo(pntz)

plot(pntz)
for (i in 1:nrow(sts)) {
  x <- rbind(pntz[sts[i, 1], ], pntz[sts[i, 2], ])
  lines(z)
}

plot(pntz)
for (i in 1:nrow(tst)) {
  x <- rbind(pntz[tst[i, 1], ], pntz[tst[i, 2], ])
  lines(z)
}

