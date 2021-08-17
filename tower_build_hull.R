library(data.tree)
library(OjaNP)

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

not_dominated <- function(pntz, skln) {
  is_over <- function(p) {
    tmp <- apply(skln, 1, `<`, p)
    all(apply(tmp, 1, any))
  }
  return(apply(pntz, 1, is_over))
}

is_it_outside <- function(pntz, facet, eye = NULL ) {
  #' Finds points on the other side of the facet:
  #' more distant from the eye than the hyperplane of the facet.
  #' Can be used to find points outside the convex hull,
  #' or help determine visibility of the point.
  
  if (!is.null(eye)) {
    pntz <- pntz - eye
    facet <- facet - eye
  }
  
  f <- solve (as.matrix(facet), rep(1, ncol(facet)))
  
  # origin <- as.matrix(eye - facet[1, ]) %*% f
  
  pos <- vapply(1:nrow(pntz), function(x) {
    round(as.matrix(pntz[x, ] 
                    - facet[1, ]) %*% as.matrix(f), 12)}, double(1))
  
  out <- pntz[pos > 0, ]
  if (!is.null(eye)) {
    out <- out + eye
  }
  
  return(out)
}

find_level_max <- function(pntz, mask = NULL) {
  dimz <- ncol(pntz)
  if(is.null(mask)) mask <- rep(1, dimz)
  
  cmpr <- 1 : dimz * mask
  cmpr <- cmpr[cmpr != 0]

  maxs <- apply(pntz, 2, max) # find max for each dimension
  has_max <- t(vapply(cmpr, function(i) {(maxs[cmpr] %in% pntz[i, cmpr])}, numeric(length(cmpr))))
  max_pos <- which(has_max == 1, arr.ind = TRUE)
  mask <- has_max[max_pos[which(duplicated(max_pos[ , "row"])), "row"], ] #Pozor, kaj če jih je več?!!
  num_max <- rowSums(has_max)
  if(any(num_max > 1)) {
    tmp <- pntz[num_max > 1, ]
    pntz[num_max > 1, ] <- 0
    return(rbind(tmp, find_level_max(pntz, mask)))
  } else {
    return(pntz[num_max == 1, ])
  }
}

find_all_max <- function(pntz) {
    
  if(!is.matrix(pntz)) {
    if(is.vector(pntz)) {
      pntz <- as.matrix(t(pntz))
    } else {
      pntz <- as.matrix(pntz)
    }
  }
  
  dimz <- ncol(pntz)
  np <- nrow(pntz)
  tmp_pntz <- pntz
  mask <- rep(1, dimz)
  repeat {
    maxs <- apply(tmp_pntz, 2, max) # find max for each dimension
    has_max <- t(vapply(1:np, function(i) {sum(maxs[1:dimz] %in% tmp_pntz[i, ])}, numeric(1)))
    if (all(has_max < 2)) break
    else{
      tmp_pntz[has_max > 1, ] <- 0 # To moraš še rešiti
    }
  }
  
  skln <- pntz[which(has_max == 1), ]

  skln_nrm <- apply(skln, 1, vec_norm) # fast and dirty norms
  max_max <- vapply(1:dimz, function(i) {
    max(skln_nrm[which(skln[, i] == max(skln[, i]))])
  }, double(1))
  skln <- skln[vapply(skln_nrm, function (i) {i %in% max_max }, logical(1)), ]
}

find_sky_line <- function(pntz, to_origin = FALSE) {
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
  
  vec_norm <- function(i) sqrt(sum(i^2)) # drop(i %*% i) but faster
  
  if(to_origin) {
    colMin <- apply(pntz, 2, min)
    pntz <- sweep(pntz, 2, colMin)
  }
  
  skyline <- data.frame(matrix(ncol = dimz, nrow = 0))
  colnames(skyline) <- paste0("x", 1:dimz)
  
  repeat {
    
    # ##################### V FUNKCIJO: Find_all_max od tod ###################### 
    # maxs <- apply(pntz, 2, max) # find max for each dimension
    # skln <- pntz[which(vapply(1:np, function(i) {any(maxs[1:dimz] %in% pntz[i, ])}, logical(1))), ]
    # 
    # skln_nrm <- apply(skln, 1, vec_norm) # fast and dirty norms
    # max_max <- vapply(1:dimz, function(i) {
    #   max(skln_nrm[which(skln[, i] == max(skln[, i]))])
    # }, double(1))
    # skln <- skln[vapply(skln_nrm, function (i) {i %in% max_max }, logical(1)), ]
    # ##################### do tod V FUNKCIJO: Find_all_max ###################### 
    skln <- find_all_max(pntz)
    
    skyline <- rbind(skyline, skln) # put in skyline
    
    pntz_over <- pntz[is_it_same_side(pntz, skln, other = TRUE), ]
    np <- nrow(pntz_over)
    
    if (np < dimz) {
      skyline <- rbind(skyline, pntz_over)
      break
    }
    
    pntz <- pntz_over
  }
  
  return(skyline)
}

#' find_hull0 <- function(pntz, to_origin = TRUE) {
#'   #' Finds the upper convex hull of the data
#'   
#'   dimz <- ncol(pntz)
#'   np <- nrow(pntz)
#'   vec_norm <- function(i) sqrt(sum(i^2))
#'   
#'   if(is.vector(pntz)) {
#'     pntz <- as.matrix(t(pntz))
#'   } else {
#'     pntz <- as.matrix(pntz)
#'   }
#'   
#'   colMin <- apply(pntz, 2, min)
#'   
#'   if(to_origin) {
#'     pntz <- sweep(pntz, 2, colMin)
#'   }
#'   skyline <- data.frame(matrix(ncol = dimz, nrow = 0))
#'   colnames(skyline) <- paste0("x", 1:dimz)
#'   
#'   repeat {
#'     maxs <- apply(pntz, 2, max) # find max for each dimension
#'     skln <- pntz[which(vapply(1:np, function(i) {any(maxs[1:dimz] %in% pntz[i, ])}, logical(1))), ]
#'     
#'     skln_nrm <- apply(skln, 1, vec_norm) # fast and dirty norms
#'     max_max <- vapply(1:dimz, function(i) {
#'       max(skln_nrm[which(skln[, i] == max(skln[, i]))])
#'     }, double(1))
#'     skln <- skln[vapply(skln_nrm, function (i) {i %in% max_max }, logical(1)), ]
#'     
#'     skyline <- rbind(skyline, skln)
#'     pntz_over <- pntz[!is_it_same_side(pntz, skln), ]
#'     np <- nrow(pntz_over)
#'     if (is.null(np)) {
#'       skyline <- rbind(skyline, pntz_over)
#'       skyline <- sweep(skyline, 2, colMin, FUN = "+")
#'       
#'       return(skyline)
#'       
#'     }
#'     if (np <= dimz) {
#'       pnt_nrm <- apply(pntz_over, 1, vec_norm)
#'       top_pnt <- pntz_over[which(pnt_nrm == max(pnt_nrm)), ]
#'       skyline <- rbind(skyline, top_pnt)
#'       for (i in 1:dimz) {
#'         tmp <- !is_it_same_side(pntz_over, rbind(skln[-i, ], top_pnt))
#'         if (sum(tmp) > 0) skyline <- rbind(skyline, tmp)
#'       }
#'       skyline <- sweep(skyline, 2, colMin, FUN = "+")
#'       
#'       return(skyline)
#'       
#'     } else {
#'       pntz <- pntz_over
#'     }
#'   }
#'   
#'   skyline <- sweep(skyline, 2, colMin, FUN = "+")
#'   return(skyline)
#' }

find_hull <- function(pntz, to_origin = TRUE) {
  #' Finds the upper convex hull of the data
  
  dimz <- ncol(pntz)
  np <- nrow(pntz)
  vec_norm <- function(i) sqrt(sum(i^2))
  
  if(is.vector(pntz)) {
    pntz <- as.matrix(t(pntz))
  } else {
    pntz <- as.matrix(pntz)
  }
  
  colMin <- apply(pntz, 2, min)
  
  if(to_origin) {
    pntz <- sweep(pntz, 2, colMin)
  }
  skyline <- data.frame(matrix(ncol = dimz, nrow = 0))
  colnames(skyline) <- paste0("x", 1:dimz)
  
  repeat {
    maxs <- apply(pntz, 2, max) # find max for each dimension
    skln <- pntz[which(vapply(1:np, function(i) {any(maxs[1:dimz] %in% pntz[i, ])}, logical(1))), ]
    
    skln_nrm <- apply(skln, 1, vec_norm) # fast and dirty norms
    max_max <- vapply(1:dimz, function(i) {
      max(skln_nrm[which(skln[, i] == max(skln[, i]))])
    }, double(1))
    skln <- skln[vapply(skln_nrm, function (i) {i %in% max_max }, logical(1)), ]
    
    skyline <- rbind(skyline, skln)
    pntz_over <- pntz[!is_it_same_side(pntz, skln), ]
    np <- nrow(pntz_over)
    if (is.null(np)) {
      skyline <- rbind(skyline, pntz_over)
      skyline <- sweep(skyline, 2, colMin, FUN = "+")
      
      return(skyline)
      
    }
    if (np <= dimz) {
      pnt_nrm <- apply(pntz_over, 1, vec_norm)
      top_pnt <- pntz_over[which(pnt_nrm == max(pnt_nrm)), ]
      skyline <- rbind(skyline, top_pnt)
      for (i in 1:dimz) {
        tmp <- !is_it_same_side(pntz_over, rbind(skln[-i, ], top_pnt))
        if (sum(tmp) > 0) skyline <- rbind(skyline, tmp)
      }
      skyline <- sweep(skyline, 2, colMin, FUN = "+")
      
      return(skyline)
      
    } else {
      pntz <- pntz_over
    }
  }
  
  skyline <- sweep(skyline, 2, colMin, FUN = "+")
  return(skyline)
}

wrap_gift <- function(pntz) {
  
  
}

test2 <- matrix(c(2, 7, 3, 9, 4, 3, 5, 8, 6, 4, 6, 7, 6, 7.5, 7, 5, 7, 7, 8, 6, 9, 2), ncol = 2, byrow = TRUE)

p <- read.csv2("data/tek_onako.csv")
pntz <- unique(p[, 1:4])
dimz <- ncol(pntz)
np <- nrow(pntz)

find_sky_line(test2)
find_hull(test2)

pntz <- scale(pntz, scale = FALSE)

all_pos <- function(x) {all(x > 0)}

pntz <- pntz[apply(pntz, 1, all_pos), ]

find_sky_line((pntz))
find_hull(pntz)

cnt_pntz <- as.data.frame(scale(pntz))
pntz_nrm <- vapply(1:np, function(i) sqrt(sum(cnt_pntz[i, ]^2)), double(1)) # fast and dirty norms
# vapply is safer and faster than sapply
maxs <- vapply(cnt_pntz, max, double(1)) # find max for each dimension
mins <- vapply(cnt_pntz, min, double(1)) # find min for each dimension

#.........................................#
# find the points with max/min value
hll_max <- cnt_pntz[which(vapply(1:np, function(i) {any(maxs[1:dimz] %in% cnt_pntz[i, ])}, logical(1))), ]
nrm_max <- pntz_nrm[which(vapply(1:np, function(i) {any(maxs[1:dimz] %in% cnt_pntz[i, ])}, logical(1)))]
hll_min <- cnt_pntz[which(vapply(1:np, function(i) {any(mins[1:dimz] %in% cnt_pntz[i, ])}, logical(1))), ]
nrm_min <- pntz_nrm[which(vapply(1:np, function(i) {any(mins[1:dimz] %in% cnt_pntz[i, ])}, logical(1)))]

max_max <- vapply(1:dimz, function(i) {
  max(nrm_max[which(hll_max[, i] == max(hll_max[, i]))])
}, double(1))
hll_max <- hll_max[vapply(nrm_max, function (i) {i %in% max_max }, logical(1)), ]

max_min <- vapply(1:dimz, function(i) {
  max(nrm_min[which(hll_min[, i] == min(hll_min[, i]))])
}, double(1))
hll_min <- hll_min[vapply(nrm_min, function (i) {i %in% max_min }, logical(1)), ]
#.........................................#

# Initial hull 
init <- lapply(1:dimz, function (i)  rbind(hll_max[-i, ], hll_min[i, ]))

#' TODO uporabi to_positive za vsako faceto v init:
#' izberi njene točke in delaj dalje na njih.
#' 
#' izdelaj n-dimenzionalni gift wrap

# rbind(hll_max[-1, ], hll_min)
# rbind(hll_max[-1, ], hll_min[1, ])
# print(is_it_outside(rbind(hll_max[-1, ], hll_min), rbind(hll_max[-1, ], hll_min[1, ]), c(1, 1, 3, 0)))
# print(is_it_same_side(rbind(hll_max[-1, ], hll_min), rbind(hll_max[-1, ], hll_min[1, ])))

print(is_it_same_side(c(0.5,1), matrix(c(1, 0, 1, 1), nrow = 2, byrow = TRUE)))
print(is_it_same_side(c(1.5,1), matrix(c(1, 0, 1, 1), nrow = 2, byrow = TRUE)))
print(is_it_same_side(c(1,2), matrix(c(1, 0, 1, 1), nrow = 2, byrow = TRUE)))
print(is_it_same_side(c(2,2), matrix(c(1, 0, 1, 1), nrow = 2, byrow = TRUE)))


