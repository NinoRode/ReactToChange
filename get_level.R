
drop_rows_one <- function(pntz, rows = NULL) {
  
  dim_num <- length(dim(pntz)) + 1
  if (dim_num == 1)
    max_row <- length(pntz)
  else
    max_row <- dim(pntz)[1]
  
  
  stopifnot("Too many dimensions: only 3-dim supported" = dim_num < 5)
  stopifnot("Rows do not exist" = max(rows) <= max_row, min(rows) > 0)
  
  switch(dim_num,
         x <- x[-rows],
         x <- x[-rows],
         x <- x[-rows, ], 
         x <- x[-rows, , ] 
  )
}

get_level <- function(pntz) {
 
  dimz <- ncol(pntz)
  np <- nrow(pntz)
  levl <- matrix(numeric(), nrow = 0, ncol = dimz)
  
  while (nrow(levl != dimz && np > 0)) {
    #.........................................................................:#
    # Najdi točke z največjo vednostjo za vsako dimenzijo
    #.........................................................................:#
    maxs <- apply(pntz, 2, max) # find max for each dimension
    has_max <- t(vapply(1:np, function(i) {(maxs[cmpr] %in% pntz[i, cmpr])}, numeric(nc)))
    max_pos <- which(has_max == 1, arr.ind = TRUE)
    mask <- has_max[max_pos[which(duplicated(max_pos[ , "row"])), "row"], ] # Pozor, če jih je več postane matrika #####
    if (is.matrix(mask)) mask <- ifelse(colSums(mask) > 0, 1, 0)
    
    num_max <- rowSums(has_max)
    
    #......................:#
    drop <- which(num_max > 1)
    levl <- rbind(pntz[drop, ])
    pntz <- drop_rows_one(pntz, drop)
    num_max <- drop_rows_one(num_max, drop)
    
    
    
    #......................:#
    
    if (is.vector(pntz)) pntz <- t(as.matrix(pntz))
    np <- nrow(pntz)
    
  }
}
