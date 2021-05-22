library(data.tree)
library(OjaNP)

is_it_same_side <- function(pntz, facet, eye = NULL ) {
  
  if (!is.null(eye)) {
    pntz <- pntz - eye
    facet <- facet - eye
  }
  
  pntz <- as.matrix(pntz)
  facet <- as.matrix(facet)
  
  f <- solve (facet, rep(1, ncol(facet)))
  
  pos <- vapply(1:nrow(pntz), function(x) 
    {round(t(pntz[x, ] - facet[1, ]) %*% f, 12)}, double(1))
  
  return(ifelse(pos <= 0, TRUE, FALSE ))
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
  
  pos <- vapply(1:nrow(pntz), function(x) {round(as.matrix(pntz[x, ] - facet[1, ]) %*% as.matrix(f), 12)}, double(1))

  out <- pntz[pos > 0, ]
  if (!is.null(eye)) {
    out <- out + eye
  }
  
  return(out)
}

p <- read.csv2("data/tek_onako.csv")
pntz <- p[, 1:4]
dimz <- ncol(pntz)
np <- nrow(pntz)

cnt_pntz <- as.data.frame(scale(pntz))
pntz_nrm <- vapply(1:np, function(i) sqrt(sum(cnt_pntz[i, ]^2)), double(1)) # fast and dirty norms
# vapply is safer and faster than sapply
maxs <- vapply(cnt_pntz, max, double(1)) # find max for each dimension
mins <- vapply(cnt_pntz, min, double(1)) # find min for each dimension

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

# is_it_outside(cnt_pntz, hll_max)
# for (i in 1:length(hll_max)) {
#   print(hll_max[i, ])
#   print("----max-----")
#   print(is_it_outside(rbind(hll_max[-i, ], hll_min), rbind(hll_max[-i, ], hll_min[i, ])))
#   print(hll_min[i, ])
#   print("----min-----")
#   print(is_it_outside(rbind(hll_max, hll_min[-i, ]), rbind(hll_max[i, ], hll_min[-i, ])))
#   print("++++++++++++")
# }

rbind(hll_max[-1, ], hll_min)
rbind(hll_max[-1, ], hll_min[1, ])
print(is_it_outside(rbind(hll_max[-1, ], hll_min), rbind(hll_max[-1, ], hll_min[1, ]), c(1, 1, 3, 0)))
print(is_it_same_side(rbind(hll_max[-1, ], hll_min), rbind(hll_max[-1, ], hll_min[1, ])))


