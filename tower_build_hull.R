library(data.tree)
library(OjaNP)

is_outside <- function(pntz, facet) {
  #' Finds points outside the convex hull:
  #' more distant from the origin than the hyperplane of the facet.
  
  f <- solve (as.matrix(facet), rep(1, ncol(facet)))
  origin <- as.matrix(rep(0, length(f)) - facet[1, ]) %*% f
  
  pos <- vapply(1:nrow(pntz), function(x) {round(as.matrix(pntz[x, ] - facet[1, ]) %*% as.matrix(f), 12)}, double(1))
  out <- pntz[pos > 0, ]
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

# is_outside(cnt_pntz, hll_max)
for (i in 1:length(hll_max)) {
  print(hll_max[i, ])
  print("----max-----")
  print(is_outside(rbind(hll_max[-i, ], hll_min), rbind(hll_max[-i, ], hll_min[i, ])))
  print(hll_min[i, ])
  print("----min-----")
  print(is_outside(rbind(hll_max, hll_min[-i, ]), rbind(hll_max[i, ], hll_min[-i, ])))
  print("++++++++++++")
}
