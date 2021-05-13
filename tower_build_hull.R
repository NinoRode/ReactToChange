library(data.tree)

p <- read.csv2("data/tek_onako.csv")
pntz <- p[, 1:4]
dimz <- ncol(pntz)
np <- nrow(pntz)

cnt_pntz <- as.data.frame(scale(pntz))
cnt_pntz$nrm <- vapply(1:np, function(i) sqrt(sum(cnt_pntz[i, ]^2)), double(1)) # fast and dirty norms
# cnt_pntz$nrm <- vapply(1:np, function(i) norm(cnt_pntz[i, ]), double(1)) # official norms
# vapply is safer and faster than sapply
maxs <- vapply(cnt_pntz, max, double(1)) # find max for each dimension
mins <- vapply(cnt_pntz, min, double(1)) # find min for each dimension

# find the points with max/min value
hll_max <- cnt_pntz[which(vapply(1:np, function(i) {any(maxs[1:dimz] %in% cnt_pntz[i, ])}, logical(1))), ]
hll_min <- cnt_pntz[which(vapply(1:np, function(i) {any(mins[1:dimz] %in% cnt_pntz[i, ])}, logical(1))), ]

max_max <- vapply(1:dimz, function(i) {max(hll_max[which(hll_max[, i] == maxs[i]), dimz+1])}, 
                  double(1))
hll_max <- hll_max[vapply(hll_max[, dimz+1], function (i) {i %in% max_max }, logical(1)), ]

max_min <- vapply(1:dimz, function(i) {max(hll_min[which(hll_min[, i] == mins[i]), dimz+1])}, 
                  double(1))
hll_min <- hll_min[vapply(hll_min[, dimz+1], function (i) {i %in% max_min }, logical(1)), ]
