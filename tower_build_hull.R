library(data.tree)

pntz <- read.csv2("data/tek_onako.csv")
dimz <- ncol(pntz)
np <- nrow(pntz)

cnt_pntz <- as.data.frame(scale(pntz))
cnt_pntz$nrm <- vapply(1:np, function(i) sqrt(sum(cnt_pntz[i, ]^2)), double(1)) #fast and dirty norms
maxs <- vapply(cnt_pntz, max, double(1)) #safer and faster than sapply
mins <- vapply(cnt_pntz, max, double(1))

faces <- rbind(cnt_pntz[which(sapply(1:nrow(cnt_pntz), function(i) any(maxs %in% cnt_pntz[i, ]))), ],
               cnt_pntz[which(sapply(1:nrow(cnt_pntz), function(i) any(mins %in% cnt_pntz[i, ]))), ])
