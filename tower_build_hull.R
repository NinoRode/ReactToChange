library(data.tree)

pntz <- read.csv2("data/tek_onako.csv")
dimz <- ncol(pntz)
np <- nrow(pntz)

cnt_pntz <- as.data.frame(scale(pntz))
maxs <- sapply(cnt_pntz, max)
mins <- sapply(cnt_pntz, min)

faces <- rbind(cnt_pntz[which(sapply(1:nrow(cnt_pntz), function(i) any(maxs %in% cnt_pntz[i, ]))), ],
               cnt_pntz[which(sapply(1:nrow(cnt_pntz), function(i) any(mins %in% cnt_pntz[i, ]))), ])
