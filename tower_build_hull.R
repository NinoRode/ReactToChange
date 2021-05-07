library(data.tree)

pntz <- read.csv2("data/tek_onako.csv")
dimz <- ncol(pntz)
np <- nrow(pntz)

cnt_pntz <- scale(pntz)

