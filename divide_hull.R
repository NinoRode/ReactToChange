
pnt7 <- read.csv2("/home/nino/Dokumenti/Programiranje/R/Projekti/tek_onako.csv")
multitude <- 8

min_pnt7 <- mapply(min, pnt7)
max_pnt7 <- mapply(max, pnt7)
cnt_pnt7 <-colMeans(pnt7)
full_d <- rbind(cnt_pnt7 - min_pnt7, max_pnt7 - cnt_pnt7)
pnt7 <- rbind(pnt7, cnt_pnt7)

di <- max(full_d)
kock <- t(matrix(c(cnt_pnt7 - di, cnt_pnt7 + di), ncol = 2))
prt <- di/multitude
kocka <- rbind(kock[1, ], 
               t(sapply(1:(2 * multitude - 1), function(x) kock[1, ] + x * prt)), 
               kock[2, ])
# Premakni v izhodišče
