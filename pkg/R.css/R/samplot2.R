`samplot2` <-
function () {
 qqstat<- SAMRes 
 observed <- switch(statSAMplot, E2 = qqstat[[1]][, 1], Williams = qqstat[[3]][, 
        1], Marcus = qqstat[[5]][, 1], M = qqstat[[7]][, 1], 
        ModifM = qqstat[[9]][, 1])
 expected <- switch(statSAMplot, E2 = qqstat[[1]][, 2], Williams = qqstat[[3]][, 
        2], Marcus = qqstat[[5]][, 2], M = qqstat[[7]][, 2], 
        ModifM = qqstat[[9]][, 2])
   numsig <- Delta.table[pos, 4]
   FDR <- Delta.table[pos, 5]
   delta <- Delta.table[pos, 1]
   params <- par(bg="white")
   plot(expected, observed)
    abline(0, 1)
    abline(delta, 1, lty = 2, col = "blue",lwd=2)
    abline(-delta, 1, lty = 2, col = "blue",lwd=2)
    oe <- order(expected)
    q.mat <- switch(statSAMplot, E2 = qqstat[[1]][oe, ], Williams = qqstat[[3]][oe, 
        ], Marcus = qqstat[[5]][oe, ], M = qqstat[[7]][oe, ], 
        ModifM = qqstat[[9]][oe, ])
    if (sum(q.mat[, 3] >= delta) > 0) {
        x.exp <- min(q.mat[q.mat[, 3] >= delta, 2])
        points(q.mat[q.mat[, 2] >= x.exp, 2], q.mat[q.mat[, 2] >= 
            x.exp, 1], col = "red")
    }
    if (sum(q.mat[, 3] <= -delta) > 0) {
        x.exp <- max(q.mat[q.mat[, 3] <= -delta, 2])
        points(q.mat[q.mat[, 2] <= x.exp, 2], q.mat[q.mat[, 2] <= 
            x.exp, 1], col = "red")
    }

    legend(min(expected), max(observed), c(paste("Delta =", delta, 
        sep = "") , paste("FDR = ", FDR , sep = ""), 
  paste ("Significant =",numsig,sep = "")) , lty = 0)

    title(paste("observed vs. expected statistics for: ", statSAMplot))

}

