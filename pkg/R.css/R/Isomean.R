`Isomean` <-
function (x,y)
{
        ordx <- order(x)
        x <- x[ordx]
        y <- y[, ordx]
        unx <- unique(x)
        ydf <- as.data.frame(t(y))
        y.m <- do.call("cbind", unclass(by(ydf, x, mean)))
        y.m.tot <- matrix(rep(rowMeans(y), length(x)), ncol = length(x))
        n.p <- table(x)
        n.g <- length(n.p)
        y.is.u <- t(apply(y.m, 1, function(x) pava(x, w = n.p)))
      y.is.d <- t(apply(y.m, 1, function(x) pava(x, w = n.p, decreasing = TRUE)))
        rep.iso.d <- y.is.d[, rep(1:length(n.p), n.p)]
        rep.iso.u <- y.is.u[, rep(1:length(n.p), n.p)]
        y.m.all <- y.m[, rep(1:length(n.p), n.p)]
        SST0 <- rowSums((y - rowMeans(y))^2)
        SSIS.u1 <- rowSums((rep.iso.u - y)^2)
        SSIS.d1 <- rowSums((rep.iso.d - y)^2)
        SST <- rowSums((y - y.m.all)^2)
        direction = NULL
        iso.dir <- ifelse(SSIS.u1 <= SSIS.d1, "u", "d")
        iso.mean <- matrix(0,nrow(y),length(unx))
        for (i in 1: nrow(y)) {
         if (iso.dir[i]=="u"){
           iso.mean[i,] <- y.is.u[i,]
           } 
    else {
          iso.mean[i,] <- y.is.d[i,]
          }
  }
 return(iso.mean )
}

