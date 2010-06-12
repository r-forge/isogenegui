`MeanDiff` <-
function(x,y) {
isomean <- Isomean (x,y)
meandif <- isomean[,ncol(isomean)]-isomean[,1]
return (meandif)
}

