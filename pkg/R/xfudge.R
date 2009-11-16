`xfudge` <-
function(x.dif, si2, inputFudge ) {
  xs.alpha <- quantile(si2, (0:20) / 20)
  xs.perc <- quantile(si2, (0:100) / 100)
  xs.perc.gr <- as.numeric(cut(si2, breaks=xs.perc, include.lowest=TRUE))
  xs.mad <- matrix(0, length(xs.alpha), length(xs.perc)-1)
  for (ii in 1:100){
    k0 <- xs.perc.gr == ii
    xs.mad[,ii] <- apply(x.dif[k0]/outer(si2[k0],xs.alpha,"+"),2,mad, constant = 1)
  }
  xcv <- function(x) sqrt(var(x)) / mean(x)   # CV
  xs.cv <- apply(xs.mad, 1, xcv)
  if (inputFudge == "auto")  { xfudge <- xs.alpha[sort.list(xs.cv)[1]] }
  else   {
pct <- (0:20) *10/ 2
xfudge  <- xs.alpha[pct==inputFudge ]
} 
  resFudge <- list ( xfudge,xs.cv )
  return(resFudge )
}

