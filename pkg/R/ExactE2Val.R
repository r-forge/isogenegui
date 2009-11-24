`ExactE2Val` <-
function (x,y)
#
  {
  
val <- c(0.33300 ,   0.25, 0.20000 ,0.16667, 0.14286,
0.50000, 0.45833, 0.41667, 0.38056, 0.35000,
0.16667 ,   0.25 ,0.29167 ,0.31250 ,0.32222,
0, 0.04167, 0.08333, 0.11806, 0.14583,
 0,      0, 0.00833 ,0.02083, 0.03472,
0,     0 ,0, 0.00139, 0.00417,
0,     0 ,0, 0, 0.00020)

plevel <- t(matrix(val,5,7))
dose <- x
data <- y
stat <- IsoGenem(dose,data) 
E2 <- cbind(stat[[1]],stat[[6]]) ## TAKING THE E^2 VALUE ##
E2max  <- apply(E2,1,max)
Dir <-  stat [[11]]
NN <- ncol(data)
doselev <- length (unique(dose))
pval <- function (E,lev )
{
pv <- 0
    for (i in 2:lev )
{
pv <- pv + ((plevel[i,lev -2]*(1-pbeta(E,0.5*(i-1),0.5*(NN-i)))))
}
return(pv)
}

asympval <- data.frame(E2max ,pval (E2max,doselev ),Dir )
names (asympval) <- c("E2", "Asymp.pvalue","Direction")
return(asympval )
}

