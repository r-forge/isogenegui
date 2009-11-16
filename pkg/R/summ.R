`summ` <-
function (x, y) 
{
   y1 <- as.numeric(y)[order(x)]
    m.y <- tapply(y1, as.factor(sort(x)), mean)
    n <- tapply(y1, as.factor(sort(x)), length)
    sd <- tapply(y1, as.factor(sort(x)), sd)
    std.err <- sd/sqrt(length(y1))

    unx <- sort(unique(x))
    y.is.u <- isoreg(unx, m.y)$yf
    y.is.d <- rev(isoreg(rev(unx), m.y)$yf)

sumary <- data.frame(unx ,n, m.y ,  std.err ,y.is.u , y.is.d)
names(sumary ) <- c(" Dose", " N", "Mean  ", "S.E    ", "Isotonic Mean Up","Isotonic Mean Down")
rownames(sumary) <- NULL
return(sumary)
}

