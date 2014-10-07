`IsoPlot.mod` <-
function (x, y, type = c("continuous", "ordinal"), add.curve = FALSE) 
{
    Probe.ID <- rownames(y)
	y <- as.numeric(y)
    type <- match.arg(type)
    if (!(type %in% c("continuous", "ordinal"))) 
        stop("The dose can be only continuous or ordinal")
   stat <- IsoGene1(x,as.matrix(y))


   E2up <- round(stat$E2.up,2)
   E2down <- round(stat$E2.dn,2)
    miny <- min(y)
    maxy <- max(y)
    ordx <- order(x)
    unx <- sort(unique(x))
    y1 <- as.numeric(y)[ordx]
    y.m <- tapply(y1, as.factor(x[order(x)]), mean)
    y.m.tot <- rep(mean(y), length(unx))
    n.p <- table(x)
    n.g <- length(n.p)
    y.is.u <- pava(y = y.m, w = n.p)
    y.is.d <- pava(y = y.m, w = n.p, decreasing = TRUE)

    dire <- IsoGene1(x, as.numeric(y))[[11]]
    if (type == "continuous") {
        plot(sort(x), y1, lwd = 2, xlab = "Doses", ylab = "Gene Expression")
        points(sort(unique(x)), y.m, pch = "+", lwd = 3, col = 2)
            points(unx, y.is.u, pch = "*", lwd = 2)
            if (add.curve) 
                lines(unx, y.is.u, lty = 1, col = 2, lwd = 2)
        
            points(unx, y.is.d, pch = "*", lwd = 2)
            if (add.curve) 
                lines(unx, y.is.d, lty = 1, col = 4, lwd = 2)
        
    }
    else {
        catx <- factor(x, levels = sort(unique.default(x)), labels = unx, 
            ordered = FALSE)
        a <- 1:length(unx)
        plot(a, ylim = c(miny, maxy), pch = "", ylab = "Gene Expression", 
            xlab = "Doses", axes = FALSE)
        axis(1, sort(unique(a)), as.character(unx))
        axis(2)
        points(catx, y, lwd = 2)
        points(sort(unique(catx)), y.m, pch = "+", lwd = 3, col = 2)
            points(a, y.is.u, pch = "*", lwd = 2)
            if (add.curve) 
                lines(a, y.is.u, lty = 1, col = 4, lwd = 2)
        
            points(a, y.is.d, pch = "*", lwd = 2)
            if (add.curve) 
                lines(a, y.is.d, lty = 1, col = 2, lwd = 2)
    }

 l <- length(unx)
    maxx <- 0.35*(max(unx)+unx[l-1])

    legend("bottomright" , c(paste("Up,     E2: ", E2up , sep = "" ), paste("Down, E2: ", E2down , sep = "" )), col = c(4,2),
       lty = 1, lwd=2, merge = TRUE, cex=0.6)


    title(paste("Gene: ", Probe.ID, sep = ""))
}

