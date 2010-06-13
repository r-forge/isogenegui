
library(IsoGeneGUI)
require(multtest)

test.Isomean <- function() {
x <- c(rep(1,3),rep(2,3),rep(3,3))
y1 <- c(1,2,3,10,9,11,10,6,8)
y2 <- rev(y1)
y <- data.frame(rbind(y1,y2)) # y needs to be a data frame 
res <-  matrix(c(2,9,9,9,9,2),2,3)
 checkTrue(is.numeric(IsoGeneGUI:::Isomean (x,y)))
 checkEquals(dim(IsoGeneGUI:::Isomean (x,y) ) , c(nrow(y),length(unique(x)) ) )
 checkEqualsNumeric(IsoGeneGUI:::Isomean (x,y), res )
 }

test.MeanDiff <- function() {
 x <- c(rep(1,3),rep(2,3),rep(3,3))
 y1 <- c(1,2,3,10,9,11,10,6,8)
 y2 <- rev(y1)
 y <- data.frame(rbind(y1,y2)) 
 checkTrue(is.numeric(IsoGeneGUI:::MeanDiff (x,y)))
 checkEquals(length(IsoGeneGUI:::MeanDiff (x,y) ) , nrow(y)  )
 checkEqualsNumeric(IsoGeneGUI:::MeanDiff (x,y), c(7 ,-7))
 }


test.PvalAdj <- function() {
pvl <- cbind(1:10,c(0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,
        0.001),runif(10),runif(10),runif(10) )
res <- mt.rawp2adjp(pvl[,2], "BH")
adjp <- res$adjp[order(res$index), ]
checkTrue(is.numeric(IsoGeneGUI:::PvalAdj (pvl ,FDR=0.5,"BH","E2")))
checkEqualsNumeric(IsoGeneGUI:::PvalAdj (pvl ,FDR=0.5,"BH","E2") , adjp  )
 }


test.summ <- function() {
 x <- c(rep(1,3),rep(2,3),rep(3,3))
 y1 <- c(1,2,3,10,9,11,10,6,8)
 checkTrue(is.data.frame(IsoGeneGUI:::summ (x,y1 )))
 checkEquals(dim(IsoGeneGUI:::summ (x,y1)) , c(length(unique(x)) ,6) )
 }


test.IsoGenemMod <- function() {
x <- c(rep(1,3),rep(2,3),rep(3,3))
y1 <- c(1,2,3,10,9,11,10,6,8)
y2 <- rev(y1)
y <- data.frame(rbind(y1,y2)) # y needs to be a data frame 
 checkTrue(is.list(IsoGeneGUI:::IsoGenemMod (x,y) ))
 checkEquals(length(IsoGeneGUI:::IsoGenemMod (x,y) ) ,20 )
 }

test.IsoTest <- function() {

pvl <- cbind(1:10,c(0.00001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,
        0.001),runif(10),runif(10),runif(10) )
res <- mt.rawp2adjp(pvl[,2], "BH")
adjp <- res$adjp[order(res$index), ]
checkEqualsNumeric( t(IsoGeneGUI:::IsoTest(pvl ,FDR=0.05,type="BH",stat="E2")[4]), 
   adjp [adjp[,2] <=0.05 ,2]  )
}

test.ExactE2Val<- function() {
 x <- c(rep(1,3),rep(2,3),rep(3,3))
 y1 <- c(1,2,3,10,9,11,10,6,8)
 y2 <- rev(y1)
 y <- data.frame(rbind(y1,y2)) 
 checkEquals( round(as.numeric(unlist(IsoGeneGUI:::ExactE2Val(x,y)[1] ) ), digits = 5) , 
   c(0.84483,0.84483)   )
}




