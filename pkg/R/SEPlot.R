`SEPlot` <-
function(stat,fudge)
{
  stat1 <- get(paste(stat,"Val",sep="" ))
  se <- get(paste("Se",stat,"Val",sep="" ))
params <- par(bg="white")
par(mfrow=c(2,1))
xx <- cbind(se ,stat1 )
colors  <- densCols(xx)
plot(xx , col=colors,xlab="Standar error", ylab=stat)
      if(fudge != 0) abline(v=fudge,col="red" )
plot(density(SeModMVal ) ,xlab="Standar error",main="",lwd=2,col="blue")
if(fudge != 0) abline(v=fudge,col="red"  )
}

