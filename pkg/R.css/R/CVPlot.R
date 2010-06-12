`CVPlot` <-
function(cv){
params <- par(bg="white")
pct <- (0:20)/20
plot(pct ,cv,xlab="percentile", ylab="CV", ,col="blue",lwd=2,ylim=c(min(cv),max(cv)) , axes = FALSE)
#plot(pct ,cv,xlab="percentile", ylab="CV", ,col="blue",lwd=2,ylim=c(min(xs.cv),max(xs.cv)) , axes = FALSE)

axis(1, pct , c("0%","5%", "10%","15%","20%","25%","30%","35%","40%","45%","50%","55%","60%","65%", 
"70%","75%","80%","85%","90%","95%","100%"))
axis(2)
}

