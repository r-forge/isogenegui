`PlotFdrDlt` <-
function () {

if (!exists("SAMRes"))  
   {  
        tkmessageBox(title="Error Message",message="The SAM statistics have not calculated yet!",icon="error",type="ok")
     } 
   else {
    ChooseStat ()
    AllFDR <<- Isoallfdr(SAMRes, , stat=statUsed)
    FDR50 <- AllFDR [, 5]
    FDR90 <- AllFDR [, 6]
    delta <- AllFDR [, 1]

    plotFdrD <- function() 
 { 
params <- par(bg="white")
    plot(delta, FDR50, pch = ".", ylab = "FDR")
    lines(delta, FDR90, lty = 1)
    lines(delta, FDR50, lty = 2)
    abline(0.05, 0)
    abline(0.1, 0)
    legend("topright", c("FDR90%", "FDR50%"), lty = 1:2,cex=0.7)
    title("Plot of FDR vs. Delta")
 }
   Plot2(plotFdrD ,1,1 ,title="Windows Graph Output: Plot of FDR vs. Delta")
   }
}

