`PlotFPDlt` <-
function () {

if (!exists("SAMRes"))  
   {  
        tkmessageBox(title="Error Message",message="The SAM statistics have not calculated yet!",icon="error",type="ok")
     } 
   else {
    ChooseStat ()
    AllFDR <<- Isoallfdr(SAMRes, , stat=statUsed)
    delta <- AllFDR [, 1]
    fp50 <- AllFDR [, 2]
    fp90 <- AllFDR [, 3]


    plotFPD <- function() 
 { 
params <- par(bg="white")
    plot(delta, fp50, pch = ".", ylab = "# of false positives")
    lines(delta, fp90, lty = 1)
    lines(delta, fp50, lty = 2)
    legend(max(delta) * (1/2), nrow(SAMRes[[1]])/2, c("FP90%", 
        "FP50%"), lty = 1:2,cex=0.7)
    title("Plot of number of FP vs. Delta")
}
    Plot2(plotFPD ,1,1 ,title="Windows Graph Output: Plot of number of FP vs. Delta")
    }
 }

