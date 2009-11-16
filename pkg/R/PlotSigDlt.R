`PlotSigDlt` <-
function () {

if (!exists("SAMRes"))  
   {  
        tkmessageBox(title="Error Message",message="The SAM statistics have not calculated yet!",icon="error",type="ok")
     } 
   else {
    ChooseStat ()
    AllFDR <<- Isoallfdr(SAMRes, , stat=statUsed)
    delta <- AllFDR [, 1]
    signnum <- AllFDR [, 4]

    plotSigD <- function() 
 { 
params <- par(bg="white")
    plot(delta, signnum, pch = ".", ylab = "# of significant genes")
    lines(delta, signnum)
    title("Plot of # of sign genes vs. Delta",cex=0.7)

      }

Plot2(plotSigD ,1,1 ,title="Windows Graph Output: Plot of # of Sig genes vs. Delta")
}
}

