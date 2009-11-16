\name{PlotFdrDlt}
\alias{PlotFdrDlt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotFdrDlt()
}
%- maybe also 'usage' for other objects documented here.
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
    legend("topright", c("FDR90\%", "FDR50\%"), lty = 1:2,cex=0.7)
    title("Plot of FDR vs. Delta")
 }
   Plot2(plotFdrD ,1,1 ,title="Windows Graph Output: Plot of FDR vs. Delta")
   }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
