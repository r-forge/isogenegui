\name{PlotFPDlt}
\alias{PlotFPDlt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotFPDlt()
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
    delta <- AllFDR [, 1]
    fp50 <- AllFDR [, 2]
    fp90 <- AllFDR [, 3]


    plotFPD <- function() 
 { 
params <- par(bg="white")
    plot(delta, fp50, pch = ".", ylab = "# of false positives")
    lines(delta, fp90, lty = 1)
    lines(delta, fp50, lty = 2)
    legend(max(delta) * (1/2), nrow(SAMRes[[1]])/2, c("FP90\%", 
        "FP50\%"), lty = 1:2,cex=0.7)
    title("Plot of number of FP vs. Delta")
  }
    Plot2(plotFPD ,1,1 ,title="Windows Graph Output: Plot of number of FP vs. Delta")
    }
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
