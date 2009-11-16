\name{PlotSigDlt}
\alias{PlotSigDlt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotSigDlt()
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
