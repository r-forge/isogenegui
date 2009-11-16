\name{CVPlot}
\alias{CVPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CVPlot(cv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cv}{ ~~Describe \code{cv} here~~ }
}
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
function(cv){
params <- par(bg="white")
pct <- (0:20)/20
plot(pct ,cv,xlab="percentile", ylab="CV", ,col="blue",lwd=2,ylim=c(min(cv),max(cv)) , axes = FALSE)
#plot(pct ,cv,xlab="percentile", ylab="CV", ,col="blue",lwd=2,ylim=c(min(xs.cv),max(xs.cv)) , axes = FALSE)

axis(1, pct , c("0\%","5\%", "10\%","15\%","20\%","25\%","30\%","35\%","40\%","45\%","50\%","55\%","60\%","65\%", 
"70\%","75\%","80\%","85\%","90\%","95\%","100\%"))
axis(2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
