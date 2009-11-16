\name{SEPlot}
\alias{SEPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SEPlot(stat, xfudge)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{ ~~Describe \code{stat} here~~ }
  \item{xfudge}{ ~~Describe \code{xfudge} here~~ }
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
function(stat,xfudge)
{
  stat1 <- get(paste(stat,"Val",sep="" ))
  se <- get(paste("Se",stat,"Val",sep="" ))
params <- par(bg="white")
par(mfrow=c(2,1))
xx <- cbind(se ,stat1 )
colors  <- densCols(xx)
plot(xx , col=colors,xlab="Standar error", ylab=stat)
      if(xfudge != 0) abline(v=xfudge,col="red" )
plot(density(SeModMVal ) ,xlab="Standar error",main="",lwd=2,col="blue")
if(xfudge != 0) abline(v=xfudge,col="red"  )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
