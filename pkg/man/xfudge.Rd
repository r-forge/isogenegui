\name{xfudge}
\alias{xfudge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xfudge(x.dif, si2, inputFudge)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.dif}{ ~~Describe \code{x.dif} here~~ }
  \item{si2}{ ~~Describe \code{si2} here~~ }
  \item{inputFudge}{ ~~Describe \code{inputFudge} here~~ }
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
function(x.dif, si2, inputFudge ) {
  xs.alpha <- quantile(si2, (0:20) / 20)
  xs.perc <- quantile(si2, (0:100) / 100)
  xs.perc.gr <- as.numeric(cut(si2, breaks=xs.perc, include.lowest=TRUE))
  xs.mad <- matrix(0, length(xs.alpha), length(xs.perc)-1)
  for (ii in 1:100){
    k0 <- xs.perc.gr == ii
    xs.mad[,ii] <- apply(x.dif[k0]/outer(si2[k0],xs.alpha,"+"),2,mad, constant = 1)
  }
  xcv <- function(x) sqrt(var(x)) / mean(x)   # CV
  xs.cv <- apply(xs.mad, 1, xcv)
  if (inputFudge == "auto")  { xfudge <- xs.alpha[sort.list(xs.cv)[1]] }
  else   {
pct <- (0:20) *10/ 2
xfudge  <- xs.alpha[pct==inputFudge ]
  } 
  resFudge <- list ( xfudge,xs.cv )
  return(resFudge )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
