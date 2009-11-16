\name{summ}
\alias{summ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summ(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
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
function (x, y) 
{
   y1 <- as.numeric(y)[order(x)]
    m.y <- tapply(y1, as.factor(sort(x)), mean)
    n <- tapply(y1, as.factor(sort(x)), length)
    sd <- tapply(y1, as.factor(sort(x)), sd)
    std.err <- sd/sqrt(length(y1))

    unx <- sort(unique(x))
    y.is.u <- isoreg(unx, m.y)$yf
    y.is.d <- rev(isoreg(rev(unx), m.y)$yf)

sumary <- data.frame(unx ,n, m.y ,  std.err ,y.is.u , y.is.d)
names(sumary ) <- c(" Dose", " N", "Mean  ", "S.E    ", "Isotonic Mean Up","Isotonic Mean Down")
rownames(sumary) <- NULL
return(sumary)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
