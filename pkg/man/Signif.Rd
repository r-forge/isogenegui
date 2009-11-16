\name{Signif}
\alias{Signif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Signif(rawpval, FDR, proc = c("BH", "BY", "Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawpval}{ ~~Describe \code{rawpval} here~~ }
  \item{FDR}{ ~~Describe \code{FDR} here~~ }
  \item{proc}{ ~~Describe \code{proc} here~~ }
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
function (rawpval,FDR, proc = c("BH", "BY","Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD"))
  {
    adjp <- adjustment (rawpval, proc )
    sig  <- which(adjp[, 2] <= FDR)
    adjp1 <- data.frame(sig,adjp[sig,])
    names(adjp1 ) <- c("row.num", "raw p-values", paste (proc , sep = " ", "p-values"))
    return(adjp1 )
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
