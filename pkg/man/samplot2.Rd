\name{samplot2}
\alias{samplot2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
samplot2()
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
 qqstat<- SAMRes 
 observed <- switch(statSAMplot, E2 = qqstat[[1]][, 1], Williams = qqstat[[3]][, 
        1], Marcus = qqstat[[5]][, 1], M = qqstat[[7]][, 1], 
        ModifM = qqstat[[9]][, 1])
 expected <- switch(statSAMplot, E2 = qqstat[[1]][, 2], Williams = qqstat[[3]][, 
        2], Marcus = qqstat[[5]][, 2], M = qqstat[[7]][, 2], 
        ModifM = qqstat[[9]][, 2])
   numsig <- Delta.table[pos, 4]
   FDR <- Delta.table[pos, 5]
   delta <- Delta.table[pos, 1]
   params <- par(bg="white")
   plot(expected, observed)
    abline(0, 1)
    abline(delta, 1, lty = 2, col = "blue",lwd=2)
    abline(-delta, 1, lty = 2, col = "blue",lwd=2)
    oe <- order(expected)
    q.mat <- switch(statSAMplot, E2 = qqstat[[1]][oe, ], Williams = qqstat[[3]][oe, 
        ], Marcus = qqstat[[5]][oe, ], M = qqstat[[7]][oe, ], 
        ModifM = qqstat[[9]][oe, ])
    if (sum(q.mat[, 3] >= delta) > 0) {
        x.exp <- min(q.mat[q.mat[, 3] >= delta, 2])
        points(q.mat[q.mat[, 2] >= x.exp, 2], q.mat[q.mat[, 2] >= 
            x.exp, 1], col = "red")
    }
    if (sum(q.mat[, 3] <= -delta) > 0) {
        x.exp <- max(q.mat[q.mat[, 3] <= -delta, 2])
        points(q.mat[q.mat[, 2] <= x.exp, 2], q.mat[q.mat[, 2] <= 
            x.exp, 1], col = "red")
    }

    legend(min(expected), max(observed), c(paste("Delta =", delta, 
        sep = "") , paste("FDR = ", FDR , sep = ""), 
  paste ("Significant =",numsig,sep = "")) , lty = 0)

    title(paste("observed vs. expected statistics for: ", statSAMplot))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
