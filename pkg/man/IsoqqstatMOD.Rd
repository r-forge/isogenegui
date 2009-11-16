\name{IsoqqstatMOD}
\alias{IsoqqstatMOD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsoqqstatMOD(x, y, fudge, niter, seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{fudge}{ ~~Describe \code{fudge} here~~ }
  \item{niter}{ ~~Describe \code{niter} here~~ }
  \item{seed}{ ~~Describe \code{seed} here~~ }
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
function (x, y, fudge, niter, seed) 
{

    set.seed(seed)
    xiter.index <- t(sapply(1:niter, function(i) sample(x)))
    to1 <- to2 <- to3 <- to4 <- to5 <- matrix(0, nrow(y), niter)
    if (fudge == "pooled" ) {
        fudge.factor <- IsofudgeMod2(dose,exprs2SAM,fudge="auto")
     }
    if (fudge == "specFudge" ) {
 fudge.factor <- IsofudgeMod2(dose,exprs2SAM ,fudge=fudgeVal )
  }

    if (fudge == "no") {
        fudge.factor <- c(rep(0, 5))
    }
    for (i in 1:niter) {
        yyy0 <- IsoGenemSAM(xiter.index[i, ], as.matrix(y), fudge.factor)
        to1[, i] <- sort(yyy0[[1]])
        to2[, i] <- sort(yyy0[[2]])
        to3[, i] <- sort(yyy0[[3]])
        to4[, i] <- sort(yyy0[[4]])
        to5[, i] <- sort(yyy0[[5]])
 
     tkwm.deiconify(samPermute)
tkgrab.set(samPermute)
tkfocus(samPermute)
  if (i < niter ) { tclvalue(PermuteText1 ) <- "Please wait...." }
  else {tclvalue(PermuteText1 ) <- "Permutation is finished...."}
  tclvalue(PermuteText2 ) <- paste( "Performing",i,"/",NumPermval,"permutations",sep = " " )


    }
    L <- IsoGenemSAM(x, as.matrix(y), fudge.factor)
    d <- L[[1]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to1, 1, mean)
    aa1 = cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    d <- L[[2]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to2, 1, mean)
    aa2 = cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    d <- L[[3]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to3, 1, mean)
    aa3 = cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    d <- L[[4]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to4, 1, mean)
    aa4 = cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    d <- L[[5]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to5, 1, mean)
    aa5 <- cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    Direction <- L[[6]]
    res <- list(aa1 = aa1, to1 = to1, aa2 = aa2, to2 = to2, aa3 = aa3, 
        to3 = to3, aa4 = aa4, to4 = to4, aa5 = aa5, to5 = to5, Direction=Direction )
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
