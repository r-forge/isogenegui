\name{IsopvaluePlot}
\alias{IsopvaluePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsopvaluePlot(x, y, niter, seed, stat = c("E2", "Williams", "Marcus", "M", "ModifM"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{niter}{ ~~Describe \code{niter} here~~ }
  \item{seed}{ ~~Describe \code{seed} here~~ }
  \item{stat}{ ~~Describe \code{stat} here~~ }
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
function (x, y, niter, seed, stat = c("E2", "Williams", "Marcus", 
    "M", "ModifM")) 
{
    Probe.ID <- row.names(y)
    obs <- IsoGene1(x, y)
    stat <- match.arg(stat)
    obs.up <- switch(stat, E2 = obs[[1]], Williams = obs[[2]], 
        Marcus = obs[[3]], M = obs[[4]], ModifM = obs[[5]])
    obs.dn <- switch(stat, E2 = obs[[6]], Williams = obs[[7]], 
        Marcus = obs[[8]], M = obs[[9]], ModifM = obs[[10]])
    exp.up <- exp.dn <- 1:niter
    set.seed(seed)
    x.niter <- t(sapply(1:niter, function(i) sample(x)))
    for (j in 1:niter) {
        exps <- IsoGene1(x.niter[j, ], y)
        exp.up[j] <- switch(stat, E2 = exps[[1]], Williams = exps[[2]], 
            Marcus = exps[[3]], M = exps[[4]], ModifM = exps[[5]])
        exp.dn[j] <- switch(stat, E2 = exps[[6]], Williams = exps[[7]], 
            Marcus = exps[[8]], M = exps[[9]], ModifM = exps[[10]])
        #cat(paste(j, ". "))
    }
    rawp.up <- sum(obs.up < exp.up)/niter
    rawp.dn <- sum(obs.dn > exp.dn)/niter
    if (stat == "E2") 
        rawp.dn <- sum(obs.dn < exp.dn)/niter
    #par(mfrow = c(2, 1))
    hist(exp.up, main = "", nclass = 1000, col = 0, probability = TRUE, 
        xlim = c(min(exp.up, obs.up), max(exp.up, obs.up)), xlab = paste(stat))
    dx <- density(exp.up, from = min(exp.up), to = max(exp.up))
    abline(v = obs.up, col = 7, lwd = 3)
    lines(dx$x, dx$y, lwd = 3, col = 5)
    title(paste("Gene", Probe.ID, ":p-value^{up}=", rawp.up, 
        sep = ""))
    hist(exp.dn, main = "", nclass = 1000, col = 0, probability = TRUE, 
        xlim = c(min(exp.dn, obs.dn), max(exp.dn, obs.dn)), xlab = paste(stat))
    dx <- density(exp.dn, from = min(exp.dn), to = max(exp.dn))
    abline(v = obs.dn, col = 7, lwd = 3)
    lines(dx$x, dx$y, lwd = 3, col = 5)
    title(paste("Gene: ", Probe.ID, ":p-value^{down}=", rawp.dn, 
        sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
