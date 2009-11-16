\name{IsoTest}
\alias{IsoTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsoTest(rp, FDR, type = c("BH", "BY", "Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD"), stat = c("E2", "Williams", "Marcus", "M", "ModM"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rp}{ ~~Describe \code{rp} here~~ }
  \item{FDR}{ ~~Describe \code{FDR} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
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
function (rp, FDR, type = c("BH", "BY","Bonferroni", "Holm", 
"Hochberg", "SidakSS", "SidakSD"), stat = c("E2", "Williams", 
    "Marcus", "M", "ModM")) 
{
    Probe.ID <- rp[, 1]
    type <- match.arg(type)
    stat <- match.arg(stat)
    if (stat == "E2") 
        rpraw <- rp[, 2]
    else if (stat == "Williams") 
        rpraw <- rp[, 3]
    else if (stat == "Marcus") 
        rpraw <- rp[, 4]
    else if (stat == "M") 
        rpraw <- rp[, 5]
    else if( stat == "ModM")
rpraw <- rp[, 6]
  
    adjp <- adjustment (rpraw , type ) 
    sig  <- which(adjp[, 2] <= FDR)
    adjp1 <- adjp[sig,]

   if (is.null (adjp1)== TRUE ) { 
     sign.genes <- 0
      }
  else { 
    sign.Probe.ID <-Probe.ID[sig]
    sign.genes <- data.frame(sign.Probe.ID ,sig,adjp1)
    row.names(sign.genes) <- NULL
    names(sign.genes) <- c("Probe.ID", "row.num", paste (stat, sep = " ", "Raw p-values"), paste (type, sep = " ", "Adjusted p-values"))
     }
  return(sign.genes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
