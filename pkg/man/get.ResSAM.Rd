\name{get.ResSAM}
\alias{get.ResSAM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.ResSAM()
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
function() {
    qval <- Isoqval(delta.now, AllFDR, SAMRes, stat=statSAMplot )
    Res.SAM  <- qval[[1]]
    siggeneSAM <- qval[[2]]

    if (nrow (Res.SAM ) > nrow(exprs2SAM) ) {
reprow <- which(table( Res.SAM[,2])==2)
delrow <- which(Res.SAM[,2]== reprow )[1]
Res.SAM <- Res.SAM[-delrow ,]
  }

    probe.id <- rownames(exprs2SAM[Res.SAM [,1],] )
    if (is.null(probe.id)) probe.id <- Res.SAM [,1]
    Res.SAM <- data.frame(probe.id,Res.SAM ) 
    Res.SAM <- Res.SAM [order(Res.SAM [,2]),]
    Res.SAM <- data.frame(Res.SAM,MeanDiffSAM, DirectionSAM) 

    names(Res.SAM ) <- c("Probe ID", "row num", paste(statSAMplot ,"value"),"q-value", "Mean.Diff", "Direction")
    rownames(Res.SAM )  <- NULL

 
    rowsig <- siggeneSAM  [,1]
    siggeneSAM   <- Res.SAM [sort(rowsig ),]

    names(siggeneSAM ) <- c("Probe ID", "row num", paste(statSAMplot ,"value"),"q-value", "Mean.Diff", "Direction")
    rownames(siggeneSAM )  <- NULL

    resSAM <- list (Res.SAM,siggeneSAM  )
    return(resSAM )
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
