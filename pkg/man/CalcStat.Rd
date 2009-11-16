\name{CalcStat}
\alias{CalcStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CalcStat()
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

 Direction <<-StatVal$direction
         up.gene <- which(Direction =="u")
         down.gene <- which(Direction =="d")

         ## E2 #

         E2.up <- cbind(StatVal$E2.up [up.gene ],up.gene )

   E2.down <-cbind( StatVal$E2.dn [down.gene ],down.gene)
         E2bind <- rbind(E2.up,  E2.down )
   E2Val <<- E2bind [order(E2bind[,2] ),1]

         ## M ##
    M.up <- cbind(StatVal$M.up [up.gene ],up.gene )
   M.down <-cbind( StatVal$M.dn [down.gene ],down.gene)
         Mbind <- rbind(M.up,  M.down )
   MVal <<-Mbind [order(Mbind[,2] ),1]

       ## Williams ##
         Williams.up <- cbind(StatVal$Williams.up [up.gene ],up.gene )
   Williams.down <-cbind( StatVal$Williams.dn [down.gene ],down.gene)
         Williamsbind <- rbind(Williams.up,  Williams.down )
   WilliamsVal <<-Williamsbind [order(Williamsbind[,2] ),1]

         ## Marcus##
         Marcus.up <- cbind(StatVal$Marcus.up [up.gene ],up.gene )
   Marcus.down <-cbind( StatVal$Marcus.dn [down.gene ],down.gene)
         Marcusbind <- rbind(Marcus.up,  Marcus.down )
   MarcusVal <<-Marcusbind [order(Marcusbind[,2] ),1]

         ## ModifM##
         ModM.up <- cbind(StatVal$ModM.up [up.gene ],up.gene )
   ModM.down <- cbind( StatVal$ModM.dn [down.gene ],down.gene)
         ModMbind <- rbind(ModM.up,  ModM.down )
   ModMVal <<- ModMbind [order(ModMbind[,2] ),1]

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
