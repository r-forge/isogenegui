\name{about}
\alias{about}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
about()
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

fontinfo <- tkfont.create(family="times",size=10)
fontinfo2 <- tkfont.create(family="times",size=10,weight="bold")

abt <- tktoplevel()
text1 <-tkframe(abt ,borderwidth=2)
text2 <-tkframe(abt ,borderwidth=2)


tkwm.title(abt ," About   ")
tkgrid(tklabel(abt ,text="    About   ",font=fontinfo2))

tkgrid(tklabel(text1 ,text=" The IsoGene package is a package developed by Dan Lin(dan.lin@uhasselt.be) 
 aimed to perform analysis of dose-response studies in microarray experiments",font=fontinfo))

tkgrid(tklabel(text2 ,text=" The IsoGene GUI package is developed for users with no or limited knowledge 
about R programming so they can perform the analysis of dose-response in 
a microarray setting  easily. This is developed and maintained by Setia Pramana 
(setia.pramana@uhasselt.be)",font=fontinfo))

tkgrid.configure(text1 ,sticky="w")
tkgrid.configure(text2 ,sticky="nsw")

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
