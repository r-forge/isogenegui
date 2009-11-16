\name{printasymp}
\alias{printasymp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
printasymp(num, FDR, result)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{ ~~Describe \code{num} here~~ }
  \item{FDR}{ ~~Describe \code{FDR} here~~ }
  \item{result}{ ~~Describe \code{result} here~~ }
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
function(num,FDR,result)
{
  
infoBox  <- tktoplevel()
tkwm.title(infoBox,"Asymptotic E2") 

       xscr <- tkscrollbar(infoBox, repeatinterval=5,orient="horizontal",
                       command=function(...)tkxview(txt,...))
       yscr <- tkscrollbar(infoBox, repeatinterval=5,
                       command=function(...)tkyview(txt,...))

      txt <- tktext(infoBox,bg="white",font="courier", xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(yscr,...),
       wrap="none")
tkgrid(txt,yscr)
      tkgrid(xscr)
      tkgrid.configure(yscr,sticky="ns")
      tkgrid.configure(xscr,sticky="ew")
       
      tkinsert(txt,"end","                               \n")
tkinsert(txt,"end", paste("Results for E2 using Asymptotic p-value \n"))
tkinsert(txt,"end","                               \n")
      tkinsert(txt,"end", paste("Number of Genes analyzed  :", num ," \n"))
      tkinsert(txt,"end", paste("Overall significant level :", FDR ," \n"))
tkinsert(txt,"end","                         \n")
for (i in (1:nrow(result)))
{
      tkinsert(txt,"end", paste("P-value adjustment:", result[i,1]," \n"))
      tkinsert(txt,"end", paste("Number Significant Genes  :", result[i,2]," \n"))
  }

     
      tkconfigure(txt, state="disabled")
tkfocus(txt)
tkwm.deiconify(infoBox  )
tkfocus(infoBox  )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
