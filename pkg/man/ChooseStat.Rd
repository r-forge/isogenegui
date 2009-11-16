\name{ChooseStat}
\alias{ChooseStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ChooseStat()
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
ChooseStatWin <- tktoplevel()
tkwm.title(ChooseStatWin ,"Choose the statistic - IsoGene GUI" )
tkwm.deiconify(ChooseStatWin )
tkgrab.set(ChooseStatWin )
tkfocus(ChooseStatWin )
frame1 <- tkframe(ChooseStatWin ,borderwidth=2)
butFrame <-tkframe(ChooseStatWin ,borderwidth=2)

### Select the statistic value  ##
rb1 <- tkradiobutton( frame1 )
rb2 <- tkradiobutton( frame1 )
rb3 <- tkradiobutton( frame1 )
rb4 <- tkradiobutton( frame1 )
rb5 <- tkradiobutton( frame1 )
rbValue1 <- tclVar("ModM")

tkconfigure(rb1,variable=rbValue1 ,value="E2",text="E2")
tkconfigure(rb2,variable=rbValue1 ,value="Williams", text="Williams")
tkconfigure(rb3,variable=rbValue1 ,value="Marcus",text="Marcus")
tkconfigure(rb4,variable=rbValue1 ,value="M", text="M")    
tkconfigure(rb5,variable=rbValue1 ,value="ModM",text="M'")

tkgrid(tklabel(frame1 ,text="Choose the statistic:"))
tkgrid(rb1,rb2,rb3,rb4,rb5) 

on.Ok <- function () 
{

rbVal1 <- as.character(tclvalue(rbValue1))
if (rbVal1 == "E2" ) stat <- "E2"
if (rbVal1 == "Williams" ) stat <- "Williams"
if (rbVal1 == "Marcus" ) stat <- "Marcus"
if (rbVal1 == "M" ) stat <- "M"
if (rbVal1 == "ModM" ) stat <- "ModifM"
 statUsed <<- stat

tkdestroy(ChooseStatWin )

  } 

on.Cancel <- function () 
{
tkdestroy(ChooseStatWin )
            statUsed <<- NULL
  } 


ok.but <-tkbutton(butFrame ,text="   Ok   ",command=on.Ok)
cancel.but <-tkbutton(butFrame ,text="  Cancel  ",command=on.Cancel)

tkgrid(ok.but,cancel.but)
tkgrid(frame1)
tkgrid.configure(frame1,sticky="w")
tkgrid(butFrame)
tkwait.window(ChooseStatWin )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
