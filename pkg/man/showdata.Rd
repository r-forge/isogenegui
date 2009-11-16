\name{showdata}
\alias{showdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
showdata()
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

if (!exists("exprs") | !exists("dose"))  
   {  
      ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
  } 
else {

shdat<-tktoplevel()
tkwm.deiconify(shdat)
tkwm.title(shdat,"Show Data") 
buttonFrame <-tkframe(shdat,borderwidth=2)

tkgrab.set(shdat)
tkfocus(shdat)
scr <- tkscrollbar(shdat, repeatinterval=5, command=function(...)tkyview(tl1,...))

#####
tl1<-tklistbox(shdat,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(tklabel(shdat,text="Choose the object you want to see: "))
tkgrid(tl1,scr)
tkgrid.configure(scr,rowspan=4,sticky="nsw")
for (i in (1:length(dataSet)))
{
    tkinsert(tl1,"end",dataSet[i])
  }
tkselection.set(tl1,2)  

OnOK <- function()
{
loc <- as.numeric(tkcurselection(tl1))+1
  if (loc ==1) {     
tkdestroy(shdat)
     showData(dose)}
  else {
     tkdestroy(shdat)
     showData(exprs)
     }
  }

onCancel <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(shdat)
tkdestroy(shdat)
tkwm.deiconify(tt ) 
       tkfocus(tt )
 }

OK.but <-tkbutton(buttonFrame,text="   OK   ",command=OnOK)
Cancel.but <-tkbutton(buttonFrame,text=" Cancel ",command=onCancel)

tkgrid(OK.but,Cancel.but)
tkgrid(buttonFrame)

tkfocus(shdat)
     }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
