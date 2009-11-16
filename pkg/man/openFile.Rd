\name{openFile}
\alias{openFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
openFile()
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
function() 
{

    openfile <-tktoplevel()
tkwm.title(openfile ,"Open File") 
tkgrab.set(openfile )
tkfocus(openfile )
      spec.frm <- tkframe(openfile ,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
buttonFrame <-tkframe(openfile ,borderwidth=2)

      Browse.but1 <-tkbutton(frame1,text="browse",command=getDose )
      Browse.but2 <-tkbutton(frame1,text="browse",command=getExprs )
fileDose <<- tclVar("        ")
fileExprs <<- tclVar("        ")
Dose <-tkentry(frame1,width="25",textvariable=fileDose )
Exprs <-tkentry(frame1,width="25",textvariable=fileExprs)

lab0 <- tklabel(frame1,text="Specify the file for: ")

lab1 <- tklabel(frame1,text="Dose: ")
lab2 <- tklabel(frame1,text="Gene expression: ")

      tkgrid(lab0)
      tkgrid(lab1,Dose , Browse.but1 )
      tkgrid(lab2,Exprs , Browse.but2 )
tkgrid(frame1)

 OnOK <- function()
{
if (is.list(dose))  dose <- unlist(dose)
assign("dose",dose ,envir=.GlobalEnv)

exprs <<- data.frame(exprs)
assign("exprs",exprs,envir=.GlobalEnv)

dataSet <- c("dose","exprs")
    assign("dataSet",dataSet ,envir=.GlobalEnv)

## putting info in tree ##
N.Gene <- nrow(exprs)
N.arry <- ncol(exprs)
N.dose <- length(unique(dose))
try( tkdelete(treeWidget,"data"), silent = T )
try( tkdelete(treeWidget,"dataNode"), silent = T )
tkinsert(treeWidget,"end","Record1Node","dataNode",text= "Available")
tkinsert(treeWidget,"end","dataNode","geneNode",text=paste("Genes: ",N.Gene))
tkinsert(treeWidget,"end","dataNode","arrayNode",text=paste("Arrays:",N.arry) )
tkinsert(treeWidget,"end","dataNode","doseNode",text=paste("Dose: ",N.dose,"levels"))


 tkgrab.release(openfile)
tkdestroy(openfile )
 }

   onCancel <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(openfile )
tkdestroy(openfile )
tkwm.deiconify(tt ) 
       tkfocus(tt )
 }

   OK.but <-tkbutton(buttonFrame ,text="   OK   ",command=OnOK)
   Cancel.but <-tkbutton(buttonFrame ,text=" Cancel ",command=onCancel)
   tkgrid(OK.but,Cancel.but)
   tkgrid(spec.frm)
   tkgrid(buttonFrame )
   tkfocus(openfile )

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
