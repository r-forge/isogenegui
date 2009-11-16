\name{UserPlot}
\alias{UserPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
UserPlot(object.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object.list}{ ~~Describe \code{object.list} here~~ }
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
function (object.list) {  
UserPlot  <-tktoplevel()
tkwm.title(UserPlot ,"User Defined Plot") 
      spec.frm <- tkframe(UserPlot ,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
buttonFrameiso <-tkframe(UserPlot ,borderwidth=2)

      scr <- tkscrollbar( spec.frm , repeatinterval=2, command=function(...)tkyview(tl,...))

      tl<-tklistbox(spec.frm,height=10,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")

for (i in (1:length(object.list )))
{
    tkinsert(tl,"end",object.list [i])
  }
tkselection.set(tl,0)

lab0 <- tklabel(spec.frm,text="  ")

tkgrid(tklabel(frame1,text="Specify the objects:"))

Name1 <- tclVar("-- None --")
lab1 <- tklabel(frame1,text="x - axis")
edit1 <-tkentry(frame1,width="20",textvariable=Name1)
assignVariable1 <- function()
{
    fruitIndex <- as.integer(tkcurselection(tl))
       tclvalue(Name1) <- object.list[as.numeric(tkcurselection(tl))+1]
  }
assignVariable.but1 <- tkbutton(frame1,text="<--",command=assignVariable1)
tkgrid(lab1,edit1,tklabel(spec.frm,text="  "),assignVariable.but1)

cb1 <- tkcheckbutton(frame1)
cbValue1 <- tclVar("0")
tkconfigure(cb1,variable=cbValue1,text=" -log10( ) ")
tkgrid(tklabel(frame1,text="             "),cb1)


Name2 <- tclVar("-- None --")
lab2 <- tklabel(frame1,text="y - axis")
edit2 <-tkentry(frame1,width="20",textvariable=Name2)
assignVariable2 <- function()
{
    fruitIndex <- as.integer(tkcurselection(tl))
       tclvalue(Name2) <- object.list[as.numeric(tkcurselection(tl))+1] 
  }
assignVariable.but2 <- tkbutton(frame1,text="<--",command=assignVariable2)
tkgrid(tklabel(frame1,text=""))

tkgrid(lab2,edit2,tklabel(spec.frm,text="  "),assignVariable.but2)

cb2 <- tkcheckbutton(frame1)
cbValue2 <- tclVar("0")
tkconfigure(cb2,variable=cbValue2,text=" -log10( ) ")
tkgrid(tklabel(frame1,text="             "),cb2)

tkgrid(tklabel(frame1,text=""))

tkgrid(frame1,lab0,lab0,tl,scr)

tkgrid.configure(tl,sticky="nse")
tkgrid.configure(scr,rowspan=10,sticky="nsw")


    ######
   OnOK <- function()
{
      Userplot <- function(x,y) 
   { 
xaxis <- get(tclvalue(Name1))
yaxis <- get(tclvalue(Name2))
xobject <- tclvalue(Name1)
yobject <- tclvalue(Name2)
cbVal1 <- as.character(tclvalue(cbValue1))
cbVal2 <- as.character(tclvalue(cbValue2))
       
if (length(xaxis ) != length(yaxis ) )  {
    ReturnVal <- tkmessageBox(title="Error Message",message="The objects have different length!",icon="error",type="ok")
        if (tclvalue(ReturnVal)== "ok") 
  tkdestroy(UserPlot )
        }
if (cbVal1 =="1") {
 dig <- max (nchar(xaxis ))
 xaxis [xaxis == 0] <- 1/(10^dig)
 xaxis <- -log10(xaxis )
 xobject <- paste("-log10(",xobject,")",sep="" )
  }

if (cbVal2 =="1") {
 dig <- max (nchar(yaxis ))
 yaxis [yaxis == 0] <- 1/(10^dig)
 yaxis <- -log10(yaxis )
 yobject <- paste("-log10(",yobject,")",sep="" )
  }

  params <- par(bg="white")
x <-cbind(xaxis ,yaxis)
colors  <- densCols(x)
  plot (x,col=colors, xlab=xobject ,ylab=yobject )
    title(paste("Plot of ",xobject , "vs.", yobject ,sep=""))

   }
     Plot2(Userplot,1.2,1.2 ,title="Windows Graph Output: User Defined Scatter Plot")
   
    }

   onCancel <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(UserPlot )
tkdestroy(UserPlot)
  }

   OK.but <-tkbutton(buttonFrameiso ,text="  Display  ",command=OnOK)
   Cancel.but <-tkbutton(buttonFrameiso ,text="    Exit    ",command=onCancel)
   tkgrid(OK.but,tklabel(buttonFrameiso ,text=" "),Cancel.but)

   tkgrid(spec.frm)
   tkgrid(buttonFrameiso)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
