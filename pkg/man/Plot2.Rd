\name{Plot2}
\alias{Plot2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Plot2(x, Myhscale, Myvscale, title)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{Myhscale}{ ~~Describe \code{Myhscale} here~~ }
  \item{Myvscale}{ ~~Describe \code{Myvscale} here~~ }
  \item{title}{ ~~Describe \code{title} here~~ }
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
function(x,Myhscale,Myvscale,title )
{

winPlot <- tktoplevel()
tkwm.title(winPlot ,title )
buttonFramePlot <-tkframe(winPlot ,borderwidth=2)

scr <- tkscrollbar(winPlot , repeatinterval=1, command=function(...)tkyview(tl,...))
tl <-tklistbox(winPlot ,height=2,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")

type <- c("ps","png","tiff","bmp","jpeg")

for (i in (1:length(type )))
{
    tkinsert(tl,"end",type [i])
  }
tkselection.set(tl,0)


img <- tkrplot(winPlot ,fun=x,hscale=Myhscale,vscale=Myvscale)

CopyToClip <- function()
{
  tkrreplot(img)
 tkmessageBox(message="The graph have been copied into clipboard",icon="info",type="ok")

  }

SaveGraph <- function()
{
fileName<- tclvalue(tkgetSaveFile())
if (!nchar(fileName))
    tkmessageBox(message="No graph to copy!")
else
{ 
  fileTypeChoice <- type[as.numeric(tkcurselection(tl))+1]
            split <- strsplit(fileName,".",fixed=TRUE)
            fileName <- paste(split[[1]][1],".",fileTypeChoice ,sep="")
            if(fileTypeChoice == "ps") fileTypeChoice <- "postscript"
            eval(parse(text=paste(fileTypeChoice ,"(file=fileName)",sep="")))
x()
            dev.off()
       }
  }


copy.but <- tkbutton(buttonFramePlot,text="Copy to Clipboard",command=CopyToClip)
save.but <- tkbutton(buttonFramePlot,text="Browse",command=SaveGraph)

tkgrid(img)
tkgrid(copy.but,tklabel(buttonFramePlot,text="Save image as:"),tl,scr,save.but)

tkgrid(buttonFramePlot)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
