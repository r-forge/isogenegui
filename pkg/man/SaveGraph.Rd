\name{SaveGraph}
\alias{SaveGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SaveGraph()
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
fileName<- tclvalue(tkgetSaveFile(initialfile="isoplot.ps",filetypes="{{Postscript} {.ps}} {{JPEG Files} {.jpg .jpeg}} {{All files} *}"))
if (!nchar(fileName))
    tkmessageBox(message="No graph to copy!")
else
{ 
splitted <- strsplit(fileName,".",fixed=TRUE)
if(length(unlist(splitted)) == 1)
{
fileName <- paste(fileName,".ps")
postscript(file=fileName)
x()
dev.off()}

if(unlist(splitted)[2] == "ps")
{
postscript(file=fileName)
x()
dev.off()

  }

if(unlist(splitted)[2] == "jpeg")
{
jpeg(file=fileName)
x()
dev.off()
  }
if(unlist(splitted)[2] == "png")
{
png(file=fileName)
x()
dev.off()
  }
       }



copy.but <- tkbutton(buttonFramePlot,text="Copy to Clipboard",command=CopyToClip)
save.but <- tkbutton(buttonFramePlot,text="Save as....",command=SaveGraph)

tkgrid(img)
tkgrid(copy.but,save.but )
tkgrid(buttonFramePlot)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
