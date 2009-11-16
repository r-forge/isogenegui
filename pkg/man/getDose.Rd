\name{getDose}
\alias{getDose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getDose()
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
 fileName <- tclvalue(tkgetOpenFile(filetypes=
        gettext('{"text" {".txt"}} {"Excel Files" {".xls"}}
{"All Files" {"*"}}')))
tclvalue(fileDose) <<- fileName
if (!nchar(fileName)) 
{
    tkmessageBox(message = "No file was selected!")
  } 
else 
{
splitted <- strsplit(fileName,".",fixed=TRUE)
if(unlist(splitted)[2] == "txt")
{
dose <- read.table(file=fileName,header=FALSE)
assign("dose",dose,envir=.GlobalEnv)
  }
else
{ 
if(unlist(splitted)[2] == "xls")
{
dose <- unlist(read.xls(fileName))
assign("dose",dose,envir=.GlobalEnv)
  }
else {
tkmessageBox(message = "The files is not either txt or excel file")
  }
  }
      }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
