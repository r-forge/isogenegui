\name{saveXls}
\alias{saveXls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
saveXls()
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

fileName <- tclvalue(tkgetSaveFile(filetypes="{{Excel Files} {.xls}} {{All files} *}"))
split <- strsplit(fileName,".",fixed=TRUE)
fileXls2 <<- paste(split[[1]][1],".xls",sep="")
assign("fileXls",fileXls,envir=.GlobalEnv)
tclvalue(fileXls) <<- fileXls2 
if (!nchar(fileName)) {
   tclvalue(fileXls) <<- c("")  
   tkmessageBox(message="No file was selected!")
   }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
