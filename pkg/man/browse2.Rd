\name{browse2}
\alias{browse2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
browse2()
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

    file <- tclvalue(tkgetOpenFile(filetypes=
        gettext('{"R Data Files" {".rda" ".Rdata" ".RDA"}} {"All Files" {"*"}}')))
    assign("file",file,envir=.GlobalEnv)
    
if (file == "")
 { 
  tkmessageBox(message = "No file was selected!")
return()
  }
else {
      SAMResdata <- load(file)
    #assign("SAMResdata",SAMResdata ,envir=.GlobalEnv)
      
chPval <-tktoplevel()
tkwm.title(chPval ,"Obtaining previous SAM permutation ") 
tkgrab.set(chPval )
tkfocus(chPval )
      spec.frm <- tkframe(chPval ,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
buttonFrameiso <-tkframe(chPval ,borderwidth=2)

      scr <- tkscrollbar( spec.frm , repeatinterval=2, command=function(...)tkyview(tl,...))

      tl <-tklistbox(spec.frm,height=5,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")

for (i in (1:length(SAMResdata)))
{
    tkinsert(tl,"end",SAMResdata[i])
  }
tkselection.set(tl,0)

lab0 <- tklabel(chPval ,text="Choose the object contain the raw p-values")
lab1 <- tklabel(spec.frm,text="  ")
tkgrid(lab0)

tkgrid(lab1,lab1,tl,scr)
tkgrid.configure(tl,sticky="nse")
tkgrid.configure(scr,rowspan=10,sticky="nsw")
tkgrid(frame1)


    ######
    onCancel <- function()
  {
    ReturnVal <- 0
      tkgrab.release(chPval )
tkdestroy(chPval )
tkwm.deiconify(chPval) 
       tkfocus(chPval)
 }
   OnOK <- function()
{
     SAMResChoice <- SAMResdata [as.numeric(tkcurselection(tl))+1]
tclvalue(pvalName) <<- file 

SAMRes <- get(SAMResChoice)
    assign("SAMRes.temp",SAMRes ,envir=.GlobalEnv)

 try(SAMResID <- rownames(SAMRes$aa1))
      exprsID <- rownames(exprs)
      matchID <- match (SAMResID ,exprsID )
      exprs2SAM <- exprs[matchID ,]
DirectionSAM <- SAMRes [[11]][matchID ]
    assign("matchID.temp",matchID ,envir=.GlobalEnv)
    assign("exprs2SAM.temp",exprs2SAM  ,envir=.GlobalEnv)
 assign("DirectionSAM",DirectionSAM,envir=.GlobalEnv)

N <- length(matchID)
 MeanDiffSAM  <<- MeanDiff (dose,exprs2SAM )

   tkdelete(treeWidget,"nodeSAM")  
 try( tkdelete(treeWidget,"PermSAMNode"), silent=T)
      tkinsert(treeWidget,"end","Record4Node","PermSAMNode",text="SAM Permutations ")
tkinsert(treeWidget,"end","PermSAMNode","NumGeneSAMNode",text= paste("Number genes analyzed: ",N )  )
tkinsert(treeWidget,"end","PermSAMNode","FudgeNode",text= paste("Fudge Factor: ",SAMRes [[13]] ) )
tkinsert(treeWidget,"end","PermSAMNode","NumpermSAMNode",text= paste("Number permutations: ",SAMRes [[12]])  )
   tkdestroy(chPval)
      }

  
   OK.but <-tkbutton(buttonFrameiso ,text="   OK   ",command=OnOK)
   Cancel.but <-tkbutton(buttonFrameiso ,text=" Cancel ",command=onCancel)
   tkgrid(OK.but,Cancel.but)

   tkgrid(spec.frm)
   tkgrid(buttonFrameiso)
   tkfocus(chPval)
   }
   
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
