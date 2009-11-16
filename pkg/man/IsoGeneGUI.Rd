\name{IsoGeneGUI}
\alias{IsoGeneGUI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsoGeneGUI()
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
library("tcltk")
library(tkrplot)
library(Biobase)
library(multtest)
library(relimp)
library("geneplotter")  ## from BioConductor
library("RColorBrewer") ## from CRAN
library("WriteXLS")
library("gdata")
library(IsoGene)
#############################################
# Creating a main window with menu and plot #
#############################################

tt <<- tktoplevel()
tkwm.title(tt,"IsoGene GUI")
##############################

#####################################
# Making a menu for the main widget #
#####################################

topMenu <- tkmenu(tt)           # Create a menu
tkconfigure(tt, menu = topMenu) # Add it to the 'tt' window
welcome <-tkframe(tt ,borderwidth=2,width=1500,height=1000)
tclRequire("BWidget")
message <-tkframe(tt ,borderwidth=2)

fileMenu <- tkmenu(topMenu, tearoff = FALSE)
analysisMenu<- tkmenu(topMenu, tearoff = FALSE)
PlotsMenu<- tkmenu(topMenu, tearoff = FALSE)
HelpMenu<- tkmenu(topMenu, tearoff = FALSE)

loadMenu<- tkmenu(fileMenu,tearoff = FALSE)
SamMenu <- tkmenu(analysisMenu, tearoff = FALSE)
SamPlotMenu <- tkmenu(PlotsMenu, tearoff = FALSE)


tkadd(fileMenu,"cascade", label = "Open dataset", menu= loadMenu)
tkadd(loadMenu, "command", label = "R workspace", command = loadDataSet )
tkadd(loadMenu, "command", label = "Excel or Text files", command = openFile )
tkadd(fileMenu,"separator")

tkadd(fileMenu,"cascade", label = "Show dataset", command = showdata)

tkadd(fileMenu,"separator")
tkadd(fileMenu, "cascade", label = "Exit", command = exit)

tkadd(topMenu, "cascade", label = "File", menu = fileMenu)

tkadd(analysisMenu, "cascade", label = "Set Seed", command = Setseed)
tkadd(analysisMenu, "cascade", label = "Asymptotic E2", command = Asymptotic )
tkadd(analysisMenu, "cascade", label = "Permutation", command = Permutation )


tkadd(analysisMenu, "cascade", label = "Significance Analysis of Microarrays", menu = SamMenu )
tkadd(SamMenu, "command", label = "SAM Permutation", command = PermuteSAM )
tkadd(SamMenu, "command", label = "SAM Analysis", command = IsoPlotSldr )

tkadd(topMenu, "cascade", label = "Analysis", menu = analysisMenu)

tkadd(PlotsMenu, "cascade", label = "IsoPlot", command = Isoplot)
tkadd(PlotsMenu, "cascade", label = "Permutation P-values Plot", command = IsopvalPlot )
tkadd(PlotsMenu, "cascade", label = "SAM Plots", menu = SamPlotMenu)
tkadd(SamPlotMenu, "command", label = "Plot of FDR vs. Delta", command = PlotFdrDlt )
tkadd(SamPlotMenu, "command", label = "Plot of number of significant genes vs. Delta", command = PlotSigDlt )
tkadd(SamPlotMenu, "command", label = "Plot of number of FP vs. Delta", command = PlotFPDlt )
tkadd(PlotsMenu, "cascade", label = "User Defined Scatter Plot", command = UserPlotting )

tkadd(topMenu, "cascade", label = "Plots", menu = PlotsMenu)
tkadd(HelpMenu, "cascade", label = "IsoGene Help", command = onHelp)
tkadd(HelpMenu, "cascade", label = "About", command = about )
tkadd(topMenu, "cascade", label = "Help", menu = HelpMenu)


fontHeading <- tkfont.create(family="times",size=22,weight="bold",slant="italic")
fontTextLabel <- tkfont.create(family="times",size=10)

tkgrid(tklabel(welcome ,text="                             ",font=fontHeading))
tkgrid(tklabel(welcome ,text="    Welcome to IsoGene GUI   ",font=fontHeading))
tkgrid(tklabel(welcome ,text=" A Graphical User Interface for dose response analysis in microarray experiments",font=fontTextLabel))
#tkgrid(tklabel(welcome ,text=" contact: setia.pramana@uhasselt.be",font=fontTextLabel))
tkgrid(tklabel(welcome ,text="                              ",font=fontHeading))

SeedNum <<- 1234
tkgrid(welcome )

yScr       <- tkscrollbar(tt,command=function(...)tkyview(treeWidget,...))
treeWidget <<- tkwidget(tt,"Tree",yscrollcommand=function(...)tkset(yScr,...),width=50,height=15)

tkgrid(tklabel(tt ,text=" Infobox  ",font=fontTextLabel))
tkgrid(treeWidget,yScr)
tkgrid.configure(yScr,stick="nsw")


# Insert at the end of the nodes in "root" a new node, called
# "Record1Node", which displays the text "Record 1", etc.

tkinsert(treeWidget,"end","root","Record1Node",text="Data")
tkinsert(treeWidget,"end","root","Record2Node",text="Asymptotic E2")
tkinsert(treeWidget,"end","root","Record3Node",text="Permutation ")
tkinsert(treeWidget,"end","root","Record4Node",text="SAM")


tkinsert(treeWidget,"end","Record1Node","data",text="Not Available")
tkinsert(treeWidget,"end","Record2Node","nodeAsymp",text="Not Available")
tkinsert(treeWidget,"end","Record3Node","nodePerm",text="Not Available")
tkinsert(treeWidget,"end","Record4Node","nodeSAM",text="Not Available")

tkgrid(tklabel(tt,text="") )

tkwait.window(tt)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
