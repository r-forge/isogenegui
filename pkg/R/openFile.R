`openFile` <-
function() 
{

openfile <-tktoplevel()
tkwm.title(openfile ,"Open File") 
tkgrab.set(openfile )
tkfocus(openfile )

spec.frm <- tkframe(openfile ,borderwidth=2)


frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)

cb1 <- tkcheckbutton(frame2)
	cbValue1 <- tclVar("1")
	tkconfigure(cb1, variable=cbValue1, text="Header")
cb2 <- tkcheckbutton(frame2)
	cbValue2 <- tclVar("1")
	tkconfigure(cb2, variable=cbValue2, text="Gene names in first column")
	
      tkgrid(cb1, sticky="w")
      tkgrid(cb2, sticky="w")


getExprs <- function() {
 fileName <- tclvalue(tkgetOpenFile(filetypes=
        gettext('{"text" {".txt"}} {"Excel Files" {".xlsx"}}
{"All Files" {"*"}}')))
tclvalue(fileExprs) <<- fileName
}


frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
buttonFrame <-tkframe(openfile ,borderwidth=2)

      Browse.but1 <-tkbutton(frame1,text="browse",command=getDose )
      Browse.but2 <-tkbutton(frame1,text="browse",command=getExprs)
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

tkgrid(frame2)


OnOK <- function()
{

dataHeader <- as.character(tclvalue(cbValue1))
dataGeneNames <- as.character(tclvalue(cbValue2))

fileName <- tclvalue(fileExprs)
if (!nchar(fileName)) 
{
    tkmessageBox(message = "No file was selected!")
} 
else 
{
splitted <- strsplit(fileName,".",fixed=TRUE)
if(unlist(splitted)[2] == "txt")
{
  if (dataHeader=="1" & dataGeneNames =="1"){
	exprs.temp <- read.table(file=fileName,header=TRUE)
	exprs <- data.frame(exprs.temp [,-1])
	rownames(exprs ) <- exprs.temp [,1]
  }
  if (dataHeader=="1" & dataGeneNames =="0"){
	exprs <- read.table(file=fileName,header=TRUE)
  }
  if (dataHeader=="0" & dataGeneNames =="1"){
	exprs.temp <- read.table(file=fileName,header=FALSE)
	exprs <- data.frame(exprs.temp [,-1])
	rownames(exprs ) <- exprs.temp [,1]
  }
  if (dataHeader=="0" & dataGeneNames =="0"){
	exprs <- read.table(file=fileName,header=FALSE)
  }
assign("exprs",exprs,envir=.GlobalEnv)
}
else
{ 
if(unlist(splitted)[2] == "xls" | unlist(splitted)[2] == "xlsx")
{
  if (dataHeader=="1" & dataGeneNames =="0"){
	exprs1 <- read.xlsx2(fileName, 1, header=TRUE)
	ncols <- ncol(exprs1)
	exprs<- read.xlsx2(fileName, 1, header =TRUE, colClasses=rep("numeric", ncols))
  }
  if (dataHeader=="0" & dataGeneNames =="1"){
	exprs1 <- read.xlsx2(fileName, 1, header=FALSE)
	ncols <- ncol(exprs1)
	exprs.temp <- read.xlsx2(fileName, 1, header =FALSE, colClasses=c("character",rep("numeric", ncols -1)))
	exprs <- data.frame(exprs.temp [,-1])
	rownames(exprs ) <- exprs.temp [,1]
  }
  if (dataHeader=="0" & dataGeneNames =="0"){
	exprs1 <- read.xlsx2(fileName, 1, header=FALSE)
	ncols <- ncol(exprs1)
	exprs<- read.xlsx2(fileName, 1, header =FALSE, colClasses=rep("numeric", ncols))
  }
 if (dataHeader=="1" & dataGeneNames =="1"){
	exprs1 <- read.xlsx2(fileName, 1, header=TRUE)
	ncols <- ncol(exprs1)
	exprs.temp <- read.xlsx2(fileName, 1, header =TRUE, colClasses=c("character",rep("numeric", ncols -1)))
	exprs <- data.frame(exprs.temp [,-1])
	rownames(exprs ) <- exprs.temp [,1]
  }
assign("exprs",exprs,envir=.GlobalEnv)
}
else {
tkmessageBox(message = "The files is not either txt or Excel file")
}
}
   }

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

