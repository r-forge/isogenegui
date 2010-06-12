`loadDataSet` <-
function() {

    initdir <- system.file("doc",package="IsoGeneGUI")
     initdir <- paste(initdir,"/exampleData", sep="")
    file <- tclvalue(tkgetOpenFile(filetypes=
        gettext('{"R Data Files" {".rda" ".Rdata" ".RDA"}} {"All Files" {"*"}}'), initialdir =initdir   ) )


   assign("file",file,envir=.GlobalEnv)
    
if (file == "")
 { 
  tkmessageBox(message = "No file was selected!")
return()
}
else {
      dataSet <- load(file)
    assign("dataSet",dataSet ,envir=.GlobalEnv)
chdat <-tktoplevel()
tkwm.deiconify(tt )
tkwm.title(chdat ,"Data") 
tkgrab.set(chdat )
tkfocus(chdat )
      spec.frm <- tkframe(chdat ,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
buttonFrameiso <-tkframe(chdat ,borderwidth=2)

      scr <- tkscrollbar( spec.frm , repeatinterval=2, command=function(...)tkyview(tl,...))

      tl<-tklistbox(spec.frm,height=5,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")

for (i in (1:length(dataSet)))
{
    tkinsert(tl,"end",dataSet[i])
}
tkselection.set(tl,0)

lab0 <- tklabel(spec.frm,text="  ")

Name1 <- tclVar("-- None --")
lab1 <- tklabel(frame1,text="Dose")
edit1 <-tkentry(frame1,width="20",textvariable=Name1)
assignVariable1 <- function()
{
    fruitIndex <- as.integer(tkcurselection(tl))
       tclvalue(Name1) <- dataSet[as.numeric(tkcurselection(tl))+1]
}
assignVariable.but1 <- tkbutton(frame1,text="<--",command=assignVariable1)
tkgrid(lab1,edit1,tklabel(spec.frm,text="  "),assignVariable.but1)

Name2 <- tclVar("-- None --")
lab2 <- tklabel(frame1,text="Gene Expression")
edit2 <-tkentry(frame1,width="20",textvariable=Name2)
assignVariable2 <- function()
{
    fruitIndex <- as.integer(tkcurselection(tl))
       tclvalue(Name2) <- dataSet[as.numeric(tkcurselection(tl))+1] 
}
assignVariable.but2 <- tkbutton(frame1,text="<--",command=assignVariable2)
tkgrid(lab2,edit2,tklabel(spec.frm,text="  "),assignVariable.but2)
tkgrid(frame1,lab0,lab0,tl,scr)
tkgrid.configure(tl,sticky="nse")
tkgrid.configure(scr,rowspan=10,sticky="nsw")


    ######
   OnOK <- function()
{
dose <- get(tclvalue(Name1))
exprs <- get(tclvalue(Name2))

    assign("dose",dose ,envir=.GlobalEnv)
    assign("exprs",exprs,envir=.GlobalEnv)
  assign("dataSet",c("Dose","Expression") ,envir=.GlobalEnv)
N.Gene <- nrow(exprs)
N.arry <- ncol(exprs)
N.dose <- length(unique(dose))
try( tkdelete(treeWidget,"data"), silent = T )
try( tkdelete(treeWidget,"dataNode"), silent = T )
tkinsert(treeWidget,"end","Record1Node","dataNode",text= "Available")
tkinsert(treeWidget,"end","dataNode","geneNode",text=paste("Genes: ",N.Gene))
tkinsert(treeWidget,"end","dataNode","arrayNode",text=paste("Arrays:",N.arry) )
tkinsert(treeWidget,"end","dataNode","doseNode",text=paste("Dose: ",N.dose,"levels"))
msg <- paste("You have assigned dose object : ",tclvalue(Name1),", gene expresion object:",tclvalue(Name2),sep="")
      ReturnVal <- tkmessageBox(title="Message:",message=msg ,icon="info",type="ok")
tkdestroy(chdat)
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
 }

   onCancel <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(chdat )
tkdestroy(chdat )
tkwm.deiconify(tt ) 
       tkfocus(tt )
 }

   OK.but <-tkbutton(buttonFrameiso ,text="   OK   ",command=OnOK)
   Cancel.but <-tkbutton(buttonFrameiso ,text=" Cancel ",command=onCancel)
   tkgrid(OK.but,Cancel.but)

   tkgrid(spec.frm)
   tkgrid(buttonFrameiso)
   tkfocus(chdat)

   }

}

