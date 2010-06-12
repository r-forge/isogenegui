`browse` <-
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
      PvalDataSet <- load(file)
    assign("PvalDataSet",PvalDataSet ,envir=.GlobalEnv)
      
chPval <-tktoplevel()
tkwm.title(chPval ,"Raw P-value") 
tkgrab.set(chPval )
tkfocus(chPval )
      spec.frm <- tkframe(chPval ,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
buttonFrameiso <-tkframe(chPval ,borderwidth=2)

      scr <- tkscrollbar( spec.frm , repeatinterval=2, command=function(...)tkyview(tl,...))

      tl<-tklistbox(spec.frm,height=5,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")

for (i in (1:length(PvalDataSet )))
{
    tkinsert(tl,"end",PvalDataSet[i])
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
     rawPvalChoice <- PvalDataSet[as.numeric(tkcurselection(tl))+1]
tclvalue(pvalName) <<- rawPvalChoice 

RawPval <- get(rawPvalChoice)
    assign("RawPval",RawPval,envir=.GlobalEnv)
      tryCatch(rawpvalID <- RawPval[[1]][,1], error= function (x) x=wrong())
      wrong <- function() {
msg <- ("Wrong data stucture")
    tkmessageBox(message=msg)
options(warn = -1)
         onCancel()
} 
try(rawpvalID <- RawPval[[1]][,1])
      exprsID <- rownames(exprs)
      matchID <- match (rawpvalID ,exprsID )
assign("matchID",matchID ,envir=.GlobalEnv)
exprs2Perm <- exprs[matchID,] 
assign("exprs2Perm",exprs2Perm ,envir=.GlobalEnv)
ProbeID <- rownames(exprs2Perm )
assign("ProbeID",ProbeID ,envir=.GlobalEnv)

Nperm <- RawPval$niter
N <- length(matchID )

StatVal  <<- IsoGenem(dose, exprs2Perm )
CalcStat()
try(tkdelete(treeWidget,"nodePerm") , silent = T )
try( tkdelete(treeWidget,"RawPermNode"), silent = T )

tkinsert(treeWidget,"end","Record3Node","RawPermNode",text="Permutation (raw) P-values")
tkinsert(treeWidget,"end","RawPermNode","NumgeneNode",text=paste("Number gene analized: ",N) )
tkinsert(treeWidget,"end","RawPermNode","NumpermNode",text=paste("Number of permutations: ",Nperm ) )


## here ##
      if (is.na(matchID[1])){ 
  msg <- ("The genes are different with the one in data set")
    tkmessageBox(message=msg)
         onCancel() 
}

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

