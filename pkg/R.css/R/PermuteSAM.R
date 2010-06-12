`PermuteSAM` <-
function() {

if (!exists("exprs") | !exists("dose"))  
   {  
      ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
} 
else {


 samPermute <<-tktoplevel()
tkwm.title(samPermute,"Significance Analysis of Microarrays (SAM) - IsoGene GUI") 
      spec.frm <- tkframe(samPermute,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
frame1.1 <- tkframe(frame1 ,  borderwidth=2)
frame1.2 <- tkframe(frame1 ,  borderwidth=2)
frame1.3 <- tkframe(frame1 ,  borderwidth=2)
frame1.4 <- tkframe(frame1 ,  borderwidth=2)

frame2 <- tkframe(frame1 ,relief="groove",  borderwidth=2)
frame2.1 <- tkframe(frame2 ,  borderwidth=2)
frame3 <- tkframe(frame2 ,relief="groove", borderwidth=2)
frame3.1 <- tkframe(frame3 , borderwidth=2)

buttonFramesam<-tkframe(samPermute,borderwidth=2)

### Select the RAW P value  ##
      rb3 <- tkradiobutton( frame1 )
rb4 <- tkradiobutton( frame1.4 )
rbValue2 <- tclVar("new")

tkconfigure(rb3,variable=rbValue2,value="new",text="New Permutation")
tkconfigure(rb4,variable=rbValue2,value="old", text="Obtain the permutation result from a file:")
      
### Select genes ##
rb1 <- tkradiobutton( frame1.1 )
rb2 <- tkradiobutton( frame1.1)
rbValue1 <- tclVar("all")
tkconfigure(rb1,variable=rbValue1,value="all",text="All Genes")
tkconfigure(rb2,variable=rbValue1,value="range", text="Genes Range")

fromgene<- tclVar("")
FromNum<-tkentry(frame1.1,width="8",textvariable=fromgene)

togene <- tclVar("")
ToNum<-tkentry(frame1.1,width="8",textvariable=togene )

      tkgrid(rb3)
tkgrid.configure(rb3,sticky="w")
tkgrid( tklabel(frame1.1,text="Select the genes: "))
   
tkgrid(rb1)
tkgrid(rb2,tklabel(frame1.1,text="From"),FromNum,tklabel(spec.frm,text="To"),ToNum)

tkgrid.configure(frame1.1, sticky="w")

### Select Using Fudge Factor  ##
      rb5 <- tkradiobutton( frame2.1 )
rb6 <- tkradiobutton( frame2.1)
   rb7 <- tkradiobutton( frame2.1)
rbValue3 <- tclVar("no")

tkconfigure(rb5 ,variable=rbValue3 ,value="no",text="No fudge factor")
tkconfigure(rb6 ,variable=rbValue3 ,value="pooled", text="Automatic")
      tkconfigure(rb7 ,variable=rbValue3 ,value="specFudge", text="Specify percentile for S0:")


fudgeSpec <- tclVar("")
FudgeEdit <-tkentry(frame2.1,width="4",textvariable=fudgeSpec )
     labFudge <- tklabel(frame2.1,text="e.g. 5,45,100")

tkgrid.configure(tklabel(frame2.1,text="Specify Fudge Factor (S0):") , sticky="w")

tkgrid.configure(rb5 , sticky="w")
tkgrid.configure(rb6, sticky="w")

tkgrid.configure(rb7 ,FudgeEdit ,labFudge ,sticky="w")

#########

### Select the statistic value for SE and CV plot ##
rb8 <- tkradiobutton( frame3.1)
rb9 <- tkradiobutton( frame3.1)
rb10 <- tkradiobutton( frame3.1)
rb11 <- tkradiobutton( frame3.1)
rb12 <- tkradiobutton( frame3.1)
rbValue4 <- tclVar("ModM")

tkconfigure(rb8 ,variable=rbValue4 ,value="E2",text="E2")
tkconfigure(rb9,variable=rbValue4 ,value="Williams", text="Williams")
tkconfigure(rb10,variable=rbValue4 ,value="Marcus",text="Marcus")
tkconfigure(rb11,variable=rbValue4 ,value="M", text="M")    
tkconfigure(rb12,variable=rbValue4 ,value="ModM",text="M'")

tkgrid.configure(tklabel(frame3,text="Fudge plots:") , sticky="w")

lab1 <- tklabel(frame3.1,text="Statistic:")
tkgrid.configure(lab1 ,rb8 ,rb9,rb10,rb11,rb12, sticky="w") 
tkgrid.configure(frame3.1,sticky="w")
########
cb6 <- tkcheckbutton(frame3 )
cbValue6 <- tclVar("1")
tkconfigure(cb6,variable=cbValue6,text="Standar error plot")


cb8 <- tkcheckbutton(frame3 )
cbValue8 <- tclVar("0")
tkconfigure(cb8,variable=cbValue8,text="CV plot")

   ShowSEplot <- function()
  {

  rbVal1 <- as.character(tclvalue(rbValue1))
  rbVal3 <- as.character(tclvalue(rbValue3))
  fudgeVal <- as.numeric(tclvalue(fudgeSpec ))
  cbVal6 <- as.character(tclvalue(cbValue6))
  cbVal8 <- as.character(tclvalue(cbValue8))


pct <- (0:20) *10/ 2
if(rbVal3 == "specFudge"  & is.element(fudgeVal ,pct) == F) {
 tkmessageBox(title="Error Message",message="You can only input percentile value: 0, 5, 10, 15, 20,..., 
100!",icon="warning",type="ok")
}
else {

rbVal4 <- as.character(tclvalue(rbValue4))
if (rbVal4 == "E2" ) statUsed <- "E2"
if (rbVal4 == "Williams" ) statUsed <- "Williams"
if (rbVal4 == "Marcus" ) statUsed <- "Marcus"
if (rbVal4 == "M" ) statUsed <- "M"
if (rbVal4 == "ModM" ) statUsed <- "ModM"

if (rbVal1=="all") {
      exprs2SAM <- exprs
matchID <- c(1:nrow(exprs2SAM ))
}
      if (rbVal1=="range") {
       exprs2SAM <- exprs[tclvalue(fromgene):tclvalue(togene),] 
matchID <- c(tclvalue(fromgene):tclvalue(togene))
      }
  if (exists("matchIDtemp")) {
 if (setequal(matchID,matchIDtemp)== F)  {
IsoStat (dose,exprs2SAM )
fudgeRes <<- IsofudgeMod(dose,exprs2SAM, stat=statUsed ,fudge="auto")
}

 }

        else { IsoStat (dose,exprs2SAM ) 
 fudgeRes <<- IsofudgeMod(dose,exprs2SAM, stat=statUsed ,fudge="auto")

  }

if (rbVal3 == "no") { 

if (cbVal6 =="1") {
   plotSe <- function () {SEPlot (statUsed,0)}
Plot2(plotSe  ,1.5,1.5,title=paste("Windows Graph Output: Standar error plot for ",statUsed ))
}
      if (cbVal8 =="1") {
    plotCV <- function () {CVPlot (fudgeRes [[2]])}
    Plot2(plotCV ,1.5,1.5,title=paste("Windows Graph Output: CV plot ",statUsed ))
  }
      }
if (rbVal3 == "pooled") { 

if (cbVal6 =="1") {
plotSe <- function () {SEPlot (statUsed,fudgeRes [[1]])}
Plot2(plotSe  ,1.5,1.5,title=paste("Windows Graph Output: Standar error plot for ",statUsed ))
}

if (cbVal8 =="1") {
plotCV <- function () {CVPlot (fudgeRes [[2]])}
Plot2(plotCV ,1.5,1.5,title=paste("Windows Graph Output: CV plot ",statUsed ))
}
  }

if (rbVal3 == "specFudge") { 
fudgeRes <- IsofudgeMod(dose,exprs2SAM , stat=statUsed ,fudge=fudgeVal )

if (cbVal6 =="1") {
plotSe <- function () {SEPlot (statUsed,fudgeRes [[1]])}
Plot2(plotSe  ,1.5,1.5,title=paste("Windows Graph Output: Standar error plot for ",statUsed ))
}
if (cbVal8 =="1") {
plotCV <- function () {CVPlot (fudgeRes [[2]])}
Plot2(plotCV ,1.5,1.5,title=paste("Windows Graph Output: CV plot ",statUsed ))
  }
}
        matchIDtemp <<- matchID 
   }
}

  ShowSEplot.but <-tkbutton(frame3,text=" Display",command=ShowSEplot)

tkgrid.configure(cb6 , sticky="w")
tkgrid.configure(cb8 ,tklabel(frame3,text=""), ShowSEplot.but, sticky="w")

tkgrid.configure(frame2.1,frame3, sticky="w")
tkgrid.configure(frame2, sticky="w")

### Permutation ###
numperm<- tclVar("100")
NumPerm <-tkentry(frame1.2 ,width="5",textvariable=numperm)
label.numperm <- tklabel(frame1.2,text="    Number of Permutation: ")
tkgrid(label.numperm,NumPerm )
tkgrid.configure(label.numperm,sticky="w")

cb7 <- tkcheckbutton(frame1.2)
cbValue7<- tclVar("0")
tkconfigure(cb7,variable=cbValue7,text="Save the permutations result:")

      Save.but <-tkbutton(frame1.2,text="browse",command=savingRwd )
      SavefileName <<- tclVar("")
fileEdit <-tkentry(frame1.2,width="30",textvariable=SavefileName )

      tkgrid.configure(frame1.2,sticky="w")
      tkgrid.configure(cb7,Save.but,fileEdit,sticky="w")
tkgrid( tklabel(frame1,text="                "))

Calc.PermutSAM <- function () 
  {

  rbVal1 <- as.character(tclvalue(rbValue1))
  rbVal2 <<- as.character(tclvalue(rbValue2))
        NumPermval <- as.numeric(tclvalue(numperm))
       cbVal7 <- as.character(tclvalue(cbValue7))
   rbVal3 <- as.character(tclvalue(rbValue3))
    fudgeVal <<- as.numeric(tclvalue(fudgeSpec ))

  assign("NumPermval",NumPermval,envir=.GlobalEnv)
  tkconfigure(samPermute,cursor="watch")
   tclvalue(PermuteText1 ) <- " please wait..... " 
     tclvalue(PermuteText2 ) <- " "
  fudgeOpt <- 0
   if (rbVal2 == "old") { 
ReturnVal <- tkmessageBox(title="Error Message",message="You have choosen the wrong option !",icon="warning",type="ok")
  if (tclvalue(ReturnVal)== "ok") tkfocus(samPermute)
                }
  if (rbVal2 == "new") {
      if (rbVal1=="all") {
      exprs2SAM <<- exprs
matchID <<- c(1:nrow(exprs2SAM ))
}
      if (rbVal1=="range") {
       exprs2SAM <<- exprs[tclvalue(fromgene):tclvalue(togene),] 
matchID <<- c(tclvalue(fromgene):tclvalue(togene))
      }

     }
## Calculating the qqStat values ### 

   
  SAMRes <-IsoqqstatMOD(dose, exprs2SAM, fudge=rbVal3 , niter=NumPermval , seed= SeedNum )
  N <- nrow (exprs2SAM)
     
  ## specify the fudge option ##
  if (rbVal3 == "pooled") fdgopt <- "Automatic"  
  if (rbVal3 == "no") fdgopt <- "None" 
  if (rbVal3 == "specFudge") fdgopt <- paste (fudgeVal,"percentile") 


  tkdelete(treeWidget,"nodeSAM")
  try( tkdelete(treeWidget,"PermSAMNode"), silent=T)
  tkinsert(treeWidget,"end","Record4Node","PermSAMNode",text="SAM Permutations ")
  tkinsert(treeWidget,"end","PermSAMNode","NumGeneSAMNode",text= paste("Number genes analyzed: ",N )  )
  tkinsert(treeWidget,"end","PermSAMNode","FudgeNode",text= paste("Fudge Factor: ",fdgopt) )
  tkinsert(treeWidget,"end","PermSAMNode","NumpermSAMNode",text= paste("Number permutations: ",NumPermval )  )

  DirectionSAM <<- SAMRes [[11]] 
  SAMRes [[12]] <- NumPermval 
    SAMRes [[13]]  <- fdgopt 
        assign("SAMRes.temp",SAMRes ,envir=.GlobalEnv)
        assign("exprs2SAM.temp",exprs2SAM ,envir=.GlobalEnv)

   MeanDiffSAM  <<- MeanDiff (dose,exprs2SAM )
        assign("MeanDiffSAM",MeanDiffSAM  ,envir=.GlobalEnv)

        if (cbVal7=="1") 
    {
split <- strsplit(fileRwd,".",fixed=TRUE)
fileName <- paste(split[[1]][1],".RData",sep="")
save(SAMRes , file=fileName )
ReturnVal <- tkmessageBox(message="The permutation have finished and been saved",icon="info",type="ok")
      if (tclvalue(ReturnVal)== "ok") tkfocus(samPermute)
      tkconfigure(samPermute,cursor="arrow")
    }
     else {
     ReturnVal <-tkmessageBox(message="The permutation have been finished. Do you want to save the permutation?",
       icon="question",type="yesno",default="yes")

     if (tclvalue(ReturnVal)== "no") {
  tkfocus(samPermute)
   }

     if (tclvalue(ReturnVal)== "yes")
            {
    savingRwd ()
    save(SAMRes ,file=fileRwd )
    tkfocus(samPermute)
                }

     tkconfigure(samPermute,cursor="arrow")
     }
  }



Calculate.but <-tkbutton(frame1.3,text=" Run Permutation ",command=Calc.PermutSAM)
PermuteText1 <<- tclVar("     ")
label1 <- tklabel(frame1.3,text=tclvalue(PermuteText1 ))
tkconfigure(label1,textvariable=PermuteText1 )
PermuteText2 <<- tclVar("     ")
label2 <- tklabel(frame1.3,text=tclvalue(PermuteText2 ))
tkconfigure(label2,textvariable=PermuteText2 )

tkgrid.configure(Calculate.but,label1,label2,sticky="w")

tkgrid( tklabel(frame1.3,text="                "))
      
tkgrid.configure(frame1.3,sticky="w")

Open.but <-tkbutton(frame1.4 ,text="browse",command=browse2)
      pvalName <<- tclVar("       ")
pvalEdit <-tkentry(frame1.4,width="30",textvariable=pvalName )

      tkgrid(rb4,Open.but,pvalEdit )
tkgrid.configure(rb4,sticky="w")
   
tkgrid.configure(frame1.4,sticky="w")

tkgrid(frame1)

onCancel <- function()
  {
     ReturnVal <<- 0
  try(rm(SAMRes.temp,exprs2SAM.temp,envir=.GlobalEnv), silent=T)
  tkdestroy(samPermute)
}

onOK <- function()
  {


    assign("SAMRes",SAMRes.temp ,envir=.GlobalEnv)
 assign("exprs2SAM",exprs2SAM.temp ,envir=.GlobalEnv)
       if (exists ("exprs2SAM.temp")  )  try(rm(exprs2SAM.temp,envir=.GlobalEnv), silent=T)
       if (exists ("SAMRes.temp")  )  try(rm(SAMRes.temp,envir=.GlobalEnv), silent=T)

 tkdestroy(samPermute)
}
  Exit.but <-tkbutton(buttonFramesam,text=" Cancel",command=onCancel )
  Ok.but <-tkbutton(buttonFramesam,text="  Ok  ",command=onOK )

  tkgrid(Ok.but,Exit.but)
  tkgrid.configure(Ok.but,Exit.but,sticky="e")

  tkgrid(spec.frm)
  tkgrid(buttonFramesam)
 
 }
}

