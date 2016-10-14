`PermutationVec` <-
function() {


if (!exists("exprs") | !exists("dose"))  
   {  
      ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
} 
else {


 iso<-tktoplevel()

tkwm.title(iso,"Permutation Analysis - IsoGene GUI") 

      spec.frm <- tkframe(iso,borderwidth=2)
frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
buttonFrameiso <-tkframe(iso,borderwidth=2)

### Select the RAW P value  ##
rb3 <- tkradiobutton( frame1 )
rb4 <- tkradiobutton( frame1 )
rbValue2 <- tclVar("new")

tkconfigure(rb3,variable=rbValue2,value="new",text="New Permutation")
tkconfigure(rb4,variable=rbValue2,value="old", text="Obtain the raw p-value from a file:")
      

rbValue1 <- tclVar("all")
#tkconfigure(rb1,variable=rbValue1,value="all",text="Only one sample")
#tkconfigure(rb2,variable=rbValue1,value="range", text="Genes Range")

fromgene<- tclVar(1)
FromNum<-tkentry(frame1 ,width="8",textvariable=fromgene)

togene <- tclVar(1)
ToNum<-tkentry(frame1 ,width="8",textvariable=togene )


### Permutation ###
numperm<- tclVar("100")
NumPerm <-tkentry(frame1 ,width="5",textvariable=numperm)
label.numperm <- tklabel(frame1 ,text="      Number of Permutation: ")
tkgrid(label.numperm,NumPerm )
tkgrid.configure(label.numperm,sticky="w")

cb14 <- tkcheckbutton(frame1)
cbValue14<- tclVar("0")
tkconfigure(cb14,variable=cbValue14,text="Save the permutations result:")

      Save.but <-tkbutton(frame1,text="browse",command=savingRwd )
      SavefileName <<- tclVar("")
SavefileEdit <-tkentry(frame1,width="35",textvariable=SavefileName)

      tkgrid(cb14,Save.but,SavefileEdit )

#FDR <- tclVar("0.05")
#FDRtxt <-tkentry(frame1,width="4",textvariable=FDR)

#label.FDR <- tklabel(frame1,text="Overall Significant Level: ")
#tkgrid.configure(label.FDR ,FDRtxt  ,sticky="w")
#tkgrid(label.FDR, FDRtxt)


Calc.Permut <- function() {
 
if (!exists("exprs")&!exists("dose"))  
       {  
  ReturnVal <- tkmessageBox(title="Error Message",message="No data set loaded, please load the dataset first!",icon="info",type="ok")
  if (tclvalue(ReturnVal)== "ok") tkfocus(asym)
 } else {
  tkconfigure(iso,cursor="watch")
  rbVal1 <- as.character(tclvalue(rbValue1))
  #rbVal2 <- as.character(tclvalue(rbValue2))
  #FDR <- as.numeric(tclvalue(FDR))
  NumPerm <<- as.numeric(tclvalue(numperm))
  assign("NumPerm",NumPerm ,envir=.GlobalEnv)

   cbVal14 <- as.character(tclvalue(cbValue14))
 # tclvalue(PermuteText3) <- " please wait..... " 
   #  tclvalue(PermuteText4 ) <- " "

  
  RawPval <- NULL
 # temp1 <- rbind(exprs,exprs)
  #temp2 <- temp1[1,,drop=FALSE]
#  exprs2Perm <- unname(temp2)

exprs2Perm <- t(exprs)
matchID <- c(1:nrow(exprs2Perm))
ProbeID <- rownames(exprs2Perm)
assign("exprs2Perm",exprs2Perm ,envir=.GlobalEnv)
assign("ProbeID",ProbeID ,envir=.GlobalEnv)
assign("matchID",matchID ,envir=.GlobalEnv)

RawPval <- IsoRawpModVec(dose, exprs2Perm , niter = NumPerm, seed = SeedNum)
assign("RawPval",RawPval ,envir=.GlobalEnv)

N <- 1#length(ProbeID)
try(tkdelete(treeWidget,"nodePerm") , silent = T )
try( tkdelete(treeWidget,"RawPermNode"), silent = T )

tkinsert(treeWidget,"end","Record3Node","RawPermNode",text="Permutation (raw) P-values")
#tkinsert(treeWidget,"end","RawPermNode","NumgeneNode",text=paste("Number gene analized: ",N) )
tkinsert(treeWidget,"end","RawPermNode","NumpermNode",text=paste("Number of permutations: ",NumPerm ) )
 
if (cbVal14 == "1") { 
              save(RawPval,file=fileRwd )
  }

if (cbVal14 == "1") { 
          ReturnVal <- tkmessageBox(message="The permutation have finished and been saved",icon="info",type="ok")
    if (tclvalue(ReturnVal)== "ok") tkfocus(iso)
    tkconfigure(iso,cursor="arrow")
 } else { 

      ReturnVal <-tkmessageBox(message="The permutation have been finished. Do you want to save the permutation?",
icon="question",type="yesno",default="yes")

##Here
if (tclvalue(ReturnVal)== "no") 
{ 
  tkfocus(iso)
 tclvalue(PermuteText3 ) <- " "  }

if (tclvalue(ReturnVal)== "yes")
        {
 savingRwd ()
 save(RawPval,file=fileRwd )
 tclvalue(PermuteText3 ) <- "  " 
 tkfocus(iso)
            }
tkconfigure(iso,cursor="arrow")

}


 }
 }

Calculate.but <-tkbutton(frame1,text=" Run Permutation ",command=Calc.Permut)
#tkgrid( Calculate.but)

PermuteText3 <<- tclVar("     ")
label1 <- tklabel(frame1,text=tclvalue(PermuteText3 ))
tkconfigure(label1,textvariable=PermuteText3 )
PermuteText4 <<- tclVar("     ")
label2 <- tklabel(frame1,text=tclvalue(PermuteText4 ))
tkconfigure(label2,textvariable=PermuteText4 )

#tkgrid( Calculate.but,label1,label2)
tkgrid(Calculate.but)
tkgrid( tklabel(frame1,text="                "))

Open.but <-tkbutton(frame1 ,text="browse",command=browse)
pvalName <<- tclVar("       ")
pvalEdit <-tkentry(frame1,width="25",textvariable=pvalName )

#tkgrid(rb4,Open.but,pvalEdit )
#tkgrid.configure(rb4,sticky="sw")


tkgrid.configure(frame1,sticky="nsw")
tkgrid(frame1)

############# Choose the Test Statistic #########

### Statistics Check button ##

frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)
frame2.1 <- tkframe(frame2 , borderwidth=2)
frame2.2 <- tkframe(frame2 , borderwidth=2)

tkgrid.configure(frame2.1,sticky="w")
tkgrid.configure(frame2.2,sticky="w")

cb1 <- tkcheckbutton(frame2.1)
cb2 <- tkcheckbutton(frame2.1)
cb3 <- tkcheckbutton(frame2.1)
cb4 <- tkcheckbutton(frame2.1)
cb5 <- tkcheckbutton(frame2.1)
cbValue1 <- tclVar("0")
cbValue2 <- tclVar("1")
cbValue3 <- tclVar("0")
cbValue4 <- tclVar("0")
cbValue5 <- tclVar("0")

tkconfigure(cb1,variable=cbValue1,text="M")
tkconfigure(cb2,variable=cbValue2,text="E2")
tkconfigure(cb3,variable=cbValue3,text="Marcus")
tkconfigure(cb4,variable=cbValue4,text="Williams")
tkconfigure(cb5,variable=cbValue5,text="M'")

label.stat <- tklabel(frame2.1,text="Statistic: ")
tkgrid(label.stat )
tkgrid.configure(label.stat ,sticky="w")

tkgrid(cb1,cb2,cb3,cb4,cb5)
tkgrid.configure(cb1,cb2,cb3,cb4,cb5,sticky="w")

tkgrid.configure(frame2,sticky="nsw")
tkgrid.configure(frame2.1,sticky="w")


FDR <- tclVar("0.05")
FDRtxt <-tkentry(frame2.2,width="4",textvariable=FDR)

 label.FDR <- tklabel(frame2.2,text="Overall Significant Level: ")
tkgrid.configure(label.FDR ,FDRtxt  ,sticky="w")
tkgrid(label.FDR, FDRtxt  )

#cb13 <- tkcheckbutton(frame2.2)
cbValue13 <- tclVar("1")
#tkconfigure(cb13,variable=cbValue13,text="Display result")

#tkgrid(cb13)
#tkgrid.configure(cb13,sticky="w")

#####

PermResBut <- function () {
  if (!exists("RawPval"))  
   {  
             ReturnVal <- tkmessageBox(title="Error Message",message="The raw-pvalues have not calculated yet!",icon="info",type="ok")
       if (tclvalue(ReturnVal)== "ok") tkfocus(iso)
      } 
   else {
   tkconfigure(iso,cursor="watch")

  # Calculating foldchange ##
N <- nrow(exprs2Perm ) 
 meandifPerm <<- MeanDiff (dose,exprs2Perm)
     

      Mu.high_low.Diff <- meandifPerm 

assign("Mu.high_low.Diff",Mu.high_low.Diff , envir=.GlobalEnv)

       ### Split the p-values ####
         twosided.RawPval <- RawPval[[2]]
         assign("twosided.RawPval",twosided.RawPval,envir=.GlobalEnv)
   

   cbVal1 <- as.character(tclvalue(cbValue1))
   cbVal2 <- as.character(tclvalue(cbValue2))
   cbVal3 <- as.character(tclvalue(cbValue3))
   cbVal4 <- as.character(tclvalue(cbValue4))
   cbVal5 <- as.character(tclvalue(cbValue5))
#    cbVal6 <- as.character(tclvalue(cbValue6))
#    cbVal7 <- as.character(tclvalue(cbValue7))
#         cbVal8 <- as.character(tclvalue(cbValue8))
#    cbVal9 <- as.character(tclvalue(cbValue9))
#         cbVal10 <- as.character(tclvalue(cbValue10))
#         cbVal11 <- as.character(tclvalue(cbValue11))
#         cbVal12 <- as.character(tclvalue(cbValue12))
        cbVal13 <- as.character(tclvalue(cbValue13))

   FDRval <<- as.numeric(tclvalue(FDR))

    ### Calculating Statistics  ### 

   N <- nrow(exprs2Perm)


   ## Add info on the tree ###
#          ## Here 
#    try( tkdelete(treeWidget,"AdjPvalPermNode"), silent = T )
#    tkinsert(treeWidget,"end","Record3Node","AdjPvalPermNode",text= paste ("Adjusted P-values", ",  FDR: ", FDRval) )

     stat1 <- stat2 <- stat3 <- stat4 <- stat5 <- NULL

   if (cbVal1=="1") 
stat1 <-("M")

   if (cbVal2=="1") 
stat2 <-("E2")

   if (cbVal3=="1") 
stat3 <-("Marcus")

   if (cbVal4=="1") 
stat4 <-("Williams")

   if (cbVal5=="1") 
stat5 <-("ModM")

   stat.list <-c(stat1,stat2,stat3,stat4,stat5)

Res.PermSig_temp <- IsoGenem(dose, exprs2Perm)

Direction <- Res.PermSig_temp$direction

if(Res.PermSig_temp$direction == "u"){
  
  E_val <- Res.PermSig_temp$E2.up
  W_val <- Res.PermSig_temp$Williams.up
  Mar_val <- Res.PermSig_temp$Marcus.up
  M_val <- Res.PermSig_temp$M.up
  ModM_val <- Res.PermSig_temp$ModM.up
  
} else if(Res.PermSig_temp$direction == "d"){
  
  E_val <- Res.PermSig_temp$E2.dn
  W_val <- Res.PermSig_temp$Williams.dn
  Mar_val <- Res.PermSig_temp$Marcus.dn
  M_val <- Res.PermSig_temp$M.dn
  ModM_val <- Res.PermSig_temp$ModM.dn
}

val.list <- c(M_val,E_val,Mar_val,W_val,ModM_val)
names(val.list) <- c("M","E2","Marcus","Williams","M'")

pval.list <- twosided.RawPval[c("M","E2","Marcus","Williams","ModM")]
#names(pval.list) <- c("M","E2","Marcus","Williams","M'")

         for (i in (1:length(stat.list)))
     {    
 #  tkinsert(treeWidget,"end","AdjPvalPermNode",paste("StatNode",stat.list[i],sep=""), text=stat.list[i] )

           testStat <- get(paste(stat.list[i],"Val",sep=""))
           
           test_pval <- pval.list[match(stat.list[i],names(pval.list))]
           
           
           Res.PermGenes <- data.frame(meandifPerm ,Direction ,testStat )
           names(Res.PermGenes) <- c("Mean-Diff","Direction",stat.list[i])
           
           Res.PermSig <- data.frame (Res.PermGenes,test_pval)
           names(Res.PermSig) <- c(names(Res.PermGenes),"p-Value")
           
                   

     if (cbVal13=="1")  {
  try(showData(Res.PermSig,title= paste("Result based on Permutation P-values, Test Statistic: ",
  stat.list[i],"  FDR:", FDRval ,"  N :",N )))
  }
      }
   # showData(numsigPerm, title= paste("Number of Significant Genes with Permutation, FDR:",FDRval , ", N:",N,"genes" ))
 
        tkconfigure(iso,cursor="arrow")
    }   


  }
  run.but <-tkbutton(frame2,text=" Proceed  ",command=PermResBut)
  tkgrid(run.but)

tkgrid(tklabel(frame2,text="                             "))

  tkgrid(frame2)

##### Frame saving the list of significant genes ####

#frame2.1 <- tkframe(frame2 ,relief="groove", borderwidth=2)

#savesiggene <- function() {
#save.result.mult(listgene )
#}
#savesiggene.but <- tkbutton(frame2.1 ,text="Save significant genes",command=savesiggene)


#saveallgene <- function() {
#save.result.mult(listgene2 )
#}
#saveallgene.but <- tkbutton(frame2.1 ,text="Save results for all genes",command=saveallgene )

#tkgrid.configure(savesiggene.but, saveallgene.but , sticky="w")


#  tkgrid(frame2.1)

  onExit <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(iso)
tkdestroy(iso)
}



  Exit.but <-tkbutton(buttonFrameiso ,text="Exit the analysis",command=onExit )
  tkgrid(Exit.but)
  tkgrid.configure(Exit.but,sticky="e")

  tkgrid(spec.frm)
  tkgrid(buttonFrameiso)

 }


}
