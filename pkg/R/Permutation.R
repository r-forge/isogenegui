`Permutation` <-
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
      
### Select genes ##
rb1 <- tkradiobutton( frame1 )
rb2 <- tkradiobutton( frame1 )
rbValue1 <- tclVar("all")
tkconfigure(rb1,variable=rbValue1,value="all",text="All Genes")
tkconfigure(rb2,variable=rbValue1,value="range", text="Genes Range")

fromgene<- tclVar("")
FromNum<-tkentry(frame1 ,width="8",textvariable=fromgene)

togene <- tclVar("")
ToNum<-tkentry(frame1 ,width="8",textvariable=togene )

      tkgrid(rb3)
tkgrid.configure(rb3,sticky="w")
tkgrid( tklabel(frame1,text="Select the genes: "))
  
tkgrid(rb1)
tkgrid(rb2,tklabel(frame1 ,text="From"),FromNum,tklabel(spec.frm,text="To"),ToNum)

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

Calc.Permut <- function() {
 
if (!exists("exprs")&!exists("dose"))  
       {  
  ReturnVal <- tkmessageBox(title="Error Message",message="No data set loaded, please load the dataset first!",icon="info",type="ok")
  if (tclvalue(ReturnVal)== "ok") tkfocus(asym)
 } 
  else {
  tkconfigure(iso,cursor="watch")
  rbVal1 <- as.character(tclvalue(rbValue1))
  rbVal2 <- as.character(tclvalue(rbValue2))
  FDR <- as.numeric(tclvalue(FDR))
        NumPerm <<- as.numeric(tclvalue(numperm))
  assign("NumPerm",NumPerm ,envir=.GlobalEnv)

   cbVal14 <- as.character(tclvalue(cbValue14))
  tclvalue(PermuteText3 ) <- " please wait..... " 
     tclvalue(PermuteText4 ) <- " "

  if (rbVal2 == "old") { 
      ReturnVal <- tkmessageBox(title="Error Message",message="You have choosen the wrong option !",icon="info",type="ok")
      if (tclvalue(ReturnVal)== "ok") tkfocus(iso)
                }
  if (rbVal2 == "new") {
RawPval <- NULL
      if (rbVal1=="all") {
      exprs2Perm <- exprs
matchID <- c(1:nrow(exprs2Perm ))
}
    if (rbVal1=="range") {
       exprs2Perm <- exprs[tclvalue(fromgene):tclvalue(togene),] 
matchID <- c(tclvalue(fromgene):tclvalue(togene))
      }
ProbeID <- rownames(exprs2Perm )
    assign("exprs2Perm",exprs2Perm ,envir=.GlobalEnv)
    assign("ProbeID",ProbeID ,envir=.GlobalEnv)
assign("matchID",matchID ,envir=.GlobalEnv)

## Calculating the Raw P values ### 
 RawPval <- IsoRawpMod (dose, exprs2Perm , niter = NumPerm, seed = SeedNum)
 assign("RawPval",RawPval ,envir=.GlobalEnv)
 
 N <- length(ProbeID )
 try(tkdelete(treeWidget,"nodePerm") , silent = T )
 try( tkdelete(treeWidget,"RawPermNode"), silent = T )

tkinsert(treeWidget,"end","Record3Node","RawPermNode",text="Permutation (raw) P-values")
tkinsert(treeWidget,"end","RawPermNode","NumgeneNode",text=paste("Number gene analized: ",N) )
tkinsert(treeWidget,"end","RawPermNode","NumpermNode",text=paste("Number of permutations: ",NumPerm ) )


if (cbVal14 == "1") { 
              save(RawPval,file=fileRwd )
  }
     }


if (cbVal14 == "1") { 
          ReturnVal <- tkmessageBox(message="The permutation have finished and been saved",icon="info",type="ok")
    if (tclvalue(ReturnVal)== "ok") tkfocus(iso)
    tkconfigure(iso,cursor="arrow")
 }

else { 

      ReturnVal <-tkmessageBox(message="The permutation have been finished. Do you want to save the permutation?",
icon="question",type="yesno",default="yes")

##Here
if (tclvalue(ReturnVal)== "no") 
{ tkfocus(iso)
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

tkgrid( Calculate.but,label1,label2)
tkgrid( tklabel(frame1,text="                "))



      Open.but <-tkbutton(frame1 ,text="browse",command=browse)
      pvalName <<- tclVar("       ")
pvalEdit <-tkentry(frame1,width="25",textvariable=pvalName )

      tkgrid(rb4,Open.but,pvalEdit )
tkgrid.configure(rb4,sticky="sw")

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

##############
### P-values Radio button ##
####

cb6 <- tkcheckbutton(frame2.2)
cb7 <- tkcheckbutton(frame2.2)
cb8 <- tkcheckbutton(frame2.2)
cb9 <- tkcheckbutton(frame2.2)
cb10 <- tkcheckbutton(frame2.2)
cb11 <- tkcheckbutton(frame2.2)
cb12 <- tkcheckbutton(frame2.2)

cbValue6 <- tclVar("1")
cbValue7 <- tclVar("0")
cbValue8 <- tclVar("0")
cbValue9 <- tclVar("0")
cbValue10 <- tclVar("0")
cbValue11 <- tclVar("0")
cbValue12 <- tclVar("0")

tkconfigure(cb6,variable=cbValue6,text="BH")
tkconfigure(cb7,variable=cbValue7,text="BY")
tkconfigure(cb8,variable=cbValue8,text="SidakSS")
tkconfigure(cb9,variable=cbValue9,text="SidakSD")
tkconfigure(cb10,variable=cbValue10,text="Holm")
tkconfigure(cb11,variable=cbValue11,text="Hochberg")
tkconfigure(cb12,variable=cbValue12,text="Bonferroni")


      label.fdr <- tklabel(frame2.2,text="Controlling FDR Procedure: ")
tkgrid(label.fdr )
tkgrid.configure(label.fdr,sticky="w")

tkgrid(cb6,cb7)
tkgrid.configure(cb6,sticky="w")

 label.fwer <- tklabel(frame2.2,text="Controlling FWER Procedure: ")
tkgrid(label.fwer )
tkgrid.configure(label.fwer ,sticky="w")
tkgrid(cb8,cb9,cb10,cb11,cb12)
tkgrid.configure(cb8,cb8,cb9,cb10,cb11,cb12,sticky="w")


FDR <- tclVar("0.05")
FDRtxt <-tkentry(frame2.2,width="4",textvariable=FDR)

 label.FDR <- tklabel(frame2.2,text="Overall Significant Level: ")
tkgrid.configure(label.FDR ,FDRtxt  ,sticky="w")
tkgrid(label.FDR, FDRtxt  )

cb13 <- tkcheckbutton(frame2.2)
cbValue13 <- tclVar("0")
tkconfigure(cb13,variable=cbValue13,text="Display the significant genes")

tkgrid(cb13)
tkgrid.configure(cb13,sticky="w")

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
   cbVal6 <- as.character(tclvalue(cbValue6))
   cbVal7 <- as.character(tclvalue(cbValue7))
        cbVal8 <- as.character(tclvalue(cbValue8))
   cbVal9 <- as.character(tclvalue(cbValue9))
        cbVal10 <- as.character(tclvalue(cbValue10))
        cbVal11 <- as.character(tclvalue(cbValue11))
        cbVal12 <- as.character(tclvalue(cbValue12))
        cbVal13 <- as.character(tclvalue(cbValue13))

   FDRval <<- as.numeric(tclvalue(FDR))

    ### Calculating Statistics  ### 

   N <- nrow(exprs2Perm)


   ## Add info on the tree ###
         ## Here 
   try( tkdelete(treeWidget,"AdjPvalPermNode"), silent = T )
   tkinsert(treeWidget,"end","Record3Node","AdjPvalPermNode",text= paste ("Adjusted P-values", ",  FDR: ", FDRval) )

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
   proc1 <- proc3 <- proc2 <- proc4 <- proc5 <- proc6 <- proc7 <- NULL
   linecd <- matrix(NA,7,3)

   if (cbVal6=="1") {
proc1 <-("BH")
linecd[1,] <- c("BH",1,1)

}

   if (cbVal7=="1") {
proc2 <-("BY")
linecd[2,] <- c("BY",3,1)

}

   if (cbVal8=="1") {
proc3 <-("SidakSS")
linecd[3,] <- c("SidakSS",4,1)
}

   if (cbVal9=="1") {
proc4 <-("SidakSD")
linecd[4,] <- c("SidakSD",1,2)

}

   if (cbVal10=="1") {
proc5 <-("Holm")
linecd[5,] <- c("Holm",2,2)

}

   if (cbVal11=="1") {
proc6 <-("Hochberg")
linecd[6,] <- c("Hochberg",3,2)

}
   if (cbVal12=="1") {
proc7 <-("Bonferroni")
linecd[7,] <- c("Bonferroni",4,2)

}

         proc.list <- c(proc1, proc3 ,proc2, proc4,proc5 ,proc6, proc7)
         objectname <- numsig <- numsigN <- matrix(0,length(stat.list),length(proc.list))
   linecd <<- na.exclude(linecd)
   RowNumSig <- NULL
   RowNumSig <- vector("list", length(stat.list))
   MadjpALL <- vector("list", length(stat.list))

   Madjp <-matrix(0,length(matchID),length(proc.list)+1)
   listgene <- listgene2 <-NULL
         for (i in (1:length(stat.list)))
     {    
   tkinsert(treeWidget,"end","AdjPvalPermNode",paste("StatNode",stat.list[i],sep=""), text=stat.list[i] )


    for (j in (1:length(proc.list)))
  {  

       TestResult <- IsoTest (twosided.RawPval, FDRval , type = proc.list[j], stat = stat.list[i])
  Madjp[,1] <- PvalAdj(twosided.RawPval, FDRval , type = proc.list[j], stat = stat.list[i])[,1]
       Madjp[,j+1] <- PvalAdj(twosided.RawPval, FDRval , type = proc.list[j], stat = stat.list[i])[,2]
  nsig <- nrow(TestResult) 
  nsig [is.null(nsig)] <- 0

 tkinsert(treeWidget,"end",paste("StatNode",stat.list[i], sep=""),paste(stat.list[i],proc.list[j],sep="" ) ,
text= paste ( proc.list[j], ",  Sig: ",  nsig )  )

              Stat <- get (paste(stat.list[i],"Val", sep="")) 
  Result <- data.frame(TestResult[,1:2], Stat[TestResult$row.num],TestResult[,3:4],
 Mu.high_low.Diff[TestResult$row.num],Direction [TestResult$row.num]  )
  names(Result) <- c("Probe ID ", "Row Number",stat.list[i],"Raw P-values",
 paste(proc.list[j],"Adj P-values"),"Mu.high-Mu.low","Direction"  )
              rownames(Result)<- NULL
              
  objectname[i,j] <-paste("result.",stat.list[i],".",proc.list[j],sep = "" ) 
  numsig [i,j] <- paste("numsig.",stat.list[i],".",proc.list[j],sep = "" ) 
 
  numsigN [i,j] <- nsig
      assign(numsig[i,j],nsig ,envir=.GlobalEnv)
  assign(objectname[i,j],Result,envir=.GlobalEnv)

  RowNumSig[[i]] <- unique(append(RowNumSig[[i]],Result[,2] ))
  MadjpALL [[i]] <- Madjp

       }

AdjPvalALL <- MadjpALL [[i]]

### Assigning the Adjusted p value matrix to the global enviroment ###

assign(paste("PVal",stat.list[i],sep=""),AdjPvalALL ,envir=.GlobalEnv)
RowSig <- RowNumSig[[i]]
colnames(AdjPvalALL) <- c("raw p-val",paste(  proc.list,"p-val") )

AdjPvalSig <- AdjPvalALL[RowSig,-1]
if ( ncol(AdjPvalALL)> 2 ) {
sumSig <- apply(AdjPvalSig ,1,sum)
orderedRowNum <- RowSig [order(sumSig)]
}
if ( ncol(AdjPvalALL)< 3 ) {
orderedRowNum <- RowSig [order(AdjPvalSig)]
}
 

AdjPvalALL <-round(AdjPvalALL , digits = 6)
testStat <- get(paste(stat.list[i],"Val",sep=""))


row.num <- 1:length(ProbeID )
Res.PermGenes <- data.frame(ProbeID,row.num,meandifPerm ,Direction ,testStat )
names(Res.PermGenes) <- c("ProbeID ","row.num","Mean-Diff","Direction",stat.list[i])
 rownames(Res.PermGenes ) <- NULL
Res.PermGenes1 <- data.frame (Res.PermGenes ,AdjPvalALL )
Res.PermSig <- Res.PermGenes1[orderedRowNum,]
rownames(Res.PermSig) <- NULL

listgene[i] <- paste("SignGenes.Perm.",stat.list[i],".statistic",sep = "" )  
assign(listgene[i],Res.PermSig,envir=.GlobalEnv)
assign("listgene",listgene,envir=.GlobalEnv)



listgene2[i] <- paste("ResultAllGenes.Perm",stat.list[i],".statistic",sep = "" )  
assign(listgene2[i],Res.PermGenes1 ,envir=.GlobalEnv)
assign("listgene2",listgene2,envir=.GlobalEnv)



     if (cbVal13=="1")  {
  try(showData(Res.PermSig,title= paste("Significant Genes based on Permutation P-values, Test Statistic: ",
  stat.list[i],"  FDR:", FDRval ,"  N :",N )))
  }
      }
         rownum <- length(proc.list) * length(stat.list)
   numsigPerm <- matrix(0,rownum,3)
   numsigPerm [,1] <- rep(stat.list, each=length(proc.list))
   numsigPerm [,2] <- proc.list
   numsigPerm [,3] <- as.vector(t(numsigN))
   colnames(numsigPerm) <- c("Statistics", "P-value Adjustment" , " Number of Significant Genes")
   showData(numsigPerm, title= paste("Number of Significant Genes with Permutation, FDR:",FDRval , ", N:",N,"genes" ))
 
        tkconfigure(iso,cursor="arrow")
    }   


  }
  run.but <-tkbutton(frame2,text=" Proceed  ",command=PermResBut)
  tkgrid(run.but)

tkgrid(tklabel(frame2,text="                             "))

  tkgrid(frame2)

##### Frame saving the list of significant genes ####

frame2.1 <- tkframe(frame2 ,relief="groove", borderwidth=2)

savesiggene <- function() {
save.result.mult(listgene )
}
savesiggene.but <- tkbutton(frame2.1 ,text="Save significant genes",command=savesiggene)


saveallgene <- function() {
save.result.mult(listgene2 )
}
saveallgene.but <- tkbutton(frame2.1 ,text="Save results for all genes",command=saveallgene )

tkgrid.configure(savesiggene.but, saveallgene.but , sticky="w")


  tkgrid(frame2.1)

     
############


#### Frame for Producing Plots ##

    frame3 <- tkframe(spec.frm, relief="groove", borderwidth=2)
frame3.1 <- tkframe(frame3 , borderwidth=2)

label.plots <-tklabel(frame3,text="Plots: ")
tkgrid(label.plots)
tkgrid.configure(label.plots,sticky="w")

cb18 <- tkcheckbutton(frame3.1)
cb19 <- tkcheckbutton(frame3.1)
cb20 <- tkcheckbutton(frame3.1)
cb21 <- tkcheckbutton(frame3.1)
cb22 <- tkcheckbutton(frame3.1)
cbValue18 <- tclVar("0")
cbValue19 <- tclVar("1")
cbValue20 <- tclVar("0")
cbValue21 <- tclVar("0")
cbValue22 <- tclVar("0")

tkconfigure(cb18 ,variable=cbValue18,text="M")
tkconfigure(cb19 ,variable=cbValue19,text="E2")
tkconfigure(cb20 ,variable=cbValue20,text="Marcus")
tkconfigure(cb21 ,variable=cbValue21,text="Williams")
tkconfigure(cb22 ,variable=cbValue22,text="M'")

label.stat3 <- tklabel(frame3,text="Specify the statistic that you want to plot: ")
tkgrid(label.stat3 )
tkgrid.configure(label.stat3 ,sticky="w")

tkgrid(cb18 ,cb19 ,cb20 ,cb21 ,cb22 )
tkgrid.configure(cb18 ,cb19 ,cb20 ,cb21 ,cb22,sticky="w")

tkgrid(frame3.1)
tkgrid.configure(frame3.1,sticky="w")

cb15 <- tkcheckbutton(frame3)
cb16 <- tkcheckbutton(frame3)
cb17 <- tkcheckbutton(frame3)

cbValue15 <- tclVar("0")
cbValue16 <- tclVar("0")
cbValue17 <- tclVar("0")

tkconfigure(cb15,variable=cbValue15,text="Plot of the specified statistic vs its raw p-values")
tkconfigure(cb16,variable=cbValue16,text="Plot of fold change vs the specified statistic")
tkconfigure(cb17,variable=cbValue17,text="P-values plot of the specified statistic (adjust the p-value first!)")

label.plots <-tklabel(frame3,text="Specify the plot(s): ")
tkgrid(label.plots  )
tkgrid.configure(label.plots,sticky="w")
tkgrid.configure(cb15,sticky="w")
tkgrid.configure(cb16,sticky="w")
tkgrid.configure(cb17,sticky="w")
tkgrid(cb15)
tkgrid(cb16)
tkgrid(cb17)


     PlotBut <- function () {

if (!exists("twosided.RawPval"))  
     {  
           ReturnVal <- tkmessageBox(title="Error Message",message="The raw-pvalues have not calculated yet!",icon="error",type="ok")
     if (tclvalue(ReturnVal)== "ok") tkfocus(iso)
   } 
   else {

tkconfigure(iso,cursor="watch")

     cbVal15 <- as.character(tclvalue(cbValue15))
cbVal16 <- as.character(tclvalue(cbValue16))
     cbVal17 <- as.character(tclvalue(cbValue17))
cbVal18 <- as.character(tclvalue(cbValue18))
cbVal19 <- as.character(tclvalue(cbValue19))
     cbVal20 <- as.character(tclvalue(cbValue20))
cbVal21 <- as.character(tclvalue(cbValue21))
cbVal22 <- as.character(tclvalue(cbValue22))

stat6 <- stat7 <- stat8 <- stat9 <- stat10 <-  NULL

   if (cbVal18=="1") 
stat6 <-("M")

   if (cbVal19=="1") 
stat7 <-("E2")

   if (cbVal20=="1") 
stat8 <-("Marcus")

   if (cbVal21=="1") 
stat9 <-("Williams")

   if (cbVal22=="1") 
stat10 <-("ModM")

   stat.list2 <<-c(stat6 ,stat7 ,stat8 ,stat9 ,stat10 )

twosd.pvalmat <<- twosided.RawPval[,-1]
colnames(twosd.pvalmat ) <<- c("E2","Williams","Marcus","M","ModM")

if (cbVal15=="1"){
       for (i in (1:length(stat.list2)))
      {
 stat <- stat.list2[i]
 rankPlot <- function()
{
params <- par(bg="white")
pvalue <- twosd.pvalmat[,which(colnames(twosd.pvalmat ) == stat)]
      statval <- get(paste(stat,"Val",sep="")) 
x <-cbind(pvalue , statval )
colors  <- densCols(x)
  plot(x, col=colors, pch=20, lwd=3,xlab= stat,
                   ylab="-log10(Raw p-values)") 
}
      Plot2(rankPlot,1.5,1.5, title= paste("Windows Graph Output:", stat.list2[i],"vs -log10(raw p-value) Plot" ) )

           }
     }
   
if (cbVal17=="1" ){

       for (i in (1:length(stat.list2)))
        { 
    stat <- stat.list2[i]
               rankPlot <- function() {
    
     if (!exists("linecd")) {    ### Plot only Raw-Pvalue ###
       params <- par(bg="white")
       options(show.error.messages = FALSE)
 pvalue <- twosd.pvalmat[,which(colnames(twosd.pvalmat ) == stat)]
 plot((1:length(matchID)),main="P-values",xlab=" Index ",ylab="Raw P-values",ylim=c(0,1)) 
  try(lines(sort(pvalue ),col=2,  lwd=2,lty=1,type="s"))
 options(show.error.messages = T)
 }
else {  
              pval.mat <- get(paste("PVal",stat,sep="")) 
 params <- par(bg="white")
plot((1:length(matchID)),main="P-values",xlab=" Index ",ylab=paste("P-values",stat),ylim=c(0,1))
         lines(sort(pval.mat[,1]),col=2,  lwd=2,lty=1,type="s")
 
for (j in (1:ncol(pval.mat)-1))
 {
   col <- linecd[j,2]
   typel <- linecd[j,3]
   lines(sort(pval.mat[,j+1]),col=as.numeric(col),  lwd=2,lty=as.numeric(typel))
  }
abline(FDRval ,0)
metd <- linecd[,1]
col <- linecd[,2]
typel <- linecd[,3]
legend((0.7*length(matchID)), 0.5, c("Raw P-val",metd ),
      col = c(2,as.numeric(col)),  lty = c(1,as.numeric(typel)), lwd=2,  merge = TRUE )
}
 } 
 Plot2(rankPlot,1.7,1.5 ,title=paste("Windows Graph Output: Plot p-values of ",stat)   )
 
}

   }

if (cbVal16=="1") {
   for (i in (1:length(stat.list2)))
     {
 stat <- stat.list2[i]
    E2vsDiffPlot <- function () {
   params <- par(bg="white")
   options(show.error.messages = FALSE)
   statval <- get(paste(stat,"Val",sep="")) 
   x <-cbind(meandifPerm ,statval  )
   colors  <- densCols(x)
     plot (x,col=colors, pch=20, lwd=3,ylab=stat, xlab="Fold change" )
  }

      Plot2(E2vsDiffPlot ,1.5,1.5, title= paste("Windows Graph Output:", stat.list2[i],"vs Fold change Plot" ) )

           }

   }

}
tkconfigure(iso,cursor="arrow")

}

options(show.error.messages = T)

plot.but <-tkbutton(frame3,text=" Produce plots ",command=PlotBut)
tkgrid(tklabel(frame3,text="                             "),plot.but)

tkgrid(frame3)
tkgrid.configure(frame3,sticky="nsw")

############


  onExit <- function()
  {
    ReturnVal <<- 0
      tkgrab.release(iso)
tkdestroy(iso)
}



  Exit.but <-tkbutton(buttonFrameiso ,text=" Exit the analysis",command=onExit )
  tkgrid(Exit.but)
  tkgrid.configure(Exit.but,sticky="e")

  tkgrid(spec.frm)
  tkgrid(buttonFrameiso)

 }
}

