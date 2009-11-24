`UserPlotting` <-
function () { 


 if ( !exists("pval.Asymp") & !exists("RawPval") & !exists("SAMRes") ) 
     {     
     tkmessageBox(title="Error Message",message="The analysis have not performed yet! At least one analysis had to be performed before using this feature!", icon="error",type="ok")
    }
 else {

 object.list.asymp <- object.list.perm  <- object.list.SAM <- NULL
  
 if (exists("pval.Asymp") )  {
ExactPval.E2 <<- pval.Asymp
object.list.asymp <- c("ExactPval.E2","E2.value")
FoldChange <<- Mu.Diff.Asymp
E2.value <<- E2Val.Asymp 
   }

 if ( exists("RawPval") ) {
E2.value <<- E2Val 
Marcus.value <<- MarcusVal 
ModifiedM.value <<- ModMVal 
M.value <<- MVal 
Williams.value <<- WilliamsVal 

twosided.RawPval <<- RawPval[[2]]
PermutationPval.E2 <<- twosided.RawPval[,2]
PermutationPval.Williams <<- twosided.RawPval[,3]
PermutationPval.Marcus <<- twosided.RawPval[,4]
PermutationPval.M  <<- twosided.RawPval[,5]
PermutationPval.ModM  <<- twosided.RawPval[,6]

     object.list.perm <- c("E2.value","Marcus.value","ModifiedM.value","M.value","Williams.value",
        "PermutationPval.E2","PermutationPval.Williams","PermutationPval.Marcus","PermutationPval.M","PermutationPval.ModM")
     if (exists("meandifPerm")) {
       FoldChange <<- meandifPerm 
}
     else {
 FoldChange <<- MeanDiff (dose,exprs2Perm)
}
  }

 if (exists("SAMRes") ) {

  stat.ls <- c("M","E2","Marcus","Williams","ModifM")
  Qval.mat <- matrix(0,nrow(SAMRes[[1]]),5)
  StatSAM.mat <- matrix(0,nrow(SAMRes[[1]]),5)

  FoldChange <<- MeanDiffSAM  

  for (i in (1:length(stat.ls)))
   { 
AllFDR <- Isoallfdr(SAMRes, , stat=stat.ls[i])
Delta.table <- data.frame(AllFDR) 
Delta <- Delta.table[1,1]
qval <- Isoqval(Delta, AllFDR, SAMRes, stat=stat.ls[i])
qvalue <-  qval [[1]]
qvalue <- qvalue [order(qvalue [,1]),]
if (nrow (qvalue) > nrow(exprs2SAM) ) {
reprow <- which(table(qvalue[,1])==2)
delrow <- which(qvalue[,1]== reprow )[1]
qvalue2 <- qvalue [-delrow ,]
  }
else { qvalue2 <-  qvalue }
Qval.mat[,i] <- qvalue2 [,3]
StatSAM.mat[,i] <- qvalue2 [,2]
}

SAM.M.value <<-StatSAM.mat[,1]
SAM.E2.value <<-StatSAM.mat[,2]
SAM.Marcus.value <<-StatSAM.mat[,3]
SAM.Williams.value <<-StatSAM.mat[,4]
SAM.ModifiedM.value <<-StatSAM.mat[,5]


M.q.value <<-Qval.mat[,1]
E2.q.value <<-Qval.mat[,2]
Marcus.q.value <<-Qval.mat[,3]
Williams.q.value <<-Qval.mat[,4]
  ModifiedM.q.value <<-Qval.mat[,5]

  object.list.SAM <- c("SAM.M.value","SAM.E2.value","SAM.Marcus.value","SAM.Williams.value","SAM.ModifiedM.value",
     "ExactPval.E2","E2.q.value","M.q.value","Williams.q.value","Marcus.q.value","ModifiedM.q.value")
 
     }

  All.objects <- unique(c("FoldChange",object.list.asymp, object.list.perm ,object.list.SAM) )
UserPlot(All.objects)
       
    }
}

