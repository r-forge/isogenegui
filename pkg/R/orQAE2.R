`orQAE2` <- function(){
# Permutation test for E2 

if (!exists("exprs") | !exists("dose")) {  
    ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
	if (tclvalue(ReturnVal)== "ok") {tkfocus(tt)}
} else {


  orqa <- tktoplevel()
  tkwm.title(orqa, "Global Likelihood Ratio Test (E2) Analysis: orQA") 

  spec.frm <- tkframe(orqa, borderwidth=2)
  frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
  buttonFrameasym <-tkframe(orqa, borderwidth=2)
	
  ## Select genes and calculate
  rb1 <- tkradiobutton(frame1)
  rb2 <- tkradiobutton(frame1)
  rbValue1 <- tclVar("all")
  tkconfigure(rb1, variable=rbValue1, value="all", text="All Genes")
  tkconfigure(rb2, variable=rbValue1, value="range", text="Genes Range")

  fromgene <- tclVar("")
  FromNum <- tkentry(frame1, width="7", textvariable=fromgene)

  togene <- tclVar("")
  ToNum <- tkentry(frame1, width="7", textvariable=togene)

  if (!exists("exprs")) {
	  valperm <- "1000"
  } else {
	  valperm <- as.character(nrow(exprs)*50)
  }
  
  numberPerm <- tclVar(valperm)
  numberPermut <- tkentry(frame1, width="7", textvariable=numberPerm)
    
  Calc.orQA <- function (){
	if (!exists("exprs")&!exists("dose")) {  
	  ReturnVal <- tkmessageBox(title="Error Message", message="No data set loaded, please load the dataset first!", icon="info", type="ok")
	  if (tclvalue(ReturnVal)== "ok") {tkfocus(orqa)}
	} else {
	  tkconfigure(orqa, cursor="watch")
	
      rbVal1 <- as.character(tclvalue(rbValue1))

      if (rbVal1=="all") {
 	    exprs2orqa <<- exprs 
	    matchID  <- c(1:nrow(exprs2orqa))
	  }
      if (rbVal1=="range") {
        exprs2orqa <- exprs [tclvalue(fromgene):tclvalue(togene),] 
	    matchID  <- c(tclvalue(fromgene):tclvalue(togene))
	    }

      assign("matchIDorqa", matchID, envir=.GlobalEnv)

	# foldchange ##
	ind <- rep(1, length(dose))
	doseCat <- as.numeric(factor(dose,labels=seq(1,length(unique(dose)),by=1)))
	res.orQA <<-  e2test(data=as.matrix(exprs2orqa), doseCat, B=as.numeric(tclvalue(numberPerm)), rep=ind)
	pval.orQA  <<- res.orQA$unadj
 	ProbeID <<- rownames(exprs2orqa)
	Mu.Diff <<-  MeanDiff(dose, exprs2orqa)
	#E2Val.Asymp <<- res.Asymp[,1]
	N <- length(ProbeID)
    
	ReturnVal <- tkmessageBox(message="The calculation of the p-values have been finished", icon="info", type="ok")
	if (tclvalue(ReturnVal)== "ok") {tkfocus(orqa)}
	tkconfigure(orqa, cursor="arrow")
	
	try(tkdelete(treeWidget,"nodeORQA"), silent = TRUE)
	try(tkdelete(treeWidget,"orQAPvalNode"), silent = TRUE)
	tkinsert(treeWidget, "end", "Record5Node", "orQAPvalNode", text="Raw P-values")
	tkinsert(treeWidget, "end", "orQAPvalNode", "NumGeneNodeORQA", text=paste("Number gene analyzed: ",N) )
    }
  }

  Calculate.but <- tkbutton(frame1, text=" Calculate ", command=suppressMessages(Calc.orQA))

  lab1 <- tklabel(frame1,text="Calculate the p-values for E2 ")
  lab2 <- tklabel(frame1,text="Select the genes: ")

  tkgrid.configure(lab1,sticky="w")
  tkgrid.configure(lab2,sticky="w")
  tkgrid(lab1)
  tkgrid(lab2)
  tkgrid.configure(rb1, sticky="w")
  tkgrid(rb1)
  tkgrid(rb2, tklabel(frame1, text="From"), FromNum, tklabel(frame1 ,text="To"), ToNum)
  tkgrid.configure(rb2, sticky="w")
  
  tkgrid(tklabel(frame1, text="Number of permutations: "), numberPermut, tklabel(frame1, text="          "), Calculate.but)
  
  tkgrid(frame1)
  tkgrid.configure(frame1, sticky="nsw")


  ## Frame Adjusting the raw p values 

  frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)

  cb1 <- tkcheckbutton(frame2)
  cb2 <- tkcheckbutton(frame2)
  cb3 <- tkcheckbutton(frame2)
  cb4 <- tkcheckbutton(frame2)
  cb5 <- tkcheckbutton(frame2)
  cb6 <- tkcheckbutton(frame2)
  cb7 <- tkcheckbutton(frame2)

  cbValue1 <- tclVar("1")
  cbValue2 <- tclVar("0")
  cbValue3 <- tclVar("0")
  cbValue4 <- tclVar("0")
  cbValue5 <- tclVar("0")
  cbValue6 <- tclVar("0")
  cbValue7 <- tclVar("0")


  tkconfigure(cb1, variable=cbValue1, text="BH")
  tkconfigure(cb2, variable=cbValue2, text="BY")
  tkconfigure(cb3, variable=cbValue3, text="SidakSS")
  tkconfigure(cb4, variable=cbValue4, text="SidakSD")
  tkconfigure(cb5, variable=cbValue5, text="Holm")
  tkconfigure(cb6, variable=cbValue6, text="Hochberg")
  tkconfigure(cb7, variable=cbValue7, text="Bonferroni")

  label.pvalAdj <- tklabel(frame2, text="P-value adjustment: ")
  tkgrid(label.pvalAdj)
  tkgrid.configure(label.pvalAdj, sticky="w")

  tkgrid(cb1, cb2, cb3, cb4)	
  tkgrid(cb5, cb6, cb7)	

  tkgrid.configure(cb1, cb2, cb3, cb4, cb5, cb6, cb7, sticky="w")

  FDR <- tclVar("0.05")
  FDRtxt <- tkentry(frame2, width="5", textvariable=FDR)
  tkgrid(tklabel(frame2, text="Overall Significant Level: "), FDRtxt)

  cb10 <- tkcheckbutton(frame2)
  cbValue10 <- tclVar("0")
  tkconfigure(cb10, variable=cbValue10, text="Display the significant genes")
  tkgrid(cb10)	

  AdjustBut <- function(){
    if (!exists("pval.orQA")) {  
      ReturnVal <- tkmessageBox(title="Error Message", message="The raw-pvalues have been not calculated yet!",
			  icon="info", type="ok")
	  if (tclvalue(ReturnVal) == "ok") {tkfocus(orqa)}
	} else {	
	  tkconfigure(orqa,cursor="watch")

	  cbVal1 <- as.character(tclvalue(cbValue1))
	  cbVal2 <- as.character(tclvalue(cbValue2))
	  cbVal3 <- as.character(tclvalue(cbValue3))
	  cbVal4 <- as.character(tclvalue(cbValue4))
	  cbVal5 <- as.character(tclvalue(cbValue5))
	  cbVal6 <- as.character(tclvalue(cbValue6))
	  cbVal7 <- as.character(tclvalue(cbValue7))
      
	  cbVal10 <- as.character(tclvalue(cbValue10))

	  E2Val <- pval.orQA
      FDRval <<- as.numeric(tclvalue(FDR))

	  Direction  <- res.orQA$dir 
	  Direction[Direction==TRUE] <- "up"
	  Direction[Direction==FALSE] <- "dn"

	  try(tkdelete(treeWidget,"AdjPvalORQANode"), silent = TRUE)
	  tkinsert(treeWidget,"end","Record5Node", "AdjPvalORQANode", text= paste("Adjusted P-values, FDR: ", FDRval))

      sigeneBHorqa <<- sigeneBYorqa <<- sigeneSSorqa <<- sigeneSDorqa <<- sigeneHolmorqa <<- sigeneHochorqa <<- sigeneBonorqa <<- NULL
	  adjpBHorqa <<- adjpBYorqa <<- adjpSSorqa <<- adjpSDorqa <<- adjpHolmorqa <<- adjpHochorqa <<- adjpBonorqa <<- NULL
	  result <- matrix(NA,7,4)

	  numsigBH <- numsigBY <- numsigSS <- numsigSD <- numsigHolm <- numsigHoch <- numsigBon <- 0
	  
	  if (cbVal1=="1")  {
	    adjpBHorqa <- adjustment(pval.orQA,"BH")
		assign("adjpBHorqa", adjpBHorqa ,envir=.GlobalEnv)
      	sigeneBHorqa <- Signif(pval.orQA, FDRval, "BH")
		numsigBH <- nrow(sigeneBHorqa)
		numsigBH [is.null(numsigBH)] <- 0
    	result[1, ] <- c("BH", numsigBH, 1, 1)
		tkinsert(treeWidget, "end", "AdjPvalORQANode","BHNodeORQA", text= paste("BH, Sig:", numsigBH))
      }

	  if (cbVal2=="1")  {
		adjpBYorqa <- adjustment(pval.orQA, "BY")  
		assign("adjpBYorqa", adjpBYorqa, envir=.GlobalEnv)
    	sigeneBYorqa <- Signif(pval.orQA, FDRval, "BY")
		numsigBY <- nrow(sigeneBYorqa)
		numsigBY[is.null(numsigBY)] <- 0
    	result[2,] <- c("BY", numsigBY, 3, 1)
		tkinsert(treeWidget, "end", "AdjPvalORQANode", "BYNodeORQA", text= paste("BY, Sig:", numsigBY))
	  }

	  if (cbVal3=="1")  {
		adjpSSorqa <- adjustment(pval.orQA, "SidakSS")
		assign("adjpSSorqa", adjpSSorqa, envir=.GlobalEnv)
      	sigeneSSorqa <- Signif(pval.orQA, FDRval, "SidakSS")
      	numsigSS <- nrow(sigeneSSorqa)
		numsigSS [is.null(numsigSS)] <- 0
    	result[3,] <- c("SidakSS", numsigSS, 4, 1)
		tkinsert(treeWidget, "end", "AdjPvalORQANode", "Sidak1NodeORQA", text= paste ("Sidak SS, Sig:", numsigSS))
      }

	  if (cbVal4=="1")  {
      	adjpSDorqa <- adjustment(pval.orQA,"SidakSD")
		assign("adjpSDorqa", adjpSDorqa, envir=.GlobalEnv)
		sigeneSDorqa <- Signif(pval.orQA, FDRval, "SidakSD")
      	numsigSD <- nrow(sigeneSDorqa)
		numsigSD [is.null(numsigSD)] <- 0
    	result[4, ] <- c("SidakSD", numsigSD, 1, 2)
		tkinsert(treeWidget, "end", "AdjPvalORQANode", "Sidak2NodeORQA", text= paste ("Sidak SD, Sig:", numsigSD))
      }

	  if (cbVal5=="1")  {
      	adjpHolmorqa <- adjustment(pval.orQA, "Holm")
		assign("adjpHolmorqa", adjpHolmorqa, envir=.GlobalEnv)
      	sigeneHolmorqa <- Signif(pval.orQA, FDRval, "Holm")
		numsigHolm <- nrow(sigeneHolmorqa )
		numsigHolm[is.null(numsigHolm  )] <- 0
    	result[5,] <- c("Holm", numsigHolm, 2, 2)
		tkinsert(treeWidget, "end", "AdjPvalORQANode", "HolmNodeORQA", text= paste( "Holm, Sig:", numsigHolm))
	  }

	  if (cbVal6=="1")  {
      	adjpHochorqa <- adjustment(pval.orQA, "Hochberg")
		assign("adjpHochorqa", adjpHochorqa, envir=.GlobalEnv)
		sigeneHochorqa <- Signif(pval.orQA, FDRval, "Hochberg")
		numsigHoch <- nrow(sigeneHochorqa)
		numsigHoch[is.null(numsigHoch)] <- 0
    	result[6, ] <- c("Hochberg", numsigHoch, 3, 2)
		tkinsert(treeWidget, "end", "AdjPvalORQANode", "HocbergNodeORQA", text= paste ("Hocberg, Sig:", numsigHoch))
	  }

	  if (cbVal7=="1")  {
		adjpBonorqa <- adjustment(pval.orQA, "Bonferroni")
		assign("adjpBonorqa", adjpBonorqa, envir=.GlobalEnv)
        sigeneBonorqa <- Signif(pval.orQA, FDRval, "Bonferroni")
		numsigBon <- nrow(sigeneBonorqa)
		numsigBon[is.null(numsigBon)] <- 0
    	result[7, ] <- c("Bonferroni", numsigBon, 4, 2)
		tkinsert(treeWidget, "end", "AdjPvalORQANode", "BonferroniNodeORQA", text=paste("Bonferroni, Sig:", numsigBon))
	  }
		
			
	  num <- nrow(exprs2orqa)
	  result2 <- na.exclude(result)
	  colnames(result2) <- c("P-values Adjustment", "Number of significant genes", "color", "line type")

	  if (nrow(result2) > 1){
        try(showData(result2[, 1:2], title= paste("Number of Significant Genes, FDR:", FDRval ,"  Number of Genes analyzed:", num)))
 	  }	
	  if (nrow(result2) == 1) {printOrQA(num, FDRval, result2)} 
		
	  RowNumSig <- NULL
	  if (numsigBH > 0) {RowNumSig <- unique(c(RowNumSig,sigeneBHorqa$row.num))} 
	  if (numsigBY > 0) {RowNumSig <- unique(c(RowNumSig,sigeneBYorqa$row.num))} 
	  if (numsigSS > 0) {RowNumSig <- unique(c(RowNumSig,sigeneSSorqa$row.num))} 
	  if (numsigSD > 0) {RowNumSig <- unique(c(RowNumSig,sigeneSDorqa$row.num))} 
	  if (numsigHolm > 0) {RowNumSig <- unique(c(RowNumSig,sigeneHolmorqa$row.num))} 
	  if (numsigHoch > 0) {RowNumSig <- unique(c(RowNumSig,sigeneHochorqa$row.num))} 
	  if (numsigBon > 0) {RowNumSig <- unique(c(RowNumSig,sigeneBonorqa$row.num))} 
	  
	  #RowNumSig <- unique(c(sigeneBHorqa$row.num, sigeneBYorqa$row.num, sigeneSSorqa$row.num, sigeneSDorqa$row.num,
		#	sigeneHolmorqa$row.num, sigeneHochorqa$row.num, sigeneBonorqa$row.num))

	  AdjPvalAll <- cbind(adjpBHorqa[,2], adjpBYorqa[,2], adjpSSorqa[,2], adjpSDorqa[,2],
			adjpHolmorqa[,2], adjpHochorqa[,2], adjpBonorqa[,2])
	  colnames(AdjPvalAll) <- paste(result2[, 1], "p-val")
	
	  if (ncol(AdjPvalAll) == 1) {
	    PvalSig <- AdjPvalAll[RowNumSig,] 
	    orderedRowNum <- RowNumSig[order(PvalSig)]
	  } else {
	    sumSig <- apply(AdjPvalAll[RowNumSig,], 1, sum)
	    orderedRowNum <- RowNumSig[order(sumSig)]
	  }	
	
	  AdjPvalAll <- round(AdjPvalAll, digits = 6)

 	  row.num <- 1:nrow(exprs2orqa)
	  Res.orQA <<- data.frame(ProbeID, row.num, Mu.Diff, Direction, pval.orQA, AdjPvalAll)
      rownames(Res.orQA) <<- NULL
	
	  orQASigGenes1  <- Res.orQA[orderedRowNum,]
	  rownames(orQASigGenes1) <- NULL
	  SigGenes.orqaE2 <<- orQASigGenes1 

	  if(cbVal10=="1" ) {
	    try(showData(orQASigGenes1, title= paste("List of Significant Genes based on permutations p-values (orQA), FDR:", 
							  FDRval, "  Number of Genes analyzed :", num)))
	  } 
		
	  tkconfigure(orqa, cursor="arrow")
	  result3 <<- result2
      assign("result3orqa", result3, envir=.GlobalEnv)
    }
  }

  run.but <- tkbutton(frame2, text=" Calculate ", command = AdjustBut)
  tkgrid(tklabel(frame2,text="                             "), run.but)

  tkgrid(tklabel(frame2,text="                             "))

  savesiggene <- function() {
	save.result(SigGenes.orqaE2)
  }
  savesiggene.but <- tkbutton(frame2, text="Save significant genes", command=savesiggene)


  saveallgene <- function() {
    Pval.AllGenes.orQA <<- Res.orQA
	save.result(Pval.AllGenes.orQA)
  }
  saveallgene.but <- tkbutton(frame2, text="Save results for all genes", command=saveallgene)

  tkgrid.configure(savesiggene.but, saveallgene.but, sticky="w")

  tkgrid(frame2)
  tkgrid.configure(frame2, sticky="w")

 
  #### Frame for Producing Plots ##
  
  frame3 <- tkframe(spec.frm, relief="groove", borderwidth=2)
  
  cb9 <- tkcheckbutton(frame3)
  cb11 <- tkcheckbutton(frame3)
  
  cbValue9 <- tclVar("0")
  cbValue11 <- tclVar("0")
  
  tkconfigure(cb9,variable=cbValue9,text="Ranking Plot (Adjust the p-values first)")
  tkconfigure(cb11,variable=cbValue11,text="Plot Fold change vs p-values")
  
  label.plots <-tklabel(frame3,text="Plots: ")
  tkgrid(label.plots)
  tkgrid.configure(label.plots,sticky="w")
  tkgrid.configure(cb11,sticky="w")
  tkgrid.configure(cb9,sticky="w")
  tkgrid(cb11)
  tkgrid(cb9)	
  
  
  PlotBut <- function () {
	  
	  if (!exists("pval.orQA")){  
			ReturnVal <- tkmessageBox(title="Error Message",message="The raw-pvalues have not calculated yet!",icon="info",type="ok")
			if (tclvalue(ReturnVal)== "ok") {tkfocus(orqa)}
	  } 
	  else {
		  
		  tkconfigure(orqa, cursor="watch")
		
		  cbVal9 <- as.character(tclvalue(cbValue9))
		  cbVal11 <- as.character(tclvalue(cbValue11))
		  
		  if (cbVal9=="1" ){
			  
			  tkconfigure(orqa, cursor="watch")
			  
			  rankPlot <- function() {
				  if (!exists("result3")) {
					  params <- par(bg="white")	
					  options(show.error.messages = FALSE)
					  plot((1:nrow(exprs2orqa)), main="P-values", xlab=" Index ", ylab="Raw P-values", ylim=c(0,1)) 
					  try(lines(sort(pval.orQA), col=2, lwd=2, lty=1, type="s"))
				  } else {  
					  params <- par(bg="white")	
					  options(warn = -1)
					  plot((1:nrow(exprs2orqa)), main="P-values", xlab=" Index ", ylab="P-values", ylim=c(0,1)) 
					  try(lines(sort(adjpBHorqa[, 2]), col=1, lwd=2, lty=1, type="s"), TRUE)
					  try(lines(sort(adjpBYorqa[, 2]), col=3, lwd=2, lty=1, type="s"), TRUE)
					  try(lines(sort(adjpSSorqa[, 2]), col=4, lwd=2, lty=1, type="s"), TRUE)
					  try(lines(sort(adjpSDorqa[, 2]), col=1, lwd=2, lty=1, type="s"), TRUE)
					  try(lines(sort(adjpHolmorqa[, 2]), col=2,lwd=2, lty=2, type="s"), TRUE)
					  try(lines(sort(adjpHochorqa[, 2]), col=3,lwd=2, lty=2, type="s"), TRUE)
					  try(lines(sort(adjpBonorqa[, 2]), col=4, lwd=2, lty=2, type="s"), TRUE)
					  try(lines(sort(pval.orQA), col=2, lwd=2, lty=1, type="s"))
					  abline(FDRval, 0)
					  metd <- result3[, 1]
					  col <- result3[, 3]
					  type <- result3[, 4]
					  legend((0.7*nrow(exprs2orqa)), 0.62, c("Raw P-val",metd),
							  col = c(2, as.numeric(col)),  lty = c(1, as.numeric(type)), lwd = 2,  merge = TRUE )
					  options(warn = 1)
				  }
			  }
			  
			  Plot2(rankPlot, 1.7, 1.5, title="Windows Graph Output: Plot p-values")
			  
		  }
		  
		  if (cbVal11=="1") {
			  PVALSvsDiffPlot <- function () {
				  params <- par(bg="white")	
				  options(show.error.messages = FALSE)
				  x <- cbind(Mu.Diff, -log(pval.orQA))
				  colors  <- densCols(x)
				  
				  plot (x,col=colors, pch=20, lwd=3,ylab="- log p-values", xlab="Fold change" )
			  }
			  Plot2(PVALSvsDiffPlot, 1.5, 1.5, title="Windows Graph Output: Fold change vs p-values" )
			  
			  ## Fold change = > Mu*dose.max-Mu*dose.min ##
		  }
		  tkconfigure(orqa, cursor="arrow")
		  options(show.error.messages = TRUE)
	  }
  }
  
  
  plot.but <-tkbutton(frame3,text=" Produce plots ", command=PlotBut)
  tkgrid(tklabel(frame3,text="                             "), plot.but)
  
  tkgrid(frame3)
  tkgrid.configure(frame3,sticky="nsw")
  

  onExit <- function(){
   	 	ReturnVal <<- 0
		tkdestroy(orqa)
  }

  tkgrid(tklabel(buttonFrameasym, text="                             "))
  Exit.but <- tkbutton(buttonFrameasym, text=" Exit the analysis ", command=onExit )
  tkgrid(Exit.but)
  tkgrid.configure(Exit.but, sticky="e")

  tkgrid(spec.frm)
  tkgrid(buttonFrameasym)

}
}
