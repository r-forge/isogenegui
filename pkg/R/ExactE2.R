`ExactE2` <-
function() 

## Asymp in this function corresponds to exact pvalues ##
{

if (!exists("exprs") | !exists("dose"))  
   {  
      ReturnVal <- tkmessageBox(title="Error Message",message="Load the data first!",icon="error",type="ok")
	if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
	} 
else {


	asym<-tktoplevel()
	tkwm.title(asym,"Global Likelihood Ratio Test (E2) Analysis: Asymptotic") 

      spec.frm <- tkframe(asym,borderwidth=2)
	frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	buttonFrameasym <-tkframe(asym,borderwidth=2)
	
### Select genes ##
rb1 <- tkradiobutton( frame1 )
rb2 <- tkradiobutton( frame1 )
rbValue1 <- tclVar("all")
tkconfigure(rb1,variable=rbValue1,value="all",text="All Genes")
tkconfigure(rb2,variable=rbValue1,value="range", text="Genes Range")

fromgene<- tclVar("")
FromNum<-tkentry(frame1 ,width="7",textvariable=fromgene)

togene <- tclVar("")
ToNum<-tkentry(frame1 ,width="7",textvariable=togene )

Calc.Asymp <- function ()
  {
	if (!exists("exprs")&!exists("dose"))  
       {  
	  ReturnVal <- tkmessageBox(title="Error Message",message="No data set loaded, please load the dataset first!",icon="info",type="ok")
	  if (tclvalue(ReturnVal)== "ok") tkfocus(asym)
	 } 
  else {
	tkconfigure(asym,cursor="watch")
	
     	rbVal1 <- as.character(tclvalue(rbValue1))

      if (rbVal1=="all") {
 	   exprs2Asymp <<- exprs 
	   matchID  <- c(1:nrow(exprs2Asymp ))
	  }
    	if (rbVal1=="range") {
         exprs2Asymp <<- exprs [tclvalue(fromgene):tclvalue(togene),] 
	   matchID  <- c(tclvalue(fromgene):tclvalue(togene))

	    }

         assign("matchIDAsymp",matchID  ,envir=.GlobalEnv)

	# foldchange ##
      res.Asymp <<-ExactE2Val(dose,exprs2Asymp )
	pval.Asymp  <<- res.Asymp[,2]
 	ProbeID <<- rownames(exprs2Asymp)
	Mu.Diff.Asymp <<-  MeanDiff (dose,exprs2Asymp)
	E2Val.Asymp <<- res.Asymp[,1]
	N <- length(ProbeID )
      ReturnVal <- tkmessageBox(message="The calculation of the p-values have been finished",icon="info",type="ok")
	if (tclvalue(ReturnVal)== "ok") tkfocus(asym)
	tkconfigure(asym,cursor="arrow")
	
	try(tkdelete(treeWidget,"nodeAsymp") , silent = T )
	try( tkdelete(treeWidget,"AsymPvalNode"), silent = T )
	tkinsert(treeWidget,"end","Record2Node","AsymPvalNode",text="Raw P-values")
	tkinsert(treeWidget,"end","AsymPvalNode","NumGeneNode",text=paste("Number gene analyzed: ",N) )

      }
}

Calculate.but <-tkbutton(frame1,text=" Calculate ",command=Calc.Asymp)

lab1 <- tklabel(frame1,text="Calculate the p-values for E2 ")
lab2 <- tklabel(frame1,text="Select the genes: ")

tkgrid.configure(lab1,sticky="w")
tkgrid.configure(lab2,sticky="w")

tkgrid(lab1)
tkgrid(lab2)

tkgrid.configure(rb1,sticky="w")
tkgrid(rb1)
tkgrid(rb2,tklabel(frame1 ,text="From"),FromNum,tklabel(frame1 ,text="To"),ToNum,
tklabel(frame1,text="                        "),Calculate.but)
tkgrid.configure(rb2,sticky="w")

tkgrid(frame1)

tkgrid.configure(frame1,sticky="nsw")


#### Frame Adjusting the raw p values ####


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


tkconfigure(cb1,variable=cbValue1,text="BH")
tkconfigure(cb2,variable=cbValue2,text="BY")
tkconfigure(cb3,variable=cbValue3,text="SidakSS")
tkconfigure(cb4,variable=cbValue4,text="SidakSD")
tkconfigure(cb5,variable=cbValue5,text="Holm")
tkconfigure(cb6,variable=cbValue6,text="Hochberg")
tkconfigure(cb7,variable=cbValue7,text="Bonferroni")

label.pvalAdj <- tklabel(frame2,text="P-value adjustment: ")
tkgrid(label.pvalAdj)
tkgrid.configure(label.pvalAdj,sticky="w")

tkgrid(cb1,cb2,cb3,cb4)	
tkgrid(cb5,cb6,cb7)	

tkgrid.configure(cb1,cb2,cb3,cb4,cb5,cb6,cb7,sticky="w")

FDR <- tclVar("0.05")
FDRtxt <-tkentry(frame2 ,width="5",textvariable=FDR)
tkgrid(tklabel(frame2,text="Overall Significant Level: "),FDRtxt )

cb10 <- tkcheckbutton(frame2)
cbValue10 <- tclVar("0")
tkconfigure(cb10,variable=cbValue10,text="Display the significant genes")
tkgrid(cb10)	


######
AdjustBut <- function()
{
  if (!exists("pval.Asymp"))  
   {  
      ReturnVal <- tkmessageBox(title="Error Message",message="The raw-pvalues have been not calculated yet!",icon="info",type="ok")
	if (tclvalue(ReturnVal)== "ok") tkfocus(asym)
	} 
  else {
	
	tkconfigure(asym,cursor="watch")

	cbVal1 <- as.character(tclvalue(cbValue1))
	cbVal2 <- as.character(tclvalue(cbValue2))
	cbVal3 <- as.character(tclvalue(cbValue3))
	cbVal4 <- as.character(tclvalue(cbValue4))
	cbVal5 <- as.character(tclvalue(cbValue5))
	cbVal6 <- as.character(tclvalue(cbValue6))
	cbVal7 <- as.character(tclvalue(cbValue7))
     	cbVal10 <- as.character(tclvalue(cbValue10))

	E2Val <- E2Val.Asymp
     	FDRval <<- as.numeric(tclvalue(FDR))

	Direction  <- res.Asymp[,3]
	try( tkdelete(treeWidget,"AdjPvalNode"), silent = T )
	tkinsert(treeWidget,"end","Record2Node","AdjPvalNode",text= paste("Adjusted P-values, FDR: ", FDRval)  )

      sigeneBH <<- sigeneBY <<- sigeneSS <<- sigeneSD <<- sigeneHolm <<- sigeneHoch <<- sigeneBon <<- NULL
	adjpBH <<- adjpBY <<- adjpSS <<- adjpSD <<- adjpHolm <<- adjpHoch <<- adjpBon <<- NULL
	result <- matrix(NA,7,4)

		if (cbVal1=="1")  {
			adjpBH <- adjustment (pval.Asymp,"BH")
			assign("adjpBH",adjpBH ,envir=.GlobalEnv)
      		sigeneBH <- Signif (pval.Asymp, FDRval ,"BH")
			numsigBH <- nrow(sigeneBH )
			numsigBH [is.null(numsigBH )] <- 0
    			result[1,] <- c("BH",numsigBH,1,1 )
			tkinsert(treeWidget,"end","AdjPvalNode","BHNode",text= paste("BH, Sig:",  numsigBH ) )

			}

		if (cbVal2=="1")  {
			adjpBY <- adjustment (pval.Asymp,"BY")  
			assign("adjpBY",adjpBY ,envir=.GlobalEnv)
    		
			sigeneBY <- Signif (pval.Asymp, FDRval ,"BY")
			numsigBY <- nrow(sigeneBY )
			numsigBY[is.null(numsigBY)] <- 0
    			result[2,] <- c("BY",numsigBY ,3,1)
			tkinsert(treeWidget,"end","AdjPvalNode","BYNode",text= paste("BY, Sig:",  numsigBY ) )

			}

		if (cbVal3=="1")  {
			adjpSS <- adjustment (pval.Asymp,"SidakSS")
			assign("adjpSS",adjpSS ,envir=.GlobalEnv)
      		sigeneSS  <- Signif (pval.Asymp, FDRval ,"SidakSS")
      		numsigSS <- nrow(sigeneSS )
			numsigSS [is.null(numsigSS )] <- 0
    			result[3,] <- c("SidakSS",numsigSS,4,1 )
			tkinsert(treeWidget,"end","AdjPvalNode","Sidak1Node",text= paste ("Sidak SS, Sig:",  numsigSS ) )

			}

		if (cbVal4=="1")  {
      		adjpSD <- adjustment (pval.Asymp,"SidakSD")
			assign("adjpSD",adjpSD,envir=.GlobalEnv)
			sigeneSD <- Signif (pval.Asymp, FDRval ,"SidakSD")
      		numsigSD <- nrow(sigeneSD )
			numsigSD [is.null(numsigSD )] <- 0
    			result[4,] <- c("SidakSD",numsigSD,1,2 )
			tkinsert(treeWidget,"end","AdjPvalNode","Sidak2Node",text= paste ("Sidak SD, Sig:",  numsigSD ) )

			}

		if (cbVal5=="1")  {
      		adjpHolm <- adjustment (pval.Asymp,"Holm")
			assign("adjpHolm",adjpHolm,envir=.GlobalEnv)
      		sigeneHolm <- Signif (pval.Asymp, FDRval ,"Holm")
			numsigHolm  <- nrow(sigeneHolm )
			numsigHolm  [is.null(numsigHolm  )] <- 0
    			result[5,] <- c("Holm",numsigHolm,2,2  )
			tkinsert(treeWidget,"end","AdjPvalNode","HolmNode",text= paste( "Holm, Sig:",  numsigHolm  ) )

			}

		if (cbVal6=="1")  {
      		adjpHoch <- adjustment (pval.Asymp,"Hochberg")
			assign("adjpHoch",adjpHoch ,envir=.GlobalEnv)
			sigeneHoch <- Signif (pval.Asymp, FDRval ,"Hochberg")
			numsigHoch <- nrow(sigeneHoch)
			numsigHoch  [is.null(numsigHoch  )] <- 0
    			result[6,] <- c("Hochberg",numsigHoch , 3,2 )
			tkinsert(treeWidget,"end","AdjPvalNode","HocbergNode",text= paste ("Hocberg, Sig:",  numsigHoch ) )

			}

		if (cbVal7=="1")  {
			adjpBon <- adjustment (pval.Asymp,"Bonferroni")
			assign("adjpBon",adjpBon ,envir=.GlobalEnv)
        		sigeneBon <- Signif (pval.Asymp, FDRval ,"Bonferroni")
			numsigBon  <- nrow(sigeneBon)
			numsigBon  [is.null(numsigBon  )] <- 0
    			result[7,] <- c("Bonferroni",numsigBon ,4,2 )
			tkinsert(treeWidget,"end","AdjPvalNode","BonferroniNode",text=paste("Bonferroni, Sig:",  numsigBon  ) )
			}
		
		num <- nrow(exprs2Asymp )
		result2 <- na.exclude(result)
		colnames(result2) <- c("P-values Adjusment","Number of significant genes ", "color","line type")

		if (nrow(result2 )> 1 ) 
    			try(showData(result2[,1:2] ,title= paste("Number of Significant Genes, FDR:", FDRval ,"  Number of Genes analyzed:",num)))
		
		if (nrow(result2 )== 1 ) 
		printExact(num,FDRval ,result2)
		
		RowNumSig <- unique(c(sigeneBH$row.num,sigeneBY$row.num,sigeneSS$row.num,sigeneSD$row.num,
			sigeneHolm$row.num,sigeneHoch$row.num,sigeneBon$row.num) )


		AdjPvalAll <- cbind(adjpBH[,2], adjpBY[,2],adjpSS[,2],adjpSD[,2],adjpHolm[,2],adjpHoch[,2],adjpBon [,2])
		
		colnames(AdjPvalAll ) <- paste(result2[,1],"p-val")
		if (ncol(AdjPvalAll )== 1 ) {
			PvalSig <- AdjPvalAll [RowNumSig ,] 
			orderedRowNum <- RowNumSig [order(PvalSig )]
			}
		else {
			sumSig <- apply(AdjPvalAll[RowNumSig, ], 1, sum)
			orderedRowNum <- RowNumSig[order(sumSig)]
			}	
	
		AdjPvalAll <-round(AdjPvalAll , digits = 6)

 		Mu.Diff <-  Mu.Diff.Asymp
		row.num <- 1:nrow(exprs2Asymp)
		Res.Asymtot <<- data.frame(ProbeID, row.num, Mu.Diff,Direction, E2Val, pval.Asymp,AdjPvalAll)
      	rownames(Res.Asymtot) <<- NULL
		
		AsymtotSigGenes1  <- Res.Asymtot[orderedRowNum,]
		rownames(AsymtotSigGenes1) <- NULL
		SigGenes.AsymptotE2 <<- AsymtotSigGenes1 

	
		if(cbVal10=="1" ) {
		try(showData(AsymtotSigGenes1 ,title= paste("List of Significant Genes based on exact P-values, FDR:", FDRval ,"  Number of Genes analyzed :",num)))
		} 
		
	  tkconfigure(asym,cursor="arrow")
	  result3 <- result2
        assign("result3",result3 ,envir=.GlobalEnv)


  }
}

run.but <-tkbutton(frame2,text=" Calculate ",command = AdjustBut)
tkgrid(tklabel(frame2,text="                             "),run.but)

tkgrid(tklabel(frame2,text="                             "))

savesiggene <- function() {
	save.result(SigGenes.AsymptotE2)
}
savesiggene.but <- tkbutton(frame2,text="Save significant genes", command=savesiggene)


saveallgene <- function() {
Pval.AllGenes.Asymp <<- Res.Asymtot
	save.result("Pval.AllGenes.Asymp")
}
saveallgene.but <- tkbutton(frame2,text="Save results for all genes",command=saveallgene )

tkgrid.configure(savesiggene.but, saveallgene.but , sticky="w")

tkgrid(frame2)
tkgrid.configure(frame2,sticky="w")

#### Frame for Producing Plots ##

frame3 <- tkframe(spec.frm, relief="groove", borderwidth=2)

cb8 <- tkcheckbutton(frame3)
cb9 <- tkcheckbutton(frame3)
cb11 <- tkcheckbutton(frame3)

cbValue8 <- tclVar("0")
cbValue9 <- tclVar("0")
cbValue11 <- tclVar("0")

tkconfigure(cb8,variable=cbValue8,text="Plot E2 values vs raw p-values")
tkconfigure(cb9,variable=cbValue9,text="Ranking Plot (Adjust the p-values first)")
tkconfigure(cb11,variable=cbValue11,text="Plot Fold change vs E2")

label.plots <-tklabel(frame3,text="Plots: ")
tkgrid(label.plots  )
tkgrid.configure(label.plots,sticky="w")
tkgrid.configure(cb8,sticky="w")
tkgrid.configure(cb11,sticky="w")
tkgrid.configure(cb9,sticky="w")
tkgrid(cb8)
tkgrid(cb10)
tkgrid(cb9)	


PlotBut <- function () {

	if (!exists("pval.Asymp"))  
  	 {  
        ReturnVal <- tkmessageBox(title="Error Message",message="The raw-pvalues have not calculated yet!",icon="info",type="ok")
	  if (tclvalue(ReturnVal)== "ok") tkfocus(asym)
	 } 
   	else {

	tkconfigure(asym,cursor="watch")

     	cbVal8 <- as.character(tclvalue(cbValue8))
	cbVal9 <- as.character(tclvalue(cbValue9))
     	cbVal11 <- as.character(tclvalue(cbValue11))

	if (cbVal8=="1"){
     		rankPlot <- function()
				{
				params <- par(bg="white")
				x <-cbind(res.Asymp[,1],-log10(pval.Asymp))
				colors  <- densCols(x)
  				plot(x, col=colors, pch=20, lwd=3,xlab=" E2 ",
		                   ylab="-log10(Raw P-values)") 	
				}
	      Plot2(rankPlot,1.5,1.5, title="Windows Graph Output: E2 vs -log10(raw p-value) Plot" )     
		
		}

		if (cbVal9=="1" ){
		
     		 rankPlot <- function()
		  {
			if (!exists("result3")) {
			params <- par(bg="white")	
			options(show.error.messages = FALSE)
			plot((1:nrow(exprs2Asymp )),main="P-values",xlab=" Index ",ylab="Raw P-values",ylim=c(0,1)) 
 			try(lines(sort(pval.Asymp),col=2,  lwd=2,lty=1,type="s"))

			}
			else{  
             
 			params <- par(bg="white")	
			options(warn = -1)
			plot((1:nrow(exprs2Asymp )),main="P-values",xlab=" Index ",ylab="P-values",ylim=c(0,1)) 
			try(lines(sort(adjpBH[,2] ),col=1,  lwd=2,lty=1,type="s"),T)
			try(lines(sort(adjpBY[,2] ),col=3,  lwd=2,lty=1,type="s"),T)
			try(lines(sort(adjpSS[,2] ),col=4,  lwd=2,lty=1,type="s"),T)
			try(lines(sort(adjpSD[,2] ),col=1,  lwd=2,lty=1,type="s"),T)
			try(lines(sort(adjpHolm[,2] ),col=2, lwd=2,lty=2,type="s"),T)
			try(lines(sort(adjpHoch[,2] ),col=3, lwd=2,lty=2,type="s"),T)
			try(lines(sort(adjpBon[,2] ),col=4,  lwd=2,lty=2,type="s"),T)
 			try(lines(sort(pval.Asymp),col=2,  lwd=2,lty=1,type="s"))
			abline(FDRval ,0)
			metd <- result3[,1]
			col <- result3[,3]
			type <- result3[,4]
			legend((0.6*nrow(exprs2Asymp )), 0.9, c("Raw P-val",metd ),
     		 	col = c(2,as.numeric(col)),  lty = c(1,as.numeric(type)), lwd=2,  merge = TRUE )
			options(warn = 1)
			}
		  }
		Plot2(rankPlot,1.7,1.5 ,title="Windows Graph Output: Plot p-values"  )
		}

		if (cbVal11=="1") {
		      E2vsDiffPlot <- function () {
			params <- par(bg="white")	
			options(show.error.messages = FALSE)
			x <-cbind( Mu.Diff.Asymp ,res.Asymp[,1] )
			colors  <- densCols(x)
  			
			plot (x,col=colors, pch=20, lwd=3,ylab="E2", xlab="Fold change" )
			}
             Plot2(E2vsDiffPlot ,1.5,1.5, title="Windows Graph Output: Fold change vs E2" )

		## Fold change = > Mu*dose.max-Mu*dose.min ##
		}
	tkconfigure(asym,cursor="arrow")
	options(show.error.messages = T)
	}
}


plot.but <-tkbutton(frame3,text=" Produce plots ",command=PlotBut)
tkgrid(tklabel(frame3,text="                             "),plot.but)

tkgrid(frame3)
tkgrid.configure(frame3,sticky="nsw")



onExit <- function()
  	{
   	 	ReturnVal <<- 0
		tkdestroy(asym)
	}


tkgrid(tklabel(buttonFrameasym ,text="                             "))
Exit.but <-tkbutton(buttonFrameasym ,text=" Exit the analysis ",command=onExit )
tkgrid(Exit.but)
tkgrid.configure(Exit.but,sticky="e")

tkgrid(spec.frm)
tkgrid(buttonFrameasym )
}
}
