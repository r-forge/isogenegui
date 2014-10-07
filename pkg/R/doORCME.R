`doORCME` <-
function() {

if (!exists("exprs") | !exists("dose"))  
{  
	ReturnVal <- tkmessageBox(title="Error Message", message="Load the data first!", icon="error", type="ok")
	if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
	
} else {
	
	orcme <- tktoplevel() 
	#tkwm.deiconify(orcme)
	tkwm.title(orcme ,"Order restricted clustering") 
	#tkgrab.set(orcme)
	
	tkgrid(tklabel(orcme,text="    "))
	phi <- tclVar("2")
	entry.phi <- tkentry(orcme, width="5", textvariable=phi)
	phi.label <- tklabel(orcme, text="Enter the minimal cluster size:   ")
	tkgrid(phi.label, entry.phi)
	tkgrid.configure(phi.label, entry.phi, sticky="w")
	tkgrid(tklabel(orcme,text="    "))
	
	direction <- tkradiobutton(orcme)
	direction2 <- tkradiobutton(orcme)
		
	rb.direction <- tclVar(0)
	tkconfigure(direction, variable=rb.direction,value=0,text="Increasing")
	tkconfigure(direction2, variable=rb.direction,value=1, text="Decreasing")
		
	label.trans <- tklabel(orcme, text="Decide which genes should be clustered?")
	tkgrid(label.trans)
	tkgrid.configure(label.trans, sticky="w")
	
	
	tkgrid(direction, direction2)
	tkgrid.configure(direction,direction2,sticky="w")
	tkgrid(tklabel(orcme,text="    "))
	
	robustF <- tkradiobutton(orcme)
	robustT <- tkradiobutton(orcme)
	
	rb.robust <- tclVar(0)
	tkconfigure(robustF, variable=rb.robust,value=0,text="Classical (ANOVA)")
	tkconfigure(robustT, variable=rb.robust,value=1, text="Robust (median polish)")
	
	label.robust <- tklabel(orcme, text="Perform classical or median polish version?")
	tkgrid(label.robust)
	tkgrid.configure(label.robust, sticky="w")
		
	tkgrid(robustF, robustT)
	tkgrid.configure(robustF, robustT, sticky="w")
	tkgrid(tklabel(orcme,text="    "))
	
	
	#doseCat <- as.numeric(factor(dose,labels=seq(1,length(unique(dose)),by=1)))
	
	calculateLambdaK <- function(){
		direction.par <- as.numeric(tclvalue(rb.direction))
		robust.par <- as.numeric(tclvalue(rb.robust))
		robustMethod <- robust.par==1
		
		dirData <- monotoneDirection(geneData = exprs, doseData = dose)
		
		if (direction.par==0){	clusterData <- as.data.frame(dirData$incData)
		} else {clusterData <- as.data.frame(dirData$decData)}
		
		
		lambdaVector <- seq(0.05,0.95, by=0.05)
		lambdaChoiceOutput <- resampleORCME(clusteringData=clusterData, 
				lambdaVector=lambdaVector, robust=robustMethod)
		
		nOutput <- wOutput <- NULL
		ncMatrix <- sapply(c(1:length(lambdaChoiceOutput)), function(x) nOutput <- cbind(nOutput, 
							lambdaChoiceOutput[[x]][, 4]))
		wssMatrix <- sapply(c(1:length(lambdaChoiceOutput)), 
				function(x) wOutput <- cbind(wOutput, lambdaChoiceOutput[[x]][, 
									2]))
		pwss <- wssMatrix + 2 * ncMatrix
		pwssMean <- rowMeans(pwss)
		
		
		lambdaOpt <- round(lambdaVector[which.min(pwssMean)],2)
		tclvalue(lambda) <- lambdaOpt
		
		ReturnVal <- tkmessageBox(title="Result Message", message=paste("Resampling completed! 
								Optimal homogeneity parameter value is ", lambdaOpt,".",sep=""), icon="info", type="ok")
		if (tclvalue(ReturnVal)== "ok") {
			par(mfrow=c(2,3))
			plotLambda(lambdaChoiceOutput,output="wss")
			plotLambda(lambdaChoiceOutput,output="ncluster")
			plotLambda(lambdaChoiceOutput,output="pwss")
			plotLambda(lambdaChoiceOutput,output="ch")
			plotLambda(lambdaChoiceOutput,output="h")	
		}		
			
	}
	
	
	lambda <- tclVar("0.5")
	calcLambda.but <- tkbutton(orcme, text="Optimize parameter", command=calculateLambdaK)
	entry.lambda <- tkentry(orcme, width="5", textvariable=lambda)
	label.lambda <- tklabel(orcme, text="Enter the homogeneity parameter:   ")
	tkgrid(label.lambda, entry.lambda, calcLambda.but)
	tkgrid.configure(label.lambda, entry.lambda, sticky="w")
	tkgrid.configure(calcLambda.but, sticky="e")
	tkgrid(tklabel(orcme,text="    "))
	
		
	OnOK <- function()
	{
		direction.par <- as.numeric(tclvalue(rb.direction))
		lambda.par <- as.numeric(tclvalue(lambda))
		phi.par <-  as.numeric(tclvalue(phi))
		robust.par <- as.numeric(tclvalue(rb.robust))
		robustMethod <- robust.par==1
		stop <- FALSE
		
		if (lambda.par < 0  | lambda.par > 1) {
			tkmessageBox(title="Error Message", message="Homogeneity parameter has to be in interval [0,1]!",icon="warning",type="ok")
			stop <- TRUE
		}
		
		if (phi.par%%1!=0 | phi.par >= dim(exprs)[1] ) {
			tkmessageBox(title="Error Message", message="Minimal cluster size has to be integer value lower than number of samples in the data!",icon="warning",type="ok")
			stop <- TRUE
		}
		
		if (!stop)
		{
		dirData <- monotoneDirection(geneData = exprs, doseData = dose)
		
		if (direction.par==0){	clusteringData <<- as.data.frame(dirData$incData)
		} else {clusteringData <<- as.data.frame(dirData$decData)}
		
		ORCMEoutput <<- ORCME(DRdata=clusteringData, lambda=lambda.par, phi=phi.par, robust=robustMethod)
		nClusters <<- dim(ORCMEoutput)[2]
		
		ReturnVal <- tkmessageBox(title="Result Message", message="ORCME succeeded!", icon="info", type="ok")
		if (tclvalue(ReturnVal)== "ok") showData(ORCMEoutput)
		
		try(tkdelete(treeWidget,"nodeORCME"), silent = TRUE)
		try(tkdelete(treeWidget,"ORCMEresults"), silent = TRUE)
		tkinsert(treeWidget, "end", "Record7Node", "ORCMEresults", text="Clustering Output")
		tkinsert(treeWidget, "end", "ORCMEresults", "NumClustersORCME", text=paste("Number of clusters: ", nClusters) )
		
		}
				
	}


	# Calculations
		
		OK.but <- tkbutton(orcme, text="   Calculate   ", command=OnOK)
		tkbind(entry.lambda, "<Return>", OnOK)
		#tkgrid(OK.but)
		tkgrid(tklabel(orcme,text="    "))
		
	# Saving the results button
		saveClusters <- function() {
			save.result.ORCME(ORCMEoutput)
		}
		saveResults.but <- tkbutton(orcme, text="Save resulting clusters", command=saveClusters)
	
	# Exit button
		onExit <- function(){
			ReturnVal <<- 0
			tkdestroy(orcme)
		}
		
		Exit.but <- tkbutton(orcme, text=" Exit the analysis ", command=onExit )
		
		tkgrid(OK.but, saveResults.but, Exit.but)
		#tkgrid.configure(Exit.but, sticky="e")
		#tkgrid.configure(saveResults.but, sticky="w")
		tkgrid(tklabel(orcme,text="    "))
		
	
}

}




