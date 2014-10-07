`doGORIC` <-
function() {

if (!exists("exprs") | !exists("dose"))  
{  
	ReturnVal <- tkmessageBox(title="Error Message", message="Load the data first!", icon="error", type="ok")
	if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
	
} else {
	
	gor <- tktoplevel() 
	#tkwm.deiconify(gor)
	tkwm.title(gor ,"Generalized Order Restricted Information Criterion") 
	#tkgrab.set(gor)
	
	spec.frm <- tkframe(gor, borderwidth=2)
	frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	buttonFrameasym <-tkframe(gor, borderwidth=2)
	
	tkgrid(tklabel(frame1,text="    "))
	gene <- tclVar("1")
	entry.gene <- tkentry(frame1, width="5", textvariable=gene)
	gene.label <- tklabel(frame1, text="Gene of interest:   ")
	tkgrid(gene.label, entry.gene)
	tkgrid.configure(gene.label, entry.gene, sticky="w")
	tkgrid(tklabel(frame1,text="    "))
	
	direction <- tkradiobutton(frame1)
	direction2 <- tkradiobutton(frame1)
		
	rb.direction <- tclVar(0)
	tkconfigure(direction, variable=rb.direction,value=0,text="Increasing")
	tkconfigure(direction2, variable=rb.direction,value=1, text="Decreasing")
		
	label.trans <- tklabel(frame1, text="Order restriction:")
	tkgrid(label.trans)
	tkgrid.configure(label.trans, sticky="w")
	
	tkgrid(direction, direction2)
	tkgrid.configure(direction,direction2,sticky="w")
	tkgrid(tklabel(frame1,text="    "))
	
	tkgrid(tklabel(gor,text="    "))
	iter <- tclVar("1000")
	entry.iter <- tkentry(frame1, width="5", textvariable=iter)
	iter.label <- tklabel(frame1, text="Number of iterations:   ")
	tkgrid(iter.label, entry.iter)
	tkgrid.configure(iter.label, entry.iter, sticky="w")
	tkgrid(tklabel(frame1, text="    "))
	
	OnOK <- function()
	{
		direction.par <- as.numeric(tclvalue(rb.direction))
		gene.par <-  as.numeric(tclvalue(gene))
		iter.par <-  as.numeric(tclvalue(iter))
		stop <- FALSE
		
		if (gene.par > nrow(exprs)) {
			tkmessageBox(title="Error Message", message="Specified gene is not present in data set!",icon="warning",type="ok")
			stop <- TRUE
		}
		
		if (!stop){
		response.gene <<- as.numeric(exprs[gene.par, ])
		N <- length(unique(dose))-1
		
		if (direction.par==0){ constr <- cbind(-diag(N), 0) + cbind(0, diag(N))
		} else {constr <- cbind(diag(N), 0) + cbind(0, -diag(N))}
		
		l1 <- list()
		for (i in 1:N){
			l1[[i]] <- c(0,1)
		}		
		modelsMatrix <- expand.grid(l1)
		rownames(modelsMatrix) <- paste("M_",seq(0, nrow(modelsMatrix)-1,by=1),sep="")
		
		models <<- list()		
		for (i in 1:nrow(modelsMatrix)){
			equalities <- modelsMatrix[i,]==0
			models[[i]] <<- orlm(response.gene ~ as.factor(dose)-1, 
					constr=constr, rhs=rep(0, nrow(constr)), nec=equalities)
		}
		names(models) <<- paste("M_",seq(0, nrow(modelsMatrix)-1,by=1),sep="")
		
		
		GORICoutput <<- goric(models, it=iter.par)
		modelSelected <<- rownames(GORICoutput)[which.max(GORICoutput[,"goric_weights"])]
		maxProbability <<- max(GORICoutput[,"goric_weights"])
				
		
		ReturnVal <- tkmessageBox(title="Result Message", message="GORIC succeeded!", icon="info", type="ok")
		if (tclvalue(ReturnVal)== "ok") showData(GORICoutput)
		
		try(tkdelete(treeWidget,"nodeGORIC"), silent = TRUE)
		try(tkdelete(treeWidget,"GORICresults"), silent = TRUE)
		tkinsert(treeWidget, "end", "Record6Node", "GORICresults", text="GORIC Output")
		tkinsert(treeWidget, "end", "GORICresults", "selectedMod", text=paste0("Selected model: ", modelSelected) )
		tkinsert(treeWidget, "end", "GORICresults", "selectedWeights", text=paste0("Weights = ", maxProbability) )
		}
				
	}
	
	# Calculations

		OK.but <- tkbutton(frame1, text="   Calculate   ", command=OnOK)
		#tkgrid(OK.but)
		tkgrid(tklabel(frame1,text="    "))
		
	# Saving the results button
		saveResults <- function() {
			save.result.ORCME(GORICoutput)
		}
		saveResults.but <- tkbutton(frame1, text="Save results", command=saveResults)
	
	tkgrid(OK.but, saveResults.but)
	tkgrid(frame1)
	tkgrid(tklabel(frame1, text="    "))
	tkgrid.configure(frame1, sticky="nsw")
		
	
	frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	
	tkgrid(tklabel(frame2,text="    "))
	
	PlotAll <- function(){
		
		PlotAllModels <- function () {
			params <- par(bg="white")	
			#options(show.error.messages = FALSE)
			Nm <- dim(GORICoutput)[1]-1
			par(las=2)
			barplot(GORICoutput[,"goric_weights"], names.arg= paste0("M_",seq(0, Nm, by=1)),
					xlab="Model", col="darkblue", cex.lab=1.5, cex.axis=1.15, cex.names=1,
					ylab="GORIC weights")
		}
		Plot2(PlotAllModels, 1.5, 1.5, title="Windows Graph Output: GORIC weights" )
		par(las=1)
		tkconfigure(gor, cursor="arrow")
		#options(show.error.messages = TRUE)
	}
	
	
	PlotBest <- function(){
		
		PlotBestModel <- function () {
			params <- par(bg="white")	
			options(show.error.messages = FALSE)
			
			boxplot(response.gene ~ as.factor(dose), xlab = "Dose", ylab = "Gene expression value", 
					border = "white", lwd=2, main=modelSelected)
			points(response.gene ~ as.factor(dose), pch = 1, lwd=2)
			points(models[[modelSelected]]$coeff ~ as.factor(unique(dose)), 
					pch=4, lwd=2, col="red")
			points(models[[modelSelected]]$coeff ~ as.factor(unique(dose)), 
					pch=4, lwd=2, col="red", type="l")
		}
		Plot2(PlotBestModel, 1.5, 1.5, title="Windows Graph Output: Best model" )
		
		tkconfigure(gor, cursor="arrow")
		options(show.error.messages = TRUE)
	}
	
	BestPlot.but <- tkbutton(frame2, text="   Plot 'best' model   ", command=PlotBest)
	tkgrid(BestPlot.but)
	tkgrid(tklabel(frame2,text="    "))
		
	AllPlots.but <- tkbutton(frame2, text="   Plot GORIC weights   ", command=PlotAll)
	tkgrid(AllPlots.but)
	tkgrid(tklabel(frame2,text="    "))
		
	tkgrid(tklabel(frame2,text="    "))
	model <- tclVar("1")
	entry.model <- tkentry(frame2, width="5", textvariable=model)
	model.label <- tklabel(frame2, text="Model to plot:   ")
	tkgrid(model.label, entry.model,  tklabel(frame2, text="          "))
	tkgrid.configure(model.label, entry.model, sticky="w")
	tkgrid(tklabel(frame2,text="    "))
	
	PlotModel <- function(){
						
		PlotSelectedModel <- function () {
			model.par <-  tclvalue(model)
			params <- par(bg="white")	
			options(show.error.messages = FALSE)

			boxplot(response.gene ~ as.factor(dose), xlab = "Dose", ylab = "Gene expression value", 
					border = "white", lwd=2,  main=paste0("M_",model.par))
			points(response.gene ~ as.factor(dose), pch = 1, lwd=2)
			points(models[[paste0("M_",model.par)]]$coeff ~ as.factor(unique(dose)), 
					pch=4, lwd=2, col="red")
			points(models[[paste0("M_",model.par)]]$coeff ~ as.factor(unique(dose)), 
					pch=4, lwd=2, col="red", type="l")
		}
		Plot2(PlotSelectedModel, 1.5, 1.5, title="Windows Graph Output: Selected model" )
		
		tkconfigure(gor, cursor="arrow")
		options(show.error.messages = TRUE)
	}
	
	
	Plot.but <- tkbutton(frame2, text="   Plot selected model   ", command=PlotModel)
	tkgrid(Plot.but)
	tkgrid(tklabel(frame2,text="    "))
	
	tkgrid(frame2)
	tkgrid.configure(frame2, sticky="w")
	
	
	# Exit button
		onExit <- function(){
			ReturnVal <<- 0
			tkdestroy(gor)
		}
		
	tkgrid(tklabel(buttonFrameasym, text="                             "))
	Exit.but <- tkbutton(buttonFrameasym, text=" Exit the analysis ", command=onExit )
	tkgrid(Exit.but)
	tkgrid.configure(Exit.but, sticky="e")
		
	tkgrid(spec.frm)
	tkgrid(buttonFrameasym)		
	
}

}




