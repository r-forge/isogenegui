`doORIClust` <-
function() {

if (!exists("exprs") | !exists("dose"))  
{  
	ReturnVal <- tkmessageBox(title="Error Message", message="Load the data first!", icon="error", type="ok")
	if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
	
} else {
	
	oric <- tktoplevel() 
	#tkwm.deiconify(oric)
	tkwm.title(oric ,"Order restricted Information Criterion-based Clustering Algorithm") 
	#tkgrab.set(oric)
	
	spec.frm <- tkframe(oric,borderwidth=2)
	
	# top number
	frame0 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	
	nTop <- tclVar("50")
	entry.nTop  <- tkentry(frame0, width="10", textvariable=nTop)
	tkgrid(tklabel(frame0, text="Number of genes kept for the final clustering result"))
	tkgrid(entry.nTop)
	tkgrid.configure(entry.nTop,sticky="w")
	tkgrid(frame0)
	tkgrid.configure(frame0,sticky="nsw")
	
	# radiobutton transformation
	frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	
	#buttonFrameisoplot  <-tkframe(oric,borderwidth=2)
	
	rb1 <- tkradiobutton(frame1)
	rb2 <- tkradiobutton(frame1)
	rb3 <- tkradiobutton(frame1)
	rb4 <- tkradiobutton(frame1)
	
	rbValue1.transformation <- tclVar(0)
	tkconfigure(rb1, variable=rbValue1.transformation,value=0,text="None")
	tkconfigure(rb2, variable=rbValue1.transformation,value=1, text="Natural log")
	tkconfigure(rb3, variable=rbValue1.transformation,value=2, text="Square root")
	tkconfigure(rb4, variable=rbValue1.transformation,value=3, text="Cubic root")
	
	label.trans <- tklabel(frame1,text="Transformation of the original data")
	tkgrid(label.trans)
	tkgrid.configure(label.trans,sticky="w")
	
	
	tkgrid(rb1,rb2,rb3,rb4)
	tkgrid.configure(rb1,rb2,rb3,rb4,sticky="w")
	tkgrid(frame1)
	tkgrid.configure(frame1,sticky="nsw")
	
	# checkbutton complete.profile
		
	frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	
	cb1 <- tkcheckbutton(frame2)
	cbValue1 <- tclVar("0")
	tkconfigure(cb1,variable=cbValue1,text="Complete profile as candidate profile")
	tkgrid(cb1)
	tkgrid.configure(cb1,sticky="w")
	tkgrid(frame2)
	tkgrid.configure(frame2,sticky="nsw")
	
	# radiobutton onefile
	
	frame3 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	
	rb21 <- tkradiobutton(frame3)
	rb22 <- tkradiobutton(frame3)
	
	
	rbValue2.onefile <- tclVar(TRUE)
	tkconfigure(rb21, variable=rbValue2.onefile,value=FALSE,text="No")
	tkconfigure(rb22, variable=rbValue2.onefile,value=TRUE, text="Yes")
	
	
	label.onefile <- tklabel(frame3,text="Plots into one file")
	tkgrid(label.onefile)
	tkgrid.configure(label.onefile,sticky="w")
	
	
	tkgrid(rb21,rb22)
	tkgrid.configure(rb21,rb22,sticky="w")
		
	# radiobutton plot.format
	
	rb31 <- tkradiobutton(frame3)
	rb32 <- tkradiobutton(frame3)
	
	
	rbValue3.plot <- tclVar("eps")
	tkconfigure(rb31, variable=rbValue3.plot,value="eps",text="eps")
	tkconfigure(rb32, variable=rbValue3.plot,value="jpg", text="jpg  ")
	
	
	label.plot <- tklabel(frame3,text="Plot format")
	tkgrid(label.plot)
	tkgrid.configure(label.plot,sticky="w")
	
	cb2 <- tkcheckbutton(frame3)
	cbValue2 <- tclVar("0")
	tkconfigure(cb2, variable=cbValue2, text="Show plots (jpg only)")
	
	tkgrid(rb31,rb32)
	tkgrid.configure(rb31,rb32, sticky="w")
	
	tkgrid(cb2)
	tkgrid.configure(cb2, sticky="w")
	
	tkgrid(frame3)
	tkgrid.configure(frame3, sticky="w")
	
	# check button show plots
	
	
	
	# radiobutton version of ORICC
	
	frame5 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	
	rb41 <- tkradiobutton(frame5)
	rb42 <- tkradiobutton(frame5)
	
	
	rbValue4.oricc <- tclVar(1)
	tkconfigure(rb41, variable=rbValue4.oricc, value=1, text="one-stage ORICC1")
	tkconfigure(rb42, variable=rbValue4.oricc, value=2, text="two-stage ORICC2")
	
	
	label.oricc <- tklabel(frame5,text="ORICC procedure")
	tkgrid(label.oricc)
	tkgrid.configure(label.oricc,sticky="w")
	
	
	tkgrid(rb41,rb42)
	tkgrid.configure(rb41,rb42,sticky="w")
	tkgrid(frame5)
	tkgrid.configure(frame5,sticky="nsw")
	
	# saving location
	frameS <- tkframe(spec.frm, relief="groove", borderwidth=2)

	saving <- tclVar("C:/")
	entry.saving <- tkentry(frameS, width="100", textvariable=saving)
	tkgrid(tklabel(frameS, text="Location to save output plots"))
	tkgrid(entry.saving)
	tkgrid.configure(entry.saving, sticky="w")
	tkgrid(frameS)
	tkgrid.configure(frameS,sticky="nsw")
	
	
	
	tkgrid(spec.frm)
	
	#phi <- tclVar("2")
	#entry.phi <- tkentry(oric, width="10", textvariable=phi)
	#tkgrid(tklabel(oric, text="Enter the minimal cluster size"))
	#tkgrid(entry.phi)
	
	OnOK <- function()
	{
		
		nTop.par <- as.numeric(tclvalue(nTop))
		transform.par <- as.numeric(tclvalue(rbValue1.transformation))
		complete.par <- as.numeric(tclvalue(cbValue1))
		onefile.par <- as.numeric(tclvalue(rbValue2.onefile))
		plotformat.par <- as.character(tclvalue(rbValue3.plot))
		oricc.par <- as.numeric(tclvalue(rbValue4.oricc))
		saving.par <- as.character(tclvalue(saving))
		showplots.par <- as.numeric(tclvalue(cbValue2))
		
				
		stop <- FALSE
		
		if (nTop.par%%1!=0 | nTop.par >= dim(exprs)[1]) {
			tkmessageBox(title="Error Message", message="Number of genes kept has to be integer value lower than number of genes in the data!",icon="warning",type="ok")
			stop <- TRUE
		}
		
				
		if (!stop)
		{
		setwd(saving.par)
			
		IDs <- 1:dim(exprs)[1]
		oricData <- t(rbind(IDs, t(exprs)))
		
		if(exists("oriccOutput")) {rm("oriccOutput")}
		
		if(oricc.par==1){
			if (complete.par==1){
				oriccOutput <<- try(ORICC1(oricData, data.col=2:dim(oricData)[2], id.col=1, n.rep=as.numeric(table(dose)),
						n.top=nTop.par, transform= transform.par, name.profile="all", 
						complete.profile=1,	onefile=onefile.par, plot.format=plotformat.par), silent=TRUE)
			} else {
				oriccOutput <<- try(ORICC1(oricData, data.col=2:dim(oricData)[2], id.col=1, n.rep=as.numeric(table(dose)),
						n.top=nTop.par, transform= transform.par, name.profile="all", 
						onefile=onefile.par, plot.format=plotformat.par), silent=TRUE)
				
			}			
		} else {
			oriccOutput <<- try(ORICC2(oricData, data.col=2:dim(oricData)[2], id.col=1, n.rep=as.numeric(table(dose)),
					n.top=nTop.par, transform=transform.par, name.profile="all", 
					onefile=onefile.par, plot.format=plotformat.par), silent=TRUE)
			
		}
		
		if(class(oriccOutput)== "try-error") {
			ReturnVal <- tkmessageBox(title="Error Message", message="Number of genes kept for final clustering is too high! Modify it according to ORICC output in main R console.", icon="error", type="ok")
			if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
		} else {
		
		ReturnVal <- tkmessageBox(title="Result Message", message="ORICC succeeded!", icon="info", type="ok")
		#if (tclvalue(ReturnVal)== "ok") showData(oriccOutput)
		
	    nClustersORICC <<- max(oriccOutput$cluster)
	
		try( tkdelete(treeWidget,"nodeORIClust"), silent = TRUE)
		try(tkdelete(treeWidget,"ORIClustResults"), silent = TRUE)
		tkinsert(treeWidget, "end", "Record8Node","ORIClustResults", text="Clustering Output")
		tkinsert(treeWidget, "end", "ORIClustResults","NumClustersORICC", text=paste("Number of clusters: ", nClustersORICC))
		
		if (showplots.par==1 & plotformat.par=="jpg"){
			if (onefile.par==1){
				clusters <- readJPEG("cluster of fitted mean data.jpg")
				plot(1, type="n", axes=F, xlab="", ylab="")
				rasterImage(clusters, 0.5, 0.5, 1.45, 1.45)
			} else {
				for (i in 1:nClustersORICC){
					clusters <- readJPEG(paste("cluster ", i," of fitted mean data.jpg", sep=""))
					x11()
					plot(1, type="n", axes=F, xlab="", ylab="")
					rasterImage(clusters, 0.5, 0.5, 1.45, 1.45)
				}
			}
		}
	
		}
		}
				
	}
	
	# Calculations
		OK.but <- tkbutton(oric, text="   Calculate   ", command=OnOK)
		
	# Saving the results button
		saveClusters <- function() {
			save.result.ORICC(oriccOutput)
		}
		saveResults.but <- tkbutton(oric, text="Save resulting clusters", command=saveClusters )
	
	# Exit button
		onExit <- function(){
			ReturnVal <<- 0
			tkdestroy(oric)
		}
		
		Exit.but <- tkbutton(oric, text=" Exit the analysis ", command=onExit )
		
		tkgrid(tklabel(oric,text="    "))
		tkgrid(saveResults.but, OK.but, Exit.but)
		tkgrid.configure(Exit.but, sticky="e")
		tkgrid.configure(OK.but, sticky="n")
		tkgrid.configure(saveResults.but, sticky="w")
		tkgrid(tklabel(oric,text="    "))
		
	
}

}




