`plotORCME` <- function() {
	
if (!exists("clusteringData")){  
    ReturnVal <- tkmessageBox(title="Error Message", message="run ORCME first!", icon="error", type="ok")
	if (tclvalue(ReturnVal)== "ok") tkfocus(tt)
} else {
	orcmePlot <- tktoplevel()
	tkwm.deiconify(orcmePlot)
	tkwm.title(orcmePlot, "Plot of ORCME clusters") 
	tkfocus(orcmePlot)
	spec.frm <- tkframe(orcmePlot, borderwidth=2)
	frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)

	buttonFrameOrcmePlot  <-tkframe(orcmePlot, borderwidth=2)

	rb2 <- tkradiobutton(frame1)
	rb3 <- tkradiobutton(frame1)

	rbValue1 <- tclVar("id")
	tkconfigure(rb2, variable=rbValue1, value="id", text="Cluster ID(s), e.g., 2,3,4         ")
	tkconfigure(rb3, variable=rbValue1, value="idrange", text="Range of IDs         From: ")

	idnum <- tclVar("")
	IDNum <-tkentry(frame1, width="6", textvariable=idnum)
	
	fromID <- tclVar("")
	FromID <-tkentry(frame1, width="6", textvariable=fromID)
	
	toID <- tclVar("")
	ToID <-tkentry(frame1, width="6", textvariable=toID)
	
	tkgrid(tklabel(frame1, text=paste("Insert cluster ID(s) you want to draw (there are ", nClusters," clusters):", sep="")))
	
	tkgrid(rb2, IDNum)
	tkgrid.configure(rb2, sticky="w")
	
	tkgrid(rb3, FromID, tklabel(frame1, text="To"), ToID)
	tkgrid.configure(rb3, sticky="w")
	
	tkgrid(frame1)
	tkgrid.configure(frame1, sticky="nsw")
	
	
	frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)
	
	cb1 <- tkcheckbutton(frame2)
	cbValue1 <- tclVar("0")
	tkconfigure(cb1,variable=cbValue1,text="Zero centered means")
	tkgrid(cb1)
	tkgrid.configure(cb1, sticky="w")
	
	tkgrid(frame2)
	tkgrid.configure(frame2, sticky="nsw")

	doseCat <- as.numeric(factor(dose,labels=seq(1,length(unique(dose)),by=1)))

	OnOK <- function(){
			tkdestroy(orcmePlot)
			
			rbVal1 <- as.character(tclvalue(rbValue1))
			cbVal1 <- as.character(tclvalue(cbValue1))
			
			if (rbVal1=="idrange") {
				
				range <- as.numeric(tclvalue(fromID):tclvalue(toID))
				nrange <- length(range)
				
				plotClusters <- function() {
					nrange [nrange > 3 & nrange <= 12] <- 3
					nrange [nrange > 12] <- 6
					lengthplot <- round ((length(range)+1)/nrange)
					lengthplot [nrange <= 2] <- 1
					par(mfrow=c(lengthplot, nrange))
					for (i in (1:length(range))) {
						params <- par(bg="white")
						if (cbVal1 =="1") {
							plotCluster(DRdata=clusteringData, doseData=doseCat, ORCMEoutput=ORCMEoutput,
									clusterID=range[i], zeroMean=TRUE, xlabel="Dose", ylabel="Gene Expression")
						} else {
							plotCluster(DRdata=clusteringData, doseData=doseCat, ORCMEoutput=ORCMEoutput,
									clusterID=range[i], zeroMean=FALSE, xlabel="Dose", ylabel="Gene Expression")
						}
					}
				}
				
				Myhscale <- 1
				Myvscale <- 1 
				
				Myvscale[nrange > 2 & nrange < 6] <- 1.7
				Myhscale[nrange > 1 & nrange <12] <- 2
				Myvscale[nrange >  6 ] <- 1.7
				Myhscale[nrange > 12] <- 3
				Plot2(plotClusters, Myhscale, Myvscale, title="Windows Graph Output: ORCME" )
				#if (cbVal2 =="1") printsummary(dose, exprs, range )
			}
			
			if (rbVal1=="id") {
				a <- tclvalue(idnum)
				b <- strsplit(a, ",")
				e <- 0
				#length(b[[1]])
				
				for (i in (1:length(b[[1]]))){
					c <- strsplit(b[[1]][i]," ")
					d <- as.numeric(c[[1]])
					e[i] <- na.omit(d)
				}
				
				range <- as.numeric(e)
				nrange <- length(range)
				
				plotClusters <- function() {
					nrange [nrange > 3 & nrange <= 12] <- 3
					nrange [nrange > 12] <- 6
					lengthplot <- round ((length(range)+1)/nrange)
					lengthplot [nrange <= 2] <- 1
					par(mfrow=c(lengthplot, nrange))
					for (i in (1:length(range))) {
						params <- par(bg="white")
						if (cbVal1 =="1") {
							plotCluster(DRdata=clusteringData, doseData=doseCat, ORCMEoutput=ORCMEoutput,
									clusterID=range[i], zeroMean=TRUE, xlabel="Dose", ylabel="Gene Expression")
						} else {
							plotCluster(DRdata=clusteringData, doseData=doseCat, ORCMEoutput=ORCMEoutput,
									clusterID=range[i], zeroMean=FALSE, xlabel="Dose", ylabel="Gene Expression")
						}
					}
				}
				
				Myhscale <- 1
				Myvscale <- 1 
				
				Myvscale[nrange > 2 & nrange < 6] <- 1.7
				Myhscale[nrange > 1 & nrange <12] <- 2
				Myvscale[nrange >  6 ] <- 1.7
				Myhscale[nrange > 12] <- 3
				Plot2(plotClusters, Myhscale, Myvscale, title="Windows Graph Output: ORCME clusters" )
				#if (cbVal2 =="1") printsummary(dose,exprs,range )		 
			}
 
		}
		
		onCancel <- function(){
			tkgrab.release(orcmePlot)
			tkdestroy(orcmePlot)
			tkwm.deiconify(tt) 
			tkfocus(tt)
		}
		
		OK.but <- tkbutton(buttonFrameOrcmePlot, text="   OK   ", command=OnOK)
		Cancel.but <- tkbutton(buttonFrameOrcmePlot, text=" Cancel ", command=onCancel)
		tkgrid(spec.frm)
		tkgrid(OK.but, Cancel.but)
		tkgrid(buttonFrameOrcmePlot)	
	}
}




