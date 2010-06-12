`get.ResSAM` <-
function() {
    qval <- Isoqval(delta.now, AllFDR, SAMRes, stat=statSAMplot )
    Res.SAM  <- qval[[1]]
    siggeneSAM <- qval[[2]]

    if (nrow (Res.SAM ) > nrow(exprs2SAM) ) {
reprow <- which(table( Res.SAM[,2])==2)
delrow <- which(Res.SAM[,2]== reprow )[1]
Res.SAM <- Res.SAM[-delrow ,]
  }

    probe.id <- rownames(exprs2SAM[Res.SAM [,1],] )
    if (is.null(probe.id)) probe.id <- Res.SAM [,1]
    Res.SAM <- data.frame(probe.id,Res.SAM ) 
    Res.SAM <- Res.SAM [order(Res.SAM [,2]),]
    Res.SAM <- data.frame(Res.SAM,MeanDiffSAM, DirectionSAM) 

    names(Res.SAM ) <- c("Probe ID", "row num", paste(statSAMplot ,"value"),"q-value", "Mean.Diff", "Direction")
    rownames(Res.SAM )  <- NULL

 
    rowsig <- siggeneSAM  [,1]
    siggeneSAM   <- Res.SAM [sort(rowsig ),]

    names(siggeneSAM ) <- c("Probe ID", "row num", paste(statSAMplot ,"value"),"q-value", "Mean.Diff", "Direction")
    rownames(siggeneSAM )  <- NULL

    resSAM <- list (Res.SAM,siggeneSAM  )
    return(resSAM )
 }

