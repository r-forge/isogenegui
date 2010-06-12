`IsoTest` <-
function (rpval, FDR, type = c("BH", "BY","Bonferroni", "Holm", 
"Hochberg", "SidakSS", "SidakSD"), stat = c("E2", "Williams", 
    "Marcus", "M", "ModM")) 
{
    rp <- rpval
    Probe.ID <- rp[, 1]
    type <- match.arg(type)
    stat <- match.arg(stat)
    if (stat == "E2") 
        rpraw <- rp[, 2]
    else if (stat == "Williams") 
        rpraw <- rp[, 3]
    else if (stat == "Marcus") 
        rpraw <- rp[, 4]
    else if (stat == "M") 
        rpraw <- rp[, 5]
    else if( stat == "ModM")
    rpraw <- rp[, 6]
  
    adjp <- adjustment (rpraw , type ) 
    sig  <- which(adjp[, 2] <= FDR)
    adjp1 <- adjp[sig,]

   if (is.null (adjp1)== TRUE ) { 
     sign.genes <- 0
      }
  else { 
    sign.Probe.ID <-Probe.ID[sig]
    if (length(sig) == 1 ) 
    sign.genes <- c(sign.Probe.ID ,sig,adjp1)
    else {
         sign.genes <- data.frame(sign.Probe.ID ,sig,adjp1)
         row.names(sign.genes) <- NULL
         }
     names(sign.genes) <- c("Probe.ID", "row.num", paste (stat, sep = "", 
           "Raw p-values"), paste (type, sep = " ", "Adjusted p-values"))
     }
  return(sign.genes)
}

