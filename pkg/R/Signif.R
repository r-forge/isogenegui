`Signif` <-
function (rawpval,FDR, proc = c("BH", "BY","Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD"))
  {
    adjp <- adjustment (rawpval, proc )
    sig  <- which(adjp[, 2] <= FDR)
    if (length(sig) == 0 ) {
                   print("no gene is significant")
                   }
    else {
          if (length(sig) > 1) {
              adjp1 <- data.frame(sig,adjp[sig,])
               }
          else  {
               adjp1 <- c(sig,adjp[sig,])
	         }
          names(adjp1 ) <- c("row.num", "raw p-values", paste (proc , sep = " ", "p-values"))
          return(adjp1)
	    }

    }

