`Signif` <-
function (rawpval,FDR, proc = c("BH", "BY","Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD"))
  {
    adjp <- adjustment (rawpval, proc )
    sig  <- which(adjp[, 2] <= FDR)
    adjp1 <- data.frame(sig,adjp[sig,])
    names(adjp1 ) <- c("row.num", "raw p-values", paste (proc , sep = " ", "p-values"))
    return(adjp1 )
    }

