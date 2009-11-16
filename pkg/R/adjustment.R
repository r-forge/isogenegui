`adjustment` <-
function (pval,type = c("BH", "BY","Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD"))
  {
    res <- mt.rawp2adjp(pval, type)
    adjp <- res$adjp[order(res$index), ]
    return(adjp)
   }

