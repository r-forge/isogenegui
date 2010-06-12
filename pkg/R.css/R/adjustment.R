`adjustment` <-
function (rpval,type = c("BH", "BY","Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD"))
  {
    res <- mt.rawp2adjp(rpval, type)
    adjp <- res$adjp[order(res$index), ]
    return(adjp)
   }

