`adjustment` <-
function (rpval,type = c("BH", "BY","Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD"))
  {
  
  if(length(rpval)==1){
    
    return(rpval)
  } else{
    
    res <- mt.rawp2adjp(rpval, type)
    adjp <- res$adjp[order(res$index), ]
    return(adjp)
  }
}

