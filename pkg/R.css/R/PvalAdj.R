`PvalAdj` <-
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
colnames(adjp )<- c("raw p-val",paste(type,"p-val"))
   return(adjp )
}

