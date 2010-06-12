`diginum` <-
function (res) {
y <- res
digit <- strsplit(as.character(y),".",fixed=TRUE)
c <- sapply(digit, function(x)length(x))
if (sum(c) == length(y)) { dig  <- NULL }
            else {
 list2 <- which(c==2)
 ls <-sapply(list2, function(x)nchar(digit[[x]][[2]]))
 dig <- max(ls)
 }
return (dig )
   }

