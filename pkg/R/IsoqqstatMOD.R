IsoqqstatMOD <-
function (x, y, fudge, niter, seed) 
{

    set.seed(seed)
    xiter.index <- t(sapply(1:niter, function(i) sample(x)))
    to1 <- to2 <- to3 <- to4 <- to5 <- matrix(0, nrow(y), niter)
    if (fudge == "pooled" ) {
        fudge.factor <- IsofudgeMod2(dose,exprs2SAM,fudge="auto")
     }
    if (fudge == "specFudge" ) {
 fudge.factor <- IsofudgeMod2(dose,exprs2SAM ,fudge=fudgeVal )
}

    if (fudge == "no") {
        fudge.factor <- c(rep(0, 5))
    }

    total <-niter
    # create progress bar
    #pb <- winProgressBar(title = "SAM progress bar", min = 0,
     #                max = total, width = 300)
    pb <- tkProgressBar(title = "SAM progress bar", "Permutations are started",min = 0, max = total, 
         width = 300)

    for (i in 1:niter) {
        yyy0 <- IsoGenemSAM(xiter.index[i, ], as.matrix(y), fudge.factor)
        to1[, i] <- sort(yyy0[[1]])
        to2[, i] <- sort(yyy0[[2]])
        to3[, i] <- sort(yyy0[[3]])
        to4[, i] <- sort(yyy0[[4]])
        to5[, i] <- sort(yyy0[[5]])
        #setTkProgressBar(pb, i , title=paste("SAM Permutations are in progress", round(i /total*100, 0),
        #                                 "% done"))
        info <- sprintf("%d%% done", round(i /total*100, 0))
        setTkProgressBar(pb, i , title=paste("SAM permutations are in progress "), info)


     tkwm.deiconify(samPermute)
     tkgrab.set(samPermute)
     tkfocus(samPermute)
     if (i < niter ) { tclvalue(PermuteText1 ) <- "Please wait...." }
     else {tclvalue(PermuteText1 ) <- "Permutation is finished...."}
     tclvalue(PermuteText2 ) <- paste( "Performing",i,"/",NumPermval,"permutations",sep = " " )


    }
    close(pb)
    L <- IsoGenemSAM(x, as.matrix(y), fudge.factor)
    d <- L[[1]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to1, 1, mean)
    aa1 = cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    d <- L[[2]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to2, 1, mean)
    aa2 = cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    d <- L[[3]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to3, 1, mean)
    aa3 = cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    d <- L[[4]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to4, 1, mean)
    aa4 = cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    d <- L[[5]]
    d.sort.list <- sort.list(d)
    d.sort <- d[d.sort.list]
    perm.mean <- apply(to5, 1, mean)
    aa5 <- cbind(d.sort, perm.mean, d.sort - perm.mean, d.sort.list)
    Direction <- L[[6]]
    res <- list(aa1 = aa1, to1 = to1, aa2 = aa2, to2 = to2, aa3 = aa3, 
        to3 = to3, aa4 = aa4, to4 = to4, aa5 = aa5, to5 = to5, Direction=Direction )
    return(res)
}

