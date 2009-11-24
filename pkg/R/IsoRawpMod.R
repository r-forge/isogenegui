`IsoRawpMod` <-
function (x, y, niter, seed) 
  {
    if (niter > 2500) {
        jmatsize <- 2500
        nmats <- floor(niter/jmatsize)
        endmat <- c(1:nmats * jmatsize, niter)
        begmat <- c(1, endmat[-length(endmat)] + 1)
    }
    else {
        nmats <- 1
        begmat <- 1
        endmat <- niter
    }
    y <- data.matrix(y, rownames.force = TRUE)
    E <- IsoGenem(x, y)
    obs.E.up <- matrix(E[[1]], nrow(y), 1)
    obs.W.up <- E[[2]]
    obs.WC.up <- E[[3]]
    obs.M.up <- E[[4]]
    obs.I.up <- E[[5]]
    obs.E.dn <- E[[6]]
    obs.W.dn <- E[[7]]
    obs.WC.dn <- E[[8]]
    obs.M.dn <- E[[9]]
    obs.I.dn <- E[[10]]
    dire <- E[[11]]
    StatVal <- E 
    assign("StatVal",StatVal ,envir=.GlobalEnv)
    rm(E)

#### Calculating statistics ##
CalcStat ()

    nchunks <- 10
    chunklength <- floor(nrow(y)/nchunks)
    endpos <- c(1:(nchunks - 1) * chunklength, nrow(y))
    begpos <- c(1, endpos[-length(endpos)] + 1)
    options(warn = -1)
    exp.E.up <- ff("exp.E.up", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.W.up <- ff("exp.W.up", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.WC.up <- ff("exp.WC.up", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.M.up <- ff("exp.M.up", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.I.up <- ff("exp.I.up", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.E.dn <- ff("exp.E.dn", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.W.dn <- ff("exp.W.dn", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.WC.dn <- ff("exp.WC.dn", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.M.dn <- ff("exp.M.dn", vmode = "double", dim = c(nrow(y), 
        niter))
    exp.I.dn <- ff("exp.I.dn", vmode = "double", dim = c(nrow(y), 
        niter))
    set.seed(seed)
    x.niter <- t(replicate(niter, sample(x)))
    ffmatrices <- c("exp.E.up", "exp.W.up", "exp.WC.up", "exp.M.up", 
        "exp.I.up", "exp.E.dn", "exp.W.dn", "exp.WC.dn", "exp.M.dn", 
        "exp.I.dn")
    obsvecs <- c("obs.E.up", "obs.W.up", "obs.WC.up", "obs.M.up", 
        "obs.I.up", "obs.E.dn", "obs.W.dn", "obs.WC.dn", "obs.M.dn", 
        "obs.I.dn")
    compvec <- c("<", "<", "<", "<", "<", "<", ">", ">", ">", 
        ">")
    outnames <- c("raw1.up", "raw2.up", "raw3.up", "raw4.up", 
        "raw5.up", "raw1.dn", "raw2.dn", "raw3.dn", "raw4.dn", 
        "raw5.dn")
    raw1.up <- raw2.up <- raw3.up <- raw4.up <- raw5.up <- raw1.dn <- raw2.dn <- raw3.dn <- raw4.dn <- raw5.dn <- nrow(y)
    for (ichunk in seq(along = begpos)) {
        begchunk <- begpos[ichunk]
        endchunk <- endpos[ichunk]
        suby <- y[begchunk:endchunk, ]
        for (jmat in 1:nmats) {
            jbegmat <- begmat[jmat]
            jendmat <- endmat[jmat]
            ncolmat <- jendmat - jbegmat + 1
            subx.niter <- x.niter[jbegmat:jendmat, ]
            res <- apply(subx.niter, 1, function(x) IsoGenem(x = factor(x), 
                y = suby))
            exp.E.up[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[1]])
            exp.W.up[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[2]])
            exp.WC.up[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[3]])
            exp.M.up[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[4]])
            exp.I.up[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[5]])
            exp.E.dn[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[6]])
            exp.W.dn[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[7]])
            exp.WC.dn[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[8]])
            exp.M.dn[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[9]])
            exp.I.dn[begchunk:endchunk, jbegmat:jendmat] <- sapply(res, 
                function(x) x[[10]])

  # if (jmat  < nmats) { tclvalue(PermuteText3 ) <- "Please wait...." }
  #else {tclvalue(PermuteText3 ) <- "Permutation is finished...."}

  tclvalue(PermuteText3 ) <- "Please wait...."

        }
        for (i in seq(along = ffmatrices)) {
            lhs <- as.list(quote(OBSVEC[begchunk:endchunk]))
            lhs[[2]] <- as.name(obsvecs[i])
            rhs <- as.list(quote(FFMAT[begchunk:endchunk, ]))
            rhs[[2]] <- as.name(ffmatrices[i])
            rsarg <- as.call(list(as.name(compvec[i]), as.call(lhs), 
                as.call(rhs)))
            RHS <- as.call(list(as.name("rowSums"), rsarg))
            LHS <- as.list(quote(OUTNAME[begchunk:endchunk]))
            LHS[[2]] <- as.name(outnames[i])
            finalcall <- as.call(list(as.name("<-"), as.call(LHS), 
                as.call(RHS)))
            eval(finalcall)
        }
    }
    rm(suby)
    rm(res)
    rm(subx.niter)
    raw.p.up <- data.frame(raw1.up/niter, raw2.up/niter, raw3.up/niter, 
        raw4.up/niter, raw5.up/niter)
    raw.p.dn <- data.frame(raw1.dn/niter, raw2.dn/niter, raw3.dn/niter, 
        raw4.dn/niter, raw5.dn/niter)
    rm(raw1.up, raw2.up, raw3.up, raw4.up, raw5.up, raw1.dn, 
        raw2.dn, raw3.dn, raw4.dn, raw5.dn)
    rawp.up <- data.frame(row.names(y), raw.p.up)
    rawp.dn <- data.frame(row.names(y), raw.p.dn)
    raw.p.one <- data.frame(row.names(y), apply(cbind(raw.p.up[, 
        1], raw.p.dn[, 1]), 1, min), apply(cbind(raw.p.up[, 2], 
        raw.p.dn[, 2]), 1, min), apply(cbind(raw.p.up[, 3], raw.p.dn[, 
        3]), 1, min), apply(cbind(raw.p.up[, 4], raw.p.dn[, 4]), 
        1, min), apply(cbind(raw.p.up[, 5], raw.p.dn[, 5]), 1, 
        min))
    raw.p.two <- raw.p.one
    raw.p.two[, 2:6] <- 2 * (raw.p.one[, 2:6])
    raw.p.two[, 2:6][raw.p.two[, 2:6] > 1] <- 1
    colnames(raw.p.one) <- colnames(raw.p.two) <- colnames(rawp.up) <- colnames(rawp.dn) <- c("Probe.ID", 
        "E2", "Williams", "Marcus", "M", "ModM")
    res <- list(raw.p.one = raw.p.one, raw.p.two = raw.p.two, 
        rawp.up = rawp.up, rawp.dn = rawp.dn, niter=niter)
    rm(exp.E.up, exp.W.up, exp.WC.up, exp.M.up, exp.I.up, exp.E.dn, 
        exp.W.dn, exp.WC.dn, exp.M.dn, exp.I.dn)
    gc()
    return(res)
options(warn = 0)
tclvalue(PermuteText3 ) <- "Permutation is finished...."
}
