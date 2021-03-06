STEP <- function (data, datatarget = data, previous = NA, previous.wt = NA,
    weight = TRUE, random = FALSE, nsteps = min(ncol(data), ncol(datatarget)) - 
        1, top = 1) 
{
# updated 22/7/2018 to allow user-defined weights, like other functions in easyCODA
# second update, same day: include previous logratios or other variables forced in first
# updated 8/11/2018 to set rownames of ratios.top to be rationames.top
# updated 14/11/2018 to set names of ratios to be names (not rationames) and 
#   names of top ratios to be names.top (not rationames.top))
# updated 16/12/2018 to sort out weights for possible additional variables in data
# updated 4/1/2019 to insert many times omitted 'previous' into matrix of logratios
# updated 8/12/2019 to (temporarily) rove the two "if" conditions

# stepwise variable selection process to find logratios that
# explain maximum variance
#
# data        = the matrix on which logratios are constructed
# datatarget  = the matrix whose variance is to be explained (=data by default)
# previous    = logratios or other variables forced in before searching for other logratios
# previous.wt = weights of previous logratios forced in
# weight      = use weighted variance (=TRUE by default)
# random      = TRUE for random breaking of ties (FALSE by default, using Procrustes)
# nsteps      = number of steps (if not specified, one less than number of columns of datatarget)
# top         = number of top variance-explaining logratios returned after last step

# preliminaries

    data <- as.matrix(data)
    datatarget <- as.matrix(datatarget)
    if (length(weight) == ncol(data)) {
        if (sum(weight <= 0) > 0) 
            stop("Error: some weights zero or negative")
        if (sum(weight) != 1) 
            print("Sum of weights not exactly 1, but are rescaled")
        weights <- weight/sum(weight)
    }

#################  if(!is.na(previous[1]) & is.na(previous.wt[1])) previous.wt <- rep(1, ncol(as.matrix(previous)))

# test whether data and datatarget are the same matrices

    datasame <- FALSE
    datasamedim <- FALSE
    if (prod(dim(data) == dim(datatarget)) == 1) {
        datasamedim <- TRUE
        if (prod(data == datatarget) == 1) 
            datasame <- TRUE
    }
    if (nrow(data) != nrow(datatarget)) 
        stop("Number of rows of target matrix not the same as data matrix")
    if (sum(data == 0) > 0) 
        stop("Zero values in matrix data on which logratios constructed -- please replace")
    if (sum(datatarget == 0) > 0) 
        stop("Zero data values in datatarget matrix -- please replace")
  # sort out the weights, arriving at an r and a c in each case
    if(length(weight)==1) {             # weight=TRUE or FALSE
  # unweighted logratio variance
        if (!weight) {
            ldata <- log(datatarget)
            ldata <- sweep(ldata, 1, apply(ldata, 1, mean))
            r <- rep(1/nrow(datatarget), nrow(datatarget))
            ldata <- sweep(ldata, 2, apply(ldata, 2, mean))
            c <- rep(1/ncol(datatarget), ncol(datatarget))
            ldata <- sqrt(1/nrow(datatarget)) * ldata * sqrt(1/ncol(datatarget))
            ldata.svd <- svd(ldata)
            nontriv <- which(ldata.svd$d > 1e-12)
            data.rpc <- diag(1/sqrt(r)) %*% ldata.svd$u[, nontriv] %*% 
                diag(ldata.svd$d[nontriv])
        }
        else {
  # weighted logratio variance
            r <- apply(datatarget, 1, sum)/sum(datatarget)
            c <- apply(datatarget, 2, sum)/sum(datatarget)
            ldata <- log(datatarget)
            data.mr <- apply(ldata %*% diag(c), 1, sum)
            data.c1 <- sweep(ldata, 1, data.mr)
            data.c2 <- sweep(data.c1, 2, apply(data.c1, 2, mean))
            ldata <- diag(sqrt(r)) %*% data.c2 %*% diag(sqrt(c))
            ldata.svd <- svd(ldata)
            nontriv <- which(ldata.svd$d > 1e-12)
            data.rpc <- diag(1/sqrt(r)) %*% ldata.svd$u[, nontriv] %*% 
                diag(ldata.svd$d[nontriv])
        }
    }
    if (length(weight) > 1) {
  # logratio variance with given weights
        ldata <- log(datatarget)
        r <- apply(datatarget, 1, sum)/sum(datatarget)
        if (length(weight) < ncol(datatarget)) 
            stop("Not enough pre-specified weights")
        if (length(weight) == ncol(datatarget)) 
            c <- weights
        if (length(weight) > ncol(datatarget)) 
            c <- weights[1:ncol(datatarget)]
        data.mr <- apply(as.matrix(ldata) %*% diag(c), 1, sum)
        data.c1 <- sweep(ldata, 1, data.mr)
        data.c2 <- sweep(data.c1, 2, apply(data.c1, 2, mean))
        ldata <- diag(sqrt(r)) %*% data.c2 %*% diag(sqrt(c))
        ldata.svd <- svd(ldata)
        nontriv <- which(ldata.svd$d > 1e-12)
        data.rpc <- diag(1/sqrt(r)) %*% ldata.svd$u[, nontriv] %*% 
            diag(ldata.svd$d[nontriv])
    }
  # column variances
    data.rpc.scale <- diag(sqrt(r)) %*% data.rpc
    ldata.totvar <- sum(ldata^2)
  
  # find best one to start off process
    nratios <- ncol(datatarget) - 1
    rationames <- rep("", nsteps)
    procrust <- rep(0, nsteps)
    R2 <- matrix(0, ncol(data), ncol(data))
    for (j in 2:ncol(data)) {
        for (i in 1:(j - 1)) {
            labs <- c(colnames(data)[i], colnames(data)[j])
#            if (length(Reduce(intersect, regmatches(labs, gregexpr("\\w+", labs)))) == 0) {
                foo <- cbind(rep(1, nrow(data)), log(data[, i]/data[, 
                  j]))
                if (!is.na(previous[1])) 
                  foo <- cbind(rep(1, nrow(data)), previous, 
                    log(data[, i]/data[, j]))
                foo.inv <- try(solve(t(foo) %*% foo), silent = TRUE)
                if (is.matrix(foo.inv)) {
                  R2[i, j] <- sum((foo %*% foo.inv %*% t(foo) %*% 
                    data.rpc.scale)^2)/ldata.totvar
                }
#            }
        }
    }
    R2max <- max(R2)
    ratios <- as.matrix(which(R2 == R2max, arr.ind = TRUE))
    logratios <- log(data[, ratios[1, 1]]/data[, ratios[1, 2]])
    if (is.na(previous[1])) 
        procrust[1] <- protest(data.rpc, logratios, permutations = 0)$t0
    if (!is.na(previous[1])) 
        procrust[1] <- protest(data.rpc, cbind(previous, logratios), 
            permutations = 0)$t0
    if (nsteps == 1 & top == 1) {
        logratios <- log(data[, ratios[1, 1]]/data[, ratios[1, 
            2]])
        rationames <- paste(colnames(data)[ratios[1, 1]], colnames(data)[ratios[1, 
            2]], sep = "/")
        if (is.na(previous[1])) 
            procrust <- protest(data.rpc, logratios, permutations = 0)$t0
        if (!is.na(previous[1])) 
            procrust <- protest(data.rpc, cbind(previous, logratios), 
                permutations = 0)$t0
        return(list(names = rationames, ratios = ratios, logratios = logratios, 
            R2max = R2max, pro.cor = procrust, totvar = ldata.totvar))
    }
    if (nsteps == 1 & top > 1) {
        logratios <- log(data[, ratios[1, 1]]/data[, ratios[1, 
            2]])
        rationames <- paste(colnames(data)[ratios[1, 1]], colnames(data)[ratios[1, 
            2]], sep = "/")
        procrust <- protest(data.rpc, logratios, permutations = 0)$t0
        if (!is.na(previous[1])) 
            procrust <- protest(data.rpc, cbind(previous, logratios), 
                permutations = 0)$t0
        R2.top <- sort(R2, decreasing = TRUE)[1:top]
        ratios.top <- which(R2 >= R2.top[top], arr.ind = TRUE)
        ratios.top <- ratios.top[order(R2[ratios.top], decreasing = TRUE), 
            ]
        rationames.top <- paste(colnames(data)[ratios.top[, 1]], 
            colnames(data)[ratios.top[, 2]], sep = "/")
        rownames(ratios.top) <- rationames.top
        logratios.top <- log(data[, ratios.top[, 1]]/data[, ratios.top[, 
            2]])
        procrust.top <- rep(0, top)
        for (jratio in 1:top) {
            if (is.na(previous[1])) 
                procrust.top[jratio] <- protest(data.rpc, logratios.top[, 
                  jratio], permutations = 0)$t0
            if (!is.na(previous[1])) 
                procrust.top[jratio] <- protest(data.rpc, cbind(previous, 
                  logratios.top[, jratio]), permutations = 0)$t0
        }
        colnames(logratios.top) <- rationames.top
        return(list(names = rationames, ratios = ratios, logratios = logratios, 
            R2max = R2max, pro.cor = procrust, names.top = rationames.top, 
            ratios.top = ratios.top, logratios.top = logratios.top, 
            R2.top = R2.top, pro.cor.top = procrust.top, totvar = ldata.totvar))
    }

  # --------------------- start of loop ------------------------------
  # loop over ratios as many times as there are columns in data minus 1
  # but reduce if there are previous variables

    niter <- nsteps
    if(!is.na(previous[1] & (nsteps==(min(ncol(data), ncol(datatarget)) - 
        1)))) niter <- niter-ncol(as.matrix(previous))
    for (jratio in 2:niter) {
        R2 <- matrix(0, ncol(data), ncol(data))
        for (j in 2:ncol(data)) {
            for (i in 1:(j - 1)) {
                labs <- c(colnames(data)[i], colnames(data)[j])
#                if (length(Reduce(intersect, regmatches(labs, gregexpr("\\w+", labs)))) == 0) {
                  if (sum(apply(ratios, 1, function(x) all(x == 
                    c(i, j)))) == 0) {
                    logratios <- log(data[, ratios[1, 1]]/data[, 
                      ratios[1, 2]])
                    if (!is.na(previous[1])) logratios <- cbind(previous, logratios)
                    if (jratio > 2) {
                      for (jrats in 2:(jratio - 1)) logratios <- cbind(logratios, 
                        log(data[, ratios[jrats, 1]]/data[, ratios[jrats, 
                          2]]))
                    }
                    foologratios <- cbind(logratios, log(data[, 
                      i]/data[, j]))

  # matrix way...
  #                 foo <- cbind(rep(1, nrow(data)), foologratios)
  #                 R2[i,j] <- sum((foo %*% tryCatch(solve(t(foo)%*%foo), error=function(e) e=matrix(0, nrow=ncol(foo), ncol=ncol(foo))) %*% t(foo) %*% data.rpc.scale)^2 ) / ldata.totvar

                    foo <- rda(ldata, foologratios)
                    R2[i, j] <- foo$CCA$tot.chi/foo$tot.chi
                  }
#                }
            }
        }
        R2max <- c(R2max, max(R2))
  #     R2max <- c(R2max, max(R2[R2<0.999999]))
        foo <- as.matrix(which(abs(R2 - max(R2)) < 1e-10, arr.ind = TRUE))
        if (nrow(foo) == 1) 
            ratios <- rbind(ratios, foo)
        if (nrow(foo) > 1 & !random) {

  # tie: find best Procrustes fit
  # have to weight the logratios for testing Procrustes fit
  # use PCA function on logratios

            procr <- rep(0, nrow(foo))
            for (ip in 1:nrow(foo)) {
                foorats <- cbind(logratios, log(data[, foo[ip, 1]]/data[, foo[ip, 2]]))
  # patch for additional data
                alldat.c <- apply(data, 2, mean)
                if (length(weight) == 1 & !weight) 
                  alldat.c <- rep(1/ncol(data), ncol(data))
                if (length(weight) == ncol(datatarget) & !datasamedim) 
                  stop("Weights of additional columns in data have not been given, necessary for Procrustes")
                foorats.c <- c(alldat.c[ratios[, 1]] * alldat.c[ratios[, 
                  2]], alldat.c[foo[ip, 1]] * alldat.c[foo[ip, 
                  2]])
                if (!is.na(previous[1])) foorats.c <- c(previous.wt, foorats.c)
                foorats.rpc <- PCA(foorats, row.wt = r, weight = foorats.c)$rowpcoord
                procr[ip] <- protest(data.rpc, foorats.rpc, permutations = 0)$t0
            }
            ratios <- rbind(ratios, foo[which(procr == max(procr)), 
                ])
        }
  # tie: break randomly
        if (nrow(foo) > 1 & random) {
            foo <- foo[sample(1:nrow(foo)), ]
            ratios <- rbind(ratios, foo[1, ])
        }
    }

  # ------------- end of loop --------------------------------------------------------------

    if (nratios == nsteps & datasame) R2max[nratios] <- 1
    logratios <- cbind(logratios, log(data[, ratios[niter, 1]]/data[, ratios[niter, 2]]))
  # remove previous logratios from final step (will be added with computations)
    if(!is.na(previous[1])) logratios <- logratios[,-c(1:ncol(as.matrix(previous)))]
    rationames <- paste(colnames(data)[ratios[,1]],colnames(data)[ratios[,2]], sep="/")
    rownames(ratios) <- rationames
    if(length(rationames) == ncol(logratios)) colnames(logratios) <- rationames
    rownames(logratios) <- rownames(data)

  # Procrustes correlations
    alldat.c <- apply(data, 2, mean)
    if (length(weight) == 1 & !weight) 
        alldat.c <- rep(1/ncol(data), ncol(data))
    if (length(weight) == ncol(datatarget) & !datasamedim) 
        stop("Weights of additional columns in data have not been given, necessary for Procrustes")
    if (length(weight) == ncol(data)) alldat.c <- weights
    procrust <- rep(0, ncol(logratios))
    foorats.w <- logratios[, 1]
    procrust[1] <- protest(data.rpc, foorats.w, permutations = 0)$t0
    if (!is.na(previous[1])) {
        foorats.w <- cbind(previous, logratios[, 1])
        foorats.rpc <- PCA(foorats.w, row.wt = r, 
                           weight = c(previous.wt, alldat.c[ratios[1,1]]*alldat.c[ratios[1,2]]))$rowpcoord
        procrust[1] <- protest(data.rpc, foorats.w, permutations = 0)$t0
    }
    for (j in 2:ncol(logratios)) {
        foorats <- logratios[, 1:j]
        foorats.c <- alldat.c[ratios[1:j, 1]] * alldat.c[ratios[1:j, 
            2]]
        foorats.rpc <- PCA(foorats, row.wt = r, weight = foorats.c)$rowpcoord
        if (!is.na(previous[1]) & is.na(previous.wt)) 
            stop("No weights for previous logratios are given in previous.wt option, necessary for Procrustes")
        if (!is.na(previous[1])) 
            foorats.rpc <- PCA(cbind(previous, foorats), row.wt = r, 
                weight = c(previous.wt, foorats.c))$rowpcoord
        procrust[j] <- protest(data.rpc, foorats.rpc, permutations = 0)$t0
    }
    if (top == 1) {
        return(list(names = rationames, ratios = ratios, logratios = logratios, 
            R2max = R2max, pro.cor = procrust, totvar = ldata.totvar))
    }
    if (nsteps < nratios & top > 1) {
        R2.top <- sort(R2, decreasing = TRUE)[1:top]
        ratios.top <- which(R2 >= R2.top[top], arr.ind = TRUE)
        ratios.top <- ratios.top[order(R2[ratios.top], decreasing = TRUE), 
            ]
        rationames.top <- paste(colnames(data)[ratios.top[, 1]], 
            colnames(data)[ratios.top[, 2]], sep = "/")
        rownames(ratios.top) <- rationames.top
        logratios.top <- log(data[, ratios.top[, 1]]/data[, ratios.top[, 
            2]])
        procrust.top <- rep(0, top)
        foorats <- logratios[, 1:(nsteps - 1)]
        for (jratio in 1:top) {
            foorats.wt <- foorats.c[ratios[1:(nsteps - 1), 1]] * 
                foorats.c[ratios[1:(nsteps - 1), 2]]
            foorats.top <- cbind(foorats, logratios.top[, jratio])
            foorats.wt <- c(foorats.wt, foorats.c[ratios.top[jratio, 
                1]] * foorats.c[ratios.top[jratio, 2]])
            foorats.w <- diag(sqrt(r)) %*% foorats.top %*% diag(sqrt(foorats.wt))
            if (!is.na(previous[1])) 
                foorats.w <- diag(sqrt(r)) %*% cbind(previous, 
                  foorats.top) %*% diag(sqrt(c(previous.wt, foorats.wt)))
            procrust.top[jratio] <- protest(data.rpc, foorats.w, 
                permutations = 0)$t0
        }
        colnames(logratios.top) <- rationames.top
        return(list(names = rationames, ratios = ratios, logratios = logratios, 
            R2max = R2max, pro.cor = procrust, names.top = rationames.top, 
            ratios.top = ratios.top, logratios.top = logratios.top, 
            R2.top = R2.top, pro.cor.top = procrust.top, totvar = ldata.totvar))
    }
}
