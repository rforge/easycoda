STEP <- function(data, datatarget=data, weight=TRUE, random=FALSE, nsteps=min(ncol(data), ncol(datatarget))-1, top=1) {
#
# stepwise variable selection process to find logratios that
# explain maximum variance
#
# data        = the matrix on which logratios are constructed
# datatarget  = the matrix whose variance is to be explained (=data by default)
# weight      = use weighted variance (=TRUE by default)
# random      = TRUE for random breaking of ties (FALSE by default, using Procrustes)
# nsteps      = number of steps (if not specified, one less than number of columns of datatarget)
# top         = number of top variance-explaining logratios returned after last step

# double-centre datatarget and log-transform
  data <- as.matrix(data)
  datatarget <- as.matrix(datatarget)

# test whether data and datatarget are the same matrices
  datasame <- FALSE
  datasamedim <- FALSE
  if(prod(dim(data)==dim(datatarget))==1) {
    datasamedim <- TRUE
    if(prod(data==datatarget)==1) datasame <- TRUE
  }
  if(nrow(data)!=nrow(datatarget)) stop("Number of rows of target matrix not the same as data matrix") 
  if(sum(data==0)>0) stop("Zero values in matrix data on which logratios constructed -- please replace")
  if(sum(datatarget==0)>0) stop("Zero data values in datatarget matrix -- please replace")
  
# if same data reduce target to logratios and compute total variance
# if not same data, use datatarget "as is", simply centre and compute total variance  
  if(datasame) {
    ldata <- log(datatarget)
# unweighted logratio variance
    if(!weight) {
      ldata <- sweep(ldata, 1, apply(ldata, 1, mean))
      r <- rep(1/nrow(data), nrow(data))
      ldata <- sweep(ldata, 2, apply(ldata, 2, mean))
      c <- rep(1/ncol(data), ncol(data))
      ldata <- sqrt(1/nrow(data)) * ldata * sqrt(1/ncol(data))
      ldata.svd <- svd(ldata)
      nontriv <- which(ldata.svd$d > 1.e-12)
      data.rpc <- ldata.svd$u[,nontriv] %*% diag(ldata.svd$d[nontriv]) * (1/sqrt(r))
    } else {
# weighted logratio variance
      r <- apply(datatarget, 1, sum) / sum(datatarget)
      c <- apply(datatarget, 2, sum) / sum(datatarget)
      data.mr  <- apply(as.matrix(ldata) %*% diag(c), 1, sum)
      data.c1 <- sweep(ldata, 1, data.mr) 
      data.c2 <- sweep(data.c1, 2, apply(data.c1, 2, mean)) 
      ldata <- diag(sqrt(r)) %*% data.c2 %*% diag(sqrt(c))     
      ldata.svd <- svd(ldata)
      nontriv <- which(ldata.svd$d > 1.e-12)
      data.rpc <- ldata.svd$u[,nontriv] %*% diag(ldata.svd$d[nontriv]) * (1/sqrt(r))
    }
  }
  if(!datasame) {
    r <- rep(1/nrow(data), nrow(data))
    ldata <- datatarget
    ldata <- sweep(ldata, 2, apply(ldata, 2, mean))
    ldata <- sqrt(1/nrow(data)) * ldata * sqrt(1/ncol(data))
    ldata.svd <- svd(ldata)
    nontriv <- which(ldata.svd$d > 1.e-12)
    data.rpc <- ldata.svd$u[,nontriv] %*% diag(ldata.svd$d[nontriv]) * (1/sqrt(r))    
  }  

# column variances
  data.rpc.scale <- diag(sqrt(r)) %*% data.rpc 
  ldata.totvar <- sum(ldata^2)

# find best one to start off process
  nratios <- ncol(datatarget)-1
  rationames <- rep("", nsteps)

  R2 <- matrix(0, ncol(data), ncol(data))
  for(j in 2:ncol(data)){
    for(i in 1:(j-1)) {
      labs <- c(colnames(data)[i], colnames(data)[j])
      if(length(Reduce(`intersect`,regmatches(labs,gregexpr("\\w+", labs))))==0) {   
        foo <- cbind(rep(1, nrow(data)), log(data[,i]/data[,j]))
        R2[i,j] <- sum((foo %*% solve(t(foo)%*%foo) %*% t(foo) %*% data.rpc.scale)^2 ) / ldata.totvar
      }
    }
  }

  R2max <- max(R2)
  ratios <- as.matrix(which(R2==R2max, arr.ind=TRUE))

  if(nsteps==1 & top==1) {
    logratios <- log(data[,ratios[1,1]]/data[,ratios[1,2]])
    rationames <- paste(colnames(data)[ratios[1,1]], colnames(data)[ratios[1,2]], sep="/" )
    procrust <- protest(data.rpc, logratios, permutations=0)$t0
    return(list(rationames=rationames, ratios=ratios, logratios=logratios, R2max=R2max, procr=procrust, 
           rationames.top=rationames, ratios.top=ratios, logratios.top=logratios, R2.top=R2max, procr.top=procrust))
  }
  if(nsteps==1 & top>1) {
    logratios  <- log(data[,ratios[1,1]]/data[,ratios[1,2]])
    rationames <- paste(colnames(data)[ratios[1,1]], colnames(data)[ratios[1,2]], sep="/" )
    procrust   <- protest(data.rpc, logratios, permutations=0)$t0  
    R2.top     <- sort(R2, decreasing=TRUE)[1:top]
    ratios.top <- which(R2 >= R2.top[top], arr.ind=TRUE)
    ratios.top <- ratios.top[order(R2[ratios.top], decreasing=TRUE),]
    rationames.top <- paste(colnames(data)[ratios.top[,1]], colnames(data)[ratios.top[,2]], sep="/" )
    logratios.top  <- log(data[,ratios.top[,1]]/data[,ratios.top[,2]])
    procrust.top <- rep(0,top)
    for(jratio in 1:top) procrust.top[jratio] <- protest(data.rpc, logratios.top[,jratio], permutations=0)$t0 
    colnames(logratios.top) <- rationames.top
    return(list(rationames=rationames, ratios=ratios, logratios=logratios, R2max=R2max, procr=procrust, 
           rationames.top=rationames.top, ratios.top=ratios.top, logratios.top=logratios.top, R2.top=R2.top, procr.top=procrust.top))
  }
  
# loop over ratios as many times as there are columns in data minus 1
  for(jratio in 2:nsteps) {
    R2 <- matrix(0, ncol(data), ncol(data))
    for(j in 2:ncol(data)){
      for(i in 1:(j-1)) {
        labs <- c(colnames(data)[i], colnames(data)[j])
        if(length(Reduce(`intersect`,regmatches(labs,gregexpr("\\w+", labs))))==0) { 
          if(sum(apply(ratios, 1, function(x) all(x == c(i,j)))) == 0) {
            logratios <- log(data[,ratios[1,1]]/data[,ratios[1,2]])
            if(jratio>2) {
              for(jrats in 2:(jratio-1)) logratios <- cbind(logratios,  log(data[,ratios[jrats,1]]/data[,ratios[jrats,2]]))
            }
            foologratios <- cbind(logratios, log(data[,i]/data[,j]))
#            foo <- rda(ldata, foologratios)
            foo <- cbind(rep(1, nrow(data)), foologratios)
            R2[i,j] <- sum((foo %*% tryCatch(solve(t(foo)%*%foo), error=function(e) e=matrix(0, nrow=ncol(foo), ncol=ncol(foo))) %*% t(foo) %*% data.rpc.scale)^2 ) / ldata.totvar
#            R2[i,j] <- foo$CCA$tot.chi / foo$tot.chi
          }
        }
      }
    }
    R2max <- c(R2max, max(R2))
#    R2max <- c(R2max, max(R2[R2<0.999999]))
    foo <- as.matrix(which(abs(R2-max(R2))<1E-8, arr.ind=TRUE))
    if(nrow(foo)==1) ratios <- rbind(ratios, foo)
    if(nrow(foo)>1 & !random) {
# tie: find best Procrustes fit
# have to weight the logratios for testing Procrustes fit
# use PCA function on logratios
      procr <- rep(0, nrow(foo))
      for(ip in 1:nrow(foo)) {
        foorats     <- cbind(logratios, log(data[,foo[ip,1]]/data[,foo[ip,2]]))
        foorats.c   <- c(c[ratios[,1]] * c[ratios[,2]], c[foo[ip,1]]*c[foo[ip,2]])
        foorats.w <- diag(sqrt(r)) %*% foorats %*% diag(sqrt(foorats.c))
        procr[ip]   <- protest(data.rpc, foorats.w, permutations=0)$t0       
      }
      ratios <- rbind(ratios, foo[which(procr==max(procr)),])
    }
    if(nrow(foo)>1 & random) {
# tie: break randomly
      foo <- foo[sample(1:nrow(foo)),]
      ratios <- rbind(ratios, foo[1,])
    }
  }
  if(nratios==nsteps) R2max[nratios] <- 1
  logratios <- cbind(logratios, log(data[,ratios[nsteps,1]]/data[,ratios[nsteps,2]]))
  for(jratio in 1:nsteps) rationames[jratio] <- paste(colnames(data)[ratios[jratio,1]], colnames(data)[ratios[jratio,2]], sep="/" )
  colnames(logratios) <- rationames
  rownames(logratios) <- rownames(data)

# Procrustes correlations
  foorats.c <- apply(data, 2, mean)
  procrust  <- rep(0, ncol(logratios))
  foorats.w <- logratios[,1] 
  procrust[1] <- protest(data.rpc, foorats.w, permutations=0)$t0

  for(j in 2:ncol(logratios)) {
    foorats    <- logratios[,1:j]
    foorats.wt <- foorats.c[ratios[1:j,1]]*foorats.c[ratios[1:j,2]]
    foorats.w  <- diag(sqrt(r)) %*% foorats %*% diag(sqrt(foorats.wt))
    procrust[j] <- protest(data.rpc, foorats.w, permutations=0)$t0
  }
  rownames(ratios) <- rationames
# If nsteps < nratios and top > 1 evaluate top's ratios etc...'  
  if(top==1) {
    rationames.top <- rationames[nsteps]
    ratios.top     <- ratios[nsteps,]
    logratios.top  <- logratios[,nsteps]
    R2.top         <- R2max[nsteps]
    procrust.top   <- procrust[nsteps]
  }
  if(nsteps<nratios & top>1){
    R2.top     <- sort(R2, decreasing=TRUE)[1:top]
    ratios.top <- which(R2 >= R2.top[top], arr.ind=TRUE)
    ratios.top <- ratios.top[order(R2[ratios.top], decreasing=TRUE),]
    rationames.top <- paste(colnames(data)[ratios.top[,1]], colnames(data)[ratios.top[,2]], sep="/" )
    rownames(ratios.top) <- rationames.top
    logratios.top  <- log(data[,ratios.top[,1]]/data[,ratios.top[,2]])
    procrust.top <- rep(0,top)
    foorats    <- logratios[,1:(nsteps-1)]
    for(jratio in 1:top) {
      foorats.wt <- foorats.c[ratios[1:(nsteps-1),1]]*foorats.c[ratios[1:(nsteps-1),2]]
      foorats.top<- cbind(foorats, logratios.top[,jratio])
      foorats.wt <- c(foorats.wt, foorats.c[ratios.top[jratio,1]]*foorats.c[ratios.top[jratio,2]])
      foorats.w  <- diag(sqrt(r)) %*% foorats.top %*% diag(sqrt(foorats.wt))
      procrust.top[jratio] <- protest(data.rpc, foorats.w, permutations=0)$t0
    }
    colnames(logratios.top) <- rationames.top
  }
  return(list(rationames=rationames, ratios=ratios, logratios=logratios, R2max=R2max, procr=procrust, 
         rationames.top=rationames.top, ratios.top=ratios.top, logratios.top=logratios.top, R2.top=R2.top, procr.top=procrust.top))
}

