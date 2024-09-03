## Function to find peaks in a signal
# inputs:
#   x   a signal vector
#   m   points either side of a peak being smaller than it
# returns:
#   a vector containing the indices of the peaks
# source: http://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data

find_peaks <- function (x, m){
  if(!is.null(names(x))){
    names(x)[-1] <- names(x)[-length(x)]
  }
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  return(unlist(pks))
}

## Function to find peaks, valleys and zeros in a signal
# inputs:
#   x   a signal vector
#   m   points to consider at either side for peak, valley or zeroes identification
# returns:
#   a vector containing the indices of the peaks
fpvz <- function(x, m = 3){
  pvz <- c(find_peaks(abs(x), m = m), find_peaks(-abs(x), m = m))
  return(pvz)
}

## Function to find common peaks valleys and zeros between two signals (a reference and a slave)
# inputs:
#   xr      a signal vector or a list with p, z and v objects (peaks, zeroes and valleys)
#   xs      a signal vector (optional)
#   m       points to consider at either side for peak, valley or zeroes identification
#   wavs    the wavelengths or wavenumbers
#   ws.tol  the minimum distance (in the x-units of the signal) at which 
#           the selected peaks must be apart from each other (in order to avoid 
#           worng aligments) 
#   eg.rm   the wavelengths or wavenumbers to be removed from the anylisis (only at the 
#           edges/extremes of the spectra)


# returns:
#   a list of containing the indices of the peaks for the reference and the slave (if provided)
#   if only reference signal is provided then it returns a list 
#   with p, z and v objects (peaks, zeroes and valleys)


# matplot(wls,
#         -abs(t(sspc[c(wms, j),])),
#         type = "l",
#         col = c("black", "red"),
#         lty = 1)
# 
# abline(v = wls[j.pvz$reference])
# abline(v = wls[j.pvz$slave], col = "red")
# 
# abline(v = wls[xsv], col = "red")
# 
# xr = sspc[wms,]
# xs = sspc[j,]
# 
# eg.rm <- c(seq(857, 865, by = 0.5),
#            seq(1033.0, 1041,by = 0.5))
# 
# 
# 
# cpvz(xr = sspc[wms,], xs = sspc[j,], m = mpd, wavs = wavs, ws.tol = j.wav.altol, eg.rm = wavs[1:4])
# cpvz(xr = sspc[wms,], xs = sspc[j,], m = mpd, wavs = wavs, ws.tol = j.wav.altol)

#xr = wm.pvzpts; xs = sspc[j,]; m = mpd; wavs = wls; ws.tol = j.wav.altol; eg.rm = egdes2rm
cpvz <- function(xr, xs, m = 3, wavs, ws.tol, eg.rm){

  if(!missing(eg.rm)){
    wavs.p <- wavs[!wavs %in% eg.rm]
    if(is.list(xr)){
      p <- wavs[xr$p]
      z <- wavs[xr$z]
      v <- wavs[xr$v]
      
      xr$p <- (1:length(wavs.p))[wavs.p  %in% p]
      xr$z <- (1:length(wavs.p))[wavs.p  %in% z]
      xr$v <- (1:length(wavs.p))[wavs.p  %in% v]
      
      names(xr$p) <- p
      names(xr$z) <- z
      names(xr$v) <- v

    }else{
      xr <- xr[!wavs %in% eg.rm]
    }
    
    if(!missing(xs)){
      xs <- xs[!wavs %in% eg.rm]
    }
  }else{
    wavs.p <- wavs
  }
  
  if(missing(xs)){
    names(xr) <- wavs.p
    ## first find the peaks
    xrp <- find_peaks(xr, m = m)
    
    ## second find the zeroes
    xrz <- find_peaks(abs(xr), m = m)
    xrz <- xrz[!xrz %in% xrp]
    
    ## then find the valleys
    xrv <- find_peaks(-abs(xr), m = m)
    xrv <- xrv[!xrv %in% c(xrp, xrz)]
    
    if(!missing(eg.rm)){
      xrp <- (1:length(wavs))[wavs %in% as.numeric(names(xrp))]
      xrz <- (1:length(wavs))[wavs %in% as.numeric(names(xrz))]
      xrv <- (1:length(wavs))[wavs %in% as.numeric(names(xrv))]
    }
    
    return(list(p = xrp, z = xrz, v = xrv))
    
  }else{
    
    if(is.list(xr)){
      
      names(xs) <- wavs.p
      # insert here some sanity checks for the x-units
      
      ## first find the peaks
      xrp <- xr$p
      xsp <- find_peaks(xs, m = m)
      
      ## second find the zeroes
      xrz <- xr$z
      xsz <- find_peaks(abs(xs), m = m)
      
      xrz <- xrz[!xrz %in% xrp]
      xsz <- xsz[!xsz %in% xsp]
      
      ## then find the valleys
      xrv <- xr$v
      xsv <- find_peaks(-abs(xs), m = m)
      
      xrv <- xrv[!xrv %in% c(xrp, xrz)]
      xsv <- xsv[!xsv %in% c(xsp, xsz)]
      
    }else{
     
      names(xr) <- names(xs) <- wavs.p
      
      # insert here some sanity checks for the x-units
      
      ## first find the peaks
      xrp <- find_peaks(xr, m = m)
      xsp <- find_peaks(xs, m = m)
      
      ## second find the zeroes
      xrz <- find_peaks(abs(xr), m = m)
      xsz <- find_peaks(abs(xs), m = m)
      
      xrz <- xrz[!xrz %in% xrp]
      xsz <- xsz[!xsz %in% xsp]
      
      ## then find the valleys
      xrv <- find_peaks(-abs(xr), m = m)
      xsv <- find_peaks(-abs(xs), m = m)
      
      xrv <- xrv[!xrv %in% c(xrp, xrz)]
      xsv <- xsv[!xsv %in% c(xsp, xsz)]
    }
    
    #compute the distances between bands
    
    if(length(xrp) > 0 & length(xsp) > 0){  
      bandist.p <- fDiss(Xr = as.matrix(wavs.p[xrp]), 
                         X2 = as.matrix(wavs.p[xsp]),
                         method = "euclid", 
                         center = F, scaled = F)
      
      xr.p2r.p <- apply(bandist.p, 1, min) < ws.tol
      xrp <- xrp[xr.p2r.p]
      xsp <- xsp[apply(bandist.p, 1, which.min)][xr.p2r.p]
    }else{
      xrp <- xsp <- NULL
    }
    
    if(length(xrz) > 0 & length(xsz) > 0){  
      bandist.z <- fDiss(Xr = as.matrix(wavs.p[xrz]), 
                         X2 = as.matrix(wavs.p[xsz]),
                         method = "euclid", 
                         center = F, scaled = F)
      
      xr.p2r.z <- apply(bandist.z, 1, min) < ws.tol
      xrz <- xrz[xr.p2r.z]
      xsz <- xsz[apply(bandist.z, 1, which.min)][xr.p2r.z]
    }else{
      xrz <- xsz <- NULL
    }
    
    if(length(xrv) > 0 & length(xsv) > 0){  
      bandist.v <- fDiss(Xr = as.matrix(wavs[xrv]), 
                         X2 = as.matrix(wavs[xsv]),
                         method = "euclid", 
                         center = F, scaled = F)
      
      xr.p2r.v <- apply(bandist.v, 1, min) < ws.tol
      xrv <- xrv[xr.p2r.v]
      xsv <- xsv[apply(bandist.v, 1, which.min)][xr.p2r.v]
    }else{
      xrv <- xsv <- NULL
    }
    
    if(!missing(eg.rm)){
      xrp <- (1:length(wavs))[wavs %in% as.numeric(names(xrp))]
      xrz <- (1:length(wavs))[wavs %in% as.numeric(names(xrz))]
      xrv <- (1:length(wavs))[wavs %in% as.numeric(names(xrv))]

      xsp <- (1:length(wavs))[wavs %in% as.numeric(names(xsp))]
      xsz <- (1:length(wavs))[wavs %in% as.numeric(names(xsz))]
      xsv <- (1:length(wavs))[wavs %in% as.numeric(names(xsv))]
    }

    xrpzv <- c(xrp, xrz, xrv)
    xspzv <- c(xsp, xsz, xsv)
    
    dpr <- duplicated(xrpzv)
    dps <- duplicated(xspzv)
    
    xrpzv <- xrpzv[!dpr*dps]
    xspzv <- xspzv[!dpr*dps]
    
    
    return(list(reference = xrpzv, slave = xspzv))
  }
}

mround <- function(x,base){ 
  base*round(x/base) 
} 

## Function for piece-wise spectra transfer
## Arguments:
##  master       the reference spectra (n x d dimensions for model calibrations)
##  slave        the spectra to be standardized (n x d dimensions for model calibrations)
##  pred.slave   the spectra to be standardized based on the model master ~ f(slave)
##  wavs         numeric vector correspo9nding to the wavelengths/wavenumbers of the spectra
##  seed         seed for random number initialization
##  ws           window size (must be odd).
##  method       character string the regression method
##  center       logical indicating wheter to center the slave data before builing the piece-wise models
##  scaled       logical indicating wheter to scale the slave data before builing the piece-wise models
##  noisev       if method = "gp" (gaussin process regression), the noise variance
##  lm.control   if method = "lm" the control of for the linear model regression see lmrob.control function in robustbase package
##  glm.control  if method = "glm" the control of for the linear model regression see glmrobMqle.control function in robustbase package
##  ncores       the number of cores for parallel processing
## Value:
##  A list with two objects:
##  $coefficients a matrix with regression coefficients (if the method is either "lm" or "glm")
##  $trpredicted  a matrix containing the transferred/predicted spectra

transfSpec <- function(master,
                       slave,
                       pred.slave,
                       wavs,
                       seed = 1,
                       ws,
                       method = c("lm", "glm", "gp"),
                       center = FALSE,
                       scaled = TRUE,
                       noisev = 0.001,
                       lm.control = lmrob.control(refine.tol= 1.e-7, 
                                                  nResample = 500,
                                                  solve.tol = 1.e-6,
                                                  k.max = 40000, 
                                                  max.it = 4000),
                       glm.control = glmrobMqle.control(acc = 1e-04, 
                                                        test.acc = "coef", 
                                                        maxit = 4000, 
                                                        tcc = 10),
                       ncores =  detectCores() - 1,
                       ...){
  
  require(parallel)
  #require(snow)
  require(robustbase)
  require(matrixStats)
  require(kernlab)
  
  set.seed(seed)
  
  if(!is.matrix(master)){
    stop("master must be a matrix")
  }
  
  if(!is.matrix(slave)){
    stop("slave must be a matrix")
  }
  
  if(sum(!dim(master) == dim(slave)) > 0){
    stop("The dimensions of master must match the dimensions of slave")
  }
  
  if(sum(!colnames(master) == colnames(slave)) > 0){
    stop("The variables in master do not match the variables in slave")
  }
  
  if(ncol(master) != length(wavs)){
    stop("The length of wavs must match the number of variables in both master and slave")
  }
  
  if(!is.numeric(wavs)){
    stop("wavs must be numeric")
  }
  
  if(!ws %% 2 != 0){
    stop("ws must be odd")
  }
  
  
  match.arg(method, c("lm", "glm", "gp", "pls", "svm"))
  
  colnames(master) <- paste("master", wavs, sep = "")
  colnames(slave) <- paste("slave", wavs, sep = "")
  i.ws <- (ws/2)-0.5
  wscomps <- matrix(NA, nrow(slave), i.ws)
  slave <- cbind(wscomps, slave, wscomps)
  
  slavec <- scale(slave, center = center, scale = scaled)
  
  if(missing(pred.slave)){
    pred.slave <- data.frame(slavec)
    pred.slave <- data.frame(pred.slave)
  }else{
    colnames(pred.slave) <- paste("slave", wavs, sep = "")
    wscomps <- matrix(NA, nrow(pred.slave), i.ws)
    pred.slave <- cbind(wscomps, pred.slave, wscomps)
    cnt <- attr(slavec, "center")
    scl <- attr(slavec, "scale")
    if(!is.null(cnt)){
      pred.slave <- sweep(pred.slave, MARGIN = 2, FUN = "-", STATS = cnt)
    }
    if(!is.null(scl)){
      pred.slave <- sweep(pred.slave, MARGIN = 2, FUN = "/", STATS = scl)
    }
    pred.slave <- data.frame(pred.slave)
  }
  
  slave <- as.data.frame(slavec)
  
  if(method == "glm"){
    control <- glm.control
    inoisev <- NULL
  }
  if(method == "lm"){
    control <- lm.control
    inoisev <- NULL
  }
  if(method %in% c("gp", "svm", "pls")){
    control <- NULL
    inoisev <- noisev
  }
  

  i.fnctn <- function(..i.., 
                      imaster,
                      islave,
                      imethod,
                      slave.preds, 
                      iws,
                      icontrol,
                      inoisev,
                      ...){
    
    data <- islave[,..i..:(..i..+ iws - 1)]
    
    init <- rep(0,ncol(data))
    init[ceiling(iws/2)] <- 1
    init <- c(mean(imaster[,..i..] -  data[,init == 1]), init[!colSums(is.na(data)) > 0])
    
    data <- data.frame(x = imaster[,..i..], data[,!colSums(is.na(data)) > 0])
    
    names(init) <- c("(Intercept)",names(data)[-1])
    
    i.formula <- as.formula(paste("x","~", paste(colnames(data)[-1], collapse = "+")))
    
    i.slave.preds <- slave.preds[,colnames(data)[-1]]
    # gpm <- resemble:::gpr.dp(Xn = data, 
    #                          Yn = master[,..i..], 
    #                          noise.v = noise.v, 
    #                          scaled = scaled, 
    #                          center = center)
    
    
    
    if(imethod == "glm"){
      pen <- try(glmrob(formula =  i.formula,   
                        data = data, family = gaussian, 
                        start = init,
                        control = icontrol), 
                 silent = T)
    }
    
    if(imethod == "lm"){
      pen <- try(lmrob(formula =  i.formula,   
                       data = data, 
                       init = list(coefficients = init,  scale = 1), 
                       control = icontrol),
                 silent = T)
      
    }
    
    if(imethod == "gp"){
      
      pen <- try(resemble:::gprdp(X = as.matrix(data[,-1]), 
                                  Y = as.matrix(data[,1]),
                                  noisev = inoisev, scale = F),
                 silent = T)
    }
    
    if(imethod == "svm"){
      pen <- kernlab:::ksvm(x = as.matrix(data[,-1]), y = as.matrix(data[,1]),
                           scaled = F, kernel="vanilladot", kpar = list(), C=inoisev)
    }
    
    if(imethod == "pls"){
      pen <- pls:::plsr(i.formula, ncomp = inoisev, scale = F, data = data, method="oscorespls")
    }
    
    ##resemble:::opls
    
    if(class(pen)[[1]] != "try-error"){
      
      if(imethod %in% c("lm", "glm")){
        i.pred <- predict(pen, slave.preds)
        i.cfcs <- pen$coefficients
      }
      
      if(imethod == "gp"){
        i.pred <- resemble:::predgprdp(Xz = pen$Xz,
                                       alpha = pen$alpha,
                                       newdata = as.matrix(i.slave.preds),
                                       scale = pen$is.scaled,
                                       Xcenter = pen$Xcenter,
                                       Xscale = pen$Xscale,
                                       Ycenter = pen$Ycenter,
                                       Yscale = pen$Yscale)
        i.cfcs <- NULL
      }
      
      if(imethod == "svm"){
        i.pred <- kernlab:::predict(pen, i.slave.preds)
        i.cfcs <- NULL
      }
      
      if(imethod == "pls"){
        i.pred <- pls:::predict.mvr(pen, i.slave.preds, ncomp = inoisev)[,,1]
        i.cfcs <- c(NA, pen$coefficients[,,inoisev])
      }
      
    }else{
      i.pred <- rep(NA, nrow(slave.preds))
      i.cfcs <- rep(NA, ncol(data))
    }
    if(length(i.cfcs)-1 < iws){
      i.cfcs <- c(i.cfcs, rep(NA, iws - length(i.cfcs) + 1))
    }
    return(c(i.cfcs, i.pred))
  }
  
  cl <- makePSOCKcluster(ncores, outfile = "")
  junk <- clusterEvalQ(cl, require(robustbase, quietly = TRUE))
  
  
  
  bk <- parSapply(cl = cl,
                  X = 1:ncol(master),
                  FUN = i.fnctn,
                  imaster = master,
                  islave = slave,
                  imethod = method,
                  slave.preds = pred.slave,
                  iws = ws,
                  inoisev = noisev,
                  icontrol = control)
  
  stopCluster(cl)
  
  
  # bk <- sapply(X = 1:ncol(master), 
  #              FUN = i.fnctn, 
  #              imaster = master,
  #              islave = slave, 
  #              imethod = method,
  #              slave.preds = pred.slave,
  #              iws = ws,
  #              icontrol = control)
  
  coeffs <- bk[1:(ws+1),]
  rownames(coeffs)[-1] <- paste("coeff", 1:ws, sep = "_")
  colnames(coeffs) <- wavs
  
  trspreds <- bk[-c(1:(ws+1)),]
  colnames(trspreds) <- wavs
  rownames(trspreds) <- 1:nrow(trspreds)
  return(list(coefficients = coeffs,trpredicted = trspreds))
}


