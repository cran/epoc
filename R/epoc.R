# Tobias Abenius 2011
#
# S4 classes not used because of ignorance
#setClass("EPOCA",contains="list")
#setClass("EPOCG",contains="list")
c.lambda = "\u03BB" #small lambda
c.square = "\u00B2" #superscript square
c.infty = "\u221E"  #infinity
progressbar.width = 50
progressbar <- function(i,k,p,q,progress) {
  progress.old <- round(progress,digits=0)
  progress <- round(progressbar.width * (p * (k-1) + i) / (p*q),digits=0)
  if (progress.old == 0 | progress > progress.old) {
    cat(paste('\r|',paste(rep('=', progress),collapse=''), '>',paste(rep(' ', progressbar.width-progress),collapse=''),'|',sep=''))
  }
  return(progress)
}
plapply <- function(X1,X2,FUN, ...) {
  FUN <- match.fun(FUN)
  if (length(X1) != length(X2)) stop("x1 and x2 are not of same length")
  if (!is.vector(X1) || is.object(X1)) X1 <- as.list(X1)
  if (!is.vector(X2) || is.object(X2)) X2 <- as.list(X2)
  l.new <- list()
  for (k in 1:length(X1))
    l.new[[k]] <- FUN(X1[[k]], X2[[k]], ...)
  return(l.new)
}
reg <- function(y,x) {
#  mode <- 3
#  if (mode==1) {
#    require('corpcor')
#    pinvu <- pseudoinverse(as.matrix(t(x)))
#    d <- as.matrix(t(y)) %*% pinvu
#  } else if (mode==2) {
#    d <- coef(lm(y~x))[1]
#  } else if (mode==3) {
    d <- coef(lsfit(x,y,intercept=T))[2]
#  }
  d
}
coef.EPOCA <- function(object, k=1, ...) {
  object$coefficients[[k]]
}
predict.EPOCG <- function(object,newdata,k=1,trace=0, ...) {
  if (typeof(newdata) == "list") # works for data.frame also
    U <- newdata$U
  else
    U <- newdata
  if (is.null(U)) stop("predict require U")
  m <- dim(U)[1]
  p <- dim(U)[2]
  if (trace > 0) cat("Predicting for p =",p,"variables, in",m,"points\n")
  (U - object$U.mean) %*% coef(object,k=k, ...)
}
predict.EPOCA <- function(object,newdata,k=1,trace=0, ...) {
  if (typeof(newdata) == "list") { # works for data.frame also
    Y <- newdata$Y
    if (is.null(Y))
      stop("Y is required")
    N <- dim(Y)[1]
    p <- dim(Y)[2]
    U <- newdata$U
    if (is.null(U))
      U <- array(object$U.mean,dim=c(N,p))
    else
      U <- U - object$U.mean
  } else {
    Y <- newdata
    N <- dim(Y)[1]
    p <- dim(Y)[2]
    U <- array(object$U.mean,dim=c(N,p))
  }
  Y <- Y - object$Y.mean
  if (is.null(Y)) stop("predict require Y")
  if (trace > 0)
    cat("\nUsing direct effects from object\n")
  YonU <- U%*%diag(object$d) 
 
  if (trace > 0) cat("Predicting for p =",p,"variables, in",N,"points\n")
  Y %*% coef(object,k=k, ...) + YonU
}
summary.EPOCA <- function(object, ...) {
  K <- length(object$lambda)
  sp <- array(0,dim=c(K,4))
  dimnames(sp) <- list(paste(c.lambda,"=",round(object$lambda,digits=4),sep=''),c(paste("R",c.square,sep=''),"Cp","RSS","non-zeros"))
  sp[,1] <- round(object$R2,digits=4)
  sp[,2] <- round(object$Cp,digits=4)
  sp[,3] <- round(object$RSS,digits=4)
  p <- dim(coef(object))[1]
  #links <- function (B) as.integer(sum(B * (1 - diag(array(1,dim=p))) != 0))
  links <- function (B) as.integer(sum(B != 0))
  q <- length(object$coefficients)
  for(k in 1:q) sp[k,4] <- links(object$coefficients[[k]])
  ans <- list(call=object$call,models=sp,SS.tot=object$SS.tot,d=object$d)
  class(ans) <- "summary.EPOCA"
  return(ans)
}
print.summary.EPOCA <- function(x, ...) {
  digits = max(3, getOption("digits") - 3)
  cat("\nCall:\n",deparse(x$call), "\n", sep="")
#  cat("\nDirect effects: \n")
#  print.default(x$d, ...)
  cat("\nModels:\n")
  print.default(x$models, print.gap=2, quote=F, ...)
  cat("\nSStot:",x$SS.tot,"\n")
  cat("\n")
}
print.EPOCA <- function(x, ...) {
  require(methods)
  digits = max(3, getOption("digits") - 3)
  cat("\nCall:\n",deparse(x$call), "\n\n", sep="")
  cat("Coefficients:\n")
  K <- length(x$lambda)
  for(k in 1:K) {
    print(paste("For ", c.lambda, "=", x$lambda[k], sep=''))
    #print(format(coef(x,k=k), digits=digits), print.gap=2, quote=FALSE, ...)
    print(coef(x,k=k))#, note.dropping.colnames=T,...)
  }
  cat("\nDirect effects: \n")
  print.default(x$d, ...)
  s <- summary(x)
  cat("\nModels: \n")
  print.default(s$models, print.gap=2, ...)
  cat("\nSStot:",x$SS.tot,"\n")
  cat("\n")
  invisible(x)
}
print.EPOCG <- function(x, ...) {
  require(methods)
  digits = max(3, getOption("digits") - 3)
  cat("\nCall:\n",deparse(x$call), "\n", sep="")
  cat("\nCoefficients:\n")
  K <- length(x$lambda)
  for(k in 1:K) {
    print(paste("For ", c.lambda, "=", x$lambda[k], sep=''))
    print(coef(x,k=k)) #, note.dropping.colnames=T, ...)
    #print.default(format(coef(x,k=k), digits=digits), print.gap=2, quote=FALSE, ...)
  }
  s <- summary(x)
  cat("\nDirect effects: \n")
  print.default(x$d, ...)
  cat("\nModels: \n")
  print.default(s$models, print.gap=2, ...)
  cat("\nSStot:",x$SS.tot,"\n")
  cat("\n")
  invisible(x)
}
as.igraph.EPOCA <- function(model,k=1) {
  p <- dim(model$coefficients)[1]
  adjm <- coef(model, k=k) 
  # columns are targets in igraph adjacency matrices
  require('igraph')
  g <- graph.adjacency(adjm,mode='directed',weighted=T,diag=F)
  return(g)
}
as.graph.EPOCA <- function(model, k=1) {
  require('graph')
  p <- dim(coef(model,k=k))[1]
  A <- abs(coef(model,k=k)) * (1 - diag(array(1,dim=p)))
  return( new("graphAM", adjMat=A, edgemode='directed') )
}
write.sif <- function(model, k=1, file="", append=F) {
  if (file == "") 
    file <- stdout()
  else if (is.character(file)) {
    file <- file(file, ifelse(append, "a", "w"))
    on.exit(close(file))
  }
  else if (!isOpen(file, "w")) {
    open(file, "w")
    on.exit(close(file))
  }
  if (!inherits(file, "connection")) 
    stop("'file' must be a character string or connection")
  A <- coef(model,k=k)
  p <- dim(A)[1]
  for (i in 1:p)
    for(j in 1:p) {
      e <- zapsmall(A[i,j],digits=3)
      if (e != 0)
	if (e < 0) {
	  cat(rownames(A)[i],'inhibits',colnames(A)[j],"\n",file=file)
	} else {
	  cat(rownames(A)[i],'stimulates',colnames(A)[j],"\n",file=file)
	}
    }
}
plot.EPOCA <- function (x, layout=NULL, k = 1, threed=F, ...) {
  p <- dim(x$coefficients[[1]])[1]
  adjm <- coef(x, k=k) * (1 - diag(array(1,dim=p)))
  # columns are targets in igraph adjacency matrices
  require('igraph')
  g <- graph.adjacency(adjm,mode='directed',weighted=T,diag=F)
  vx <- rownames(adjm)
  if (is.null(layout)) {
    if (!threed)
      g$layout <- layout.circle
    else
      g$layout <- layout.sphere
  } else {
    g$layout <- layout
  }
  if(threed) {
    rglplot(g,vertex.label=vx,vertex.size=2, ...)
  } else {
    plot(g,vertex.label=vx,vertex.label.cex=0.8,vertex.size=35,vertex.color=0,vertex.shape="rectangle",edge.width=1,edge.color='black', ...)
    title(x$call)
  }
}
plot.EPOCG <- plot.EPOCA
coef.EPOCG <- coef.EPOCA
print.summary.EPOCG <- print.summary.EPOCA
summary.EPOCG <- summary.EPOCA
as.graph.EPOCG <- as.graph.EPOCA
as.igraph.EPOCG <- as.igraph.EPOCA
as.graph.EPOCG <- as.graph.EPOCA

epoc.lambdamax <- function(X,Y,getall=F) {
  dims <- dim(Y)
  if (is.null(dims)) {
    return( norm(t(X) %*% Y,'i') )
  } else {
    n <- dims[2] #number of variables = genes
    lambdamax <- array(NaN,dim=n)
    for (k in 1:n) {
      predk = X
      predk[,k] <- 0
      lambdamax[k] <- norm(t(predk) %*% Y[,k],'i')
    }
    if (getall) return (lambdamax)
    else return(max(lambdamax))
  }
}
epoc.bootstrap <- function(Y,U,nboots=100,bthr=NULL,method='epocG',...) {
  first = T
  N <- dim(Y)[1]
  for (i in 1:nboots) {
    ix <- sample(1:N, N, replace=T)
    if (method == 'epocG')
      mod.boot1 <- epocG(Y[ix,],U[ix,],...)
    else
      mod.boot1 <- epocA(Y[ix,],U[ix,],...)
    D2 <- lapply(mod.boot1$coefficients, function (A) (zapsmall(A) != 0)*1)
    if (first) {
      D <- D2
      first = F
    } else {
      D <- plapply(D, D2, function(A,B) A+B)
    }
  }
  D <- lapply(D,function (A) A/nboots)
  if (!is.null(bthr))
    D <- lapply(D,function(A) A >= bthr)
  D
}
epocA <- function(Y,U=NULL,lambdas=NULL,thr=1e-10,trace=0) {
  require('lassoshooting')
  cl <- match.call()
#  require('Matrix')
  N <- dim(Y)[1] #number of equations = experiments
  p <- dim(Y)[2] #number of variables = genes
  if (trace > 0) cat("Solving for p =",p,"variables,",N,"equations\n")
  if (trace > 0) cat("Centering...")
  #center Y and U
  muY <- colMeans(Y)
  Y <- Y - muY
  hasU <- !is.null(U)
  if (hasU) {
    muU <- colMeans(U)
    U <- U - muU
  } else {
    U <- array(0,dim=c(N,p))
    muU <- array(0,dim=c(p))
  }
  if (trace > 0) cat("DONE\n")
  #/center
  #regressing Y on U
  if (trace > 0) cat("Regressing Y on U...")
  if (hasU) {
    d <- sapply(1:p,function(i) reg(Y[,i], U[,i]))
    d <- pmax(d,0)
  } else {
    d <- array(0,dim=p)
  }
  names(d) <- dimnames(Y)[[2]]
  if (trace > 0) cat("Correcting for direct effects...")
  YonU <- array(0,dim=c(N,p))
  for (i in 1:p) YonU[,i] <- d[i]*U[,i]
#  YonU <- U%*%diag(d)  # requires much more memory
  Yres <- Y - YonU
#  warning("breakpoint")

  muYres <- colMeans(Yres)
  Yres <- Yres - muYres
  
  #cat("max(abs(muYres)):",max(abs(muYres)),"\n")

  if (trace > 0) cat("DONE\nDirect effects of CNA:",d,"\n")

  #/regression
  if (is.null(lambdas)) {
    iv <- 0:12
    lambdaseries <- exp(-iv/6)
    iv <- 5:9
    lambdaseries <- c(lambdaseries,exp(-iv/2))
#    lambdaseries <- c(lambdaseries,0)
  } else {
    lambdaseries <- lambdas
  }
  q <- length(lambdaseries)
  #finding maximum lambda
  if (trace > 0) cat("Finding ",c.lambda,"_max...",sep='')
  inorms <- epoc.lambdamax(Y,Yres,getall=T)
  if (trace > 3) cat("\n||.||",c.infty,": ",inorms,"\n",sep='')
  lambdamax <- max(inorms)
  extreme.gene <- colnames(Y)[which.max(inorms)]
  if (trace > 0) {
    cat("DONE\nRel.",c.lambda,"s:",lambdaseries,"\n",sep='')
    cat("extreme gene:",extreme.gene,", lambdamax =",lambdamax,"\n")
  }
  #/finding maximum lambda
  ##############
  if (trace == 1) cat("Lasso regression...")
  lasso <- lassoshooting
  #B <- array(NaN,dim=c(n,n,q))
  B <- list()
  s2 <- array(NaN,dim=q)
  RSS <- array(NaN,dim=q)
  R2 <- array(NaN,dim=q)
  Cp <- array(NaN,dim=q)
  RMSD <- array(NaN,dim=q)
  if (trace == 1) cat("Lasso regression...")
  # Resp: Yres = Y - dU, Pred: Y
  pred <- Y
  XtX <- t(pred) %*% pred
  progress <- 0
  for(k in 1:q) {
    B1 <- Matrix(0,nrow=p,ncol=p,sparse=T)
    lambda <- lambdamax * lambdaseries[k]
    for (i in 1:p) {
      if (trace == 2) progress <- progressbar(i,k,p,q,progress)
      respk <- Yres[,i]
      predk <- pred 
      predk[,i] <- 0 # if we don't update this, lambdamax is wrong, perhaps lassoshooting forcezero should set this column to 0
      XtY <- t(predk) %*% respk 
      if (inorms[i] >= lambda) { # avoid overhead
	#cat("lambda > inorm:",lambda > inorms[i],", inorm = ",inorms[i],"\n")
	#cat("new inorm: ",epoc.lambdamax(pred,respk),"\n")
	#cat("|Xty|:", abs(XtY),"\n")
	#cat("max|Xty|:", max(abs(XtY)),"\n")
	if (F & lambdaseries[k] == 0) { # FIXME: time if this really is faster
	  model <- lm(respk ~ predk)
	  b <- coef(model)[-1] # skip intercept since centered
	  b[i] <- 0
	} else {
	  l <- lasso(xtx=XtX,xty=XtY,lambda=lambda,forcezero=i,thr=thr, trace=0)
	  b <- l$coefficients
	  #require(lars)
	  #l.lars <- lars(predk, respk, intercept=F, normalize=F, trace=0)
	  #p.lars <- predict(l.lars, s=100000000000, mode='lambda', type='coefficients')
	  #cat("lars lambdamax: ",p.lars$s,"\n")
	  #b.lars <- p.lars$coefficients
	  #b.lars[b.lars!=0]
	  #(1:p)[b.lars!=0]
	  #print(sum((b.lars - b)^2))
	}
	nonz <- (1:p)[b != 0]
	if (length(nonz)>0){
	  betas <- Matrix(0,nrow=p,ncol=1,sparse=T)
	  betas[nonz,1] <- b[nonz]
	  B1[,i] <- B1[,i] + betas
	}
      }
    }
    B[[k]] <- B1
    #yhat2 <- array(0,dim=c(m,n))
    #for(i in 1:n) {
    #  predk <- pred
    #  predk[,i] <- 0
    #  yhat2[,i] <- predk %*% B1[,i] + muYres[i] + YonU[,i] 
    #}
    muYresM <- t(array(rep(muYres,N),dim=c(p,N)))
    yhat <- Y %*% B1 + muYresM + YonU 
    #cat("yhat-diff: ",sum(sum((yhat - yhat2)^2)),"\n")
    if(trace > 3) {
      coryy <- array(0,dim=p)
      for(i in 1:p) {
	coryy[i] <- cor(Y[,i],yhat[,i])
	cat("cor(y,y^)_i=",i,":",coryy[i],"\n")
      }
      cat(k,"avgcor:",mean(coryy,na.rm=T))
      cat(", mincor:",min(coryy,na.rm=T),"\n")
    }
    resp <- Y
    if (k == 1) 
      SS.tot <- sum((resp - colMeans(resp))^2)
    e <- resp - yhat
    RSS[k] <- sum(e^2)
    R2[k] <- 1 - RSS[k] / SS.tot
#    R2.adj[k] <- 1 - (1 - R2[k])*( (m-1)/(m-n-1) )
    s2[k] <- sum(e^2) / (N - p)
    links <- function (B) as.integer(sum(B != 0))
    p.subset <- links(B1)
    d.subset <- sum(d != 0)
    Cp[k] <- RSS[k]/s2[k] - (N-2*(p.subset + d.subset))
    RMSD[k] <- sqrt(RSS[k]/N)
  }
  if (trace > 0) cat("\rDONE",rep(' ',progressbar.width),'\n',sep='')
  gs <- dimnames(Y)[[2]]
  for (k in 1:q)
    dimnames(B[[k]]) <- list(gs,gs)
  obj <- list(call=cl,coefficients=B,Y.mean=muY,U.mean=muU,Yres.mean=muYres,d=d,s2=s2,RMSD=RMSD,RSS=RSS,SS.tot=SS.tot,R2=R2,Cp=Cp,lambda=lambdaseries,lambdamax=lambdamax)
#  obj <- new("EPOCA",obj)
  class(obj) <- "EPOCA"
  return(obj)
}
epocG <- function(Y,U,lambdas=NULL,predictorix=NULL,thr=1e-10,trace=0) {
  require('lassoshooting')
  require('Matrix')
  givenB = NULL # only used for debugging
  N <- dim(Y)[1] #number of equations = experiments
  p <- dim(Y)[2] #number of variables = genes
  if (trace > 0) cat("Solving for p =",p,"variables,",N,"equations\n")
  if (trace > 0) cat("Centering...")
  muY <- colMeans(Y)
  Y <- Y - muY
  muU <- colMeans(U)
  U <- U - muU
  if (trace > 0) cat("DONE\n")
  if (is.null(predictorix)) {
    predictorix <- 1:p
    P <- p
  } else {
    P <- length(predictorix)
  }
  reindex <- array(0,dim=p)
  reindex[predictorix] <- 1:P

  if (is.null(lambdas)) lambdas <- c(0.99999, 1.25^(-(1:10))) #,0)
  if (trace > 0) cat("lambdas: ",lambdas,"\n")

  if (trace > 0) cat("Regressing Y on U...")
  d <- sapply(1:p,function(i) reg(Y[,i], U[,i]))
  d <- pmax(d,0)
  names(d) <- dimnames(Y)[[2]]
  if (trace > 0) cat("DONE\n")
  if (trace > 3) cat("Direct effects of CNA:",d,"\n")

  q <- length(lambdas)
  if (is.null(givenB)) {
    #B <- array(0, dim=c(p,p,q))
    B <- list()
  } else {
    B <- givenB
  }
  s2 <- array(NaN,dim=q)
  RSS <- array(NaN,dim=q)
  R2 <- array(NaN,dim=q)
  Cp <- array(NaN,dim=q)
  RMSD <- array(NaN,dim=q)

  if (trace > 0) cat("Gram matrix calculation of predictors...")
  pred <- U[,predictorix]
  XtX <- t(pred) %*% pred
  if (trace > 0) cat("DONE\n")
  if (trace > 0) cat("Correcting for direct effects...")
  YonU <- array(0,dim=c(N,p))
  for (i in 1:p) YonU[,i] <- d[i]*U[,i]
#  YonU <- U%*%diag(d)  # requires too much memory
  resp <- Y - YonU
  if (trace > 0) cat("DONE\n")

  #finding maximum lambda
  if (trace > 0) cat("Finding ",c.lambda,"_max...",sep='')
  inorms <- epoc.lambdamax(pred,resp,getall=T)
  #load('inorms.Rdata')

  if (trace > 3) cat("\n||.||",c.infty,": ",inorms,"\n",sep='')
  lambdamax <- max(inorms)
  extreme.gene <- colnames(Y)[which.max(inorms)]
  if (trace > 0) {
    cat("DONE\nRel.",c.lambda,"s:",lambdas,"\n",sep='')
    cat("extreme gene:",extreme.gene,", lambdamax =",lambdamax,"\n")
  }
  #/finding maximum lambda
  ########################
  # resp: y - b U  pred: U
  gs <- dimnames(Y)[[2]]
  if (trace == 1) cat("Lasso regression...")
  lasso <- lassoshooting
  progress <- 0
  for(k in 1:q) {
    B1 <- sparseMatrix(i=1:p, j=1:p, x=d)
    dimnames(B1) <- list(gs, gs)
    #B1 <- Matrix(0,nrow=p,ncol=p) # Make it sparse
    #B1 <- array(0,dim=c(p,p))
    if (is.null(givenB)) {
      lambda <- lambdas[k] * lambdamax
      for(i in 1:p) {
	if (trace == 2) progress <- progressbar(i,k,p,q,progress)
	if (inorms[i] >= lambda) {
	  if (trace == 3) cat("for var i =",i,"lasso...\n")
	  l <- lasso(xtx=XtX,x=pred,y=resp[,i],lambda=lambda,forcezero=i,thr=thr)
	  if (trace == 3) cat("for var i =",i," lasso done\n")
	  nonz <- (1:P)[l$coefficients != 0]
	  betas <- Matrix(0,nrow=p,ncol=1,sparse=T) # sparse M don't go well with arrays..
	  dimnames(betas)[[1]] <- gs
	  if (length(nonz)>0){
	    betas[predictorix[nonz],1] <- l$coefficients[nonz]
	    B1[,i] <- B1[,i] + betas
	  }
	}
      }
      #for(i in 1:p) B1[i,i] <- B1[i,i] + d[i] # extremely slow
      #diag(B1) <- diag(B1) + d                # too memory hungry
    }
    if (trace==3) cat("for lambda_k, k =",k,"\n")
    B[[k]] <- B1
    if (k==1) 
      SS.tot <- sum((Y - colMeans(Y))^2)
    yhat <- U %*% B1
    e <- Y - yhat
    RSS[k] <- sum(e^2)
    R2[k] <- 1 - RSS[k] / SS.tot
    #R2.adj[k] <- 1 - (1 - R2[k])*( (N-1)/(N-p-1) )
    s2[k] <- RSS[k] / (N - p)
    links <- function (B) as.integer(sum(B != 0))
    p.subset <- links(B1)
    d.subset <- sum(d != 0)
    Cp[k] <- RSS[k]/s2[k] - (N-2*(p.subset+d.subset))
    RMSD[k] <- sqrt(RSS[k]/N)
  }
  if (trace > 0) cat("\rDONE",rep(' ',progressbar.width),'\n',sep='')
  cl <- match.call()
  obj <- list(call=cl,coefficients=B,lambda=lambdas,lambdamax=lambdamax,d=d,U.mean=muU,R2=R2, Cp=Cp, SS.tot=SS.tot,RSS=RSS,RMSD=RMSD,s2=s2)
  class(obj) <- "EPOCG"
  obj
}


