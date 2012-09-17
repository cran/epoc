pkgname <- "epoc"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('epoc')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("epoc")
### * epoc

flush(stderr()); flush(stdout())

### Name: EPoC
### Title: EPoC
### Aliases: epoc epocA epocG summary.EPOCA summary.EPOCG predict.EPOCA
###   predict.EPOCG print.EPOCA print.EPOCG coef.EPOCA coef.EPOCG
###   epoc.lambdamax write.sif as.graph.EPOCA as.graph.EPOCG
###   as.igraph.EPOCA as.igraph.EPOCG
### Keywords: cancer selection multivariate models graphs

### ** Examples

## Not run: 
##D modelA <- epocA(X,U)
##D modelG <- epocG(X,U)
##D 
##D # plot sparsest A and G models using the igraph package
##D # arrows only tell direction, not inhibit or stimulate
##D par(mfrow=c(1,2))
##D plot(modelA)
##D plot(modelG)
##D 
##D # OpenGL 3D plot on sphere using the igraph and rgl packages
##D plot(modelA,threed=T)
##D 
##D # Write the graph to a file in SIF format for import in e.g. Cytoscape
##D write.sif(modelA,file="modelA.sif")
##D 
##D # plot graph in Cytoscape using Cytoscape XMLRPC plugin and 
##D # R packages RCytoscape, bioconductor graph, XMLRPC
##D require('graph')
##D require('RCytoscape')
##D g <- as.graph.EPOCA(modelA,k=5)
##D cw <- CytoscapeWindow("EPoC", graph = g)
##D displayGraph(cw)
##D 
##D # prediction
##D N <- dim(X)[1]
##D ii <- sample(1:N, N/3)
##D 
##D modelG <- epocG(X[ii,], U[ii,])
##D K <- length(modelA$lambda) # densest model index index
##D newdata <- list(U=U[-ii,])
##D e <- X[-ii,] - predict(modelA, newdata, k=K)
##D RSS <- sum(e^2)
##D cat("RMSD:", sqrt(RSS/N), "\n")
##D 
## End(Not run)



cleanEx()
nameEx("plapply")
### * plapply

flush(stderr()); flush(stdout())

### Name: plapply
### Title: Parallell list apply
### Aliases: plapply
### Keywords: iteration list

### ** Examples

X1 <- array(1:4,dim=c(2,2))
X2 <- array(5:8,dim=c(2,2))
X3 <- array(9:12,dim=c(2,2))
X4 <- array(13:16,dim=c(2,2))
l <- plapply(list(X1,X2),list(X3,X4), function(E1,E2) E2 - E1)
stopifnot(all(sapply(l,sum)/4 == 4*2))



cleanEx()
nameEx("synth")
### * synth

flush(stderr()); flush(stdout())

### Name: synth
### Title: Blinded cancer mRNA, CNA and survival data
### Aliases: synth
### Keywords: datasets cancer epoc

### ** Examples

## Not run: 
##D   
##D   data(synth)
##D   y <- synth$y
##D   # standardize u
##D   u <- apply(synth$u, 2, function(x) x/sd(x))
##D   G <- epocG(Y=y, U=u)
##D   summary(G)
##D   plot(G)
## End(Not run)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
