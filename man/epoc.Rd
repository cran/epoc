\name{EPoC}
\alias{epoc}
\alias{epocA}
\alias{epocG}
\alias{summary.EPOCA}
\alias{summary.EPOCG}
\alias{predict.EPOCA}
\alias{predict.EPOCG}
\alias{print.EPOCA}
\alias{print.EPOCG}
\alias{coef.EPOCA}
\alias{coef.EPOCG}
\alias{epoc.bootstrap}
\alias{epoc.lambdamax}
\alias{plot.EPOCA}
\alias{plot.EPOCG}
\alias{write.sif}
\alias{as.graph.EPOCA}
\alias{as.graph.EPOCG}
\alias{as.igraph.EPOCA}
\alias{as.igraph.EPOCG}
\title{EPoC}
\encoding{utf-8}
\description{
  EPoC (Endogenous Perturbation analysis of Cancer)
}
\usage{
epocA(Y, U=NULL, lambdas=NULL, thr=1.0e-10, trace=0)
epocG(Y, U, lambdas=NULL, predictorix=NULL, thr=1.0e-10, trace=0)
epoc.bootstrap(Y, U, nboots=100, bthr=NULL, method='epocG',...)
epoc.lambdamax(X, Y, getall=F)
as.igraph.EPOCA(model,k=1)
as.igraph.EPOCG(model,k=1)
as.graph.EPOCA(model,k=1)
as.graph.EPOCG(model,k=1)
write.sif(model, k=1, file="", append=F)
\method{print}{EPOCA}(x,...)
\method{print}{EPOCG}(x,...)
\method{summary}{EPOCA}(object, ...)
\method{summary}{EPOCG}(object, ...)
\method{coef}{EPOCA}(object, k=1, ...)
\method{coef}{EPOCG}(object, k=1, ...)
\method{predict}{EPOCA}(object, newdata,k=1,trace=0, ...)
\method{predict}{EPOCG}(object, newdata,k=1,trace=0, ...)
\method{plot}{EPOCA}(x, layout=NULL,k=1,threed=F,...)
\method{plot}{EPOCG}(x, layout=NULL,k=1,threed=F,...)
}
\arguments{
\item{Y}{N x p matrix of mRNA transcript levels for p genes and N samples for epocA and epocG. For \code{epoc.lambdamax} \code{Y} is a multi-response matrix}
\item{U}{N x p matrix of DNA copy number}
\item{lambdas}{Non-negative vector of relative regularization parameters for lasso. lambda=0 means no regularization. Default=NULL means let epoc create a vector}
\item{predictorix}{Vector. Use a subset of the variables \code{1:p} as predictors, default is to use all}
\item{thr}{Threshold for convergence. Default value is 1e-10.  Iterations stop when max absolute parameter change is less than thr}
\item{trace}{Level of detail for printing out information as iterations proceed.
Default 0 -- no information}
\item{nboots}{Number of bootstrap iterations to run}
\item{method}{For \code{epoc.bootstrap} method is \code{"epocG"} or \code{"epocA"}}
\item{bthr}{Require presence of links in 100*bthr\% of the bootstrap iterations}
\item{X}{For\code{epoc.lambdamax} \code{Y} is the design matrix}
\item{getall}{Logical. For \code{epoc.lambdamax} get a vector of all inf-norms instead of a single maximum}
\item{layout}{Default NULL means layout.circle for 2D and layout.sphere for 3D plot}
\item{threed}{Default FALSE means 2D plot, 3D plot requires package \code{rgl}}
\item{file}{either a character string naming a file or a connection open for writing. \code{""} indicates output to the console}
\item{append}{logical. Only relevant if \code{file} is a character string.  If \code{TRUE}, the output is appended to the file.  If \code{FALSE}, any existing file of the name is destroyed}
\item{model}{Model set from epocA or epocG}
\item{k}{Select a model of sparsity level k in [1,K]}
\item{newdata}{List of Y and U matrices required for prediction. \code{epocG} requires just U.}
\item{...}{Parameters passed down to underlying function, e.g. print.default, plot or rglplot, or for epoc.bootstrap, epocG or epocA}
\item{x}{Model parameter to \code{print} and \code{plot}}
\item{object}{Model parameter to \code{summary}, \code{coef} and \code{predict}}
}

\details{
\code{epocA} and \code{epocG} estimates sparse matrices A or G using fast lasso regression from mRNA transcript levels Y and CNA profiles U. Two models are provided, EPoC A where

      AY + U + R = 0

and EPoC G where

      Y = GU + E.

The matrices R and E are so far treated as noise. For details see the reference section and the manual page of `lassoshooting'.

\code{epoc.bootstrap} run \code{epocA} or \code{epocG} using bootstrap, see values section.

\code{epoc.lambdamax} returns the maximal lambda value in a series of lasso regression models such that all coefficients are zero.

\code{plot} plot graph of model using the \code{igraph} package
arrows only tell direction, not inhibit or stimulate
}
\value{
  \code{epocA} and \code{epocG} returns an object of \code{class} `"epocA"' and `"epocG"' respectively.

  The methods \code{summary}, \code{print}, \code{coef}, \code{predict} can be used as with other models. \code{coef} and \code{predict} take an extra optional integer parameter \code{k} (default 1) which gives the model at the given density level.
  
An object of these classes is a list containing at least the following components:

\item{coefficients}{list of A or G matrices for the different lambdas}
%\item{fitted.values}{the fitted mean values}
%\item{residuals}{the residuals, i.e. the response minus fitted values}
\item{R2}{R², coefficient of determination}
\item{R2.adj}{adjusted R², coefficient of determination adjusted for number of explanatory terms}
\item{s2}{OLS estimate of the error variance}
\subsection{epocG}{objects of class \code{epocG} also contain

  diagonal: Direct effects of CNA to corresponding gene
}
\subsection{epoc.bootstrap}{returns a p x p x k array of values in [0,1] where 1 is presence of link in 100\% of bootstrap iterations for the k different lambda values for p different genes.
}
}
\author{
  Tobias Abenius
}
\references{
   Rebecka Jörnsten, Tobias Abenius, Teresia Kling, Linnéa Schmidt, Erik Johansson, Torbjörn Nordling, Bodil Nordlander, Chris Sander, Peter Gennemark, Keiko Funa, Björn Nilsson, Linda Lindahl, Sven Nelander. (2011)  Network modeling of the transcriptional effects of copy number aberrations in glioblastoma.   Molecular Systems Biology 7 (to appear)
}
\examples{
\dontrun{
modelA <- epocA(X,U)
modelG <- epocG(X,U)

# plot sparsest A and G models using the igraph package
# arrows only tell direction, not inhibit or stimulate
par(mfrow=c(1,2))
plot(modelA)
plot(modelG)

# OpenGL 3D plot on sphere using the igraph and rgl packages
plot(modelA,threed=T)

# Write the graph to a file in SIF format for import in e.g. Cytoscape
write.sif(modelA,file="modelA.sif")

# plot graph in Cytoscape using Cytoscape XMLRPC plugin and 
# R packages RCytoscape, bioconductor graph, XMLRPC
require('graph')
require('RCytoscape')
g <- as.graph.EPOCA(modelA,k=5)
cw <- CytoscapeWindow("EPoC", graph = g)
displayGraph(cw)

# prediction
N <- dim(X)[1]
ii <- sample(1:N, N/3)

modelG <- epocG(X[ii,], U[ii,])
K <- length(modelA$lambda) # densest model index index
newdata <- list(U=U[-ii,])
e <- X[-ii,] - predict(modelA, newdata, k=K)
RSS <- sum(e^2)
cat("RMSD:", sqrt(RSS/N), "\n")

}
}
\keyword{cancer}
\keyword{selection}
\keyword{multivariate}
\keyword{models}
\keyword{graphs}

