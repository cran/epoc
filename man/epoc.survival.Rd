\name{epoc.survival}
\alias{epoc.survival}
\alias{epoc.svd}
\alias{epoc.svdplot}
\alias{plot.EPoC.survival}
\alias{summary.EPoC.survival}
\alias{print.summary.EPoC.survival}
\title{epoc.survival}
\encoding{utf-8}
\description{Survival analysis}
\usage{
  epoc.svd(model, k=1, C=1, numload=NULL)
  epoc.survival(G.svd, Y, U, surv, C=1, type=NULL)
	epoc.svdplot(G.svd, C=1)
  \method{plot}{EPoC.survival}(x,...)
  \method{summary}{EPoC.survival}(object,...)
  \method{print}{summary.EPoC.survival}(x,...)
}
\arguments{
  \item{model}{An object from \code{epocG} or \code{epocA} or a \code{Matrix} from \code{epoc.bootstrap} and friends.}
  \item{k}{In case \code{model} come from \code{epocG} or \code{epocA} select a model of sparsity level k in [1,K]. The default \code{k=1} means first/most sparse.}
  \item{C}{Default 1. For \code{epoc.svd} the number of components. For \code{epoc.survival} and \code{epoc.svdplot}, which component to use.}
  \item{numload}{Number of loadings in the sparse components, a vector for each component. Default 10 for all components.}
  \item{G.svd}{The list obtained from \code{epoc.svd}.}
  \item{Y}{mRNA, samples x genes.}
  \item{U}{CNA, samples x genes.}
  \item{surv}{Survival data for the samples.}
  \item{type}{\code{'G'} means EPoC G and \code{'A'} means EPoC A.}
  \item{x}{An object from \code{epoc.survival}}
  \item{object}{An object from \code{epoc.survival}}
  \item{...}{Parameters passed down to underlying functions, \code{plot.default} for \code{plot} and \code{print.default} for \code{print}.}
}
\details{
}
\value{
  A list of class \code{plot.EPoC.validation.pred} or \code{plot.EPoC.validation.W} respectively.
}
\seealso{
  \code{\link{epoc}}, \code{\link{epoc.validation}} and \code{\link[elasticnet]{spca}}
}
\references{
   Rebecka Jörnsten, Tobias Abenius, Teresia Kling, Linnéa Schmidt, Erik Johansson, Torbjörn Nordling, Bodil Nordlander, Chris Sander, Peter Gennemark, Keiko Funa, Björn Nilsson, Linda Lindahl, Sven Nelander. (2011)  Network modeling of the transcriptional effects of copy number aberrations in glioblastoma.   Molecular Systems Biology 7 (to appear)
}
\keyword{cancer}
\keyword{survival}
\keyword{svd}
\keyword{pca}

