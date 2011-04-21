\name{plot.modelsel}
\alias{plot.modelsel}
\title{Plot BIC, Mallow's Cp and \eqn{\lambda}}
\encoding{utf-8}
\description{Plot BIC, Mallow's Cp and \eqn{\lambda} }
\usage{plot.modelsel(x, ...)}
\arguments{
  \item{x}{An EPoC G or EPoC A object}
  \item{...}{Parameters passed down to underlying functions, \code{plot}, \code{lines}, \code{points}, \code{abline}, \code{axis}, \code{text} and \code{legend}.}
}
\details{
  Creates a plot that aids in model selection.
  Scale Bayesian information criterion (BIC) and Mallow's \eqn{C_p} between zero on one and put that on the y-axis and put relative \eqn{\lambda} values on the x-axis.
}
\seealso{
  \code{\link{epoc}}
}
\references{
   Rebecka Jörnsten, Tobias Abenius, Teresia Kling, Linnéa Schmidt, Erik Johansson, Torbjörn Nordling, Bodil Nordlander, Chris Sander, Peter Gennemark, Keiko Funa, Björn Nilsson, Linda Lindahl, Sven Nelander. (2011)  Network modeling of the transcriptional effects of copy number aberrations in glioblastoma.   Molecular Systems Biology 7 (to appear)
}
\keyword{plot}
\keyword{modelselection}

