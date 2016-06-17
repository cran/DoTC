\name{getStart}
\alias{getStart}
\title{Calculate Starting Values for Fuzzy k-means Clustering}
\description{This function calculates starting values as used in the \code{wrapFKM} function.}
\usage{
getStart(d, K = 10, nrep = 100, safety = TRUE, ...)
}
\arguments{
  \item{d}{data-set with standardized columns}
	\item{K}{number of clusters}
	\item{nrep}{number of repetitions for the call to \code{kmeans} (default is 100).}
	\item{safety}{As described in the help to \code{kmeans}, the algorithm may not converge in the quick-transfer stage. If \code{safety} is set to \code{TRUE}, each run where this happens is discarded and repeated from a different random starting point. }
	\item{...}{further arguments to \code{kmeans}.}
 }
\details{This function calculates starting values for \code{wrapFKM} as the solution of k-means clustering. }
\value{A matrix with \code{K} columns and \code{nrow(d)} rows. }
\author{Holger Sennhenn-Reulen}
\examples{
\dontrun{getStart(d, K = 10, nrep = 100, safety = TRUE, ...)}
}