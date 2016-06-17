\name{extract}
\alias{extract}
\title{Auxiliary Function for Extracting Elements from the Result of \code{\link{wrapFKM}}}
\description{Function for extracting elements from the result of \code{\link{wrapFKM}}, in especially if \code{m} is a vector.}
\usage{
extract(x, what)
}
\arguments{
  \item{x}{result from \code{\link{wrapFKM}}}
	\item{what}{what should be extracted (possible values are \code{U}, \code{combined}, \code{remaining}, \code{combinations}, and \code{n_cluster}).}
 }
\value{The argument that should be extracted from \code{fkm}.}
\author{Holger Sennhenn-Reulen}
\examples{
\dontrun{extract(x, what)}
}