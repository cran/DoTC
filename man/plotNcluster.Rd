\name{plotNcluster}
\alias{plotNcluster}
\title{Plot the Cluster Solution Across Varying Fuzziness Parameter}
\description{Plot the cluster solution, ie. the number of clusters, as a step function across varying fuzziness parameter \code{m}.}
\usage{
plotNcluster(fkm, ...)
}
\arguments{
  \item{fkm}{Result for one single fuzziness parameter \code{m} as calculated by \code{\link{wrapFKM}}.}
  \item{...}{Additional attributes to \code{plot}.}
	}
\value{A step plot of the numbers of clusters (y axis) across different values for the fuzziness parameter m (x axis). The largest values of m conditional on a fixed numbers of clusters are highlighted using grey, dashed lines and bullet points. }
\author{Holger Sennhenn-Reulen}
\examples{
\dontrun{plotNcluster(fkm, ...)}
}