\name{plotCS}
\alias{plotCS}
\title{Plot Cluster Segregation}
\description{Pairwise comparisons of cluster segregations.}
\usage{
plotCS(fkm, which_clusters = NULL, colors = NULL, main = "")
}
\arguments{
  \item{fkm}{Result for one single fuzziness parameter \code{m} as calculated by \code{\link{wrapFKM}}.}
  \item{which_clusters}{Which clusters should be plotted? (Default is \code{NULL}, and all pair-wise cluster combinations are plotted).}
	\item{colors}{colors to be used (default is \code{NULL}, and colors are automatically provided)}
	\item{main}{main title (default is no title)}
	}
\value{A plot with pairwise comparisons of cluster segregations.}
\author{Holger Sennhenn-Reulen}
\examples{
\dontrun{plotCS(fkm, which_clusters = NULL, colors = NULL, main = "")}
}