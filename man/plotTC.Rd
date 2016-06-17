\name{plotTC}
\alias{plotTC}
\title{Plot Typicality Coefficients}
\description{Plot Typicality Coefficients as stapled Histograms}
\usage{
plotTC(fkm, main = NULL)
}
\arguments{
  \item{fkm}{Result from \code{\link{wrapFKM}}.}
  \item{main}{Main title (default is \code{NULL}, resulting in a main title with the fuzziness parameter \code{m}).}
	}
\details{Relies on ggplot2 and plyr.}
\value{A plot with frequencies of typicality coefficients.}
\author{Holger Sennhenn-Reulen}
\examples{
\dontrun{plotTC(fkm, main = NULL)}
}