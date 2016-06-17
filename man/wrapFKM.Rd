\name{wrapFKM}
\alias{wrapFKM}
\title{Wrapper for \code{FKM}}
\description{Wrapper function for a call to fuzzy k-means function \code{FKM}.}
\usage{
wrapFKM(d, m, start, maxit = 1e4, threshold = 0.1)
}
\arguments{
  \item{d}{data-set with standardized columns}
	\item{m}{Fuzziness parameter}
	\item{start}{Starting values as provided by \code{\link{getStart}}.}
	\item{maxit}{Maximum number of iterations (default is 10000). }
	\item{threshold}{Upper limit below which a cluster distance (as defined by matrix \code{H} in \code{FKM}) is set to be 0.}
 }
\details{The function is a specific wrapper function to a function which gives the same results as FKM from the R package \code{fclust}. 

The below example gives an exemplary complete run for an analysis as implemented by this package \code{DoTC}. 

The selection of the fuzziness parameter \code{m} is crucial for the result of the \code{wrapFKM} function. We have good experiences with following strategy: first use a coarse grid of proposal values for \code{m}, look on aggregation of clusters across this proposal vector -- as for example implemented by \code{\link{getStart}} --, and then refine this grid for a certain sub-interval of interest.}
\value{A list with the results from the call to \code{FKM}: 

\code{U} (matrix containing case-wise (rows) cluster (columns) affiliation values), 
\code{H} (pair-wise cluster distance matrix), 
\code{value} (terminal value of the fuzzy-clustering algorithm), 
\code{iter} (number of iterations needed to get to \code{value}), 
\code{k} (number od proposed clusters to the start solution \code{start}),
\code{call} (call to the interior \code{FKm} function),
\code{combined} (which clusters are members of any combinations), 
\code{remaining} (which clusters stay remaining), 
\code{combinations} (which are the combinations that lead to the reduction), 
and \code{n_cluster} (number of reduced clusters),

and further attributes of the solution across potential different proposal fuzziness values: 

\code{m} (all proposed fuzziness values), 
\code{n_cluster} (the number of reduced clusters), 
\code{m_before_step} (the maximum fuzziness parameter before a reduction in \code{n_cluster}), 
and \code{which_list_indexes_m_before_step} (where are the respective results to \code{m_before_step}).}
\author{Holger Sennhenn-Reulen}
\references{Paolo Giordani, Maria Brigida Ferraro (2015). \emph{fclust: Fuzzy Clustering}, on CRAN.}
\examples{
\dontrun{
## Load and standardize (by column) data:
d <- read.csv("data_file.csv")
d <- apply(d, MAR = 2, FUN = scale)
## Set maximal number of clusters:
K <- 10
## Set random seed:
set.seed(1604)
## Get k-means-clustering solutions as starting values:
start <- getStart(d = d, K = K)
## Proposal vector for fuzziness parameter m:
m_proposal <- seq(1.1, 2.5, by = 0.1)
## Calculate results of fuzzy clustering:
fkm_result <- wrapFKM(d = d, m = m_proposal, start = start)
## Plot cluster solution across varying m:
plotNcluster(fkm = fkm_result)
## Plot distribution of typicality coefficients:
plotTC(fkm_result[[1]])
## Plot pairwise cluster segregation comparisons:
plotCS(fkm_result[[1]])}
}