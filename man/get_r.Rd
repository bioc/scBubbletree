\name{get_r}



\alias{get_r}




\title{
Finding optimal clustering resulution \eqn{r} and number of communities \eqn{k'}
}



\description{
To perform Louvain clustering we must specify a clustering resulution \eqn{r}.
Data-driven metrics, such as the Gap statistic or the within-cluster sum of
squares (WCSS) can be used to infer appropriate \eqn{r} from the data. get_r
computes the Gap statistic and WCSS for a vector of clustering resolutions
\eqn{rs}.
}



\usage{

get_r(x,
     rs,
     B_gap = 20,
     n_start = 20,
     iter_max = 100,
     algorithm = "original",
     knn_k = 20,
     cores = 1,
     verbose = TRUE)
}


\arguments{
\item{x}{numeric matrix \eqn{A^{n x f}} with \eqn{n} cells, and \eqn{f}
low-dimensional projections}
\item{rs}{number vector, \eqn{r} values to consider}
\item{B_gap}{integer, number of Monte Carlo ("bootstrap") samples taken when
computing the Gap statistic (see documentation of function \code{clusGap},
R-package \code{cluster})}
\item{n_start, iter_max}{parameters for Louvain clustering, see documentation
of function \code{FindClusters}, R-package \code{Seurat}}
\item{algorithm}{character, four clustering algorithms: 'original',
'LMR', 'SLM' and 'Leiden', see documentation of function \code{FindClusters},
R-package \code{Seurat}}
\item{knn_k}{integer, defines k for the k-nearest neighbor algorithm, see
documentation of function \code{FindClusters}, R-package \code{Seurat}}
\item{cores}{integer, number of PC cores for parallel execution}
\item{verbose}{logical, progress messages}
}


\details{
To compute the Gap statistic \code{get_r} adapts the algorithm of function
\code{clustGap} from R-package \code{cluster} (version 2.1.3). For Louvain
clustering \code{get_r} uses the function \code{FindClusters} implemented in
the R-package \code{Seurat}. For additional information see the respective
documentations.
}

\value{
\item{boot_obj}{The results: k-means clustering solutions, the Gap statistic
and WCSS}
\item{gap_stats_summary, wcss_stats_summary}{main results; Gap statistic
and WCSS estimates. Means, standard errors and 95\% confidence intervals are
provided for each \eqn{r} and \eqn{k'}}
\item{gap_stats, wcss_stats}{intermediate results; Gap statistic and WCSS
estimates for each \eqn{r} and \eqn{k'} and bootstrap iteration \eqn{b}}
}


\author{
  Simo Kitanovski \email{simo.kitanovski@uni-due.de}
}


\examples{
# input data
data("d_500", package = "scBubbletree")
A <- d_500$A


b <- get_r(x = A,
           rs = c(0.1, 0.5, 1),
           B_gap = 10,
           n_start = 20,
           iter_max = 100,
           algorithm = "original",
           cores = 1,
           verbose = TRUE)

b$gap_stats_summary
}

\seealso{get_k, get_bubbletree_dummy, get_bubbletree_graph,
get_bubbletree_kmeans, get_gini, get_gini_k, d_500, get_num_tiles,
get_num_violins, get_cat_tiles, d_ccl}
