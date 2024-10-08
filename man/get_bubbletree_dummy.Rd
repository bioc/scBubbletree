\name{get_bubbletree_dummy}



\alias{get_bubbletree_dummy}


\title{
Build bubbletree from matrix \eqn{A} of low-dimensional projections and
vector \eqn{cs} of externally generated cluster IDs}


\description{
get_bubbletree_dummy takes two main inputs:

1. numeric matrix \eqn{A^{n\times f}}, which represents a low-dimensional
projection (obtained e.g. by PCA) of the original high-dimensional scRNA-seq
data, with \eqn{n} rows as cells and \eqn{f} columns as low-dimension features.

2. vector \eqn{cs} of cluster IDs of each cell

The function \code{get_bubbletree_dummy} performs one main operation. It
organizes the bubbles (defined by \eqn{cs}) in a hierarchical dendrogram
(bubbletree) which represents the hierarchical relationships between the
clusters (bubbles).
}


\usage{
get_bubbletree_dummy(x,
                     cs,
                     B = 200,
                     N_eff = 100,
                     hclust_distance = "euclidean",
                     hclust_method = "average",
                     cores = 1,
                     round_digits = 2,
                     show_simple_count = FALSE,
                     verbose = TRUE)
}


\arguments{
\item{x}{numeric matrix (\eqn{A^{n\times f}} with \eqn{n} cells, and \eqn{f}
low-dimensional projections of the original single cell RNA-seq dataset)}
\item{cs}{vector, cluster IDs}
\item{B}{integer, number of bootstrap iterations to perform in order to
generate bubbletree}
\item{N_eff}{integer, number of cells to draw randomly from each cluster when
computing inter-cluster distances}
\item{hclust_distance}{distance measure to be used: euclidean (default) or
manhattan, see documentation of \code{stats::dist}}
\item{hclust_method}{agglomeration method to be used, default = average.
See documentation of \code{stats::hclust}}
\item{cores}{integer, number of PC cores for parallel execution}
\item{round_digits}{integer, number of decimal places to keep when showing the
relative frequency of cells in each bubble}
\item{show_simple_count}{logical, if \code{show_simple_count}=T, cell counts
in each bubble will be divided by 1,000 to improve readability. This is only
useful for samples that are composed of millions of cells.}
\item{verbose}{logical, progress messages}
}


\details{
This function is similar to \code{get_bubbletree_kmeans} and
\code{get_bubbletree_graph} but skips the clustering step. See
the documentation of the respective functions.
}


\value{
\item{A}{input x matrix}
\item{k}{number of clusters}
\item{km}{NULL}
\item{ph}{boot_ph: bootstrap dendrograms \eqn{H_b}; main_ph: bubbletree \eqn{H}}
\item{ph_data}{two phlogenies: ph_c = phylogenity constructed from bubble 
centroids (computed from \eqn{A^{n\times f}}); ph_p = main_ph = phylogeny 
constructed from intercell distances}
\item{pair_dist}{inter-cluster distances used to generate the dendrograms}
\item{cluster}{cluster assignments of each cell}
\item{input_par}{list of all input parameters}
\item{tree}{ggtree bubbletree object}
\item{tree_simple}{simplified ggtree bubbletree object}
\item{tree_meta}{meta-data associated with the bubbletree}
}


\author{
  Simo Kitanovski \email{simo.kitanovski@uni-due.de}
}


\examples{
# input data
data("d_500", package = "scBubbletree")
A <- d_500$A

cs <- base::sample(x = LETTERS[1:5], size = nrow(A), replace = TRUE)

db <- get_bubbletree_dummy(x = A,
                           cs = cs,
                           B = 200,
                           N_eff = 100,
                           hclust_distance = "euclidean",
                           hclust_method = "average",
                           cores = 1)
}

\seealso{get_k, get_r, get_bubbletree_kmeans, get_bubbletree_graph,
         get_bubbletree_comparison, get_gini, get_gini_k, get_num_tiles,
         get_num_violins, get_cat_tiles, d_500}
