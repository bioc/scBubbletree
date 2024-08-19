\name{get_num_cell_tiles}



\alias{get_num_cell_tiles}




\title{
Visualization of numeric features of individual cells using tile plots
}



\description{
\code{get_num_cell_tiles} creates one heatmap from the cells in each bubble.
The heatmap visualizes a gradient of the sorted (from high to low) values of 
a numeric feature (e.g. expression of a certain gene) among the cells of that
bubble.}



\usage{
get_num_cell_tiles(btd,
                   f,
                   tile_bw = FALSE,
                   x_axis_name = "Feature",
                   rotate_x_axis_labels = TRUE)
}

\arguments{
\item{btd}{bubbletree object}
\item{f}{numeric vector, numeric cell feature}
\item{x_axis_name}{character, x-axis title}
\item{rotate_x_axis_labels}{logical, should the x-axis labels be shown
horizontally (\code{rotate_x_axis_labels}  = FALSE) or vertically
(\code{rotate_x_axis_labels} = TRUE)}
\item{tile_bw}{logical, tile grayscale (tile_bw = TRUE) vs. color
(\code{tile_bw} = FALSE, default)}
}


\details{
\code{get_num_cell_tiles} uses two main inputs:

1. bubbletree object

2. numeric vector of a numeric cell feature.

The order of the cells used to generate the bubbletree (input 1.)
should correspond to the order of cell features in input vector 
\code{f} (input 2.)

This function does the following procedure for each bubble:
1. sort and rank the cells in each bubble: rank = 1 for the cell with the 
   highest \code{f} value, rank = $n$ for the bubble with the lowest \code{f} 
   value
2. draw a heatmap with x=rank, y=bubble, tile-color=\code{f}  
}

\value{
\item{plot}{ggplot2, tile plot}
\item{table}{data.frame, raw data used to generate the plot}
}


\author{
  Simo Kitanovski \email{simo.kitanovski@uni-due.de}
}


\examples{
# input data
data("d_500", package = "scBubbletree")
A <- d_500$A
fs <- as.vector(d_500$fs[,1])

b <- get_bubbletree_kmeans(x = A, k = 8)

g <- get_num_cell_tiles(btd = b,
                        f = f,
                        tile_bw = TRUE,
                        x_axis_name = "Gene expression",
                        rotate_x_axis_labels = TRUE)

b$tree|g$plot
}

\seealso{get_k, get_r get_bubbletree_dummy, get_bubbletree_kmeans,
get_bubbletree_graph, get_gini, get_gini_k, get_cat_tile, get_num_tiles, 
get_num_violins, d_500, d_ccl}
