get_num_cell_tiles <- function(btd,
                               f,
                               tile_bw = FALSE,
                               x_axis_name = "cells",
                               feature_name = "Feature",
                               rotate_x_axis_labels = TRUE) {
  
  # check inputs
  check_input_num_cell_tiles(btd = btd,
                             f = f,
                             tile_bw = tile_bw,
                             x_axis_name = x_axis_name,
                             feature_name = feature_name,
                             rotate_x_axis_labels = rotate_x_axis_labels)
  

  fd <- data.frame(f = f, cluster = btd$cluster)
  fd <- fd %>% group_by(cluster) %>% 
    mutate(rank = order(order(f, decreasing=TRUE)))
  fd <- fd %>% group_by(cluster) %>% 
    mutate(norm_rank = (rank-min(rank))/(max(rank)-min(rank)))
  
  fd <- merge(x = fd, y = btd$tree_meta, by.x = "cluster", 
              by.y = "label", all = TRUE)
  fd <- fd[order(fd$tree_order, decreasing = FALSE), ]
  fd$cluster <- factor(x = fd$cluster, levels = unique(fd$cluster))
  
  w <- ggplot_num_cell_tiles(fd = fd,
                             x_axis_name = x_axis_name, 
                             feature_name = feature_name,
                             tile_bw = tile_bw, 
                             rotate_x_axis_labels = rotate_x_axis_labels)
  
  return(list(table = fd, plot = w))
}


check_input_num_cell_tiles <- function(btd,
                                       f,
                                       tile_bw,
                                       x_axis_name,
                                       feature_name,
                                       rotate_x_axis_labels) {
  
  check_btd(btd = btd)
  check_f_num(f = f, btd = btd)
  check_tile_bw(tile_bw = tile_bw)
  check_x_axis_name(x_axis_name = x_axis_name)
  check_feature_name(feature_name = feature_name)
  check_rotate_x_axis_labels(rotate_x_axis_labels = rotate_x_axis_labels)
}

ggplot_num_cell_tiles <- function(fd,
                                  x_axis_name, 
                                  feature_name,
                                  tile_bw, 
                                  rotate_x_axis_labels) {
  
  w <- ggplot(data = fd)+
    geom_tile(aes(x=rank, y=cluster, fill = f, col = f))+
    theme_bw(base_size = 10)+
    theme(legend.position = "top",
          legend.margin=margin(t=0,r=0,b=2,l=0, unit = "pt"),
          legend.box.margin=margin(-10,-10,-10,-10))+
    xlab(label = x_axis_name)+
    ylab(label = "Bubble")+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 0.7,
                                  title.position="top", title.hjust = 0.5))
  
  if(tile_bw==FALSE) {
    w <- w+scale_fill_distiller(name = feature_name,
                                palette = "Spectral",
                                na.value = 'white',
                                breaks = pretty_breaks(n = 3))+
      scale_color_distiller(name = feature_name,
                            palette = "Spectral",
                            na.value = 'white',
                            breaks = pretty_breaks(n = 3))
  } else {
    w <- w+scale_fill_gradient(name = feature_name,
                               low = "#f9f9f9",
                               high = "#848484",
                               na.value = 'white',
                               breaks = pretty_breaks(n = 3))+
      scale_color_gradient(name = feature_name,
                           low = "#f9f9f9",
                           high = "#848484",
                           na.value = 'white',
                           breaks = pretty_breaks(n = 3))
  }
  if(rotate_x_axis_labels==TRUE) {
    w <- w+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  return(w)
}
