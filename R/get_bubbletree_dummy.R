get_bubbletree_dummy <- function(x,
                                 cs,
                                 B = 200,
                                 N_eff = 100,
                                 hclust_distance = "euclidean",
                                 hclust_method = "average",
                                 cores = 1,
                                 round_digits = 2,
                                 show_simple_count = FALSE,
                                 verbose = TRUE) {
  
  # check inputs
  check_input_dummy(x = x,
                    cs = cs,
                    B = B,
                    N_eff = N_eff,
                    cores = cores,
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    verbose = verbose,
                    hclust_method = hclust_method,
                    hclust_distance = hclust_distance)
  
  # pairwise distances
  if(verbose) {
    message("Bubbletree construction ...")
  }
  
  pd <- get_dist(B = B, m = x, c = cs, N_eff = N_eff, cores = cores,
                 hclust_distance = hclust_distance)
  
  tc <- get_tree(pd = pd, B = B, hclust_method = hclust_method, 
                 cs = cs, round_digits = round_digits, 
                 show_simple_count = show_simple_count, type = "c")
  
  tp <- get_tree(pd = pd, B = B, hclust_method = hclust_method, 
                 cs = cs, round_digits = round_digits, 
                 show_simple_count = show_simple_count, type = "p")
  
  # which ph to use as main?
  if(B==0) {
    ph <- tc$ph
    t <- tc$t
  } 
  else {
    ph <- tp$ph
    t <- tp$t
  }
  
  # collect input parameters: can be used for automated update
  input_par <- list(n_start = NA,
                    iter_max = NA,
                    N_eff = N_eff,
                    B = B,
                    round_digits = round_digits,
                    show_simple_count = show_simple_count,
                    kmeans_algorithm = NA,
                    hclust_method = hclust_method,
                    hclust_distance = hclust_distance)
  
  return(structure(class = "bubbletree_dummy",
                   list(A = x,
                        k = length(unique(cs)),
                        ph = ph,
                        ph_data = list(ph_c = tc, ph_p = tp),
                        pair_dist = pd,
                        cluster = cs,
                        input_par = input_par,
                        tree = t$tree,
                        tree_simple = t$tree_simple,
                        tree_meta = t$tree_meta)))
}


# check input param
check_input_dummy <- function(x,
                              cs,
                              B,
                              N_eff,
                              cores,
                              hclust_method,
                              hclust_distance,
                              round_digits,
                              show_simple_count,
                              verbose) {
  
  check_x(x = x)
  check_cs(cs = cs, x = x)
  check_B(B = B)
  check_N_eff(N_eff = N_eff)
  check_cores(cores = cores)
  check_round_digits(round_digits = round_digits)
  check_show_simple_count(show_simple_count = show_simple_count)
  check_verbose(verbose = verbose)
  check_hclust_method(hclust_method = hclust_method)
  check_hclust_distance(hclust_distance = hclust_distance)
}
