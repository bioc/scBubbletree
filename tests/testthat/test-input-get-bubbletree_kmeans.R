context("Tests input rules of get_bubbletree_kmeans")
cat("Tests input rules of get_bubbletree_kmeans \n")


test_that("missing argument", {


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)

  expect_error(get_bubbletree_kmeans(#x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "x input not found")

  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              #k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "k input not found")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              #B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              #N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              #n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              #iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              #kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              #cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              #round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2),
                              #show_simple_count = F),
               NA)


})



test_that("null/na argument", {



  expect_error(get_bubbletree_kmeans(x = NULL,
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "x must be numeric matrix")
  expect_error(get_bubbletree_kmeans(x = NA,
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "x must be numeric matrix")

  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = NULL,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "k must be a positive integer \\(k>=2\\) to build a bubbletree")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = NA,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "k must be a positive integer \\(k>=2\\) to build a bubbletree")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = NULL,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "B must be a positive integer >= 0")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = NA,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "B must be a positive integer >= 0")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = NULL,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "N_eff must be a positive integer")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = NA,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "N_eff must be a positive integer")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = NULL,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "n_start must be a positive integer")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = NA,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "n_start must be a positive integer")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = NULL,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "iter_max must be a positive integer")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = NA,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "iter_max must be a positive integer")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = NULL,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = NA,
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               "see \\?kmeans: kmeans_algorithm must be one of: Hartigan-Wong,
      Lloyd, Forgy, MacQueen")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = NULL,
                              round_digits = 2,
                              show_simple_count = F),
               "cores must be a positive integer")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = NA,
                              round_digits = 2,
                              show_simple_count = F),
               "cores must be a positive integer")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = F),
               NA)


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = NULL,
                              show_simple_count = F),
               "round_digits must be a positive integer")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = NA,
                              show_simple_count = F),
               "round_digits must be a positive integer")


  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = NULL),
               "show_simple_count is a logical parameter \\(TRUE or FALSE\\)")
  expect_error(get_bubbletree_kmeans(x = matrix(data = rnorm(n = 100*10), ncol = 10),
                              k = 3,
                              B = 20,
                              N_eff = 50,
                              n_start = 100,
                              iter_max = 100,
                              kmeans_algorithm = "MacQueen",
                              cores = 1,
                              round_digits = 2,
                              show_simple_count = NA),
               "show_simple_count is a logical parameter \\(TRUE or FALSE\\)")


})

