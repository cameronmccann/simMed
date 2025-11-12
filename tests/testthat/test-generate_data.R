# tests/testthat/test-generate_data.R
# Smoke tests: minimal tests that verify the function runs and returns consistent, valid output every time (does not blow up & smoke)

# Helper: quick binary check
is_binary <- function(x) {
  x <- x[!is.na(x)]
  length(x) > 0 && all(x %in% c(0, 1))
}


test_that("generate_data returns expected top-level structure", {
  res <- generate_data(
    J = 5, njrange = c(5, 6),
    include_truevals = FALSE,
    include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE,
    ensure_cluster_positivity = FALSE,
    seed = 123
  )

  expect_type(res, "list")
  expect_true(all(c("data","truevals","effects","overlap","parameters") %in% names(res)))

  # core data columns
  expect_true(all(c("A","M","Y","Z","school","W_nj") %in% names(res$data)))

  # X columns should be split out (defaults to num_x = 3)
  expect_true(all(paste0("X", 1:3) %in% names(res$data)))

  # Overlap summaries exist and are character strings
  expect_true(is.character(res$overlap$ps_summary))
  expect_true(is.character(res$overlap$iptw_summary))

  # parameters are recorded
  expect_true(is.numeric(res$parameters$nj_sizes))
  expect_equal(length(res$parameters$nj_sizes), res$parameters$J)
})


test_that("W_nj is scaled to [0, 1]", {
  res <- generate_data(
    J = 4, njrange = c(10, 20),
    include_truevals = FALSE,
    include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE,
    ensure_cluster_positivity = FALSE,
    seed = 999
  )
  expect_gte(min(res$data$W_nj), 0)
  expect_lte(max(res$data$W_nj), 1)
})


test_that("families: binomial/binomial, binomial/gaussian, gaussian/binomial work", {
  # binomial mediator, binomial outcome
  bb <- generate_data(
    J = 4, njrange = c(8, 10),
    Mfamily = "binomial", Yfamily = "binomial",
    include_truevals = FALSE, include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE,
    seed = 1
  )
  expect_true(is_binary(bb$data$M))
  expect_true(is_binary(bb$data$Y))

  # binomial mediator, gaussian outcome
  bg <- generate_data(
    J = 4, njrange = c(8, 10),
    Mfamily = "binomial", Yfamily = "gaussian",
    include_truevals = FALSE, include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE,
    seed = 2
  )
  expect_true(is_binary(bg$data$M))
  expect_false(is_binary(bg$data$Y))
  expect_true(is.numeric(bg$data$Y))

  # gaussian mediator, binomial outcome
  gb <- generate_data(
    J = 4, njrange = c(8, 10),
    Mfamily = "gaussian", Yfamily = "binomial",
    include_truevals = FALSE, include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE,
    seed = 3
  )
  expect_true(is.numeric(gb$data$M))
  expect_false(is_binary(gb$data$M))
  expect_true(is_binary(gb$data$Y))
})


test_that("reproducibility: same seed => identical data", {
  r1 <- generate_data(
    J = 3, njrange = c(5, 6),
    include_truevals = FALSE, include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE,
    seed = 42
  )
  r2 <- generate_data(
    J = 3, njrange = c(5, 6),
    include_truevals = FALSE, include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE,
    seed = 42
  )
  expect_equal(r1$data, r2$data)
})


test_that("include_truevals controls truevals/effects population", {
  # Without truevals
  r_no <- generate_data(
    J = 4, njrange = c(6, 8),
    include_truevals = FALSE, include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE,
    seed = 1001
  )
  expect_null(r_no$truevals)
  # effects list exists but can contain NULLs
  expect_true("individual" %in% names(r_no$effects))
  expect_true("cluster" %in% names(r_no$effects))

  # With truevals
  r_yes <- generate_data(
    J = 4, njrange = c(6, 8),
    include_truevals = TRUE, include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE,
    seed = 1002
  )
  expect_type(r_yes$truevals, "list")
  expect_true(all(c("truevals_individual","truevals_cluster") %in% names(r_yes$truevals)))

  # Mediation effect scalars should be numeric
  expect_true(is.numeric(r_yes$effects$individual$pnde))
  expect_true(is.numeric(r_yes$effects$individual$tnie))
  expect_true(is.numeric(r_yes$effects$individual$tnde))
  expect_true(is.numeric(r_yes$effects$individual$pnie))

  expect_true(is.numeric(r_yes$effects$cluster$pnde))
  expect_true(is.numeric(r_yes$effects$cluster$tnie))
  expect_true(is.numeric(r_yes$effects$cluster$tnde))
  expect_true(is.numeric(r_yes$effects$cluster$pnie))
})


test_that("invalid family inputs error cleanly", {
  expect_error(
    generate_data(
      J = 2, njrange = c(5, 5),
      Mfamily = "poisson",  # invalid
      include_truevals = FALSE, include_overlapMsg = FALSE,
      plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE
    ),
    regexp = "Mfamily must be either 'binomial' or 'gaussian'"
  )
  # Yfamily is validated deeper; we still expect an error if it's invalid
  expect_error(
    generate_data(
      J = 2, njrange = c(5, 5),
      Yfamily = "poisson",  # invalid
      include_truevals = FALSE, include_overlapMsg = FALSE,
      plot_PSdiagnostics = FALSE, ensure_cluster_positivity = FALSE
    ),
    regexp = "Yfamily must be either 'binomial' or 'gaussian'"
  )
})


test_that("positivity guard returns NULL when cluster variances of A are NA", {
  # Each cluster has size 1 -> within-cluster var(A) is NA -> should trigger the guard
  res <- generate_data(
    J = 3, njrange = c(1, 1),
    include_truevals = FALSE, include_overlapMsg = FALSE,
    plot_PSdiagnostics = FALSE,
    ensure_cluster_positivity = TRUE,
    seed = 7
  )
  expect_null(res)
})

