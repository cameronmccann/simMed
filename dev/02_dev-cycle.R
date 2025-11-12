################################################################################
# Script:        02_dev-cycle.R
# Purpose:       Quick iteration: document -> load -> test
# Notes:         Keep interactive trials here (not in setup/release scripts).
#                Keep this fast; heavy checks belong in 04_release.R
# TODO:
#     - Go through code under 28 to create informal (& formal) tests
################################################################################


# prep --------------------------------------------------------------------

if (!requireNamespace("devtools", quietly = TRUE)) stop("devtools not installed")
if (!requireNamespace("testthat", quietly = TRUE)) stop("testthat not installed")

# Keep runs quick & deterministic
options(testthat.progress.max_fails = 5)   # stop early if a file is very broken
set.seed(101)                              # default seed for informal checks here

# Load package
devtools::document()   # roxygen -> man/ + NAMESPACE
devtools::load_all()   # load the pkg like it's installed



# 1) Run the full formal test suite (fast, frequent)
# testthat::test()       # or devtools::test()

# 2) (Optional) Focused tests when working on one feature ----------------------
# testthat::test_file("tests/testthat/test-generate_data.R")
# testthat::test_file("tests/testthat/test-generate_clusters.R")

# 3) (Optional) Single expectations during micro-iteration --------------------
# Useful when you’re editing a single function and want quick sanity checks.
# Keep these cheap & remove once covered by formal tests.



# Informal tests ----------------------------------------------------------

# Format of output
res <- generate_data(
  J = 5, njrange = c(5, 8),
  Mfamily = "binomial", Yfamily = "binomial",
  include_truevals = FALSE,                # speed up cycle
  plot_PSdiagnostics = FALSE,              # keep silent & fast
  ensure_cluster_positivity = TRUE,
  seed = 2468
)

stopifnot(
  is.list(res),
  is.data.frame(res$data),
  all(c("A","M","Y","Z","school") %in% names(res$data)),
  length(unique(res$data$school)) == 5
)


# Reproducibility
# using same seed from above
res2 <- generate_data(J = 5, njrange = c(5, 8), seed = 2468,
                      include_truevals = FALSE, plot_PSdiagnostics = FALSE)
stopifnot(identical(res$data, res2$data))

# ═══════════════════
#    edge cases?
# ═══════════════════

# Positivity assumption guard (should return NULL value)
res_small <- generate_data(J = 2, njrange = c(2, 3),
                           include_truevals = FALSE,
                           plot_PSdiagnostics = FALSE,
                           ensure_cluster_positivity = TRUE)
stopifnot(is.null(res_small))

# test gaussian family
res_gaus <- generate_data(J = 3, njrange = c(10, 50),
                          Mfamily = "gaussian", Yfamily = "gaussian",
                          include_truevals = FALSE, plot_PSdiagnostics = FALSE)
stopifnot(is.numeric(res_gaus$data$M), is.numeric(res_gaus$data$Y))


# speed test
# --- optional: quick performance smoke test (keep tiny!) ---------------------
# Aim to keep this under ~1s; move heavy perf tests to 03_quality or benches.
t0 <- proc.time()[["elapsed"]]
invisible(generate_data(J = 10, njrange = c(10, 80),
                        include_truevals = FALSE, plot_PSdiagnostics = FALSE))
t1 <- proc.time()[["elapsed"]]
if ((t1 - t0) > 2) message("⚠️ generate_data() is getting slower (>2s for tiny case).")

# --- optional: diagnostics only when needed ----------------------------------
# Toggle this manually while debugging PS overlap; keep OFF by default.
# dbg <- generate_data(J = 8, njrange = c(10, 12), plot_PSdiagnostics = TRUE)
# print(dbg$overlap$overlap_plot)
# print(dbg$overlap$overlap_plot_logit)




# Formal tests ------------------------------------------------------------


## Create tests ------------------------------------------------------------

# Create test file (if it does not already exist and ready for editing); don't need "test-" prefix or ".R" suffix
usethis::use_test("generate_clusters")
usethis::use_test("generate_data")
## can even use usethis::use_test() while in function script & it will
##    automatically go to that functions corresponding test file; no need
##    for writing the function/file name

# you might work at level of individual tests initially then work towards running the entire test files and eventually the entire test suite

# Micro-iteration:
#     This is the interactive phase where you initiate and refine a function and
#     its tests in tandem. Here you will run devtools::load_all() often, and then
#     execute individual expectations or whole tests interactively in the console.
#
# Mezzo-iteration:
#     As one file’s-worth of functions and their associated tests start to shape
#     up, you will want to execute the entire file of associated tests, perhaps
#     with testthat::test_file():
#
# Macro-iteration:
#     As you near the completion of a new feature or bug fix, you will want to
#     run the entire test suite. Most frequently, you’ll do this with devtools::test()
#
#     devtools::test() is mapped to Ctrl/Cmd + Shift + T
#     devtools::check() is mapped to Ctrl/Cmd + Shift + E
#
# Expectation is the finest level of testing. It makes a binary assertion of whether correct
# There are over 40 expectations in testthat package: https://testthat.r-lib.org/reference/index.html
#
# expect_equal() checks for equality
# expect_identical() for exact equivalence
# expect_error() to check whether an expression throws an error.
# expect_warning() to check whether an expression throws an warning
# expect_message() to check whether an expression provides message
#
# Snapshot tests
#


## Run tests ---------------------------------------------------------------

testthat::test_file("tests/testthat/test-generate_clusters.R")

testthat::test_file("tests/testthat/test-generate_data.R")

# testthat::test()  # or testthat::test_file("tests/testthat/test-*.R")



# ---- playground (manual trials) ---------------------------------------------
# ttt <- generate_data(J = 100, njrange = c(50, 100), seed = 123)
# str(ttt)
# generate_clusters(J = 100, njrange = c(50, 100), seed = 123)$N






# done --------------------------------------------------------------------
message("dev-cycle completed.")
