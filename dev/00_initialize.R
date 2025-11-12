################################################################################
# Script:        00_initialize.R
# Package:       simMed
# Purpose:       One-time project initialization (scaffold, git, license, etc.)
# Author:        Cameron McCann
# Created:       2025-11-11
# Last Updated:  2025-11-12
#
# Preconditions:
#   - Run from package root (usethis::proj_get() == getwd()).
#   - Do NOT run repeatedly unless you know what you’re doing.
#
# Side effects:
#   - Modifies DESCRIPTION, adds files/folders, writes .Rbuildignore, etc.
#
# Notes / TODO:
#   - [ ] Add pkgdown once README is stable
#   - [ ] Configure CI (GitHub Actions)
#   - Note:
#       This code was not ran. The actual code that initiated is somewhat
#       recorded in "99_old-learning-code.R"
#
# Changelog:
#   - 2025-11-11: Initial version.
################################################################################

# setup -------------------------------------------------------------------

# Check root
stopifnot(getwd() == usethis::proj_get())

# Create a 'dev' folder if it doesn't exist
if (!dir.exists("dev")) dir.create("dev")

# keep dev/ out of builds
usethis::use_build_ignore("dev")

# license/readme/test infra
usethis::use_mit_license()
usethis::use_readme_md()
usethis::use_testthat(3)
usethis::use_roxygen_md()
usethis::use_r_version("4.1")  # actually ran: usethis::use_package("R", type = "Depends", min_version = "4.1.0")

# optional: git and gitignore
# usethis::use_git()
# usethis::use_git_ignore(c(".Rproj.user", ".DS_Store"))

# optional: pkgdown, data-raw
# usethis::use_pkgdown()
# usethis::use_data_raw()

# initial setup for testing (3rd edition)
usethis::use_testthat(3)













library(devtools)
library(usethis)
library(testthat)

# Create a 'dev' folder if it doesn't exist
if (!dir.exists("dev")) dir.create("dev")

# Ignore the dev folder in the build
usethis::use_build_ignore("dev")

# # (Optional) Ignore project files, notes, etc.
# usethis::use_build_ignore("README_dev.md")
# usethis::use_build_ignore(".Rproj.user")



use_r("generate_data")




load_all()

# informal check
ttt <- generate_data(
  J = 100,
  njrange = c(50, 100),
  seed = 123
)

str(ttt)

#
check()

# set license
use_mit_license()

# write NAMESPACE and man files
document()

# Add dependencies in alphabetical order
usethis::use_package(package = "dplyr",
                     type = "Imports")
usethis::use_package(package = "mvtnorm")

#
check()

# write NAMESPACE and man files
document()

#
check()

# Add glue and purrr packages as dependencies
usethis::use_package(package = "dplyr",
                     min_version = TRUE)
usethis::use_package(package = "ggplot2",
                     min_version = TRUE)
usethis::use_package(package = "glue",
                     min_version = TRUE)
usethis::use_package(package = "mvtnorm",
                     min_version = TRUE)
usethis::use_package(package = "purrr",
                     min_version = TRUE)

# Fixed many roxygen issues in R scripts

# write NAMESPACE and man files
document()

#
check()

# Set R version as dependent since we use "|>"
usethis::use_package("R", type = "Depends", min_version = "4.1.0")

# write NAMESPACE and man files
document()

# Set rlang as a package used
usethis::use_package("rlang",
                     min_version = TRUE)

#
check()


# initial setup for testing (3rd edition)
usethis::use_testthat(3)

# Create function file (if it does not already exist and ready for editing); don't need or ".R" suffix
usethis::use_r("generate_clusters")

# Create test file (if it does not already exist and ready for editing); don't need "test-" prefix or ".R" suffix
usethis::use_test("generate_clusters")
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

# Load package
devtools::load_all()

#
testthat::test_file("tests/testthat/test-generate_clusters.R")



generate_clusters(J = 100,
                  njrange = c(50, 100),
                  seed = 123)$N


