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

# initiate readme
usethis::use_readme_rmd()

# These functions setup parts of the package and are typically called once per package:
# create_package()
# use_git()
# use_mit_license()
# use_testthat()
# use_github()
# use_readme_rmd()










