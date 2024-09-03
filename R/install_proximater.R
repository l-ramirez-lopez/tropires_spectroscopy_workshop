# --- 1. Install and load required packages ----
# This is the list of packages that we need to install before installing proximater
packages_required <- c(
  "prospectr", "remotes", "formatR", "foreach", "rmarkdown", "uuid", 
  "plotly", "flexdashboard", "mathjaxr", "lifecycle", "data.table", 
  "Rcpp", "digest", "withr", "zip", "knitr", "parallel", "doParallel", 
  "testthat", "bookdown", "roxygen2", "tinytex"
)

# Check which packages are not installed
to_install <- packages_required[!packages_required %in% rownames(installed.packages())]

# Install missing packages
if(length(to_install) > 0) {
  install.packages(to_install)
}

# Load all required packages
lapply(packages_required, FUN = require, character.only = TRUE)

# --- 1.1 Install additional functionality (TinyTeX) ----
# TinyTeX is useful for creating and compiling documentation
if (!tinytex::is_tinytex()) {
  tinytex::install_tinytex()
}

# Update and configure TinyTeX
tinytex::tlmgr_install("psnfss")

# --- 2. Install proximater ----
# Install the proximater package from GitHub
# Ensure the 'mtkn' variable (authentication token) is defined in your environment

# request the token to ramirez-lopez.l@buchi.com

mtkn <- "[this is just a placeholder,  please request this token to the instructors]"

remotes::install_github(
  "l-ramirez-lopez/proximater", 
  auth_token = mtkn, 
  upgrade = "never", 
  force = TRUE, 
  build_vignettes = FALSE
)

# NOTE: If prompted with "Enter one or more numbers, or an empty line to skip updates:", 
# you can choose "None" to skip updates.

# You can also try this (installing all the example manuals/vignettes)

remotes::install_github(
  "l-ramirez-lopez/proximater", 
  auth_token = mtkn, 
  upgrade = "never", 
  force = TRUE, 
  build_vignettes = TRUE
)

# Show useful documentation for proximater
browseVignettes(package = "proximater")
