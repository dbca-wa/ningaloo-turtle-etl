#' Read MS Access databases in Ubuntu
#'
#' This will not work under Windows
system("sudo apt-get install mdbtools")
install.packages(c("Hmisc", "dplyr", "lubridate", "sp", "rgdal", "mapview",
                   "DT","devtools"))
devtools::install_github("ropensci/ckanr")

# Install packfor from source
tmp <- tempfile(tmpdir="data")
download.file("http://download.r-forge.r-project.org/src/contrib/packfor_0.0-8.tar.gz", tmp)
install.packages(tmp, repos = NULL, type = "source")
unlink(tmp)
