#' Read MS Access databases in Ubuntu
#'
#' This will not work under Windows
system("sudo apt-get install mdbtools")
install.packages(c("Hmisc", "dplyr", "lubridate", "sp", "rgdal", "mapview",
                   "DT","devtools"))
devtools::install_github("ropensci/ckanr")
