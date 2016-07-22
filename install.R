#' Read MS Access databases in Ubuntu
#'
#' This will not work under Windows
system("sudo apt-get install mdbtools")
install.packages(
  c("Hmisc", "tidyr", "dplyr", "lubridate", "sp", "rgdal", "mapview", "DT","devtools", "vegan"))
devtools::install_github("ropensci/ckanr")

# Install packfor from source
tmp <- tempfile(tmpdir="data")
download.file("http://download.r-forge.r-project.org/src/contrib/packfor_0.0-8.tar.gz", tmp)
install.packages(tmp, repos = NULL, type = "source")
unlink(tmp)

#------------------------------------------------------------------------------#
# Install RODBC and configure data source for tagging db
# Source: http://itsalocke.com/install-sql-server-odbc-drivers-travis-ci/

## Run in terminal
# wget https://download.microsoft.com/download/2/E/5/2E58F097-805C-4AB8-9FC6-71288AB4409D/msodbcsql-13.0.0.0.tar.gz -P ..
# tar xvzf ../msodbcsql-13.0.0.0.tar.gz -C ..
# sed -i '14d' ../msodbcsql-13.0.0.0/build_dm.sh
# sed -i '/tmp=/ctmp=/tmp/odbcbuilds' ../msodbcsql-13.0.0.0/build_dm.sh
# ../msodbcsql-13.0.0.0/build_dm.sh --accept-warning
# cd /tmp/odbcbuilds/unixODBC-2.3.1
# sudo make install
# cd ~
#   sudo apt-get install libgss3 -y
# ../msodbcsql-13.0.0.0/install.sh verify
# cd ../msodbcsql-13.0.0.0/
#   sudo ./install.sh install --accept-license
# odbcinst -q -d -n "ODBC Driver 13 for SQL Server"

## Add to ~/.odbc.ini
# [turtle_tagging]
# Driver = ODBC Driver 13 for SQL Server
# Server = MSSQL_SERVER_DNSNAME
