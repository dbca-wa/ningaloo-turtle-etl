require(Hmisc)
library(tidyr)
require(dplyr)
require(lubridate)
require(ckanr)
require(DT)
require(rgdal)
require(mapview)
require(vegan)
require(RODBC)
source("setup.R")

load("data/working_data.RData")

install.packages("htmlTable")
require(htmlTable)
output <- matrix(1:4,
                 ncol=2,
                 dimnames = list(list("Row 1", "Row 2"),
                                 list("Column 1", "Column 2")))
z <- htmlTable(output)
z


d <- summary_nests_seasons %>% group_by(subsection) %>% htmlTable() %>% ungroup()

d <- sites %>% mutate(popup=make_popup(subsection))


make_popup <- function(subsection){
  htmlTable(filter(summary_nests_seasons, subsection==subsection))
}


