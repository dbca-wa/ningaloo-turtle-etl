# https://cran.r-project.org/web/packages/tidyjson/vignettes/introduction-to-tidyjson.html

require(httr)
library(plyr)
require(dplyr)
library(lubridate)
require(tidyjson)
require(DT)
library(leaflet)

# url = "https://strandings.dpaw.wa.gov.au/api/1/animal-encounters/"
# url = "https://strandings.dpaw.wa.gov.au/api/1/turtle-nest-encounters/?where=17" # THV
# url = "https://strandings.dpaw.wa.gov.au/api/1/turtle-nest-encounters/?where=17" # THV
url = "http://localhost:8220/api/1/turtle-nest-encounters/?where=17&when__year=2017&limit=10000" # THV 2017
hdr = c(Authorization = Sys.getenv("APITOKEN"))
res = httr::GET(paste0(url, "&format=json"), add_headers(.headers = hdr))
j <- res %>% content("text") %>% as.tbl_json()
jj <- attr(j, "JSON")[[1]]$results

get_stuff <- function(li){
  date <- li$when
  lon <- li$where$longitude
  lat <- li$where$latitude
  species <- li$species
  nest_age <- li$nest_age
  nest_type <- li$nest_type
  out <- c(date, lon, lat, species, nest_age, nest_type)
  out
}

tz <- "UTC"
ord <- c("YmdHMSz")

d <- ldply(jj, get_stuff) %>% tbl_df()
colnames(d) <- c("date", "longitude", "latitude", "species", "nest_age", "nest_type")
d <- mutate(d,
            date= with_tz(parse_date_time(date, orders=c("YmdHMSz"), tz="UTC"), tzone="GMT+08"),
            longitude=as.numeric(longitude),
            latitude=as.numeric(latitude)
            ) %>%
  mutate(observation_date=as_date(date))
glimpse(d)
# dates are in UTC now

save(d, file="tracks.Rda")
load("tracks.Rda")

leaflet(d) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(lng=115.2, lat=-21, zoom=9) %>%
  addAwesomeMarkers(~longitude, ~latitude, label=~species,
                    popup=~paste(nest_age, species, nest_type, observation_date))

# Tally all tracks
tally_data <- d %>%
  group_by(observation_date, species, nest_age) %>% tally(sort=F) %>% ungroup()

# Tally only "fresh" tracks
tally_fresh <- d %>% filter(nest_age=="fresh") %>%
  group_by(observation_date, species, nest_type) %>% tally(sort=F) %>% ungroup()

library(ggplot2)
ggplot(tally_fresh, aes(x=observation_date, y=n, colour=species)) +
  geom_point() + geom_line() + facet_wrap("species", ncol=1)
