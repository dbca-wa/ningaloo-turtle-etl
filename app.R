#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(shiny, tidyverse, httr, stringr, lubridate, leaflet, DT, geojsonio, sp)
library(shiny)
library(dplyr)
library(httr)
library(stringr)
library(lubridate)
library(leaflet)
library(DT)
library(geojsonio)
library(sp)
library(ggplot2)
if (file.exists("setup.R")) source("setup.R")

#' Prepare a DT datatable with sensible defaults
make_dt <- function(x, filter="top", pageLength=10){
  DT::renderDataTable(
    DT::datatable(
      x,
      filter=filter,
      options=list(
        pageLength = pageLength,
        autoWidth = TRUE,
        columnDefs = list(list(width='500px', targets=c("plotComment")))
      )))
}

# Define UI for application that draws a histogram
ui <-   navbarPage(
  "Turtle Tracker",
  id="nav",
  tabPanel("Map",
           div(class="outer",
               tags$head(includeCSS("style.css")),
               leafletOutput("map", width="100%", height="100%"),
               absolutePanel(
                 id = "controls", class = "panel panel-default",
                 fixed = TRUE, draggable = TRUE,
                 top = "auto", left = 20, right = "auto", bottom = 20,
                 width = "400", height = "auto",
                 h3("Filter data"),
                 uiOutput("siteSelector"),
                 uiOutput("trackAgeSelector"),
                 uiOutput("trackTypeSelector"),
                 uiOutput("tally_plots")
               ) # absolutePanel
           ) # div.outer
  ), # tabPanel Map
  tabPanel("Data", DT::dataTableOutput("tally_data")),
  tabPanel("Plots", uiOutput("plot_fullscreen")),
  tabPanel("About", includeMarkdown("README_TT.md"))
) # navbarPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      # addProviderTiles("Hydda.Full") %>%
      addProviderTiles("Esri.WorldImagery",
                       options=providerTileOptions(opacity=0.8, reuseTiles=T)) %>%
      setView(lng = 113.8705194, lat = -22.3820194, zoom = 7) %>%
      addMiniMap(toggleDisplay=T, zoomLevelOffset=-5) %>%
      addScaleBar() %>%
      clearShapes()
  })

  data <- reactive({
    withProgress(message = 'Getting the goodies, hang tight...', value = 0, {
      api_call <- paste0(
        "https://www.googleapis.com/fusiontables/v2/query?",
        "sql=select+*+from+", Sys.getenv("TABLE_ID"),
        "&hdrs=true&typed=true&fields=columns%2Ckind%2Crows&",
        "key=", Sys.getenv("GOOGLE_API_KEY"))
      res <- POST(api_call)
      r <-  res %>% content("parsed")

      # Column names
      cols <- unlist(r$columns)
      cols_1_last <- which(cols=="observed_at") - 1 # up until "observed_at"
      cols_2_first <- which(cols=="observed_at") + 2 # from after "details:observed_at:Accuracy"
      cols_1 <- cols[1:cols_1_last]
      cols_location <- c("featureclass", "longitude", "latitude", "altitude", "accuracy")
      cols_2 <- cols[cols_2_first:length(cols)]
      final_cols <- c(cols_1, cols_location, cols_2) %>%
        str_replace_all(pattern="-", replacement="_") %>%
        str_replace_all(pattern="\\*", replacement="")

      # Build tibble
      d_tbl <- tbl_df(matrix(unlist(r$rows), nrow=length(r$rows), byrow=T))
      colnames(d_tbl) <- final_cols

      # Tidy data types
      ord <- c("mdYHMOS")
      utc <- "UTC"
      gmt08 <- "Australia/Perth"

      d <- d_tbl %>% mutate(
        id = meta_instance_id,
        meta_submission_date = parse_date_time(meta_submission_date, orders=ord, tz=utc),
        meta_date_marked_as_complete = parse_date_time(meta_date_marked_as_complete, orders=ord, tz=utc),
        observation_start_time = parse_date_time(observation_start_time, orders=ord, tz=utc),
        observation_end_time = parse_date_time(observation_end_time, orders=ord, tz=utc),
        observation_date=as_date(with_tz(meta_submission_date, gmt08)),
        longitude = as.numeric(longitude),
        latitude = as.numeric(latitude),
        altitude = as.numeric(altitude),
        accuracy = as.numeric(accuracy),
        location="WA"
      )

      # Metadata on data currency
      data_retrieved_on <- with_tz(parse_date_time(res$headers$date, orders=c("adbYHMS"), tz=utc), gmt08)
      no_observations <- nrow(d)
      latest_observation <- with_tz(max(d$observation_start_time), gmt08)

      # Location Thevenard
      thv <- geojson_sp(geojsonio::as.json('{"type":"Polygon","coordinates":[[[114.96591567993163,-21.459618983795107],[114.97810363769531,-21.448435280495215],[115.00711441040039,-21.445719108809797],[115.02616882324219,-21.451630711830703],[115.03131866455078,-21.45913969982141],[115.01981735229491,-21.46952383302392],[114.9715805053711,-21.46712756027387],[114.96591567993163,-21.459618983795107]]]}'))

      # Location Montebello Barrow Islands
      mbi <- geojson_sp(geojsonio::as.json('{"type": "Polygon", "coordinates": [[[115.4693989900401, -20.29396276971719], [115.5097316791225, -20.27460307895765], [115.6517027446924, -20.33106884367298], [115.7146217396609, -20.51821252101519], [115.6226632085531, -20.55693190253427], [115.6258898236797, -20.7731151160158], [115.5791039043441, -20.78602157652216], [115.5339312925719, -20.69406304541435], [115.446812684154, -20.67147673952822], [115.3290412320334, -20.82635426560454], [115.3709872286791, -20.85700710930714], [115.47423891273, -20.88443333788316], [115.5403845228251, -20.86023372443373], [115.5387712152618, -21.19418839003579], [115.3968001496918, -21.19741500516238], [115.3242013093435, -21.0941633211115], [115.3274279244701, -21.00543140513028], [115.233856085799, -20.87314018494009], [115.2661222370649, -20.7731151160158], [115.261282314375, -20.73923565718661], [115.3387210774132, -20.68438320003458], [115.4048666875083, -20.54725205715451], [115.3822803816222, -20.50046613781895], [115.4129332253247, -20.44561368066692], [115.4371328387742, -20.31977569072991], [115.4693989900401, -20.29396276971719]]]}'))

      # Location Greater Perth Area
      per = geojson_sp(geojsonio::as.json('{"type":"Polygon","coordinates":[[[115.6365966796875,-31.653381399663985],[115.76293945312499,-31.63467554954133],[116.04858398437499,-31.924192605327708],[115.95520019531249,-32.26855544621476],[115.73547363281249,-32.42634016154639],[115.4168701171875,-32.01273389791075],[115.521240234375,-31.770207631866704],[115.6365966796875,-31.653381399663985]]]}'))

      wgs84 = CRS('+proj=longlat +datum=WGS84 +no_defs')
      d_sp <- SpatialPoints(coords=select(d, longitude, latitude), proj4string=wgs84)
      # d_spdf <- SpatialPointsDataFrame(d_sp, data=d, proj4string=wgs84)

      # Reverse geocode observation locations
      d[which(!is.na(sp::over(x=d_sp, y=per))),]$location = "Perth"
      d[which(!is.na(sp::over(x=d_sp, y=thv))),]$location = "Thevenard"
      # d[which(!is.na(sp::over(x=d_sp, y=mbi))),]$location = "Montebello"  # enable once data comes in
    }) # end progress

    d
  })

  # Offer filter values
  output$siteSelector <- renderUI({
    selectInput("sitepicker",
                "Location",
                c("all", "Thevenard", "Montebello", "Perth"))
  })

  output$trackAgeSelector <- renderUI({
    selectInput("trackagepicker",
                "Recentness",
                c("all", "old", "fresh")
    )
  })

  output$trackTypeSelector <- renderUI({
    selectInput("tracktypepicker",
                "Type",
                c("all", "falsecrawl", "successfulcrawl", "trackunsure",
                  "tracknotassessed", "nest", "hatchednest")
    )
  })

  # Observe sitepicker, pan&zoom map
  observe({
    s <- input$sitepicker
    if (is.null(s)) return(NULL)
    if (s=="All") leafletProxy("map") %>%
      leaflet::setView(112.7628562, -24.9451404, 5)
    if (s=="Thevenard") leafletProxy("map") %>%
      leaflet::setView(114.9892322, -21.4587209, 14)
    if (s=="Montebello") leafletProxy("map") %>%
      leaflet::setView(115.0484288, -20.7507355, 10)
    if (s=="Perth") leafletProxy("map") %>%
      leaflet::setView(115.3151572, -32.1036489, 10)
  })

  # Observe data sitepicker, return filtered data
  filteredData <- reactive({
    d <- data()
    if (is.null(d)) return(NULL)
    if (!is.null(input$sitepicker) && input$sitepicker!="all") {
      d <- filter(d, location==input$sitepicker)}
    if (!is.null(input$trackagepicker) && input$trackagepicker!="all") {
      d <- filter(d, nest_age==input$trackagepicker)}
    if (!is.null(input$tracktypepicker) && input$tracktypepicker!="all") {
      d <- filter(d, nest_type==input$tracktypepicker)}
    d
  })

  # Observe filtered data, draw filtered data on map
  observe({
    d <- filteredData()
    if (is.null(d)) return(NULL)
    leafletProxy("map", data=d) %>%
      clearShapes() %>%
      addAwesomeMarkers(d$longitude,
                        d$latitude,
                        # clusterOptions=T,
                        group="Tracks & Nests",
                        label=d$species,
                        popup=paste("<h4>", d$nest_age, d$species, d$nest_type,
                                    "</h4>", "<p>", d$observation_date, "</p>"))
  })

  # Observe filtered data, return datatable
  output$tally_data <- renderDataTable({
    d <- filteredData()
    if (is.null(d)) return(NULL)
    tally_data <- d %>%
      group_by(location, observation_date, species, nest_age, nest_type) %>%
      tally(sort=F) %>%
      ungroup()
    DT::datatable(tally_data, filter="top")
  })

  output$tally_plot <- renderPlot({
    d <- filteredData()
    if (is.null(d)) return(NULL)
    tally_fresh <- d %>%
      group_by(observation_date, species, nest_type) %>%
      tally(sort=F) %>%
      ungroup()

    plt <- ggplot(tally_fresh, aes(x=observation_date, y=n, colour=nest_type)) +
      geom_point() +
      geom_line() +
      facet_wrap(~species, ncol=1) +
      ggtitle(paste("Timeseries", input$sitepicker))
    plt
  })

  output$tally_plots <- renderUI({
    d <- filteredData()
    if (is.null(d)) return(NULL)
    plotOutput("tally_plot")
  })

  output$fullscreen_plots <- renderPlot({
    d <- filteredData()
    if (is.null(d)) return(NULL)
    tally_fresh <- d %>%
      filter(d$nest_age=="fresh") %>%
      group_by(location, observation_date, species, nest_type) %>%
      tally(sort=F) %>%
      ungroup()

    plt <- ggplot(tally_fresh, aes(x=observation_date, y=n, colour=nest_type)) +
      geom_point() +
      geom_line() +
      facet_wrap(c("species", "location"), ncol=3) +
      ggtitle(paste("Timeseries", input$sitepicker))
    plt
  })

  output$plot_fullscreen <- renderUI({
    d <- filteredData()
    if (is.null(d)) return(NULL)
    plotOutput("fullscreen_plots")
  })

}

# Run the application
shinyApp(ui = ui, server = server)

