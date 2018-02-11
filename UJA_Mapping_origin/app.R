
library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(broom)
library(rgeos)
library(sp)
library(ggplot2)
library(maptools)
library(shinythemes)
library(DT)

#Data to load in
locations <- read.csv("data/locations.csv")
re_comps <- read.csv("data/re_comps.csv")
source("data/map_supplements.R")

#School District ShapeFiles
tract <- readOGR(dsn = "tempdir",
                 layer = "torn")
fort <- fortify(tract, region = "tract")

nb <- readOGR(dsn = "nbhds", 
              layer = "nb")
#palettes for map

pal <- colorNumeric("YlOrRd", tract$chld__6)
pal2 <- colorNumeric("YlOrRd", tract$m__2015)

#sourced files

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Brooklyn Jewish Organizations", theme = shinytheme("sandstone"), 
                         #Map
                         tabPanel("Map",
                                  tags$div(class = 'outer', leafletOutput("brooklynmap", height = "100%", width = '100%')),
                                  tags$script(
                                    '$(".sidebar-toggle").on("click", function() { $(this).trigger("shown"); });'
                                  ),
                                  tags$head(tags$style(
                                    HTML("
                                         section.content{
                                         padding:0px;
                                         }
                                         .outer {
                                         height: calc(100vh - 50px);
                                         padding: 0px;
                                         margin: 0;
                                         }
                                         "), 
                                    type='text/css', ".irs-grid-text { font-size: 16pt; }", 
                                    type='text/css', ".checkbox-control.single { font-size: 32px; }"
                                    )),
                                  
                                  tags$head(
                                    # Include our custom CSS
                                    includeCSS("data/styles.css"),
                                    includeScript("data/gomap.js")
                                  ),
                                  tags$style("
                                    .checkbox { /* checkbox is a div class*/
                                      line-height: 30px;
                                      margin-bottom: 10px; /*set the margin, so boxes don't overlap*/
                                    }
                                    input[type='checkbox']{ /* style for checkboxes */
                                      width: 30px; /*Desired width*/
                                      height: 30px; /*Desired height*/
                                      line-height: 30px; 
                                    }
                                    span { 
                                        margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
                                        line-height: 30px; 
                                    }
                                "),
                                  tags$head(tags$style(" @import url('https://fonts.googleapis.com/css?family=Nunito');")),
                                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                draggable = FALSE, top = 70, left = 0, right = "auto", bottom = 0,
                                                width = 350, height = 1200, alpha = .2,
                                                div(style = "font-size: 18px;padding:0px;",
                                                checkboxGroupInput(inputId = "selected_locs", 
                                                                   label = h2("Organization Type"), 
                                                                   choices = c(as.list(unique(locations$Type)), "Real Estate"), 
                                                                   selected = "Institution"), 
                                                checkboxGroupInput(inputId = "census_data", label = h2("Demographics"), 
                                                     choices = c("Households W/ Children Under 6", "Median Income", "Non-Family Households")), 
                                                sliderInput("range", "Institution Coverage (Miles)", 0, 2,
                                                                 value = 0, step = .1),    
                                                h4(textOutput("mapclick"))
                                                     ))),
                                    
                                  
                         
                         # Sidebar with a slider input for number of bins 
                         tabPanel("Organization Information", 
                                  dataTableOutput("mytable"))
                         
                         ))



# Define server logic
server <- function(input, output) {
  
  
  output$brooklynmap<- renderLeaflet({
    
    leaflet() %>% 
      addTiles(group = "OpenStreetMap")           %>% 
      
      addProviderTiles(providers$OpenStreetMap)   %>% 
      
      setView(lat  = 40.7, 
              lng  = -73.9, 
              zoom = 11)  
      
      
    ##End of Map
  })
  
  
  observe({
    
    #Recreate polygons-----------------------------------------------------------
    #Children Under 6
    cu6 <- child_und_6()[, c(1:2, 6)]
    
    poly <- split(cu6, cu6$id)
    cu6_poly <- lapply(poly, function(x) { x["id"] <- NULL; x })
    poly <- lapply(cu6_poly, Polygon)
    p1 <- lapply(seq_along(poly), function(i) Polygons(list(poly[[i]]), ID = names(poly)[i]  ))
    cu6_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84") )
    cu6_polydf <- SpatialPolygonsDataFrame(cu6_polys, 
                                           t <- data.frame(id = unique(cu6$id), 
                                           row.names = unique(cu6$id)))
    cu6_meta <- cu6_polydf@data
    colnames(cu6_meta) <- "tract"
    cu6_meta <- left_join(cu6_meta, tract@data, by = "tract")
    cu6_polydf@data <- cu6_meta
    
    #Median Income
    
    medinc <- median_income()[, c(1:2, 6)]
    
    poly2 <- split(medinc, medinc$id)
    medinc_poly <- lapply(poly2, function(x) { x["id"] <- NULL; x })
    poly2 <- lapply(medinc_poly, Polygon)
    p2 <- lapply(seq_along(poly2), function(i) Polygons(list(poly2[[i]]), ID = names(poly2)[i]  ))
    medinc_polys <- SpatialPolygons(p2, proj4string = CRS("+proj=longlat +datum=WGS84") )
    medinc_polydf <- SpatialPolygonsDataFrame(medinc_polys, 
                                           t <- data.frame(id = unique(medinc$id), 
                                                           row.names = unique(medinc$id)))
    medinc_meta <- medinc_polydf@data
    colnames(medinc_meta) <- "tract"
    medinc_meta <- left_join(medinc_meta, tract@data, by = "tract")
    medinc_polydf@data <- medinc_meta
    
    #Non Family HH
    
    nf <- non_family()[, c(1:2, 6)]
    
    poly3 <- split(nf, nf$id)
    nf_poly <- lapply(poly3, function(x) { x["id"] <- NULL; x })
    poly3 <- lapply(nf_poly, Polygon)
    p3 <- lapply(seq_along(poly3), function(i) Polygons(list(poly3[[i]]), ID = names(poly3)[i]  ))
    nf_polys <- SpatialPolygons(p3, proj4string = CRS("+proj=longlat +datum=WGS84") )
    nf_polydf <- SpatialPolygonsDataFrame(nf_polys, 
                                              t <- data.frame(id = unique(nf$id), 
                                                              row.names = unique(nf$id)))
    nf_meta <- nf_polydf@data
    colnames(nf_meta) <- "tract"
    nf_meta <- left_join(nf_meta, tract@data, by = "tract")
    nf_polydf@data <- nf_meta
    
    #Buffer
    
    b <- buffer()[, c(1:2, 5)]
    colnames(b)[3] <- "id"
    poly4 <- split(b, b$id)
    b_poly <- lapply(poly4, function(x) { x["id"] <- NULL; x })
    poly4 <- lapply(b_poly, Polygon)
    p4 <- lapply(seq_along(poly4), function(i) Polygons(list(poly4[[i]]), ID = names(poly4)[i]  ))
    b_polys <- SpatialPolygons(p4, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

    data <- reactiveValues(clickedMarker=NULL)
    
    leafletProxy("brooklynmap") %>% clearMarkers() %>% clearShapes() %>%
      addAwesomeMarkers(icon = icon1, 
                        data = minyans(), 
                        ~Lon, 
                        ~Lat, 
                        label = ~Org.Nam, 
                        popup = paste("Name: ", minyans()$Org.Nam, "<br/>",  "<br/>",
                                      "Type: ", minyans()$Type, "<br/>",  "<br/>",
                                      "Purpose: ", minyans()$Primary.purpose, "<br/>", "<br/>",
                                      "Neighborhood Served: ", minyans()$Neighborhood)) %>% 
      
      addAwesomeMarkers(icon = icon2, 
                        data = institutions(), 
                        ~Lon, 
                        ~Lat, 
                        label = ~Org.Nam, 
                        popup = paste("Name: ", institutions()$Org.Nam, "<br/>",  "<br/>",
                                      "Type: ", institutions()$Type, "<br/>",  "<br/>",
                                      "Purpose: ", institutions()$Primary.purpose, "<br/>", "<br/>",
                                      "Neighborhood Served: ", institutions()$Neighborhood)) %>%
      
      addAwesomeMarkers(icon = icon3, 
                        data = grassroots(), 
                        ~Lon, 
                        ~Lat, 
                        label = ~Org.Nam, 
                        popup = paste("Name: ", grassroots()$Org.Nam, "<br/>",  "<br/>",
                                      "Type: ", grassroots()$Type, "<br/>",  "<br/>",
                                      "Purpose: ", grassroots()$Primary.purpose, "<br/>", "<br/>",
                                      "Neighborhood Served: ", grassroots()$Neighborhood)) %>%
      
      addAwesomeMarkers(icon = icon4, 
                        data = re_comp(), 
                        ~Lon, 
                        ~Lat, 
                        label = ~Org.Nam, 
                        popup = paste("Address: ", re_comp()$street.address, "<br/>",  "<br/>",
                                      "Price: ", re_comp()$purchase.price, "<br/>",  "<br/>",
                                      "Property Type: ", re_comp()$prop..type)) %>%
      
      addPolygons(data = cu6_polydf, 
                  fillColor   = ~pal(chld__6), 
                  fillOpacity = 0.6, 
                  color       = "#BDBDC3", 
                  weight      = 1, 
                  group       = "Children Under 6 2015", 
                  popup       = paste("There are ", 
                                      cu6_polydf$chld__6, 
                                      " families with children under 6 in this tract")) %>%
      
      addPolygons(data = medinc_polydf, 
                  fillColor   = ~pal2(m__2015), 
                  fillOpacity = 0.6, 
                  color       = "#BDBDC3", 
                  weight      = 1, 
                  group       = "Median Income 2015", 
                  popup       = paste("The median income in this tract is ", 
                                      medinc_polydf$m__2015)) %>%
      
      addPolygons(data = nf_polydf,
                  fillColor   = ~pal(n__2015), 
                  fillOpacity = 0.6, 
                  color       = "#BDBDC3", 
                  weight      = 1, 
                  group       = "Non Family Households 2015", 
                  popup       = paste("This tract has ", 
                                      nf_polydf$n__2015, "non famliy households")) %>%
      
      addPolygons(data        = nb, 
                  fillOpacity = 0.001, 
                  color       = "blue", 
                  weight      = 4, 
                  stroke      = TRUE,
                  label       = nb$NTAName, 
                  group = "Neighborhoods"
                  ) %>%
      
      addPolygons(data  = b_polys,
                  color = "blue") %>%
      
      addLayersControl(baseGroups    = c("OpenStreetMap"),
                       overlayGroups = c("Neighborhoods"),
                       options       = layersControlOptions(collapsed = TRUE)) %>%
      
      hideGroup("Neighborhoods")                       
      
     
    
  })
  
  Lat <- 20.99
  Lon <- -90
  Org.Nam <- "Org"
  empty_df <- data.frame(Lat, Lon, Org.Nam)
  
  minyans <- reactive({
    if(!("Minyan" %in% input$selected_locs)){
      return(empty_df)
    }
    else{
      return(locations[locations$Type == "Minyan", ])
    }
  })
  
  institutions <- reactive({
    if(!("Institution" %in% input$selected_locs)){
      return(empty_df)
    }
    else{
      return(locations[locations$Type == "Institution", ])
    }
  })
  
  grassroots <- reactive({
    if(!("Community Based Institution" %in% input$selected_locs)){
      return(empty_df)
    }
    else{
      return(locations[locations$Type == "Community Based Institution", ])
    }
  })
  
  re_comp <- reactive({
    if(!("Real Estate" %in% input$selected_locs)){
      return(empty_df)
    }
    else{
      return(re_comps)
    }
  })
  empty_poly <- structure(list(lon = c(96.135972, 96.1358, 96.136315, 96.136562, 
                                       96.133997, 96.133922, 96.134319, 96.134437, 96.135961, 96.135864, 
                                       96.136154, 96.136261, 96.132261), 
                               lat = c(16.833625, 16.833153, 
                                       16.832978, 16.833512, 16.831038, 16.830729, 16.830637, 16.830853, 
                                       16.83074, 16.830442, 16.83035, 16.830647, 16.85432), 
                               id = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), 
                                              .Label = c("convocation_hall", 
                                                         "judson_church", 
                                                         "the_library"), 
                                              class = "factor"), 
                              id2 = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), 
                                          .Label = c("convocation_hall", 
                                                     "judson_church", 
                                                     "the_library"),
                                          class = "factor"), 
                              id3 = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), 
                                        .Label = c("convocation_hall", 
                                                  "judson_church", 
                                                  "the_library"),
                                        class = "factor"), 
                              id4 = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), 
                                              .Label = c("convocation_hall", 
                                                         "judson_church", 
                                                         "the_library"),
                                              class = "factor")), 
                              .Names = c("lon", 
                                          "lat", 
                                          "id1", "id2", "id3", "id"
                                         ), 
                                row.names = c(NA, -13L), 
                                class = "data.frame")
  
  child_und_6 <- reactive({
    if(!("Households W/ Children Under 6" %in% input$census_data)){
      return(empty_poly)
    }
    else{
      return(fort)
    }
  })
  
  median_income <- reactive({
    if(!("Median Income" %in% input$census_data)){
      return(empty_poly)
    }
    else{
      return(fort)
    }
  })
  
  non_family <- reactive({
    if(!("Non-Family Households" %in% input$census_data)){
      return(empty_poly)
    }
    else{
      return(fort)
    }
  })
  
  
  buffer <- reactive({
    if(input$range == 0){
      return(empty_poly)
    }
    else{
      buff <- gBuffer(location_dot, width = input$range * 1609.34)
      buff <- spTransform(buff, CRS("+proj=longlat + datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      buff <- fortify(buff)
      return(buff)
    }
  })
  
  output$mytable = DT::renderDataTable({
    locations2 <- select(locations, Org.Nam, Type, 
                         Address, primary.purpose, 
                         Audience, Neighborhoods, Phone.Number, Email)
    colnames(locations2) <- c("Organization", "Category", 
                              "Address", "Purpose", "Audience", 
                              "NBHD Served", "Phone Number", "Email")
    return(locations2)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
