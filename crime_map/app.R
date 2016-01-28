library(shiny) ; library(sp); library(rgdal) ; library(leaflet) ; library(dygraphs) ; library(xts) ; library(dplyr) ; library(markdown)

crimes <- read.csv("crime_data.csv", header = T)
boroughs <-  readOGR("boroughs.geojson", "OGRGeoJSON")

ui <- navbarPage(title = "Crime map",
                 tabPanel("Interactive plots",
                          column(3,
                                 div(class="outer",
                                     tags$head(includeCSS('custom.css')),
                                     absolutePanel(id = "controls", class="panel panel-default", draggable = FALSE, fixed = TRUE, 
                                                   top = 85, left = 80, right = "auto", bottom = "auto", height = "auto", width = 260,
                                          h3("Instructions"),
                                          p('This ', a('shiny', href = 'http://shiny.rstudio.com'), 'app allows you to interactively visualise Greater Manchester Police 
                                            crime data downloaded from', a('data.police.uk', href = 'https://data.police.uk')),
                                          p('Use the dropdown menus below to select the borough and crime category 
                                                   of interest.'), 
                                          p('Then zoom and pan around the map to explore clusters of crime. Click on
                                                   the red circles for information on individual crimes.'), 
                                          p('The map will also update when you interact with the time series chart.'),
                                          hr(),
                                          div(uiOutput('borough'), style = "color:#525252", align = "left"),
                                          div(uiOutput('category'), style = "color:#525252", align = "left")))),
                            column(8,
                                   br(),
                                   div(h4(textOutput("title"), align = "left"), style = "color:#525252"),
                                   fluidRow(
                                    leafletOutput("map", width = "100%", height = "400"),
                                            absolutePanel(id = "controls", class="panel panel-default", draggable = TRUE, fixed = TRUE,
                                                 top = 160, left = "auto", right = 160, bottom = "auto", height = "20", width = "220",
                                                 strong(textOutput("frequency"), style = "color:red", align = "left"))),
                                   fluidRow(
                                     br(),
                                     dygraphOutput("dygraph", width = "100%", height = "130px")))),
                 tabPanel("About",
                          fluidRow(
                            column(8, offset = 1,
                                   includeMarkdown("about.md")))))

server <- function(input, output, session) {
  
  output$borough <- renderUI({
    selectInput("borough", label = "Select a borough",
                choices = levels(droplevels(crimes$borough)),
                selected = "Manchester")
  })
  
  output$category <- renderUI({
    selectInput("category", label = "Select a crime category",
                  choices = levels(droplevels(crimes$category)),
                selected = "Burglary")
  })
    
  selected_crimes <- reactive({crimes %>% 
      filter(borough == input$borough & category == input$category)})
  
  output$title <- renderText({
    req(input$dygraph_date_window[[1]])
    paste0(input$category, " in ", input$borough, " between ", strftime(input$dygraph_date_window[[1]], "%B %Y"), " and ", 
           strftime(input$dygraph_date_window[[2]], "%B %Y"))
  })  
  
  output$dygraph <- renderDygraph({
    req(input$category)
      df <- selected_crimes() %>% 
        mutate(month = as.Date(month, format = '%Y-%m-%d')) %>%
        group_by(month) %>%
        summarize(n = n()) %>%
        select(month, n)
      
      df.xts <- xts(df$n, order.by = as.Date(df$month, "%Y-%m-%d"), frequency = 12)
      
      dygraph(df.xts, main = NULL) %>%
        dySeries("V1", label = "Crimes", color = "red", fillGraph = TRUE, strokeWidth = 2, drawPoints = TRUE, pointSize = 4) %>%
        dyAxis("y", axisLabelWidth = 20) %>% 
        dyOptions(retainDateWindow = TRUE, includeZero = TRUE, drawGrid = FALSE,
                  axisLineWidth = 2, axisLineColor = "#525252", axisLabelFontSize = 11, axisLabelColor = "#525252") %>% 
        dyLegend(width = 200) %>% 
        dyCSS("dygraph.css")
  })
  
  points <- reactive({crimes %>% 
      mutate(month = as.Date(month, format = '%Y-%m-%d')) %>%
      filter(borough == input$borough & 
               category == input$category &
               month >= input$dygraph_date_window[[1]], month <= input$dygraph_date_window[[2]])
    
  })
  
  output$map <- renderLeaflet({
    
    req(input$dygraph_date_window[[1]])
    
    popup <- paste0("<strong>Location: </strong>", points()$location,
                    "<br><strong>Borough: </strong>", points()$borough,
                    "<br><strong>Category: </strong>", points()$category,
                    "<br><strong>Date: </strong>", points()$month)
    
      leaflet(points()) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addPolygons(data = boroughs, color = "#525252", weight = 2, fillColor = "transparent") %>% 
      addCircleMarkers(data = points(), ~long, ~lat, radius = 5, stroke = TRUE,
                       color = "red", weight = 3, opacity = 0.8, fillColor = "white",
                       popup = popup,
                       clusterOptions = markerClusterOptions(
                         # zoom to cluster bounds when clicked
                         zoomToBoundsOnClick = TRUE,
                         # render cluster markers when lowest zoom level clicked
                         spiderfyOnMaxZoom = TRUE, 
                         # maximum cluster radius in pixels from central marker
                         maxClusterRadius = 50))

  })
 
  dataInBounds <- reactive({
    df <- points()
    if (is.null(input$map_bounds))
      return(df[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(df,
           lat >= latRng[1] & lat <= latRng[2] &
             long >= lngRng[1] & long <= lngRng[2])
  })
  
  output$frequency <- renderText({
    req(input$map_bounds)
    
    df <- dataInBounds() %>% 
      group_by(category) %>%
      summarize(n = n())
    
    paste0(df$n, " crimes displayed")
  })
  
  
}

shinyApp(ui, server)
