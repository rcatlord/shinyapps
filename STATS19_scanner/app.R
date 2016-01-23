library(shiny) ; library(shinydashboard) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(RColorBrewer) ; library(ggvis) ; library(DT)

# load the casualty data
data <- readRDS(file="casualties_2005-14.Rda")
data$date <- as.Date(data$date, "%Y-%m-%d")
data$severity <- factor(data$severity, levels= c("Fatal", "Serious", "Slight"), ordered = TRUE)
data$day <- factor(data$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered=T)
data$hour <- factor(data$hour)
data$light <- ordered(data$light, levels = c("Dark", "Daylight"))
borough_choices <- c("All", levels(data$borough))
mode_choices <- c("All", levels(data$mode))
severity_choices <- c("All", levels(data$severity))
boroughs <-  readOGR("boroughs.geojson", "OGRGeoJSON")

header <- dashboardHeader(
  title = "STATS19 scanner"
)

body <- dashboardBody(
  fluidRow(
    column(width = 6,
           box(width = NULL,
               valueBoxOutput("casualtyBox"),
               valueBoxOutput("KSIBox"),
               valueBoxOutput("collisionBox"),
               leafletOutput("map", height = 800))
    ),
    column(width = 6,
           box(width = NULL, 
               dateRangeInput("Date range", inputId = "date_range",  
                              start = "2014-01-01",
                              end = "2014-12-31",
                              format = "yyyy-mm-dd"),
               selectInput("Borough", inputId = "borough",
                           choices = borough_choices,
                           selected = "All", multiple = TRUE),
               selectInput("Mode of travel", inputId = "mode",
                           choices = mode_choices, 
                           selected = "Pedal Cycle", multiple = TRUE),
               selectInput("Casualty severity",inputId = "severity",
                           choices = severity_choices, 
                           selected = "All", multiple = TRUE),
               hr(),
               tabBox(width = NULL,
                       tabPanel("Boroughs",
                                h5("Casualties by borough", style = "color:black", align = "center"),
                                ggvisOutput("borough_count")),
                       tabPanel("Months",
                                h5("Casualties by month and gender", style = "color:black", align = "center"),
                                ggvisOutput("timeband_month")),
                       tabPanel("Hours",
                                h5("Casualties by hour and severity", style = "color:black", align = "center"),
                                ggvisOutput("timeband_hour")),
                       tabPanel("Demographics",
                                h5("Casualties by ageband and gender", style = "color:black", align = "center"),
                                ggvisOutput("ageband_gender")),
                       tabPanel("Data",
                                DT::dataTableOutput("table")),
                       tabPanel("About", br(),
                                p("This Shiny application is designed to allow the user to interrogate road casualties reported in Greater London between 2005 and 2014."),
                                strong("How to use"),
                                p("The filter panel allows the user to plot reported road casualties by date range, borough, mode of travel and severity onto the map. 
                                  Details of the collision can be obtained by clicking on any of the points. 
                                  Information on the temporal and demographic profile of casualties are provided under the relevant tabs."),
                                strong("Data sources"),
                                p("STATS19 collision data for Greater London are available from",
                                  a("Transport for London",
                                    href = "https://www.tfl.gov.uk/corporate/publications-and-reports/road-safety"),
                                  "and a guide to the variables can be found",
                                  a("here.",
                                    href = "https://www.tfl.gov.uk/cdn/static/cms/documents/collision-data-guide.pdf")),
                                strong("Credits"),
                                p("The ",
                                  a("leaflet",
                                    href = "https://rstudio.github.io/leaflet/"), ", ",
                                  a("DT",
                                    href = "https://rstudio.github.io/DT/"), " and ",
                                  a("ggvis",
                                    href = "http://ggvis.rstudio.com"), " R packages were used in this ",
                                  a("Shiny",
                                    href = "http://shiny.rstudio.com"), " app. Some of the code for the STATS19_scanner app was adapted from ",
                                  a("Superzip",
                                    href = "http://shiny.rstudio.com/gallery/superzip-example.html"), "by Joe Cheng. The ui was inspired by ",
                                  a("blackspot",
                                    href = "http://blackspot.org.uk"),
                                  " by Ben Moore and ",
                                  a("Twin Cities Buses",
                                    href = "https://gallery.shinyapps.io/086-bus-dashboard/"), " by Aron Atkins."),
                                  strong("Licence"),
                                  p("Contains National Statistics data © Crown copyright and database right [2015] and 
                                  Contains Ordnance Survey data © Crown copyright and database right [2015]."),
                                br(),
                                p("Repo here: ",
                                  a(href = "https://github.com/hpartridge/STATS19_scanner", icon("github"), target = "_blank")
                                ))
               )))))

ui <- shinyUI(dashboardPage(header, dashboardSidebar(disable = TRUE), body, skin = "black"))


server <- function(input, output, session) {
  
  # reactive data
  
  casualties <- reactive({data %>% filter(date %in% seq(input$date_range[1], input$date_range[2], by = "day") &
                                            borough %in% input$borough &
                                            mode %in% input$mode &
                                            severity %in% input$severity)})
  
  observe({
    if ("All" %in% input$borough) {
      selected_choices <- setdiff(borough_choices, "All")
      updateSelectInput(session, "borough", selected = selected_choices)
    }
  })
  
  observe({
    if ("All" %in% input$mode) {
      selected_choices <- setdiff(mode_choices, "All")
      updateSelectInput(session, "mode", selected = selected_choices)
    }
  })
  
  observe({
    if ("All" %in% input$severity) {
      selected_choices <- setdiff(severity_choices, "All")
      updateSelectInput(session, "severity", selected = selected_choices)
    }
  })
  
  # reactive data in bounds
  
  dataInBounds <- reactive({
    df <- casualties()
    if (is.null(input$map_bounds))
      return(df[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(df,
           lat >= latRng[1] & lat <= latRng[2] &
             long >= lngRng[1] & long <= lngRng[2])
  })
  
  # Value boxes
  
  output$casualtyBox <- renderValueBox({
    df <- dataInBounds()
    valueBox(
      format(nrow(df), format="d", big.mark=","), "Casualties", icon = NULL,
      color = "black")
  })
  
  output$KSIBox <- renderValueBox({
    df <- dataInBounds()
    df <- df %>% filter(severity == "Fatal" | severity == "Serious")
    valueBox(
      format(nrow(df), format="d",big.mark=","), "Killed or Seriously Injured", icon = NULL,
      color = "black")
  })
  
  output$collisionBox <- renderValueBox({
    df <- dataInBounds()
    df <- df %>% distinct(AREFNO)
    valueBox(
      format(nrow(df), format="d",big.mark=","), "Collisions", icon = NULL,
      color = "black")
  })
  
  
  ## Map ##
  
  output$map <- renderLeaflet({
    leaflet(data) %>%  addProviderTiles("CartoDB.DarkMatter") %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addPolygons(data = boroughs, fill = F, color = "white", weight = 1.5, group = "Boroughs") %>% addLayersControl(
        overlayGroups = "Boroughs",
        position = "topright",
        options = layersControlOptions(collapsed = FALSE)) %>% 
      addLegend(position = "bottomleft", colors = c("#b10026", "#fd8d3c", "#ffeda0"), 
                labels = c("Fatal", "Serious", "Slight"), opacity = 1, title = "Severity")
  })
  
  observe({
    pal <- colorFactor(c("#b10026", "#fd8d3c", "#ffeda0"), domain = c("Fatal", "Serious", "Slight"), ordered = TRUE)
    if(nrow(casualties())==0) { leafletProxy("map") %>% clearMarkers()} 
    else {
      leafletProxy("map", data = casualties() ) %>% clearMarkers() %>%
        addCircleMarkers(~long, ~lat, color = ~pal(severity), radius = 5, stroke = FALSE, fillOpacity = 0.5, popup = ~text)
    }
  })
  
  
  
  ## Boroughs ## 
  
  # Casualties by borough
  borough_count <- reactive({ 
    df <- dataInBounds()
    df %>%
      group_by(borough) %>%
      summarise(count = n()) %>% 
      ggvis(~borough, ~count, fill := "black") %>%
      mutate(borough = reorder(borough, -count)) %>%
      layer_bars(stroke := "white") %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      hide_legend("fill") %>% 
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  borough_count %>% bind_shiny("borough_count")  
  
  
  ## Months ##
  
  # Casualties by month and severity
  timeband_month <- reactive({ 
    df <- dataInBounds()
    df %>%
      group_by(severity, month) %>%
      summarise(count = n()) %>% 
      ggvis(~month, ~count, fill = ~severity) %>%
      layer_bars(stroke := "white") %>%
      scale_nominal("fill", range = c("#b10026", "#fd8d3c", "#ffeda0")) %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      add_legend("fill", title = "Severity") %>%  
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  timeband_month %>% bind_shiny("timeband_month")
  
  ## Hours ##
  
  # Casualties by hour and light conditions
  timeband_hour <- reactive({ 
    df <- dataInBounds()
    df %>%
      group_by(light, hour) %>%
      summarise(count = n()) %>% 
      ggvis(~hour, ~count, fill = ~light) %>%
      layer_bars(stroke := "white") %>%
      scale_nominal("fill", range = c("midnightblue", "yellow")) %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      add_legend("fill", title = "Light conditions") %>% 
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  timeband_hour %>% bind_shiny("timeband_hour")
  
  ## Demographics ##  
  
  # Casualties by ageband and gender
  ageband_gender <- reactive({ 
    df <- dataInBounds()
    df %>%
      group_by(sex, ageband) %>%
      summarise(count = n()) %>% 
      ggvis(~ageband, ~count, fill = ~sex) %>%
      layer_bars(stroke := "white") %>%
      scale_nominal("fill", range = c("steelblue", "purple"), label=c("Male", "Female")) %>%
      add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>% 
      add_legend("fill", title = "Gender") %>% 
      set_options(width="auto") %>% 
      add_tooltip(function(casualties) (casualties$stack_upr_ - casualties$stack_lwr_))
  })
  ageband_gender %>% bind_shiny("ageband_gender") 
  
  ## Data ##
  output$table <- DT::renderDataTable({
      df <- dataInBounds()
      df %>%
      select(AREFNO, date, mode, severity, sex, ageband)
    
  }, rownames = FALSE, filter = 'none', options = list(
    pageLength = 10, autoWidth = TRUE))
  
}

shinyApp(ui, server)