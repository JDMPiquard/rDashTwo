require(shiny)
require(rCharts)
require(plyr)
require(lubridate)
require(ggmap)
require(ggplot2)

options(RCHARTS_LIB = 'polycharts')

shinyUI(fluidPage(
  
  tags$head(
    HTML("<title>Portr Operations Dash</title>")
    ),
  
  #titlePanel("Portr Dash-Two"),
  
  fluidRow(p(" ")),
  
  fluidRow(
    column(1,
           img(src="logo2.png", width = 55, height = 55, style = "border-radius: 3px; float = bottom")
           ),
    column(11,
           h1("Portr Dash Two")
           )
    ),
  
  fluidRow(p(" ")),
  
  fluidRow(
    column(3,
           wellPanel(
             p("please ignore any errors until a bookings.csv file has been uploaded"),
             p("csv file can be found at ", a(href="https://booking.portr.com/Partner/ViewBookings", target ="_blank", "booking.portr.com")),
             
             #file input
             fileInput("file", label = h5("Upload Booking Data")),
             
             hr(),
             h5("Apply Filters"),
             #date range input
             dateRangeInput("dates", label = h6("Date range")),
             
             #airport input
             selectInput("airport", label = h6("Filter by airport (not in use)"),
                         choices = list("All", "LHR", "LGW", "LCY"), selected = "All")
             
             )
           ),
    
    column(9,
           showOutput("chart1","polycharts")
           )
    ),
  
  ##LINE BREAK INTO SELECTED PERIOD DATA
  fluidRow(
    hr(),
    h3(textOutput("textDates"))
  ),
  
  ##Summary value boxes
  fluidRow(
    
    column(3,
           wellPanel(h3(textOutput("W"), align = "center"),
                     p("bookings", align = "center")
                     )
    ),
    
    column(2,
           wellPanel(h3(textOutput("X"), align = "center"),
                     p("bags", align = "center")
           )
    ),
    
    column(3,
           wellPanel(h3(textOutput("Y"), align = "center"),
                     p("net revenue", align = "center")
           )
    ),
    
    column(2,
           wellPanel(h4(textOutput("mX"), align = "center"),
                     p("avg bags/booking", align = "center")
           )
    ),
    
    column(2,
           wellPanel(h4(textOutput("mY"), align = "center"),
                     p("net revenue/booking", align = "center")
           )
    )
      
  ),
  
  fluidRow(
    tableOutput("contents")
    ),
  
  fluidRow(
    hr(),
    column(2, wellPanel(
      p("click on the button below to show map of post code locations"),
      #show map button
      actionButton("showMap", label = h6("Show Map")),
      p("click on individual markers to obtain additional information")
      )),
    column(10,
           chartOutput('mapContainer','leaflet'))
    
  )
  
  ))