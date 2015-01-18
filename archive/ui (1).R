require(shiny)
require(rCharts)
require(plyr)
require(lubridate)

options(RCHARTS_LIB = 'polycharts')

shinyUI(fluidPage(
  titlePanel("Portr Dash-Two"),
  
  fluidRow(
    column(4,
           wellPanel(
             p("please ignore any errors until a bookings.csv file has been uploaded"),
             
             #file input
             fileInput("file", label = h6("Upload Booking Data")),
             p("csv file can be found at ", a(href="https://booking.portr.com/Partner/ViewBookings", target ="_blank", "booking.portr.com")),
             
             #date range input
             dateRangeInput("dates", label = h6("Date range")),
             
             #airport input
             selectInput("airport", label = h6("Filter by airport (not in use)"),
                         choices = list("All", "LHR", "LGW", "LCY"), selected = "All")
             
             )
           ),
    
    column(8,
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
  
  #fluidRow(
  #  mapOutput('mapContainer')
  #  ),
  
  fluidRow(
    tableOutput("contents")
    )
  
  ))