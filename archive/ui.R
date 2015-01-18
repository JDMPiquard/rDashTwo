require(rCharts)
require(plyr)
require(lubridate)
options(RCHARTS_LIB = 'polycharts')

shinyUI(fluidPage(
  titlePanel("Dash-Two"),
  
  fluidRow(
    column(4,
           wellPanel(
             #file input
             fileInput("file", label = h6("Upload Booking Data")),
             #date range input
             dateRangeInput("dates", label = h6("Date range"))
             )
           ),
    
    column(8,
           h3("All time")
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
    )
  
  ))