require(shiny)
require(rCharts)
require(plyr)
require(lubridate)
require(ggmap)
require(ggplot2)
require(googleVis)

options(RCHARTS_LIB = 'polycharts')

shinyUI(fluidPage(
  
  tags$head(
    HTML("<title>Portr Operations Dash</title>"),
    tags$style(
      HTML(
        "
        .box{
            background-color:#F2F2F2;
            border-radius:5px;
            padding:3px;
            text-align:center
          }
        .box h2, .box h3,.box h4,.box h5,.box h6{
            color: #4D4D4D;
        }
        .box p{
            color: #68838B;
        }
        
        "
        )
      )
    ),
    
  fluidRow(p(" ")),
  
  fluidRow(
    column(1,
           img(src="logo2.png", width = 70, height = 70, style = "border-radius: 50%; float = bottom")
           ),
    column(11,
           h1("Portr Dash Two")
           )
    ),
  
  fluidRow(p(" ")),
  
  fluidRow(
    column(4,
           wellPanel(
             h5("Analyse Portr booking data using simple interface"),
             p("choose appropriate date range to see in action"),
             p("please ignore any errors until a bookings.csv file has been uploaded"),
             p("csv file can be found at ", a(href="https://booking.portr.com/Partner/ViewBookings", target ="_blank", "booking.portr.com")),
             
             #file input
             fileInput("file", label = h5("Upload Booking Data")),
             
             hr(),
             h5("Apply Filters"),
             #date range input
             dateRangeInput("dates",start = as.character(Sys.Date()-7), 
                            end = as.character(Sys.Date()), label = h6("Date range")),
             
             #airport input
             selectInput("airport", label = h6("Filter by airport (not in use)"),
                         choices = list("All", "LHR", "LGW", "LCY"), selected = "All")
             
             )
           ),
    
    column(8,
           fluidRow(
             htmlOutput("MAIN")
             #showOutput("chart1","polycharts")
             ),
           
           fluidRow(
             column(4,
                    plotOutput("CUM")
                    ),
             column(4,
                    htmlOutput("DAY")
             ),
             column(4,
                    
                     htmlOutput("ZONE")
             )
             
             )
           
           )
    ),
  
  ##LINE BREAK INTO SELECTED PERIOD DATA
  fluidRow(
    hr(),
    h3(textOutput("textDates"), style="color:#36648B")
  ),
  
  ##BOXES: SUMMARY VALUES
  fluidRow(
    
    column(2,
           div(h2(textOutput("W")),
                     p("bookings"),
               class = "box")
    ),
    
    column(2,
           div(h2(textOutput("X")),
                     p("bags"),
              class = "box")
    ),
    
    column(3,
           div(h2(textOutput("Y")),
                     p("net revenue"),
              class = "box")
    ),
    
    column(2,
           div(h4(textOutput("mX")),
                     p("avg bags/booking"),
               class = "box")
    ),
    
    column(2,
           div(h4(textOutput("mY")),
                     p("net revenue/booking"),
               class = "box")
    )
      
  ),
  
  fluidRow(
    div(tableOutput("contents"), style="padding:5px")
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