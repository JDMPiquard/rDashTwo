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
        padding:10px;
        text-align:center
        }
        .box h2, .box h3,.box h4,.box h5,.box h6{
        color: #4D4D4D;
        }
        .box p{
        color: #68838B;
        }
        
        .sideBox{
        background-color:#F2F2F2;
        border-radius:5px;
        padding:3px;
        text-align:left;
        padding-left:10px;
        }
        .sideBox h2, .sideBox h3,.sideBox h4, .sideBox h5,.sideBox h6{
        color: #4D4D4D;
        }
        .sideBox p{
        color: #68838B;
        }
        
        "
      )
      )
      ),
  
  div(style="margin:0 auto; max-width:800px", 
      
      fluidRow(p(" ")),
      
      ##TITLE HEADER
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
        
        ##TOP SIDEBAR
        column(3,
               div(class="sideBox",
                   h5("Analyse Portr booking data using simple interface"),
                   p("choose appropriate date range to see in action"),
                   p("please ignore any errors until a bookings.csv file has been uploaded"),
                   p("csv file can be found at ", a(href="https://booking.portr.com/Partner/ViewBookings", target ="_blank", "booking.portr.com")),
                   
                   #file input
                   fileInput("file", label = h5("Upload Booking Data")),
                   
                   hr(),
                   h5("Apply Filters"),
                   #              #date range input
                   #              dateRangeInput("dates",start = as.character(Sys.Date()-7), 
                   #                             end = as.character(Sys.Date()), label = h6("Date range")),
                   #              
                   #airport input
#                    selectInput("airport", label = h6("Filter by airport (not in use)"),
#                                choices = list("All", "LHR", "LGW", "LCY", "Storage"), selected = "All"),
                   
                   checkboxGroupInput("checkAirport", label = h6("Select booking source"), 
                                      choices = c("LHR", "LGW", "LCY", "STN", "Storage"),
                                      selected = c("LHR", "LGW", "LCY", "STN", "Storage"))
                   
               )
        ),
        
        
        ##TOP CHARTS
        column(9,
               
               ##Title
               fluidRow(
                 div(style="color:#36648B;padding:10px",
                   h5(textOutput("selectedAirports"))
                   )
                 ),
               
               ##Main
               fluidRow(
                 htmlOutput("MAIN")
               ),
               ##Smaller
               fluidRow(
                 column(6,
                        htmlOutput("DAY")
                 ),
                 column(6,
                        htmlOutput("ZONE")
                 )
               )
               
        )
        
      ),
      
      ##LINE BREAK INTO SELECTED PERIOD DATA
      fluidRow(
        hr()
      ),
      
      ##DATE RANGE SELECTION
      fluidRow(
        dateRangeInput("dates",start = as.character(Sys.Date()-7), 
                       end = as.character(Sys.Date()), 
                       label = h6("Select Date range to analyse"))
      ),
      
      fluidRow(
        column(11,
               h3(textOutput("textDates"), style="color:#36648B")
        ),
        column(1,
               " "
        )
        
      ),
      
      ##BOXES: SUMMARY VALUES
      div(style="margin:0 auto, text-align:center",
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
            
            column(1,
                   div(h4(textOutput("mX")), p("avg bags"),
                       class = "box")
            ),
            
            column(2,
                   div(h4(textOutput("mY")), p("avg net revenue"),
                       class = "box")
            ),
            
            column(2,
                   div(h4(textOutput("preBook")),
                       p("pre-bookings"),
                       class = "box")
            )
            
          )),
      
      ##DAY PLOT
      fluidRow(
        div(style="text-align:center; margin-top:21px",
            htmlOutput("dayPlot")
        )
      ),
      
#       ##TEXT SUMMARY AREA
#       
#       fluidRow(
#         
#         column(7,
#                div(style="border-style: solid; border-width: 3px; border-color: #F2F2F2; border-radius: 10px; padding: 8px",
#                    "boom"
#                    
#                    
#                    )
#           ),
#         column(5,
#                div(class="box", 
#                    "boomBox"
#                    )
#                )
#           ),
      
      ##TABLE
      fluidRow(
        div(dataTableOutput(outputId="contents"), style="padding:0px; margin-top:20px")
      ),
      
      ##MAP
      fluidRow(
        hr(),
        div(style="margin:0 auto",
            chartOutput('mapContainer','leaflet')
        )
      ),
      
      fluidRow(
        div(class="sideBox", style="text-align: center",
            p(strong("Delivery Locations Map: "), "click on individual markers to obtain additional information"),
            p("click on the button below to show map of delivery locations"),
            #show map button
            actionButton("showMap", label = h6("Show Map"))
        )
      )
  )
      ))