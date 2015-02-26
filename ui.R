require(shiny)
require(rCharts)
require(plyr)
require(lubridate)
require(ggmap)
require(ggplot2)
require(googleVis)

shinyUI(fluidPage(
  
  tags$head(
    HTML("<title>Portr Operations Dash</title>"),
    tags$link(rel="stylesheet", type="text/css", href="stylePortr.css")
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
        p(" ")
      ),
      
      ##TABS!!!
      tabsetPanel(id='main',
                  
      ##TAB 1: Summary
      tabPanel('Summary',
                           
      
      ##DATE RANGE SELECTION
      fluidRow(
        column(4, 
              dateRangeInput("dates",start = as.character(Sys.Date()-7), 
                            end = as.character(Sys.Date()), 
                            label = h6("Select Date range to analyse"))
        )

      ),
      
      
        fluidRow(
          column(11,
                 h2(textOutput("textDates"), style="color:#36648B")
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
          div(class="sideBox", style="text-align: left",
              p(strong("Delivery Locations Map: "), "click on individual markers to obtain additional information"),
              p("click on the button below to show map of delivery locations"),
              #show map button
              checkboxInput("showMap", label = h6("Show Map"), value = F)
          )
        )
  ),#closing tab panel 1
  
  tabPanel('Statistics',
           
           fluidRow(
             column(3,
                    htmlOutput("sex")),
             
             column(3,
                    htmlOutput("luggage")),
             
             column(3,
                    htmlOutput("journey")),
             
             column(3,
                    htmlOutput("type"))
             ),
           
           #HOUR PLOT
           fluidRow(p(" ")),
           fluidRow(
             div(style="text-align:left; margin-top:21px",
                 htmlOutput("hourPlot")
             )
           ),
           
           #Nationalities
           fluidRow(p(" ")),
           fluidRow(
             h3("Main user Nationalities"),
             div(class="statsContainer",
               dataTableOutput(outputId="nation"), style="padding:0px; margin-top:20px; margin-bottom:30px; border-bottom: solid #f6f6f6")
           ),
           
           #Repeat users
           fluidRow(
             h3(textOutput("reUser")),
             div(class="statsContainer",
                 dataTableOutput(outputId="custom"), style="padding:0px; margin-top:20px")
           ),
           
           #Main destinations
           fluidRow(p(" ")),
           fluidRow(
             h3("Main destinations"),
             div(class="statsContainer",
                 dataTableOutput(outputId="loc"), style="padding:0px; margin-top:20px")
           )
           
  )#closing tab panel 2

  )#closing tabs altogether
  )
      ))