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
        column(7,
               h1("Portr Dash Two")
               ),
        column(4,
               #file input
               fileInput("file", label = " ")       
        )
      ),
      
      div(class="topBox",
      fluidRow(
         column(5,
                h5("Analyse Portr booking data using simple interface"),
                p("Choose appropriate date range to see in action. Ignore any errors until a bookings.csv file has been uploaded. '.csv' file can be found at ", a(href="https://booking.portr.com/Partner/ViewBookings", target ="_blank", "booking.portr.com"))
         ),
         column(1,
                p(" ")
                ),

         column(2,
                checkboxGroupInput("checkAirport", label= "filter by airport",
                                     choices = c("LHR", "LGW", "LCY", "STN", "Storage"),
                                     selected = c("LHR", "LGW", "LCY", "STN", "Storage"))
                ),

         column(4,       
                dateRangeInput("dates",start = as.character(Sys.Date()-7), 
                               end = as.character(Sys.Date()), 
                               label = "Select Date range to analyse"),
                
                radioButtons("radio", label = h3(" "),
                             choices = list("All Time" = 1, "Date Range" = 2, "Today Only" = 3), 
                             selected = 2)
                )
        )),
      
      fluidRow(
        
        
        
        ##TOP CHARTS
        column(12,
               
               ##Title
               fluidRow(
                 div(style="color:#36648B;padding:10px",
                   h4(textOutput("selectedAirports"))
                   )
                 ),
               
               ##Main
               fluidRow(
                 htmlOutput("MAIN")
               )
              
               
        )
        
      ),
      
      fluidRow(
        h4(textOutput("sumStats"))
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
      
      ##LINE BREAK INTO SELECTED PERIOD DATA
      fluidRow(
        p(" ")
      ),
      
      ##TABS!!!
      tabsetPanel(id='main',
                  
      ##TAB 1: Detailed view
      tabPanel('Detail',    
         
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
           
           div(class="statsContainer",
           fluidRow(
             h4("Core Usage Statistics")
             ),
           fluidRow(
             
             column(4,
                    htmlOutput("sex")),
             
             column(4,
                    htmlOutput("luggage")),
             
             column(4,
                    htmlOutput("journey"))
      
             ),
           
           ##Smaller
           fluidRow(
             column(4,
                    htmlOutput("type")
                    ),
             
             column(4,
                    htmlOutput("DAY")
             ),
             column(4,
                    htmlOutput("ZONE")
             )
           )
           ), #close Div
           
           #HOUR PLOT
           div(class = "statsContainer",
           fluidRow(
             h4("Aggregated performance by hour of day")),
           fluidRow(
                 htmlOutput("hourPlot")
             )
           ),
           
           #Nationalities
           fluidRow(p(" ")),
           fluidRow(
             h4("Main user Nationalities"),
             div(class="statsContainer",
               dataTableOutput(outputId="nation"), style="padding:0px; margin-top:20px; margin-bottom:30px; border-bottom: solid #f6f6f6")
           ),
           
           #Repeat users
           fluidRow(
             h4(textOutput("reUser")),
             div(class="statsContainer",
                 dataTableOutput(outputId="custom"), style="padding:0px; margin-top:20px")
           ),
           
           #Main destinations
           fluidRow(p(" ")),
           fluidRow(
             h4("Repeat destinations"),
             div(class="statsContainer",
                 dataTableOutput(outputId="loc"), style="padding:0px; margin-top:20px")
           )
           
  )#closing tab panel 2

  )#closing tabs altogether
  )
      ))