require(shiny)
require(rCharts)
require(plyr)
require(lubridate)
require(ggmap)
require(ggplot2)
require(googleVis)
options(RCHART_WIDTH = 600)

shinyServer(function(input,output){
  
  ##TAKING CARE OF INPUTS AND MAKING THEM "GLOBAL"#############
  #store dates within: range()
  range  <-  reactive({
    input$dates
  })
  
  ready <- reactive({
    temp  <- input$file
    if (is.null(temp))
      return(NULL)
    else return(1)
  })
  
  #MAIN DATA FRAME: all()
  all  <- reactive({
    temp  <- input$file
    
    if (is.null(temp))
      return(NULL) #returns NULL if there is no file yet
    
    allData <- read.csv(temp$datapath)
    
    #start cleaning up data
    bookings <- subset(allData, Booking.Value > 0) #exclude promotional or internal deliveries
    #remember conversion into date, considering format
    bookings$day <- weekdays(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
    bookings$month <- month(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
    bookings$year <- year(as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
    bookings$date  <- as.Date(bookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y")
    bookings$rank  <- as.Date(paste0(bookings$year,'-',bookings$month,'-01'),"%Y-%m-%d")
    
    #Cleaning up postCodes
    bookings$from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
    bookings$to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)
    
    bookings
  })
  
  sumMonth <- reactive({
    sumBookings <- ddply (all(), c("month","year"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
    sumBookings$meanNetRevenue <- sumBookings$netRevenue/sumBookings$bookings
    sumBookings$monthName  <- month.abb[sumBookings$month] #getting the month name fo plotting purposes
    
    #sumBookings$order  <- paste0(sumBookings$year,'-',sumBookings$month,'-01'),"%Y-%m-%d")
    
    #sumBookings <- sumBookings[order(c(sumBookings$year,sumBookings$month)),]
    sumBookings
  })
  
  sumCum  <- reactive({
    sumBookings  <- sumMonth()
    sumBookings$rank  <- as.Date(paste0(sumBookings$year,'-',sumBookings$month,'-01'),"%Y-%m-%d")
    sumBookings <- sumBookings[order(sumBookings$rank),]
    sumBookings  <- within(sumBookings, cum  <- cumsum(netRevenue)) #calculating cummulatives
    
  })
  
  bookingsRange  <- reactive({
    bookings  <- all()
    subset(bookings, date>=range()[1]&date<=range()[2])
  })
  
  sumDate <- reactive({
    sumBookings <- ddply (bookingsRange(), c("date","Outward.Journey.Delivery.Date"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
    sumBookings$meanNetRevenue <- sumBookings$netRevenue/sumBookings$bookings
    sumBookings$day  <- weekdays(as.Date(sumBookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
    sumBookings <- sumBookings[c("date","Outward.Journey.Delivery.Date","day","bookings","totalBags","meanBags","netRevenue","meanNetRevenue")]
    sumBookings <- sumBookings[order(sumBookings$date, decreasing = TRUE),]
  })
  
  ##OUTPUTS###################################################
  #CHARTS
  
  output$MAIN <- renderGvis({
    if (is.null(ready())) return(NULL)
    df <- sumCum()
    Combo <- gvisComboChart(df, xvar="rank",
                            yvar=c("netRevenue", "cum"),
                            #options=list(seriesType="bars",series='{1: {type:"line"}}')
                            options=list(height=300,
                                         series="[{
                                         type:'bars', targetAxisIndex:0
  },{
                                         type:'line', targetAxisIndex:1
  }]",
                                          vAxes="[{title:'GBP/month'}, {title:'GBP/cumulative'}]",
                                         title= "bookings net revenue month by month"
                            )
    )
})

output$ZONE <- renderGvis({
  if (is.null(ready())) return(NULL)
  
  dat <- ddply (all(), "Tariff.Zone", summarize, count = length(Is.Cancelled))
  dat$Tariff.Zone  <- as.character(dat$Tariff.Zone)
  doughnut <- gvisPieChart(dat, 
                           options=list(
                             width=300,
                             height=300,
                             title='Zone Breakdown',
                             legend='none',
                             pieSliceText='label',
                             pieHole=0.5),
                           chartid="doughnut")
  return(doughnut)
})

output$DAY <- renderGvis({
  if (is.null(ready())) return(NULL)
  
  dat <- ddply (all(), "day", summarize, count = length(Is.Cancelled))
  #dat <- dat[order(as.Date(dat$day, format="%d"),]
  dat$day <- as.character(dat$day)
  
  doughnut <- gvisPieChart(dat, 
                           options=list(
                             width=300,
                             height=300,
                             title='Day Breakdown',
                             legend='none',
                             pieSliceText='label',
                             pieHole=0.35),
                           chartid="doughnut2")
  
})

output$dayPlot <- renderGvis({
  if (is.null(ready())) return(NULL)
  
  sumBookings <- sumDate()
  
  timeMax <- range()[2]
  timeMin <- range()[1]
  allDates  <- seq(timeMin,timeMax,by="day")
  
  allDates.frame <- data.frame(list(date=allDates))
  
  mergeDates <- merge(allDates.frame,sumBookings,all=T)
  
  mergeDates$bookings[which(is.na(mergeDates$bookings))] <- 0
  mergeDates$netRevenue[which(is.na(mergeDates$netRevenue))] <- 0
  
  df <- mergeDates
  
  
  Combo <- gvisComboChart(df, xvar="date",
                          yvar=c("netRevenue", "bookings"),
                          #options=list(seriesType="bars",series='{1: {type:"line"}}')
                          options=list(series="[{
                                       type:'line', targetAxisIndex:0
},{
                                       type:'line', targetAxisIndex:1
}]",
                                       vAxes="[{title:'net revenue in GBP'}, {title:'number of bookings'}]",
                                       title= "daily bookings and net revenue",
                                       width = 800
                          )
  )
})


#SUMMARY TEXT#
#show selected date range in title
output$textDates  <- renderText({
  paste("showing summary for",range()[1]," to ",range()[2])
})

#BOXED NUMBERS
#bookings
output$W  <- renderText({
  if(is.null(ready())) return(NULL)
  
  df <- sumDate()
  sum(df$bookings)
})
#bags
output$X  <- renderText({
  if(is.null(ready())) return(NULL)
  
  df <- sumDate()
  sum(df$totalBags)
})
#net revenue
output$Y  <- renderText({
  if(is.null(ready())) return(NULL)
  
  df <- sumDate()
  paste(round(sum(df$netRevenue)),"GBP")
})
#avg bags
output$mX  <- renderText({
  if(is.null(ready())) return(NULL)
  
  df <- sumDate()
  round(sum(df$totalBags)/sum(df$bookings), digits = 1)
})
#net revenue
output$mY  <- renderText({
  if(is.null(ready())) return(NULL)
  
  df <- sumDate()
  paste(round(sum(df$netRevenue)/sum(df$bookings), digits = 2),"GBP")
})

#MAP
output$mapContainer <- renderMap({
  if (is.null(ready())) return(NULL)
  
  if(input$showMap == 0)
    return(NULL)
  
  bookings <- bookingsRange()
  
  bookings <- head(bookings, n= 20)
  
  # creating a sample data.frame with your lat/lon points
  df  <- geocode(bookings$to)
  
  #create leaflet map
  map <- Leaflet$new()
  map$setView(c(51.507341, -0.127680), zoom = 10)
  map$tileLayer(provider = 'Stamen.TonerLite')
  #other interesting layers: Stamen.Watercolor, Esri.WorldStreetMap, Esri.NatGeoWorldMap
  #see all available tile layers @ http://leaflet-extras.github.io/leaflet-providers/preview/
  #loop through postcode markers
  for(i in 1:dim(df)[1]){
    map$marker(c(df[i,2],df[i,1]), bindPopup = paste(h3(bookings$to[i]), 
                                                     as.character(bookings$Outward.Journey.Deliver.To.Location.Name[i]),
                                                     "<br>", as.character(em("items:")), as.character(bookings$Total.Luggage.Count[i]),
                                                     "<br>", as.character(em("delivery date:")), as.character(bookings$date[i]),
                                                     "<br>", as.character(em("delivery time:")), as.character(bookings$Outward.Journey.Delivery.Time[i]),
                                                     "<br>", as.character(em("booking value")), as.character(bookings$Booking.Value[i]), "GBP"
    ))
  }
  
  map
})

#TABLE
output$contents  <- renderDataTable({
  if (is.null(ready())) return(NULL)
  
  sumBookings  <- sumDate()
  #manipulating for prettyness
  sumBookings$Outward.Journey.Delivery.Date <- NULL
  sumBookings$meanBags  <- round(sumBookings$meanBags,1)
  sumBookings$netRevenue <- round(sumBookings$netRevenue,2)
  sumBookings$meanNetRevenue <- round(sumBookings$meanNetRevenue,2)

  sumBookings
  
  })
})