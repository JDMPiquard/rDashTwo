require(shiny)
require(rCharts)
require(plyr)
require(lubridate)
require(ggmap)
require(ggplot2)
options(RCHART_WIDTH = 600)

shinyServer(function(input,output){

  ##TAKING CARE OF INPUTS AND MAKING THEM "GLOBAL"#############
  #store dates within: range()
  range  <-  reactive({
    input$dates
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
    #Cleaning up postCodes
    bookings$from <- as.character(bookings$Outward.Journey.Collect.From.Location.Postcode)
    bookings$to <- as.character(bookings$Outward.Journey.Deliver.To.Location.Postcode)
    
    bookings
  })
  
  sumMonth <- reactive({
    sumBookings <- ddply (all(), c("month","year"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
    sumBookings$meanNetRevenue <- sumBookings$netRevenue/sumBookings$bookings
    sumBookings$monthName  <- month.abb[sumBookings$month] #getting the month name fo plotting purposes
    sumBookings  <- within(sumBookings, cum  <- cumsum(netRevenue)) #calculating cummulatives
    sumBookings[order(c(sumBookings$year,sumBookings$month)),]
    sumBookings
  })
  
  bookingsRange  <- reactive({
    bookings  <- all()
    subset(bookings, date>=range()[1]&date<=range()[2])
  })
  
  sumDate <- reactive({
    sumBookings <- ddply (bookingsRange(), c("date","Outward.Journey.Delivery.Date"), summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = mean(Total.Luggage.Count), netRevenue = sum(Booking.Value)/1.2)
    sumBookings$meanNetRevenue <- sumBookings$netRevenue/sumBookings$bookings
    sumBookings$day  <- weekdays(as.Date(sumBookings$Outward.Journey.Delivery.Date, format = "%d/%m/%Y"))
    sumBookings[c("date","Outward.Journey.Delivery.Date","day","bookings","totalBags","meanBags","netRevenue","meanNetRevenue")]
    sumBookings[order(sumBookings$date, decreasing = TRUE),]
  })
  
  ##OUTPUTS###################################################
  #CHARTS
  output$chart1 <- renderChart({
    sumBookings <- sumMonth()
    #generating dataframe to plot
    df1  <- data.frame(sumBookings$month, sumBookings$monthName, sumBookings$cum)
    names(df1)  <- c( "month","monthName","netRevenue")
    df1$type <- "cum"
    
    df2  <- data.frame(sumBookings$month, sumBookings$monthName, sumBookings$netRevenue)
    names(df2)  <- c( "month","monthName","netRevenue")
    df2$type <- "month"
    
    df  <- rbind(df1,df2)
    
    ##dude works
    dude  <- rPlot(x = list(var = "monthName", sort = ("year")), y = "netRevenue", data = sumMonth(), type = 'bar')
    dude$addParams(height = 300, dom = 'chart1', title = "All time performance by month")
    return(dude)
  })
  
  #show selected date range in title
  output$textDates  <- renderText({
    paste("showing summary for",range()[1]," to ",range()[2])
  })
  #BOXED NUMBERS
  #bookings
  output$W  <- renderText({
    df <- sumDate()
    if(is.null(df))
      return(NULL)
    
    toString(sum(df$bookings))
  })
  #bags
  output$X  <- renderText({
    df <- sumDate()
    if(is.null(df))
      return(NULL)
    
    toString(sum(df$totalBags))
  })
  #net revenue
  output$Y  <- renderText({
    df <- sumDate()
    if(is.null(df))
      return(NULL)
    
    paste(round(sum(df$netRevenue)),"GBP")
  })
  #avg bags
  output$mX  <- renderText({
    df <- sumDate()
    if(is.null(df))
      return(NULL)
    
    round(sum(df$totalBags)/sum(df$bookings), digits = 1)
  })
  #net revenue
  output$mY  <- renderText({
    df <- sumDate()
    if(is.null(df))
      return(NULL)
    
    paste(round(sum(df$netRevenue)/sum(df$bookings), digits = 2),"GBP")
  })
  
  #MAP
  output$mapContainer <- renderMap({
    
    if(input$showMap == 0)
      return(NULL)
    
    bookings <- bookingsRange()
    
    bookings <- head(bookings, n= 20)
    
    # creating a sample data.frame with your lat/lon points
    df  <- geocode(bookings$to)
    
    #create leaflet map
    map <- Leaflet$new()
    map$setView(c(mean(df$lat), mean(df$lon)), zoom = 12)
    map$tileLayer(provider = 'Stamen.Watercolor')
    #loop through postcode markers
    for(i in 1:dim(df)[1]){
      map$marker(c(df[i,2],df[i,1]), bindPopup = paste(h3(bookings$to[i]), 
                                                       as.character(bookings$Outward.Journey.Deliver.To.Location.Name[i]),
                                                       "<br>", as.character(em("items:")), as.character(bookings$Total.Luggage.Count[i]),
                                                       "<br>", as.character(em("delivery time:")), as.character(bookings$Outward.Journey.Delivery.Time[i]),
                                                       "<br>", as.character(em("booking value")), as.character(bookings$Booking.Value[i]), "GBP"
      ))
    }
    
    map
  })
  
  #TABLE
  output$contents  <- renderTable({
    
    if (is.null(sumDate()))
      return(NULL)
    
    sumDate()
  })
})