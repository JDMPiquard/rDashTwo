require(rCharts)
require(plyr)
require(lubridate)

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
  
  #TABLE
  output$contents  <- renderTable({
    
    if (is.null(sumDate()))
      return(NULL)
    
    sumDate()
  })
})