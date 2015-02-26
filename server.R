require(shiny)
require(rCharts)
require(plyr)
require(lubridate)
require(ggmap)
require(ggplot2)
require(googleVis)

shinyServer(function(input,output, clientData, session){
  
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
  
  
  filter <- reactive({
    vec <- input$checkAirport
    vec <- gsub("LHR","Heathrow",vec)
    vec <- gsub("LGW","Gatwick",vec)
    vec <- gsub("STN","Stansted",vec)
    vec <- gsub("Storage","Storage",vec)
    vec <- gsub("LCY","London City Airport",vec)
    vec
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
    
    #FILTERING
    bookings$filter  <- grepl(paste(filter(),collapse="|"),bookings$Outward.Journey.Collect.From.Location.Name,ignore.case=TRUE)|grepl(paste(filter(),collapse="|"),bookings$Outward.Journey.Deliver.To.Location.Name,ignore.case=TRUE)
    bookings <- bookings[bookings$filter == 1,]
    
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
                             width=230,
                             height=230,
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
                             width=230,
                             height=230,
                             title='Day Breakdown',
                             legend='none',
                             pieSliceText='label',
                             pieHole=0.35),
                           chartid="doughnut2")
  
})

#######TAB1

##Detailed Day Plot########################################################

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
  paste("Summary for",range()[1]," to ",range()[2])
})
#show selected airports
output$selectedAirports <- renderText({
  paste("showing: ",paste(filter(),collapse=", "))
})

##BOXED NUMBERS: TAB 1######################################################
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
#ne
output$preBook <- renderText({
  if(is.null(ready())) return(NULL)
  
  df <- all()
  preBook <- round(sum(grepl("prebook",df$Department))/length(df$Department), digits=2)*100
  paste(preBook,"%")
})

##MAP: TAB 1#################################################################################
output$mapContainer <- renderMap({
  if (is.null(ready())) return(NULL)
  
  if(input$showMap == F)
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

##GENERATING TABLES!!##############################################

#SUMMARY TABLE (TAB 1: SUMMARY)
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

########TAB2

#PIE CHARTS

#Sex
output$sex <- renderGvis({
  if (is.null(ready())) return(NULL)
  
  tit <- ddply (all(), "Title", summarize, count = length(Is.Cancelled))
  tit$Title  <- as.character(tit$Title)
  
  doughnut <- gvisPieChart(tit, 
                           options=list(
                             width=230,
                             height=230,
                             title='Sex (using title)',
                             legend='none',
                             pieSliceText='label',
                             pieHole=0.5),
                           chartid="doughnutSex")
  return(doughnut)
})

#Luggage Type
output$luggage <- renderGvis({
  if (is.null(ready())) return(NULL)
  
  allData <- all()
  
  lug <- as.data.frame(matrix(ncol=2,nrow=2))
  lug[1,1] <- "Hand" 
  lug[1,2]  <- sum(allData$Hand.Luggage.Count)
  lug[2,1] <- "Hold"
  lug[2,2]  <- sum(allData$Hold.Luggage.Count)
  
  doughnut <- gvisPieChart(lug, 
                           options=list(
                             width=230,
                             height=230,
                             title='Luggage type',
                             legend='none',
                             pieSliceText='label',
                             pieHole=0.5),
                           chartid="doughnutLug")
  return(doughnut)
})

#Journey Type
output$journey <- renderGvis({
  if (is.null(ready())) return(NULL)
    
  tit <- ddply (all(), "Journey.Mode", summarize, count = length(Is.Cancelled))
  tit$Journey.Mode  <- as.character(tit$Journey.Mode)
  
  doughnut <- gvisPieChart(tit, 
                           options=list(
                             width=230,
                             height=230,
                             title='Journey type',
                             legend='none',
                             pieSliceText='label',
                             pieHole=0.5),
                           chartid="doughnutJourney")
  return(doughnut)
})

#Delivery Type
output$type <- renderGvis({
  if (is.null(ready())) return(NULL)
  
  allData <- all()
  
  lug <- as.data.frame(matrix(ncol=2,nrow=3))
  lug[1,1] <- "Residential" 
  lug[1,2]  <- sum(allData$Location.Organisation=="Residential")
  lug[2,1] <- "Airport"
  lug[2,2]  <- sum(allData$Journey.Direction=="AirportToAirport")
  lug[3,1] <- "Non-Residential"
  lug[3,2] <- length(allData$Is.Cancelled) - lug[2,2] - lug[1,2]
  
  doughnut <- gvisPieChart(lug, 
                           options=list(
                             width=230,
                             height=230,
                             title='Deliveries by type (Airport is faulty)',
                             legend='none',
                             pieSliceText='label',
                             pieHole=0.5),
                           chartid="doughnutType")
  return(doughnut)
})

##DELIVERIES/COLLECTIONS BY HOUR############################
output$hourPlot <- renderGvis({
  if (is.null(ready())) return(NULL)
  
  allData <- all()
  
  allData$Collection.Slot  <- strftime(strptime(allData$Outward.Journey.Collection.Time, format="%H:%M"), "%H")
  allData$Delivery.Slot  <- strftime(strptime(allData$Outward.Journey.Delivery.Time, format="%H:%M"), "%H")
  allData$Created.Time  <- strftime(strptime(allData$Created.Time, format="%H:%M"), "%H")
  
  time1.df <- ddply (allData, c("Collection.Slot"), summarize, collections = length(Is.Cancelled))
  #time1.df$collectionsPCT <- time1.df/length(allData$Is.Cancelled)*100
  names(time1.df)[1] <- "time"
  time2.df <- ddply (allData, c("Delivery.Slot"), summarize, deliveries = length(Is.Cancelled))
  #time2.df$collectionsPCT <- time2.df/length(allData$Is.Cancelled)*100
  names(time2.df)[1] <- "time"
  time3.df <- ddply (allData, c("Created.Time"), summarize, created = length(Is.Cancelled))
  #time2.df$collectionsPCT <- time2.df/length(allData$Is.Cancelled)*100
  names(time3.df)[1] <- "time"
  
  timeMin <- strptime("00:00", format="%H")
  timeMax <- strptime("23:00", format="%H")
  allTime  <- strftime(seq(timeMin,timeMax,by="hour"), "%H")
  
  allTime.df <- data.frame(list(time=allTime))
  
  Times <- merge(allTime.df,time1.df, all=T)
  Times <- merge(Times,time2.df, all=T)
  Times <- merge(Times,time3.df, all=T)
  Times$collections[which(is.na(Times$collections))] <- 0
  Times$deliveries[which(is.na(Times$deliveries))] <- 0
  Times$created[which(is.na(Times$created))] <- 0
  Times$time <- strftime(strptime(Times$time, format="%H"), "%H:%M")
  
  gvisLineChart(Times, options=list(
                        title='Deliveries, collections and bookings created by hour slot', height=400, width=960))
  
})


#NATIONALITIES TABLE (TAB 2: STATISTICS)

output$nation  <- renderDataTable({
  if (is.null(ready())) return(NULL)
  
  allData <- all()
  
  nat.df <- ddply (allData, "Country", summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = round(mean(Total.Luggage.Count),digits=1), netRevenue = round(sum(Booking.Value)/1.2))
  nat.df$avgRevenue <- round(nat.df$netRevenue/nat.df$bookings, digits=2)
  nat.df <- nat.df[with(nat.df,order(-bookings,-avgRevenue)), ]
  rownames(nat.df) <- NULL
  
  nat.df
}, options = list(pageLength = 5))

#REPEAT CUSTOMERS TABLE (TAB 2: STATISTICS)
reUser <- reactive({
  allData <- all()
  
  reUser.df <- ddply (allData, "Email.Address", summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = round(mean(Total.Luggage.Count),digits=1), netRevenue = round(sum(Booking.Value)/1.2))
  reUser.df$avgRevenue <- round(reUser.df$netRevenue/reUser.df$bookings, digits=2)
  reUser.df <- reUser.df[with(reUser.df,order(-bookings,-avgRevenue)), ]
  reUser.df <- reUser.df[reUser.df$bookings>1,]
  reUser.df <- reUser.df[- grep("bagstorage@portr.com",reUser.df$Email.Address), ]
  rownames(reUser.df) <- NULL
  
  reUser.df
  
})

output$reUser  <- renderText({
  if (is.null(ready())) return(NULL)
  
  allData <- all()
  
  reUser.df <- reUser()
  
  reUser <- length(reUser.df$bookings)
  reUserpct <- round(reUser/length(allData$Is.Cancelled),digits=3)*100
  paste("Repeat Users: ", reUser, "total, so about ",reUserpct,"%")
  
  })

output$custom <- renderDataTable({
  if (is.null(ready())) return(NULL)
  
  reUser()
  
}, options = list(pageLength = 5))

#TABLE MAIN DELIVERY DESTINATIONS

output$loc <- renderDataTable({
  if (is.null(ready())) return(NULL)
  
  allData <- all()
  
  loc.df <- ddply (allData, "Location.Organisation", summarize, bookings = length(Is.Cancelled), totalBags = sum(Total.Luggage.Count), meanBags = round(mean(Total.Luggage.Count),digits=1), netRevenue = round(sum(Booking.Value)/1.2))
  loc.df$avgRevenue <- round(loc.df$netRevenue/loc.df$bookings, digits=2)
  loc.df <- loc.df[with(loc.df,order(-bookings,-avgRevenue)), ]
  loc.df <- loc.df[loc.df$bookings>1,]
  loc.df <- loc.df[loc.df$netRevenue>0,]
  loc.df <- loc.df[- grep("Residential",loc.df$Location.Organisation), ]
  loc.df <- loc.df[- grep("Airportr",loc.df$Location.Organisation), ]
  rownames(loc.df) <- NULL
  
  loc.df
  
}, options = list(pageLength = 10))


})