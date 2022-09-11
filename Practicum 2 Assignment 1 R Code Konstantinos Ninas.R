setwd("C:\\Users\\ninas\\R\\RPackages")

.libPaths('C:\\Users\\ninas\\R\\RPackages')

#import of libraries
library(ggplot2)

#read the csv files containing information about flights in 1998-1999
flights_1998 <- read.csv("C:\\Users\\ninas\\OneDrive\\Desktop\\MSc Business Analytics\\3rd Quarter\\Analytics Practicum 2\\Visual Analytics\\Assignment 1\\1998.csv.bz2")
flights_1999 <- read.csv("C:\\Users\\ninas\\OneDrive\\Desktop\\MSc Business Analytics\\3rd Quarter\\Analytics Practicum 2\\Visual Analytics\\Assignment 1\\1999.csv.bz2")

summary(flights_1998)
#replace the numbers in the month variable with the month names
flights_1998$Month <- factor(flights_1998$Month, levels = c(1:12),labels = c("January", "February", "March"
                                                       , "April", "May", "June", "July"
                                                       ,"August", "September", "October"
                                                       ,"November", "December"))

#replace the numbers in the day of week variable with the day names
flights_1998$DayOfWeek <- factor(flights_1998$DayOfWeek, levels = c(1:7), labels = c('Monday', 'Tuesday', 'Wednesday'
                                                           , 'Thursday', 'Friday', 'Saturday','Sunday'))

#update the data type of the flight number,unique carrier, tail number, origin 
#destination indexes to factors
flights_1998$FlightNum <- factor(flights_1998$FlightNum)
flights_1998$UniqueCarrier <- factor(flights_1998$UniqueCarrier)
flights_1998$TailNum <- factor(flights_1998$TailNum)
flights_1998$Origin <- factor(flights_1998$Origin)
flights_1998$Dest <- factor(flights_1998$Dest)

#update the cancellation and the diverted index to binary factors
flights_1998$Cancelled <- factor(flights_1998$Cancelled, levels = c(0,1), labels = c("No","Yes"))
flights_1998$Diverted <- factor(flights_1998$Diverted, levels = c(0,1), labels = c("No","Yes"))

#function to keep n characters from the right end of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#create a time fill that will be pasted to the left end of each cell on all time columns
time_fill <- paste(replicate(4, 0), collapse = "")
#we will paste the time fill and keep the 4 right digits to find the time of each cell
flights_1998$DepTime <- factor(substrRight(paste(time_fill, flights_1998$DepTime, sep = ""), 4))  
flights_1998$DepTime[which(flights_1998$DepTime == "00NA")] <- NA

flights_1998$CRSDepTime <- factor(substrRight(paste(time_fill, flights_1998$CRSDepTime, sep = ""), 4))  
flights_1998$CRSDepTime[which(flights_1998$CRSDepTime == "00NA")] <- NA

flights_1998$ArrTime <- factor(substrRight(paste(time_fill, flights_1998$ArrTime, sep = ""), 4))  
flights_1998$ArrTime[which(flights_1998$ArrTime == "00NA")] <- NA

flights_1998$CRSArrTime <- factor(substrRight(paste(time_fill, flights_1998$CRSArrTime, sep = ""), 4))  
flights_1998$CRSArrTime[which(flights_1998$CRSArrTime == "00NA")] <- NA

#all NA
unique(flights_1998$CancellationCode)
unique(flights_1998$CarrierDelay)
unique(flights_1998$WeatherDelay)
unique(flights_1998$NASDelay)
unique(flights_1998$SecurityDelay)
unique(flights_1998$LateAircraftDelay)


#for the 1999 dataset
summary(flights_1999)
#since both datasets have identical issues, we will conduct the same actions for 1999 dataset

#replace the numbers in the month variable with the month names
flights_1999$Month <- factor(flights_1999$Month, levels = c(1:12),labels = c("January", "February", "March"
                                                                             , "April", "May", "June", "July"
                                                                             ,"August", "September", "October"
                                                                             ,"November", "December"))

#replace the numbers in the day of week variable with the day names
flights_1999$DayOfWeek <- factor(flights_1999$DayOfWeek, levels = c(1:7), labels = c('Monday', 'Tuesday', 'Wednesday'
                                                                                     , 'Thursday', 'Friday', 'Saturday','Sunday'))

#update the data type of the flight number,unique carrier, tail number, origin 
#destination indexes to factors
flights_1999$FlightNum <- factor(flights_1999$FlightNum)
flights_1999$UniqueCarrier <- factor(flights_1999$UniqueCarrier)
flights_1999$TailNum <- factor(flights_1999$TailNum)
flights_1999$Origin <- factor(flights_1999$Origin)
flights_1999$Dest <- factor(flights_1999$Dest)

#update the cancellation and the diverted index to binary factors
flights_1999$Cancelled <- factor(flights_1999$Cancelled, levels = c(0,1), labels = c("No","Yes"))
flights_1999$Diverted <- factor(flights_1999$Diverted, levels = c(0,1), labels = c("No","Yes"))

#we will paste the time fill and keep the 4 right digits to find the time of each cell
flights_1999$DepTime <- factor(substrRight(paste(time_fill, flights_1999$DepTime, sep = ""), 4))  
flights_1999$DepTime[which(flights_1999$DepTime == "00NA")] <- NA

flights_1999$CRSDepTime <- factor(substrRight(paste(time_fill, flights_1999$CRSDepTime, sep = ""), 4))  
flights_1999$CRSDepTime[which(flights_1999$CRSDepTime == "00NA")] <- NA

flights_1999$ArrTime <- factor(substrRight(paste(time_fill, flights_1999$ArrTime, sep = ""), 4))  
flights_1999$ArrTime[which(flights_1999$ArrTime == "00NA")] <- NA

flights_1999$CRSArrTime <- factor(substrRight(paste(time_fill, flights_1999$CRSArrTime, sep = ""), 4))  
flights_1999$CRSArrTime[which(flights_1999$CRSArrTime == "00NA")] <- NA

#all NA
unique(flights_1999$CancellationCode)
unique(flights_1999$CarrierDelay)
unique(flights_1999$WeatherDelay)
unique(flights_1999$NASDelay)
unique(flights_1999$SecurityDelay)
unique(flights_1999$LateAircraftDelay)

#cancellation code, carrier delay, weather delay, NAs delay, security delay and late aircraft delay
#will be removed for both datasets since they're all NA
flights_1998 <- flights_1998[,c(-23, -25, -26, -27, -28, -29)]
flights_1999 <- flights_1999[,c(-23, -25, -26, -27, -28, -29)]


#Plots
#find the number of records per year
n_1998 <- nrow(flights_1998)
n_1999 <- nrow(flights_1999)

#stack the two dataframes to one
library("scales")

unioned_df <- rbind(flights_1998, flights_1999)

rm(flights_1998)
rm(flights_1999)


#Plot 1 - Flights per Weekday in 1998 vs 1999
#create a dataframe that contains the frequency of dayofweek and year combinations
days_of_week_per_year <- as.data.frame(as.matrix(table(unioned_df$DayOfWeek
                               ,unioned_df$Year)))

#give columns proper names
colnames(days_of_week_per_year) <- c('Day_of_Week', 'Year', 'Frequency')


#update the dataframe with their corresponding percentages
days_of_week_per_year[which(days_of_week_per_year$Year==1998),3] <- days_of_week_per_year[which(days_of_week_per_year$Year==1998),3]/n_1998
days_of_week_per_year[which(days_of_week_per_year$Year==1999),3] <- days_of_week_per_year[which(days_of_week_per_year$Year==1999),3]/n_1999


#plot the frequency of flights in each weekday per year
ggplot(days_of_week_per_year, aes(Day_of_Week, Frequency, fill = Year)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("grey25", "dodgerblue1"))   + 
  labs(x = "Day of Week", y = "Flight Frequency"
         , title = "Flights per Weekday in 1998 vs 1999") +
  theme(plot.title = element_text(hjust = 0.5))



#Plot 2 - Average Daily Difference of Predicted vs. Actual Elapsed Time per Year
#identify the average elapsed time diff per year and day of month
Yearly_elapsedDiff <- aggregate(ElapsedTimeDiff ~ factor(DayofMonth,levels = c(1:31)) + Year, data=unioned_df, FUN=mean)
#update the colnames
colnames(Yearly_elapsedDiff) <- c("DayofMonth","Year","ElapsedTimeDiff")
#find the average difference of actual vs predicted elapsed time throughout 1998 and 1999
avg_elapsedTimeDiff <- mean(unioned_df$ElapsedTimeDiff[which(is.na(unioned_df$ElapsedTimeDiff)==FALSE)])


#plot two lineplots that show the elapsed time throughout the days of the months for each year against the global avearge of both years
ggplot(Yearly_elapsedDiff, aes(y = ElapsedTimeDiff, x = as.numeric(DayofMonth), group = Year, colour = factor(Year))) + 
  geom_line() + 
  scale_color_manual(name = "Year",values=c("grey25","dodgerblue1")) +
  labs(title = "Average Daily Difference of Predicted vs. Actual Elapsed Time per Year"
       , y = "Minutes"
       , x = "Day of Month") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept=avg_elapsedTimeDiff)+ 
  facet_grid(cols = vars(Year))



#Plot 3 - Cancellations per Year
mosaicplot(table(unioned_df$Cancelled,unioned_df$Year), color = c("grey25", "dodgerblue1"),
           main = "Cancellations per Year", sub = "Flight Cancellation Index", ylab = "Year",
           cex.axis = 1.1)


#Plot 4 - Monthly Wheels Down and Arrival Time at the Destination Airport per Year
TaxiIn_overTime <- aggregate(TaxiIn ~ Month + Year, data=unioned_df, FUN=mean)


ggplot(TaxiIn_overTime, aes(y = TaxiIn, x = as.numeric(factor(Month, levels = c("January", "February", "March"
                                                                , "April", "May", "June", "July"
                                                                ,"August", "September", "October"
                                                                ,"November", "December")
                                              , labels = c(1:12)))
                       , color = factor(Year))) + 
  geom_line() + 
  scale_color_manual(values = c("grey25", "dodgerblue1")) + 
  labs(title = "Monthly Wheels Down and Arrival Time at the Destination Airport per Year"
       , y = "Minutes"
       , x = "Month") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_x_continuous(breaks = c(1:12),
                     labels = c("January", "February", "March"
                                , "April", "May", "June", "July"
                                ,"August", "September", "October"
                                ,"November", "December"))

#Plot 5 - Top 10 Flight Destinations per Year
destinations_per_year <- as.data.frame(as.matrix(table(unioned_df$Dest
                                                       ,unioned_df$Year)))

colnames(destinations_per_year) <- c('Destination', 'Year', 'Frequency')

destinations_per_year <- destinations_per_year[order(destinations_per_year$Frequency, decreasing = TRUE), ] 
destinations_per_year <- Reduce(rbind,by(destinations_per_year,destinations_per_year["Year"], head, n = 10))

#sort the destination's factor levels based on their frequency and give them proper labels
destinations_per_year$Destination <- factor(destinations_per_year$Destination,levels = unique(destinations_per_year$Destination[order(destinations_per_year$Frequency, decreasing = F)])
                                            ,labels = c("Chicago O'Hare International Airport", "Atlanta Airport", "Dallas Fort Worth International Airport"
                                                        ,"Los Angeles International Airport", "St. Louis Lambert International Airport"
                                                        ,"Phoenix Sky Harbor Airport", "Detroit Metro Airport", "San Francisco International Airport"
                                                        ,"Minneapolis-Saint Paul International Airport", "Denver International Airport"))


#plot the relative proportion of the destinations per year
ggplot(destinations_per_year, aes(Destination, Frequency, fill = Year)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("grey25", "dodgerblue1"))   + 
  labs(x = "Destination", y = "Flight Frequency"
       , title = "Top 10 Flight Destinations per Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_y_continuous(labels=comma) 


#Plot 6 - Unique Carriers in 1998
carriers_1998 <- as.data.frame(table(unioned_df[which(unioned_df$Year==1998),9]))

colnames(carriers_1998) <- c('Carrier_Name', 'Frequency')
carriers_1998 <- carriers_1998[order(carriers_1998$Frequency, decreasing = TRUE), ]

#sort the unique carrier's factor levels based on their frequency and give them proper labels
carriers_1998$Carrier_Name <- factor(carriers_1998$Carrier_Name, levels = unique(carriers_1998$Carrier_Name[order(carriers_1998$Frequency, decreasing = F)]),
                                     labels = c("Delta Air Lines, Inc.", "Southwest Airlines "
                                     , "United Airlines, Inc.", "US Airways","American Airlines"
                                     ,"Northwest Airlines","Continental Airlines","Trans World Airlines"
                                     ,"America West Airlines","Alaska Airlines"))

#plot the unique carrier names against their frequency in a lollipop plot
ggplot(carriers_1998, aes(x=Carrier_Name, y=Frequency)) +
  geom_segment( aes(x=Carrier_Name, xend=Carrier_Name, y=0, yend=Frequency)) +
  geom_point( size=5, color="blue", fill=alpha("skyblue", 0.3), alpha=0.7, shape=21, stroke=2)+
  coord_flip() + 
  labs(y = "Number of Flights", x = "Flight Unique Carrier"
       , title = "Unique Carriers in 1998") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=comma) 


#Plot 7 - Delays in Flights arrivals based on their Distance in 1999
library('hexbin')
flights_diverted_per_month <- as.data.frame(as.matrix(table(unioned_df[which(unioned_df$Year==1998),2]
                                                       ,unioned_df[which(unioned_df$Year==1998),23])))
colnames(flights_diverted_per_month) <- c('Month', 'Diverted', 'Frequency')

#plot the distance against the arrival delays in a hexplot
ggplot(unioned_df[which(unioned_df$Year==1999),], aes(Distance, ArrDelay)) +
   geom_hex(bins = 20) +
  scale_y_continuous(labels=comma) +
  scale_x_continuous(labels=comma) + 
  labs(y = "Minutes", x = "Distance between Airports in Miles"
       , title = "Delays in Flight Arrivals based on their Distance in 1999") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))


#Plot 8 - Top 5 Airports with Departure Delays
depdelays_per_origin <- aggregate(DepDelay ~ Origin, data=unioned_df[which(unioned_df$Year==1999),], FUN=mean)
depdelays_per_origin <- depdelays_per_origin[order(depdelays_per_origin[,2], decreasing = T),]
depdelays_per_origin <- depdelays_per_origin[1:5,]
depdelays_per_origin$Origin <- factor(depdelays_per_origin$Origin, levels = depdelays_per_origin$Origin
                                      ,labels = c("Glacier Park International Airport", "Petersburg James A. Johnson Airport"
                                                  ,"Gustavus Airport", "Wrangell Airport", "Santa Barbara Airport"))

#plot the top 5 origin airports against the average departure delays in a lollipop plot
ggplot(depdelays_per_origin, aes(x=Origin, y=DepDelay)) +
  geom_segment( aes(x=Origin, xend=Origin, y=0, yend=DepDelay)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
  labs(y = "Average Delay in Minutes", x = "Origin Airports"
       , title = "Top 5 Airports with Departure Delays in 1999") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=comma) 


#Plot 9 - Hourly Flight Departures
unioned_df$Hour <- substr(unioned_df$DepTime, start = 1, stop = 2)
unioned_df[which(unioned_df$Hour=="24"),24] <- "00"
unioned_df$Hour <- factor(unioned_df$Hour, levels = c("00","01","02","03","04","05","06"
                                                      ,"07","08","09","10","11","12","13"
                                                      ,"14","15","16","17","18","19","20"
                                                      ,"21","22","23"))

dep_times <- as.data.frame(table(unioned_df[which(unioned_df$Year==1998 & is.na(unioned_df$Hour)==F) 
                                            ,24]))
colnames(dep_times) <- c("Hour", "Frequency")


ggplot(dep_times, aes(x = Hour, y = Frequency)) +
  geom_col(position = "dodge", fill = "lightblue") +
  coord_polar() + 
  labs(y = "Frequency", x = "Hour"
       , title = "Hourly Flight Departures in 1998") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=comma, limits = c(-17, max(dep_times$Frequency)))

#Plot 10 - Flights Diverted per Distance in 1999
ggplot(unioned_df[which(unioned_df$Year==1999),], aes(x = Distance, fill = Diverted)) + 
  geom_density(alpha = 0.4) +
  labs(title = "Salary distribution by rank") +
  labs(y = "Density", x = "Distance between Airports in Miles"
       , title = "Flights Diverted per Distance in 1999") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels=comma, limits = c(0,3000)) 


#Plot 11 - Average Time elapsed between Departure and Wheels off in Minutes per Diverted Index in 1999
#create a dataframe with the average taxiout times grouped by the diverted index and the day of week
taxiout_per_diverted_and_weekday <- aggregate(TaxiOut ~ DayOfWeek + Diverted, data=unioned_df[which(unioned_df$Year==1999),], FUN=mean)

#create an empty matrix that will be populated with the average taxiout times
taxiout_matrix <- matrix(data=NA, ncol= 7, nrow = 2)

#rename the columns and the rows
colnames(taxiout_matrix) <- c('Monday', 'Tuesday', 'Wednesday'
                              , 'Thursday', 'Friday', 'Saturday','Sunday')
rownames(taxiout_matrix) <- c('No','Yes')

#populate the empty matrix with a for loop that matches the rows and the columns of the matrix with the dataframe
for(i in 1:nrow(taxiout_per_diverted_and_weekday)){taxiout_matrix[rownames(taxiout_matrix)==taxiout_per_diverted_and_weekday[i,2],colnames(taxiout_matrix)==taxiout_per_diverted_and_weekday[i,1]] <- taxiout_per_diverted_and_weekday[i,3]}

#add two rows - one that contains the maximum values and one that contains 0
taxiout_matrix <- rbind(rep(max(taxiout_matrix),7),rep(0,7), taxiout_matrix)
taxiout_matrix <- as.data.frame(taxiout_matrix)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( taxiout_matrix  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,round(max(taxiout_matrix),0),length.out=5), cglwd=0.8,
            #custom labels
            vlcex=0.8 ,
            #title
            title = "Average Time elapsed between Departure and Wheels off in Minutes per Diverted Index in 1999"
)

# Add a legend
legend(x=1.2, y=1, legend = rownames(taxiout_matrix[-c(1,2),]), title = "Diverted"
       , bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)



