#============================#
# Process and Aggregate Data #
#============================# 
library(data.table)

# Read Data
setwd("C:/Users/hongzhili/Desktop/W4701Project")
The311 = readRDS('whole_data.RData')

# Keep certain columns
The311 = The311[,c(2,4:9,25,51,52)]

# Create necessary new variables
The311$counts = 1
The311$Date = as.POSIXlt(as.POSIXct(The311$Created.Date, format = "%m/%d/%Y %I:%M:%OS %p"))
The311$Year = The311$Date$year + 1900
The311$Month = month.abb[The311$Date$mon + 1]
The311$Hour = The311$Date$hour
The311$DayOfWeek = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[The311$Date$wday + 1] 
  # keep the date as string
  The311$Created.Date = substr(The311$Created.Date,1,10) 
  #drop the date-time column
  The311 = The311[,-which(names(The311)=="Date")] 
  # rename the created date
  names(The311)[names(The311)=="Created.Date"] <- "Date" 

# Save
saveRDS(The311, file="Light_Data_311.RData")

# Aggregate to counts (2010-2015) - without ZIP
#The311 = The311[which(The311$Year!=2016),c("Date","Agency", "Incident.Zip", "Year", "Month", "Hour", "DayOfWeek", "counts")] 
#tmp = data.table(The311)
#Agg311 <- as.data.frame(tmp[, sum(counts, na.rm = TRUE),by = list(Date, Agency, Year, Month, Hour, DayOfWeek)])
#names(Agg311)[names(Agg311)=="V1"] <- "Counts" 
#save(Agg311, file = 'Aggregated_Data_311.RData')
#write.csv(Agg311, file = 'yf2362/Aggregated_Data_311.csv')

# Aggregate to counts (2010-2015) - with ZIP
#Agg311_withZIP <- as.data.frame(tmp[, sum(counts, na.rm = TRUE),by = list(Date, Agency, Incident.Zip, Year, Month, Hour, DayOfWeek)])
#names(Agg311_withZIP)[names(Agg311_withZIP)=="V1"] <- "Counts" 
#save(Agg311_withZIP, file = 'Aggregated_Data_311_withZIP.RData')
#write.csv(Agg311_withZIP, file = 'yf2362/Aggregated_Data_311_withZIP.csv')

# Aggregate Animal Related Data for Dashboard
data_animal = read.csv("animialSubset.csv")
tmp = data.table(data_animal)
Agg_animal <- as.data.frame(tmp[, sum(counts, na.rm = TRUE),by = list(Date, Agency, Complaint.Type, Incident.Zip, Year, Month, Hour, DayOfWeek)])
names(Agg_animal)[names(Agg_animal)=="V1"] <- "Counts"
write.csv(Agg_animal, file = 'yf2362/Aggregated_Data_311_withZIP.csv')