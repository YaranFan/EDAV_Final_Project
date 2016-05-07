## Here we will attempt to plot some pigeon data in NYC

# loading the required packages
library(ggplot2)
library(ggmap)

# creating a sample data.frame with your lat/lon points
anSub <- data.frame(read.csv('animialSubset.csv'))

pigeonComplaints <- 
  subset(x = anSub,
         subset = anSub$Complaint.Type == 
           'Unsanitary Pigeon Condition')

## Cleans out Latitude "N/A" values
pigeonComplaints <- 
  subset(x = pigeonComplaints,
         subset = !is.na(pigeonComplaints$Latitude))


lon <- pigeonComplaints[11]
lat <- pigeonComplaints[10]
df <- as.data.frame(cbind(lon,lat))

monthCounts <- table(pigeonComplaints$Month)
monthCounts <- as.data.frame(monthCounts)
m <- c(4,8,12,2,1,7,6,3,5,11,10,9)
m <- as.data.frame(m)
monthCounts$monthNum <- m
monthCounts <- monthCounts[order(m),]
colnames(monthCounts) <- c("Month","Calls","m")


#Here we plot pigeon-related call volume by month and year 
monthYearCounts <- table(pigeonComplaints$Year,pigeonComplaints$Month)
monthYearCounts <- monthYearCounts[,c(5,4,8,1,9,7,6,2,12,11,10,3)]

#I tried to mimic the typical coloring of a 
#NYC pigeon using this color scheme
pigeonColors <- c("indianred3", 
                  "gray0", 
                  "gray", 
                  "darkorchid4", 
                  "seagreen4",
                  "cadetblue", 
                  "orangered1")

# plots pigeon calls by month and year
barplot(monthYearCounts, 
        main="Pigeon Complaints by Month and Year",
        xlab="Month",
        col = pigeonColors,
        legend.text = rownames(monthYearCounts),
        args.legend =  list(x="topright")
        )

# plots pigeon calls grouped by year
yearSum <- summary(as.factor(pigeonComplaints$Year))
barplot(yearSum, 
        ylim = c(0,800), 
        col = pigeonColors,
        xlab = "Year",
        ylab = "Complaints",
        las = 1,
        main = "Pigeon Complaints by Year"
        )
