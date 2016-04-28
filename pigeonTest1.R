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


# getting the map
mapNYC<- get_map(location = 
                   c(lon = mean(df$Longitude), 
                     lat = mean(df$Latitude)), 
                     zoom = 11,
                     maptype = "roadmap", 
                     scale = 2)

# plotting the map with some points on it
ggmap(mapNYC) +
  geom_point(data = df, 
             aes(x = lon, y = lat, fill = "red", alpha = 0.8), 
             size = 2, shape = 21) +
              guides(fill=FALSE, alpha=FALSE, size=FALSE)

View(head(pigeonComplaints))

monthCounts <- table(pigeonComplaints$Month)
monthCounts <- as.data.frame(monthCounts)
m <- c(4,8,12,2,1,7,6,3,5,11,10,9)
m <- as.data.frame(m)
monthCounts$monthNum <- m
monthCounts <- monthCounts[order(m),]
colnames(monthCounts) <- c("Month","Calls","m")

#Plots Pigeon calls by month
barplot(monthCounts[,2], main="Pigeon Calls by Month", 
        xlab="Month",names.arg = monthCounts[,1])

monthYearCounts <- table(pigeonComplaints$Year,pigeonComplaints$Month)
monthYearCounts <- monthYearCounts[,c(5,4,8,1,9,7,6,2,12,11,10,3)]


pigeonColors <- c("indianred3", 
                  "gray0", 
                  "gray", 
                  "darkorchid4", 
                  "seagreen4",
                  "cadetblue", 
                  "orangered1")

# plots pigeon calls by year
barplot(monthYearCounts, 
        main="Pigeon Complaints by Month and Year",
        xlab="Month",
        col = pigeonColors,
        legend.text = rownames(monthYearCounts),
        args.legend =  list(x="topright")
        )



p2010 = c(monthYearCounts[1,])
p2011 = c(monthYearCounts[2,])
p2012 = c(monthYearCounts[3,])
p2013 = c(monthYearCounts[4,])
p2014 = c(monthYearCounts[5,])
p2015 = c(monthYearCounts[6,])
p2016 = c(monthYearCounts[7,1:3],NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)

g_range <- range(0, p2010,p2011,p2012,p2013,p2014,p2015,p2016)

plot(p2010,
     type='l',ylab="Pigeon Complaints",xlab = "Month",ylim = g_range,
     col = "red", axes=FALSE,ann = FALSE,lwd=2)
axis(1, at=1:12,lab = monthCounts[,1])
axis(2,las =1, at=10*0:g_range[2])
lines(p2011, type="l", col="orange",lwd=2)
lines(p2012, type='l', col="black",lwd=2)
lines(p2013, type="l", col="grey",lwd=2)
lines(p2014, type='l', col="green",lwd=2)
lines(p2015, type="l", col="blue",lwd=2)
lines(p2016, type='l', col="pink",lwd=2)
title(main="Pigeon Complaints by Month and Year", col.main="Black")
legend(10.5, g_range[2], c(2010,2011,2012,2013,2014,2015,2016), cex=0.8, 
       col=c("red","orange","black","grey","green","blue","pink"), 
       lty=1,lwd=2)

yearSum <- summary(as.factor(pigeonComplaints$Year))
barplot(yearSum, 
        ylim = c(0,800), 
        col = pigeonColors,
        xlab = "Year",
        ylab = "Complaints",
        las = 1,
        main = "Pigeon Complains by Year"
        )
