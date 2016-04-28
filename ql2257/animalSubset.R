# read in all the 311 Data
data311 <- data.frame(readRDS('Light_Data_311.RData'))
cols311 <- colnames(data311)

## subset all data possibly related to NYC animals
animalFlag1 = grep('animal',data311$Descriptor,ignore.case = TRUE)
animalFlag2 = grep('animal',data311$Complaint.Type,ignore.case = TRUE)
animalsFlag1 = grep('animals',data311$Descriptor,ignore.case = TRUE)
animalsFlag2 = grep('animals',data311$Complaint.Type,ignore.case = TRUE)
dogFlag1 = grep('dog',data311$Descriptor,ignore.case = TRUE)
dogFlag2 = grep('dog',data311$Complaint.Type,ignore.case = TRUE)
dogsFlag1 = grep('dogs',data311$Descriptor,ignore.case = TRUE)
dogsFlag2 = grep('dogs',data311$Complaint.Type,ignore.case = TRUE)
petFlag1 = grep('pet',data311$Descriptor,ignore.case = TRUE)
petFlag2 = grep('pet',data311$Complaint.Type,ignore.case = TRUE)
wildlifeFlag1 = grep('wildlife',data311$Descriptor,ignore.case = TRUE)
wildlifeFlag2 = grep('wildlife',data311$Complaint.Type,ignore.case = TRUE)
infestationFlag1 = grep('infestation',data311$Descriptor,ignore.case = TRUE)
infestationFlag2 = grep('infestation',data311$Complaint.Type,ignore.case = TRUE)
birdFlag1 = grep('bird',data311$Descriptor,ignore.case = TRUE)
birdFlag2 = grep('bird',data311$Complaint.Type,ignore.case = TRUE)
birdsFlag1 = grep('birds',data311$Descriptor,ignore.case = TRUE)
birdsFlag2 = grep('birds',data311$Complaint.Type,ignore.case = TRUE)
pigeonFlag1 = grep('pigeon',data311$Descriptor,ignore.case = TRUE)
pigeonFlag2 = grep('pigeon',data311$Complaint.Type,ignore.case = TRUE)
pigeonsFlag1 = grep('pigeons',data311$Descriptor,ignore.case = TRUE)
pigeonsFlag2 = grep('pigeons',data311$Complaint.Type,ignore.case = TRUE)
pestFlag1 = grep('pest',data311$Descriptor,ignore.case = TRUE)
pestFlag2 = grep('pest',data311$Complaint.Type,ignore.case = TRUE)
beeFlag1 = grep('bee',data311$Descriptor,ignore.case = TRUE)
beeFlag2 = grep('bee',data311$Complaint.Type,ignore.case = TRUE)

#combines all into unique list of records
allAnimals = sort(unique(c(animalFlag1,animalFlag2,animalsFlag1,animalsFlag2,
                           dogFlag1,dogFlag2,dogsFlag1,dogsFlag2,
                           petFlag1,petFlag2,wildlifeFlag1,wildlifeFlag2,
                           infestationFlag1,infestationFlag2,
                           birdFlag1,birdFlag2,birdsFlag1,birdsFlag2,
                           pigeonFlag1,pigeonFlag2,pigeonsFlag1,pigeonsFlag2,
                           pestFlag1,pestFlag2,beeFlag1,beeFlag2)))
#subsets original dataframe
animalSubset311 = data311[allAnimals,]


## use whole_data to do calender plots
library(dplyr)
library(ggplot2)
library(openair)
fulldata311 <- data.frame(readRDS('whole_data.RData'))
animal311 = fulldata311[allAnimals,]
animal311 <- tbl_df(animal311)
animal311$Created.Date <- as.POSIXlt(animal311$Created.Date, "%m/%d/%Y %I:%M:%S %p", tz = "EST")
animal311 <- mutate(animal311, year = animal311$Created.Date$year + 1900) 
animal311 <- mutate(animal311, month = animal311$Created.Date$mon + 1) 
animal311 <- mutate(animal311, day = animal311$Created.Date$mday)
animal311 <- mutate(animal311, wkDay = animal311$Created.Date$wday + 1)
animal311 <- mutate(animal311, hour = animal311$Created.Date$hour)
dataforcal <- select(animal311, Created.Date, Unique.Key)
dataforcal <- mutate(dataforcal, Created.Date = as.character(Created.Date))
dataforcal <- mutate(dataforcal, Created.Date = substr(Created.Date, 0, 10))
dataforcal = group_by(dataforcal, Created.Date) 
dataforcal = summarise(dataforcal, count = n())
names(dataforcal)[1] <- "date"
dataforcal$date <- as.POSIXct(dataforcal$date)

qplot(date, count, data = dataforcal, geom = "smooth", xlab = "Year", ylab = "Counts", main = "Yearly Complaints 2010 - March 2016")

calPlot <- calendarPlot(dataforcal, "count", year = 2015, main = "Animal Complaints by Day in 2015", cols = "increment")

ggplot(animalSubset311, aes(x=reorder(Agency, -table(Agency)[Agency]))) + 
  geom_bar() + xlab("Agency") + ggtitle("Agencies of Animal Complaints") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))


July27Flag1 = grep('2015-07-27', animal311$Created.Date)
July27Subset = animal311[July27Flag1,]
ggplot(July27Subset, aes(x=reorder(Descriptor, -table(Descriptor)[Descriptor]))) + 
  geom_bar() + xlab("Descriptor") + ggtitle("Descriptors of Animal Complaints in July 27th, 2015") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))

Nov13Flag1 = grep('2015-11-13', animal311$Created.Date)
Nov13Subset = animal311[Nov13Flag1,]
ggplot(Nov13Subset, aes(x=reorder(Descriptor, -table(Descriptor)[Descriptor]))) + 
  geom_bar() + xlab("Descriptor") + ggtitle("Descriptors of Animal Complaints in Nov 13th, 2015") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
ggplot(Nov13Subset, aes(x=reorder(Incident.Zip, -table(Incident.Zip)[Incident.Zip]))) + 
  geom_bar() + xlab("Incident.Address") + ggtitle("Descriptors of Animal Complaints in Nov 13th, 2015") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))

Nov12Flag1 = grep('2015-11-12', animal311$Created.Date)
Nov12Subset = animal311[Nov12Flag1,]
ggplot(Nov12Subset, aes(x=reorder(Descriptor, -table(Descriptor)[Descriptor]))) + 
  geom_bar() + xlab("Descriptor") + ggtitle("Descriptors of Animal Complaints in Nov 12th, 2015") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
ggplot(Nov12Subset, aes(x=reorder(Incident.Zip, -table(Incident.Zip)[Incident.Zip]))) + 
  geom_bar() + xlab("Incident.Address") + ggtitle("Descriptors of Animal Complaints in Nov 12th, 2015") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))