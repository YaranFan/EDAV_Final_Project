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

# fulldata311 <- data.frame(readRDS('whole_data.RData'))
# animalSubset311 = fulldata311[allAnimals,]
library(dplyr)
library(ggplot2)
library(openair)
dataforcal <- animalSubset311[,c(1,11)]
dataforcal <- mutate(dataforcal, Date = as.character(Date))
dataforcal = group_by(dataforcal, Date) 
dataforcal = summarise(dataforcal, count = n())
dataforcal$Date = str_trim(dataforcal$Date)
library(stringr)
dataforcal$Date = as.POSIXlt(as.POSIXct(dataforcal$Date, format = "%m/%d/%Y"))
qplot(Date, count, data = dataforcal, geom = "smooth", xlab = "Date", ylab = "Daily Animal Complaints", main = "Daily Animal Complaints 2010 - March 2016")
calendarPlot(dataforcal, pollutant = "count", year = 2015, main = "Animal Complaints by Day 2015", cols = "increment")

ggplot(animalSubset311, aes(x=reorder(Descriptor, -table(Descriptor)[Descriptor]))) + 
  geom_bar() + xlab("Descriptors") + ggtitle("Descriptors of Animal Complaints") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))

ggplot(animalSubset311, aes(x=reorder(Complaint.Type, -table(Complaint.Type)[Complaint.Type]))) + 
  geom_bar() + xlab("Types") + ggtitle("Types of Animal Complaints") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))

rea_sol = as.data.frame(table(animalSubset311$Complaint.Type, animalSubset311$Agency))
colnames(rea_sol) = c("reason","solution","count")
ggplot(rea_sol, aes_string(x='solution', y='reason', size='count')) + geom_point() + scale_size_area(max_size = 20)

m = list(l = 250,r = 0,b=120)
plot_ly(rea_sol, x = solution, y = reason, mode = "markers", size = count, marker = list(sizeref=6))%>%
  layout(title = "Reason of Toilet Complaints vs Solution",
         xaxis = list(title = "solution"), yaxis = list(title = "reason"),
         autosize = F, width = 800, height = 600, margin = m)
