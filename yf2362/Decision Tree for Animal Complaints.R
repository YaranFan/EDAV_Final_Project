if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("rpart.plot")) install.packages("rpart.plot")
library(RColorBrewer)
library(rpart.plot)

# Read Data
setwd("C:/Users/hongzhili/Desktop/W4701Project")
The311 = readRDS('whole_data.RData')

# Keep certain columns and rows
The311 = The311[,c(2,3,4:9,25,51,52)]
The311 = The311[which(The311$Closed.Date!=''),]

# Create necessary new variables
The311$counts = 1
The311$Date = as.POSIXlt(as.POSIXct(The311$Created.Date, format = "%m/%d/%Y %I:%M:%OS %p"))
The311$ClosedDate = as.Date(as.POSIXlt(as.POSIXct(The311$Closed.Date, format = "%m/%d/%Y %I:%M:%OS %p")))
The311$Length = The311$ClosedDate - as.Date(The311$Date)
The311$Year = The311$Date$year + 1900
The311$Month = month.abb[The311$Date$mon + 1]
The311$Hour = The311$Date$hour
The311$DayOfWeek = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[The311$Date$wday + 1] 
# keep the date as string
The311$Created.Date = substr(The311$Created.Date,1,10) 
#drop the date-time column
The311 = The311[,-which(names(The311)=="Date"|names(The311)=="ClosedDate"|names(The311)=="Closed.Date")] 

# Create Season
The311$Season = ''
The311$Season[which(The311$Month=='Mar'|The311$Month=='Apr'|The311$Month=='May')]='Spring'
The311$Season[which(The311$Month=='Jun'|The311$Month=='Jul'|The311$Month=='Aug')]='Summer'
The311$Season[which(The311$Month=='Sep'|The311$Month=='Oct'|The311$Month=='Nov')]='Fall'
The311$Season[which(The311$Month=='Dec'|The311$Month=='Jan'|The311$Month=='Feb')]='Winter'

# Create Animal type
The311$Animal = ''
The311$Animal[which(grepl('Dog',The311$Complaint.Type)|grepl('Dog',The311$Descriptor))]='Dog'
The311$Animal[which(grepl('Cat',The311$Complaint.Type)|grepl('Cat',The311$Descriptor))]='Cat'
The311$Animal[which(grepl('Bees',The311$Complaint.Type)|grepl('Bees',The311$Descriptor))]='Bee'
The311$Animal[which(grepl('Bird',The311$Complaint.Type)|grepl('Bird',The311$Descriptor))]='Others'
The311$Animal[which(grepl('Farm Animal',The311$Complaint.Type)|grepl('Farm Animal',The311$Descriptor))]='Others'
The311$Animal[which(grepl('Ferret',The311$Complaint.Type)|grepl('Ferret',The311$Descriptor))]='Others'
The311$Animal[which(grepl('Iguana',The311$Complaint.Type)|grepl('Iguana',The311$Descriptor))]='Others'
The311$Animal[which(grepl('Monkey',The311$Complaint.Type)|grepl('Monkey',The311$Descriptor))]='Others'
The311$Animal[which(grepl('PESTS',The311$Complaint.Type)|grepl('PESTS',The311$Descriptor))]='Pests'
The311$Animal[which(grepl('Pigeon',The311$Complaint.Type)|grepl('Pigeon',The311$Descriptor))]='Pigeon'
The311$Animal[which(grepl('Rooster',The311$Complaint.Type)|grepl('Rooster',The311$Descriptor))]='Rooster'
The311$Animal[which(grepl('Snake',The311$Complaint.Type)|grepl('Snake',The311$Descriptor))]='Others'
The311$Animal[which(grepl('Turtle',The311$Complaint.Type)|grepl('Turtle',The311$Descriptor))]='Others'
The311$Animal[which(grepl('Wildlife',The311$Complaint.Type)|grepl('Wildlife',The311$Descriptor))]='Wildlife'
The311 = The311[which(The311$Animal!=''),]

# Animal data
Animal_data = The311[,c(2,4,8,11,12,16,17,18)]
Animal_data = Animal_data[which(Animal_data$Length>=0),]

# Time to close case
Animal_data$CaseLength = ''
Animal_data$CaseLength[which(Animal_data$Length==0)]='Same Day'
Animal_data$CaseLength[which(Animal_data$Length<=3 & Animal_data$CaseLength=='')]='In 3 Days'
Animal_data$CaseLength[which(Animal_data$Length<=7 & Animal_data$CaseLength=='')]='In a Week'
Animal_data$CaseLength[which(Animal_data$Length<=14 & Animal_data$CaseLength=='')]='In 2 Weeks'
Animal_data$CaseLength[which(Animal_data$Length<=30 & Animal_data$CaseLength=='')]='In a Month'
Animal_data$CaseLength[which(Animal_data$CaseLength=='')]='Over a Month'
Animal_data = Animal_data[,-which(names(Animal_data)=="Length")]

# Factors
Animal_data$DayOfWeek = as.factor(Animal_data$DayOfWeek)
Animal_data$Season = as.factor(Animal_data$Season)
Animal_data$Animal = as.factor(Animal_data$Animal)
Animal_data$CaseLength = as.factor(Animal_data$CaseLength)

# Save the small file
write.table(Animal_data, file = 'yf2362/Selected_Info_with_Animal_Type.csv', row.names=FALSE, sep=",")

# Read data
Animal_data = read.csv('yf2362/Selected_Info_with_Animal_Type.csv')

# Decision Trees
  color6 = c('#fee08b','#91cf60','#fc8d59','#d9ef8b','#d73027','#1a9850')
  
  # Animal only
  rfit1 = rpart(CaseLength ~ Animal, data = Animal_data, method = "class", cp = 0.00001)
  prp(rfit1, branch.type=5, faclen=0, uniform = TRUE, box.col=color6[rfit1$frame$yval],
      main="", split.col ="#023858")
  title("How long a case may take - based on Animal", cex.main=1.4, font.main = 7)
  
  # Animal + Borough
  rfit2 = rpart(CaseLength ~ Animal+Borough, data = Animal_data, method = "class", cp = 0.00001)
  prp(rfit2, branch.type=2, faclen=0, uniform = TRUE, box.col=color6[rfit2$frame$yval],
      main="", split.col ="#023858")
  title("How long a case may take - based on Animal and Borough", cex.main=1.4, font.main = 7)
  
  # Animal + Season
  rfit3 = rpart(CaseLength ~ Animal+Season, data = Animal_data, method = "class", cp = 0.00001)
  prp(rfit3, branch.type=2, faclen=0, uniform = TRUE, box.col=color6[rfit3$frame$yval],
      main="", split.col ="#023858")
  title("How long a case may take - based on Animal and Season", cex.main=1.4, font.main = 7)
  
  # Animal + Day of Week
  rfit4 = rpart(CaseLength ~ Animal+DayOfWeek, data = Animal_data, method = "class", cp = 0.00001)
  prp(rfit4, branch.type=5, faclen=0, uniform = TRUE, box.col=color6[rfit4$frame$yval],
      main="", split.col ="#023858")
  title("How long a case may take - based on Animal and Day of Week", cex.main=1.4, font.main = 7)
  
  # Animal + Complaint Type
  # code source: http://www.milbo.org/rpart-plot/prp.pdf
  rfit5 = rpart(CaseLength ~ Animal+Complaint.Type, data = Animal_data, method = "class", cp = 0.00001)
  split.fun <- function(x, labs, digits, varlen, faclen)
  {
    # replace commas with spaces (needed for strwrap)
    labs <- gsub(",", ", ", labs)
    for(i in 1:length(labs)) {
      # split labs[i] into multiple lines
      labs[i] <- paste(strwrap(labs[i], width=50), collapse="\n")
    }
    labs
  }
  prp(rfit5, branch.type=2, faclen=0, uniform = TRUE, split.fun=split.fun, cex = 0.7, 
      box.col=color6[rfit5$frame$yval], split.col ="#023858", main="")
  title("How long a case may take - based on Animal and Complaint Type", cex.main=1.4, font.main = 7)
  
  # Animal + Day of Week
  rfit6 = rpart(CaseLength ~ Animal+Agency, data = Animal_data, method = "class", cp = 0.00001)
  prp(rfit6, branch.type=2, faclen=0, uniform = TRUE, box.col=color6[rfit6$frame$yval],
      main="", split.col ="#023858")
  title("How long a case may take - based on Animal and Agency", cex.main=1.4, font.main = 7)
  