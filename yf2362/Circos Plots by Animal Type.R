if (!require("circlize")) install.packages("circlize")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("data.table")) install.packages("data.table")
library("circlize")
library("RColorBrewer")
library('data.table')
library('reshape')

setwd("C:/Users/hongzhili/Desktop/W4701Project")

data_animal = read.csv("animialSubset.csv")

# Create Animal type
data_animal$Animal = ''
data_animal$Animal[which(grepl('Dog',data_animal$Complaint.Type)|grepl('Dog',data_animal$Descriptor))]='Dog'
data_animal$Animal[which(grepl('Cat',data_animal$Complaint.Type)|grepl('Cat',data_animal$Descriptor))]='Cat'
data_animal$Animal[which(grepl('Bees',data_animal$Complaint.Type)|grepl('Bees',data_animal$Descriptor))]='Bee'
data_animal$Animal[which(grepl('Bird',data_animal$Complaint.Type)|grepl('Bird',data_animal$Descriptor))]='Bird'
data_animal$Animal[which(grepl('Farm Animal',data_animal$Complaint.Type)|grepl('Farm Animal',data_animal$Descriptor))]='Farm Animal'
data_animal$Animal[which(grepl('Ferret',data_animal$Complaint.Type)|grepl('Ferret',data_animal$Descriptor))]='Ferret'
data_animal$Animal[which(grepl('Iguana',data_animal$Complaint.Type)|grepl('Iguana',data_animal$Descriptor))]='Iguana'
data_animal$Animal[which(grepl('Monkey',data_animal$Complaint.Type)|grepl('Monkey',data_animal$Descriptor))]='Monkey'
data_animal$Animal[which(grepl('PESTS',data_animal$Complaint.Type)|grepl('PESTS',data_animal$Descriptor))]='Pests'
data_animal$Animal[which(grepl('Pigeon',data_animal$Complaint.Type)|grepl('Pigeon',data_animal$Descriptor))]='Pigeon'
data_animal$Animal[which(grepl('Rooster',data_animal$Complaint.Type)|grepl('Rooster',data_animal$Descriptor))]='Rooster'
data_animal$Animal[which(grepl('Snake',data_animal$Complaint.Type)|grepl('Snake',data_animal$Descriptor))]='Snake'
data_animal$Animal[which(grepl('Turtle',data_animal$Complaint.Type)|grepl('Turtle',data_animal$Descriptor))]='Turtle'
data_animal$Animal[which(grepl('Wildlife',data_animal$Complaint.Type)|grepl('Wildlife',data_animal$Descriptor))]='Wildlife'
data_animal$Animal[which(data_animal$Animal=='')]='Not Specified'

# Create Season
data_animal$Season = ''
data_animal$Season[which(data_animal$Month=='Mar'|data_animal$Month=='Apr'|data_animal$Month=='May')]='Spring'
data_animal$Season[which(data_animal$Month=='Jun'|data_animal$Month=='Jul'|data_animal$Month=='Aug')]='Summer'
data_animal$Season[which(data_animal$Month=='Sep'|data_animal$Month=='Oct'|data_animal$Month=='Nov')]='Fall'
data_animal$Season[which(data_animal$Month=='Dec'|data_animal$Month=='Jan'|data_animal$Month=='Feb')]='Winter'

# save intermediate file
write.table(data_animal, 'yf2362/smaller_animal_data.csv', row.names=FALSE, sep=",")


#---------------#
# Circos plot
#---------------#
# keep only useful
data_animal = read.csv('yf2362/smaller_animal_data.csv')
data_animal = data_animal[,c(3,4,7,8,9,12,14,16,17,18)]

# aggregate data
  #(1) Animal & Seasons
  tmp = data.table(data_animal)
  Agg_season = as.data.frame(tmp[, sum(counts, na.rm = TRUE),by = list(Animal, Season)])
  #(2) Animal & Boroughs
  Agg_Borough = as.data.frame(tmp[, sum(counts, na.rm = TRUE),by = list(Animal, Borough)])

# Plot two plots in one row
  par(mfrow = c(1,2))
  
  #(1) Animal & Seasons
  data = reshape(Agg_season, timevar = "Season", idvar = c("Animal"), direction = "wide")
  data[is.na(data)] = 0
  colnames(data) = sub("V1.","",colnames(data))
  rownames(data) = data[,1]
  
  data = data[,-1]
  data = data[,c(1,4,3,2)]
  data = data[order(-data$Spring),]
  data = as.matrix(data)
  
  # sum up some categories with too few records
  data = rbind(data, colSums(data[9:15,]))
  rownames(data)[16] = 'Others'

  # plot
  c1 = rev(gray.colors(ncol(data)))
  c2 = c("#31a354", "#41bbf2", "#8dd03d", "#efbd26", "#f95959", "#6b62f2")
  grid.col = NULL
  grid.col[c(colnames(data[c(4:8,16),]),rownames(data[c(4:8,16),]))] = c(c1, c2)
  chordDiagram(data[c(4:8,16),], grid.col = grid.col)
  title("Animal Types (exl. Dog and Pests) and the 4 Seasons", cex.main=1.3, font.main = 7)
  
  #(2) Animal & Boroughs
  data = reshape(Agg_Borough, timevar = "Borough", idvar = c("Animal"), direction = "wide")
  data[is.na(data)] = 0
  colnames(data) = sub("V1.","",colnames(data))
  rownames(data) = data[,1]
  
  data = data[,c(-1,-7)]
  data = data[,c(1,3,2,4,5)]
  data = data[order(-data$MANHATTAN),]
  data = as.matrix(data)
  
  # sum up some categories with too few records
  data = rbind(data, colSums(data[9:15,]))
  rownames(data)[16] = 'Others'
  
  # plot
  c1 = rev(gray.colors(ncol(data)))
  c2 = c("#31a354", "#41bbf2", "#8dd03d", "#efbd26", "#f95959", "#6b62f2")
  grid.col = NULL
  grid.col[c(colnames(data[c(4:8,16),]),rownames(data[c(4:8,16),]))] = c(c1, c2)
  chordDiagram(data[c(4:8,16),], grid.col = grid.col)
  title("Animal Types (exl. Dog and Pests) and the 5 Boroughs", cex.main=1.3, font.main = 7)

