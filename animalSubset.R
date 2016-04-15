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
#creates output
write.csv(animalSubset311,file = "animialSubset.csv")
