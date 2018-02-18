 #First, I downloaded the full dataset into Working Directory
#Then, I used 7zip to extract and delete all .rar files

library(tools)
#I though I was going to use this list of file extensions to check which file ext were actual
#But all file ext seem okay now
#Copied from https://www.webopedia.com/quick_ref/fileextensionsfull.asp on 2/12/2018 6:11 pm
#ExtensionList <- read.csv('ExtensionList.csv')
#ExtensionList[,1] <- substring(ExtensionList[,1],2)


#Create a list of all files with Legit Pharmacies (only 4 folders, so making it manually)
#For some reason, this was the way 7Zip extracted my files. You may need to change these
Files <- c('LegitPharma/LegitMedical',
                'LegitPharma/LegitMedical2/LegitMedical2',
                'LegitPharma/LegitMedical3/LegitMedical3',
                'LegitPharma/LegitMedical4/LegitMedical4')

#Run it through my loop which tallies number of filetypes for each site
source('FileCountLoop.R')

#Use DFt output from FileCountLoop.R to make Legit Website Dataframe

#Merge together all asp extensions
DFt$xxx111 <- rowSums(DFt[,grepl('as',names(DFt))],na.rm=TRUE)
DFt <- DFt[,!grepl('as',names(DFt))]
names(DFt)[names(DFt) == 'xxx111'] <- 'asp'

LegitExtCount <- as.data.frame(DFt[,-1])
LegitExtCount$Type <- "Legit"
LegitExtCount$Website <- row.names(LegitExtCount)
row.names(LegitExtCount) <- NULL

#Repeat process for fake websites
Files <- c('ConcoctedPharma/FakeHealth/FakeHealth',
           'ConcoctedPharma/FakeHealth/FakeHealth2',
           'ConcoctedPharma/FakeHealth/FakeHealth3')

source('FileCountLoop.R')


#Use DFt output from FileCountLoop.R to make Legit Website Dataframe
FakeExtCount <- as.data.frame(DFt[,-6])
FakeExtCount$Type <- "Fake"
FakeExtCount$Website <- row.names(FakeExtCount)
row.names(FakeExtCount) <- NULL

#Now combine fake and legit sites into single dataframe
library(dplyr)
ExtensionCounts <- dplyr::bind_rows(FakeExtCount,LegitExtCount)

#Get Website and Type first (still using dplyr)
ExtensionCounts <- ExtensionCounts %>% select(Website, Type, everything())


#Make a long data set for analysis
library(tidyr)
ExtensionCountsLong <- tidyr::gather(ExtensionCounts, Extension, Count, 3:ncol(ExtensionCounts), 
                                     factor_key=FALSE )

ExtensionCountsLong$Count[is.na(ExtensionCountsLong$Count)] <- 0


#Aggregegate average number of extensions by Type, rounding to nearest whole
AggExt <- aggregate(Count ~ Type + Extension, 
                    data=ExtensionCountsLong, FUN=function(x){round(mean(x))})

#Remove extensions with average of 0
AggExt <- AggExt[which(AggExt$Count != 0),]
names(AggExt) <- c('Type','Extension','AvgCount')

CommonExtensions <- unique(AggExt$Extension)

CommonExtensions <- c(CommonExtensions, c('Website','Type'))

#Subset to just extensions with an average file extension count of at least 1 to remove noise

ExtensionCounts <- ExtensionCounts[,colnames(ExtensionCounts) %in% CommonExtensions]

write.csv(ExtensionCounts,"ExtensionCounts.csv",row.names = FALSE)

#Bar Plot for EDA
library(ggplot2)
ggplot(AggExt, aes(x=Extension,
                   y=AvgCount,
                   fill=Type))+
  geom_bar(position='dodge',stat='identity')
  

ExtensionCountsLong[!is.na(ExtensionCountsLong$Count) &
                      ExtensionCountsLong$Type == 'Legit',]
