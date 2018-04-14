 #First, I downloaded the full dataset into Working Directory
#Then, I used 7zip to extract and delete all .rar files

library(tools)

#Create a list of all files with Legit Pharmacies (only 4 folders, so making it manually)
#For some reason, this was the way 7Zip extracted my files. You may need to change these
Files <- c('LegitPharma/LegitMedical',
                'LegitPharma/LegitMedical2/LegitMedical2',
                'LegitPharma/LegitMedical3/LegitMedical3',
                'LegitPharma/LegitMedical4/LegitMedical4')

#Run it through my loop which tallies number of filetypes for each site
source('FileCountLoop.R')

#Use DFt output from FileCountLoop.R to make Legit Website Dataframe


#Drop the blank values, which are parent directories
LegitExtCount <- as.data.frame(DFt[,-1])
#Label these Legit
LegitExtCount$Type <- "Legit"
LegitExtCount$Website <- row.names(LegitExtCount)
row.names(LegitExtCount) <- NULL


#Repeat process for fake websites
Files <- c('ConcoctedPharma/FakeHealth/FakeHealth',
           'ConcoctedPharma/FakeHealth/FakeHealth2',
           'ConcoctedPharma/FakeHealth/FakeHealth3')

source('FileCountLoop.R')


#Use DFt output from FileCountLoop.R to make Legit Website Dataframe
#Drop blank column
#Primary is actually html
DFt$xxx111 <- rowSums(DFt[,names(DFt) == 'html' | names(DFt) == 'primary'],na.rm=TRUE)
DFt <- DFt[,names(DFt) != 'html' & names(DFt) != 'primary']
names(DFt)[names(DFt) == 'xxx111'] <- 'html'

FakeExtCount <- as.data.frame(DFt[,-which(names(DFt) %in% "V6")])
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
AggExt <- AggExt[which(AggExt$Count > 2),]
names(AggExt) <- c('Type','Extension','AvgCount')

CommonExtensions <- unique(AggExt$Extension)

CommonExtensions <- c(CommonExtensions, c('Website','Type'))

#Subset to just extensions with an average file extension count of at least 1 to remove noise

ExtensionCounts <- ExtensionCounts[,colnames(ExtensionCounts) %in% CommonExtensions]

write.csv(ExtensionCounts,"ExtensionCounts.csv",row.names = FALSE)

head(AggExt)
#Bar Plot for EDA
library(ggplot2)
ggplot(AggExt, aes(x=Extension,
                   y=AvgCount,
                   fill=Type))+
  geom_bar(position='dodge',stat='identity')+
  theme(text=element_text(size=20))



ext <- c('asp','cfm','gif','html','jpeg','php','png','wd3')
for(i in 1:(length(ext))){
  EXT <- ExtensionCountsLong[which(ExtensionCountsLong$Extension %in% ext[i]),]
  g <- ggplot(EXT, aes(x=Count, fill=Type))+
  geom_histogram(position="identity",alpha='.5', binwidth = 100)+
    labs(x=ext[i])+theme(text=element_text(size=20))
  print(g)
  }


for(i in 3:10)
ks.test(ExtensionCounts[which(ExtensionCounts$Type == 'Legit'),10],
        ExtensionCounts[which(ExtensionCounts$Type == 'Fake'),10])
head(totals)
head(ExtensionCounts)
#wd3, png-ish, html
totals <- aggregate(Count~Website+Type,data=ExtensionCountsLong,FUN=sum)
outliers <- ExtensionCountsLong[which(ExtensionCountsLong$Count > 1000),]
ggplot(totals,aes(x=Count,fill=Type))+geom_histogram(position="identity",alpha='.5')+
  labs(x='Total Files')+theme(text=element_text(size=20))
totals[which(totals$Count<100),]

aggregate(Count~Type, data=totals, FUN=median)
wilcox.test(Count~Type, data=totals)
kruskal.test(Count~Type, data=totals)

library(Hmisc)
bootdif <- function(y, g) {
  ## Ensure treatment group is a factor
  g <- as.factor(g)
  ## Use the smean.cl.boot function to bootstrap means for
  # variable y for each treatment group (A and B); this code
  # uses 2000 samples, but can easily be changed
  a <- attr(smean.cl.boot(y[g==levels(g)[1]],
                          B=2000, reps=TRUE), 'reps')
  b <- attr(smean.cl.boot(y[g==levels(g)[2]],
                          B=2000, reps=TRUE), 'reps')
  ## Calculate the observed mean difference between groups
  meandif <- diff(tapply(y, g, mean, na.rm=TRUE))
  ## Calculate the 2.5 and 97.5 percentiles of the differences
  # in bootstrapped means
  # (can easily be changed for 90% CI, 99% CI, etc.)
  a.b <- quantile(b-a, c(.025,.975))
  ## Prepare object to return
  res <- c(meandif, a.b)
  names(res) <- c('Mean','.025','.975')
  res
}

bootdif(totals$Count,totals$Type)
