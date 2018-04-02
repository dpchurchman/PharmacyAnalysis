#Priming a list to put results from loop
resultlist <- list()

#This for loop creates a list of dataframes for each of the files extracted from .rar
for(i in 1:length(Files)){
  
  Pharma <- as.list(list.files(Files[i]))
  
  PharmaExt <- lapply(Pharma, function(x){
    #each iteration creates a data frame
    y  <- as.data.frame(
      #"table" gives frequency of each extensions
      table(
        #file_ext strips the file extensions from file names
        tools::file_ext(
          #Make all lowercase to avoid duplicates like gif vs GIF
          tolower(
            #list.files(recursive=TRUE) gives all subdirectory file names
            list.files(
              #Which files we're listing
              file.path(
                paste(Files[i],'/',
                      x,sep='')),
              recursive = TRUE)))))
    #Rename the Freq column with the name of the website
    names(y) <- c('ext',x)
    y
  })
  
  #Reduce(function(merge)) to merge together all the websites into one dataframe, drop into the list
  resultlist[[i]] <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "ext", all = TRUE), 
                            PharmaExt)
}


#Merge all the Legit website extensions
DF <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "ext", all = TRUE), resultlist)


##Decided not to do this after all
#A number of extensions seem to be ones that were not able to be read, so will cross-reference
#With a list of known file extensions.
#DF <- DF[DF$ext %in% ExtensionList$Extension,]

#Make extension names row names for clean transpose
DF2 <- data.frame(DF[,-1],row.names=DF[,1])


#Transpose to make websites row names
DFt <- t(DF2)
DFt <- as.data.frame(DFt)

#Merge together all asp extensions
DFt$xxx111 <- rowSums(DFt[,grepl('as',names(DFt))],na.rm=TRUE)
DFt <- DFt[,!grepl('as',names(DFt))]
names(DFt)[names(DFt) == 'xxx111'] <- 'asp'
#Same for port ext
DFt$xxx111 <- rowSums(DFt[,grepl('port',names(DFt))],na.rm=TRUE)
DFt <- DFt[,!grepl('port',names(DFt))]
names(DFt)[names(DFt) == 'xxx111'] <- 'port'
#HTML
DFt$xxx111 <- rowSums(DFt[,grepl('htm',names(DFt))],na.rm=TRUE)
DFt <- DFt[,!grepl('htm',names(DFt))]
names(DFt)[names(DFt) == 'xxx111'] <- 'html'
#JPEG
DFt$xxx111 <- rowSums(DFt[,grepl('jp',names(DFt))],na.rm=TRUE)
DFt <- DFt[,!grepl('jp',names(DFt))]
names(DFt)[names(DFt) == 'xxx111'] <- 'jpeg'

