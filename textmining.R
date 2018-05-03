library(tokenizers)
library(dplyr)
library(RCurl)
library(xml)
library(tm)
library(ggplot2)
library(wordcloud)

# Author: Tony Breyal
# Date: 2011-11-18
# Modified: 2011-11-18
# Description: Extracts all text from a webpage (aims to extract only the text you would see in a web browser)
# Packages Used: RCurl, XML   
# Blog Reference: Not published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}

### Compiling word list

ConcoctedPharma <- list.files(path='c:/users/jeffw/downloads/msds/7349/Project/ConcoctedPharma/FakeHealth/',pattern = "*.htm*$", full.names = T, recursive = TRUE)

html2txt <- lapply(ConcoctedPharma, htmlToText)
htmltokens <- data.frame(unlist(tokenize_words(unlist(html2txt))))
names(htmltokens) <- "words"
names(htmltokens) <- "words"
grouptown <- htmltokens %>%
  group_by(words) %>%
  count() %>%
  arrange(desc(n))

### text mining time
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE) 

dfcorpus <- Corpus(vectorSource(htmltokens$words))
docs <- tm_map(docs,removePunctuation)   
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(dfcorpus, tolower)
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, removeWords, c("syllogism", "tautology","wwwcanadianpharmacytrustcom","wwworbitalmarketingsolutionscom","wwwcanadamedicineshopcom")) 
docs <- tm_map(docs, removeWords, grep("^www"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

### Stage the Data
corpus <- Corpus(VectorSource(docs))
dtm <- DocumentTermMatrix(corpus)   
tdm <- TermDocumentMatrix(corpus)   

### Explore your data      
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
# write.csv(m, file="DocumentTermMatrix.csv")   

dtms <- removeSparseTerms(dtm, 0.1)

wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq>2534), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))

dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)
