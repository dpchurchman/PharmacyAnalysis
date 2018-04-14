library(tokenizers)
library(dplyr)
library(RCurl)
library(XML)
library(tm)
library(ggplot2)
library(wordcloud)
library(magrittr)
library(tidyverse)

# Author: Tony Breyal
# Date: 2011-11-18
# Modified: 2011-11-18
# Description: Extracts all text from a webpage (aims to extract only the text you would see in a web browser)
# Packages Used: RCurl, XML   
# Blog Reference: Not published
# https://github.com/tonybreyal/Blog-Reference-Functions/blob/master/R/htmlToText/htmlToText.R

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

####### Legitimate Site Text Mining #########

LegitPharma <- list.files(path='./LegitPharma/LegitMedical5/',pattern = "*.htm*$", full.names = T, recursive = TRUE)

html2txt_leg <- lapply(LegitPharma, htmlToText)
htmltokens_leg <- data.frame(unlist(tokenize_word_stems(unlist(html2txt_leg))))
names(htmltokens_leg) <- "words"
legit_grouping <- htmltokens_leg %>%
  group_by(words) %>%
  count() %>%
  arrange(desc(n))

dfcorpus2 <- Corpus(VectorSource(htmltokens_leg$words))
docs2 <- tm_map(dfcorpus2,removePunctuation)
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, tolower)
docs2 <- tm_map(docs2, removeWords, stopwords("en")) 
docs2 <- tm_map(docs2, removeWords, c("syllogism", "tautology","wwwcanadianpharmacytrustcom","wwworbitalmarketingsolutionscom","wwwcanadamedicineshopcom","goodrich")) 
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, PlainTextDocument)

### Stage the Data
corpus2 <- Corpus(VectorSource(docs2))
dtm2 <- DocumentTermMatrix(corpus2)   
tdm2 <- TermDocumentMatrix(corpus2)   

### Explore your data      
freq2 <- colSums(as.matrix(dtm2))   
length(freq2)   
ord2 <- order(freq2)   
m2 <- as.matrix(dtm2)   
dim(m2)   

# write.csv(m, file="DocumentTermMatrix.csv")   

dtms2 <- removeSparseTerms(dtm2, 0.1)

wf2 <- data.frame(word=names(freq2), freq=freq2)   
p <- ggplot(subset(wf, freq>369), aes(word, freq))+
  geom_bar(aes(reorder(word,freq)),stat="identity", fill="#ff6666")+
  theme(axis.text.x=element_text(hjust=1))+
  xlab("Word Stem")+ylab("Number Of Occurrences")+
  labs(title="10 Most Common Word Stems In Legitimate Sites")+
  coord_flip()

dtms2 <- removeSparseTerms(dtm2, 0.15) # Prepare the data (max 15% empty space)   
freq2 <- colSums(as.matrix(dtm2)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq2),freq2, max.words=100, scale=c(5, .2),rot.per=0.2, colors=dark2, random.order=FALSE)

freq2 <- cbind(freq,names(freq))
# write.csv(freq2, file='./legitwordcount.csv',row.names=F)

####### Concocted Site Text Mining #########

ConcoctedPharma <- list.files(path='c:/users/jeffw/downloads/msds/7349/Project/ConcoctedPharma/FakeHealth4/',pattern = "*.htm*$", full.names = T, recursive = TRUE)
html2txt_5 <- lapply(ConcoctedPharma, htmlToText)

htmltokens_conc <- data.frame(unlist(tokenize_word_stems(unlist(html2txt_5))))
names(htmltokens_conc) <- "words"
grouptown_conc <- htmltokens_conc %>%
  group_by(words) %>%
  count() %>%
  arrange(desc(n))

# library(quanteda)
### text mining time
# Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
#            "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE) 

dfcorpus <- Corpus(VectorSource(htmltokens_conc$words))
docs <- tm_map(dfcorpus,removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("en")) 
docs <- tm_map(docs, removeWords, c("syllogism", "tautology","wwwcanadianpharmacytrustcom","wwworbitalmarketingsolutionscom","wwwcanadamedicineshopcom")) 
#docs <- tm_map(docs, removeWords, grep("^www",docs))
#docs <- tm_map(docs, removeWords, grep("$com",docs))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

### Stage the Data
corpus <- Corpus(VectorSource(docs))
dtm <- DocumentTermMatrix(corpus)   
tdm <- TermDocumentMatrix(corpus)   

### Explore your data      
freq_conc <- colSums(as.matrix(dtm))   
length(freq_conc)   
ord_conc <- order(freq_conc)   
m_conc <- as.matrix(dtm)   
dim(m_conc)   
# write.csv(m, file="DocumentTermMatrix.csv")   

dtms <- removeSparseTerms(dtm, 0.1)

wf <- data.frame(word=names(freq_conc), freq=freq_conc)   
q <- ggplot(subset(wf, freq>1628), aes(word, freq))+
  geom_bar(aes(reorder(word,freq)),stat="identity", fill="#008080")+
  theme(axis.text.x=element_text(hjust=1))+
  xlab("Word Stem")+ylab("Number Of Occurrences")+
  labs(title="Top 10 Most Comomon Word Stems In Concocted Sites")+
  coord_flip()

dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2_conc <- brewer.pal(6, "Dark2")
png("wordcloud_packages.png", width=12,height=8, units='in', res=300)
par(mar = rep(0, 4))
wordcloud(names(freq_conc), freq_conc, max.words=100, scale=c(5, .2),rot.per=0.3, colors=dark2_conc, random.order=FALSE)
