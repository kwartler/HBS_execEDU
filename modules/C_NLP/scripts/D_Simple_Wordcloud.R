#' Title: Intro: Simple Wordcloud
#' Purpose: Learn about wordclouds and make one
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 14 2022
#' 

# Libs
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(readr)


# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) #new: isn't to is not
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh', 'chardonnay')

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
  }
  
# Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/chardonnay.csv')

# As of tm version 0.7-3 tabular was deprecated
names(text)[1]<-'doc_id' 

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
wineTDM  <- TermDocumentMatrix(txtCorpus, 
                               control=list(tokenize=bigramTokens))
wineTDMm <- as.matrix(wineTDM)

# See a bi-gram
findTerm <- grep('wine country', rownames(wineTDMm)):(grep('wine country', rownames(wineTDMm))+5)
wineTDMm[findTerm,870:871]

# Get Row Sums & organize
wineTDMv <- sort(rowSums(wineTDMm), decreasing = TRUE)
wineDF   <- data.frame(word = names(wineTDMv), 
                       freq = wineTDMv,
                       row.names = NULL)

# Review all Pallettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(wineDF$word,
          wineDF$freq,
          max.words=50,
          random.order=FALSE,
          colors=pal)

# End