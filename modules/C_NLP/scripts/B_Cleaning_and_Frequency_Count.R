#' Title: Intro: Cleaning and Frequency Count
#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 14, 2022
#'

# Libs
library(tm)

# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh', 'rofl')

# Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/coffee.csv') # sometimes text encoding causes problems so declaring it as latin
head(text)

# As of tm version 0.7-3 tabular was deprecated
?DataframeSource
names(text)[1] <- 'doc_id' #first 2 columns must be 'doc_id' & 'text'

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Check Meta Data; brackets matter!!
t(meta(txtCorpus[4]))
content(txtCorpus[[4]])

# Need to plain text cleaned copy?
df <- data.frame(text = sapply(txtCorpus, content),
                 stringsAsFactors=F)
#write.csv(df,'plain_coffee.csv',row.names = F)

# Compare a single tweet
df[4,]
text$text[4]

# Make a Document Term Matrix or Term Document Matrix depending on analysis
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtTdm  <- TermDocumentMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)
txtTdmM <- as.matrix(txtTdm)

# Examine
findTerm <- grep('^coffee$', colnames(txtDtmM)):(grep('^coffee$', colnames(txtDtmM))+5)
txtDtmM[610:611,findTerm]
txtTdmM[findTerm,610:611]

# End
