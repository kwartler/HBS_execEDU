#' Ted Kwartler
#' July 15, 2023
#' Purpose: Create 20 teams for group work using comparative SP500 companies


# Obtain the up to date sp500 list
getSP500 <- function(){
  library(dplyr)
  library(rvest)
  x <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>%
    html_nodes('#constituents') %>% html_table()
  x <- x[[1]]
  x$Symbol <- gsub('[.]','-',x$Symbol)
  
  # Profile Summary
  #wikiURLs <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% 
  #  html_nodes(xpath = '//td') %>% html_nodes("a") %>% html_attr("href")
  #grep('/wiki/', wikiURLs)
  
  
  # Clean up
  names(x) <-  make.names(names(x))
  return(x)
}

# Examine the results
#head(sp500)
#table(sp500$GICS.Sector)
#subset(table(sp500$GICS.Sub.Industry), table(sp500$GICS.Sub.Industry)>=2)

# Subset the data frame to keep rows with industry levels that occur more than twice
getSP500pairs <- function(sp500, factorLevel = 'GICS.Sub.Industry', grpN = 20){
  
  # Split column
  factorCol  <- as.vector(sp500[,grepl(factorLevel, names(sp500))])
  
  # Tally
  sp500Tally <- table(factorCol)
  
  # Index
  keeps <- names(sp500Tally)[sp500Tally > 2]
  idx   <- factorCol %in% keeps
  
  # Subset the data frame to keep rows with industry levels that occur more than twice
  subSP500 <- sp500[factorCol[[1]] %in% keeps, ]
  
  
  # Pair the rows based on the company
  sp500Pairs <- split(subSP500, as.vector(subSP500[,grepl(factorLevel, names(subSP500))]))
  
  # Filter out the pairs with a single row
  sp500Pairs <- sp500Pairs[sapply(sp500Pairs, nrow) > 1]
  
  # Check for and possibly drop the last row if it's an odd number of rows
  oddIndustries  <- sp500Pairs[sapply(sp500Pairs, nrow) %%2 ==1]
  oddIndustries  <- lapply(oddIndustries, head, -1) #drop last row
  evenIndustries <- sp500Pairs[sapply(sp500Pairs, nrow) %%2 !=1]
  
  # Combine all the now-even number industries
  response <- c(oddIndustries, evenIndustries)
  
  # Pair them off for individual groups
  pairGrps <- function(df){
    
    # Randomly sample them to get out of alphabetical
    idx <- sample(1:nrow(df), nrow(df))
    df <- df[idx,]
    
    allPairings <- list()
    for (i in seq(1, nrow(df), by = 2)) {
      # Subset the a single pair of rows
      onePair <- df[i:(i+1), ]
      
      # Add the Pair and ID
      onePair$grpID <- paste0('company-',c(LETTERS[1:2]))
      
      # Append the pair to the list
      allPairings[[length(allPairings) + 1]] <- onePair
    }
    
    # Print the resulting pairs
    resp <- do.call(rbind, allPairings)
    return(as.data.frame(resp))
  }
  
  # Apply the function to each data frame in the list
  response <- lapply(response, pairGrps)
  
  # One DF
  response <- do.call(rbind, response)
  
  # Select group pairings & clean up
  keeps    <- sample(grep('company-A', response$grpID), grpN)
  keeps    <- c(keeps, keeps+1)
  response <- response[keeps, ]
  response <- response[order(keeps, decreasing = F), ]
  rownames(response) <- NULL
  
  # Output the pairs
  return(response)
}

# Get HLOC Stock Data
getHLOC <- function(stkDF, pth = "~/Desktop/HBS_execEd/GroupData/"){
  library(quantmod)
  idx <- 0
  for (i in seq(1, nrow(stkDF), by = 2)) {
    idx <- idx + 1
    print(paste('working on group:',idx))
    # Subset the a single pair of rows
    onePair <- stkDF[i:(i+1), ]
    
    # Make the all group directory
    if (!dir.exists(pth)) {dir.create(pth)}
    
    # Make the individual group directory
    tmpDir <- paste0(pth, 'group-', idx)
    if (!dir.exists(tmpDir)) {dir.create(tmpDir)}
    
    # Save company info
    write.csv(onePair, paste0(tmpDir, '/group_',idx,'_',
                              onePair$Symbol[1],'_',
                              onePair$Symbol[2],'_companyInfo.csv'))
    
    # Obtain and save the data
    for(j in 1:nrow(onePair)){ # actually always 2 :)
      print(paste('working on:', onePair$Symbol[j], '-',onePair$Security[j]))
      singleStkPth <- paste0(tmpDir, '/HLOC_',onePair$Security[j])
      if (!dir.exists(singleStkPth)) {dir.create(singleStkPth)}
      
      # HLOC
      stk <- getSymbols(onePair$Symbol[j], src = "yahoo", auto.assign = F)
      write.zoo(stk, paste0(singleStkPth,'/',onePair$Symbol[j],Sys.Date(),'_HLOC.csv'))
      
    }
    
  }
  
}

# Get the transcripts
getTranscripts <- function(ticker='AAPL'){
  library(rvest)
  library(httr)
  
  
  # construct the URL
  url  <- paste('https://www.marketbeat.com/stocks/NYSE', ticker, 'earnings', sep = '/')
  page <- read_html(url)
  
  # Find the button
  button <- page %>% html_element(css = '#cphPrimaryContent_cphTabContent_pnlSummary .green-button.w-100')
  
  # Check for Exchange
  if(length(button)==0){
    warning(paste('tried NYSE, now trying NASDAQ for', ticker))
    url  <- paste('https://www.marketbeat.com/stocks/NASDAQ', ticker, 'earnings', sep = '/')
    page <- read_html(url)
    
    # Find the button
    button <- page %>% html_element(css = '#cphPrimaryContent_cphTabContent_pnlSummary .green-button.w-100')
  }
  
  # Check for no recent transcript
  if(length(button)==0){
    warning(paste('tried NYSE & NASDQ for', ticker,'still nothing so trying older transcripts for NYSE'))
    
    # construct the URL
    url  <- paste('https://www.marketbeat.com/stocks/NYSE', ticker, 'earnings', sep = '/')
    page <- read_html(url)
    
    button <- page %>% html_element(xpath = '//*[@id="earnings-history"]/tbody/tr[3]/td[9]/a[1]')
  }
  
  if(length(button)==0){
    warning(paste('tried NYSE & NASDQ for', ticker,'still nothing so trying older transcripts for NASDAQ'))
    # construct the URL
    url  <- paste('https://www.marketbeat.com/stocks/NASDAQ', ticker, 'earnings', sep = '/')
    page <- read_html(url)
    
    button <- page %>% html_element(xpath = '//*[@id="earnings-history"]/tbody/tr[3]/td[9]/a[1]')
  }
  if(length(button)==0){
    warning(paste('tried NYSE & NASDQ for', ticker,' & older docs still nothing, returning nothing. '))
  }
 
  if(length(button)!=0){
    transcriptURL <- button %>% html_attr("href")
  } else {
    transcriptURL <- 'sec.gov'
  }
  
  
  # Now get the URL for the transcript
  if(grepl('sec.gov', transcriptURL)==T){
    warning(paste('unable to obtain any transcripts, the SEC site was returned for',ticker))
    #transcriptDate <- Sys.Date()
    df <- NULL
  } else {
    

  
  
  # Given an URL, return a dataframe containing the speech bubbles.
  transcriptPG <- read_html(transcriptURL)
  
  # Get all elements
  speechBubbles <- transcriptPG %>% html_elements(css = ".transcript-line-left, .transcript-line-right")
  
  # Create the parsing function
  # Transcripts supporting functions 
  parseBubble <- function(bubble) {
    # Extracts speaker, title, and message content from a single speech bubble.
    
    speaker <- bubble %>% html_element(css = '.transcript-line-speaker .font-weight-bold') %>% html_text(trim=TRUE)
    title <- bubble %>% html_element(css = '.transcript-line-speaker .secondary-title') %>% html_text(trim=TRUE)
    paragraphs <- bubble %>% html_elements(css = '.pb-2') %>% html_text(trim=TRUE)
    msg <- paste(paragraphs, collapse = '\n\n')
    
    bubbleDF <- data.frame(speaker = speaker, title = title, msg = msg, stringsAsFactors = FALSE)
    
    return(bubbleDF)
  }
  
  resultsList <- lapply(speechBubbles, parseBubble)
  
  # Combine the list of dataframes into a single dataframe
  df <- do.call(rbind, resultsList)
  }
  # Grab the date if a transcript was found
  if(is.null(df)==T){
    warning(paste('no transcript found for', ticker, 'so transcript date is ', Sys.Date()))
    transcriptDate <- Sys.Date()
  } else {
    # Get the text
    transcriptDate <- page %>% html_nodes(xpath = '//*[@id="shareableArticle"]/div[1]/div/div[1]/text()[1]') %>% html_text()
    
    # Clean it up
    transcriptDate <- lapply(strsplit(transcriptDate, ','), trimws) 
    transcriptDate <- paste0(gsub('[.]','',unlist(transcriptDate)), collapse = '_')
    transcriptDate <- gsub(' ','_', transcriptDate)
  }
  
  response <- list(callDate = transcriptDate, script = df)
}

# get transcripts by data folder
grpTranscripts <- function(pth = "~/Desktop/HBS_execEDU/GroupData2"){
  # Get all directories for each group
  allGrps <- dir(pth, full.names = T)
  
  # Index by grp number to make sure the scrape is correct
  idx <- as.numeric(gsub('group-','',unlist(lapply(strsplit(allGrps, '/'), tail, 1))))
  idx <- order(idx, decreasing = F)
  allGrps <- allGrps[idx]
  
  # Apply the trnscript get logic
  for(i in 1:length(allGrps)){
    print(paste('Working on group:',allGrps[i]))
    # Get the stock info
    companyInfo <- list.files(path = allGrps[i], 
                              pattern = '.csv',
                              full.names = T)
    companyStk <- read.csv(companyInfo)$Symbol
    print(companyStk)
    #Sys.sleep(5)
    for(j in 1:length(companyStk)){
      #print(companyStk[j])
      oneTranscript <- getTranscripts(companyStk[j])
      Sys.sleep(1)
      nam <- paste0(allGrps[i],'/',companyStk[j],'_transcript_',oneTranscript[[1]],'.csv')
      write.csv(oneTranscript[[2]], nam, row.names = F)
    }
  }
  
}

# Get the Stock list
sp500 <- getSP500()

# Obtain the pairs for groups
spPairs <- getSP500pairs(sp500, factorLevel = 'GICS.Sub.Industry')
getHLOC(spPairs, pth = "~/Desktop/HBS_execEDU/GroupData7/")

# Add recent transcripts
grpTranscripts(pth = "~/Desktop/HBS_execEDU/GroupData7/")

grps <- dir('~/Desktop/HBS_execEDU/GroupData5', full.names = T)

# Check for missed transcripts
allInfo <- list()
for(i in 1:length(grps)){
  csvFiles <- list.files(grps[i], pattern = 'transcript', full.names = T)
  
  response <- list()
  for(j in 1:length(csvFiles)){
    chk <- file.info(csvFiles[j])
    chk$file <- csvFiles[j]
    row.names(chk) <- NULL
    response[[j]] <- chk
  }
  response <- do.call(rbind, response)
  allInfo[[i]] <- response
}
allInfo <- do.call(rbind, allInfo)

smalls <- subset(allInfo, allInfo$size==3) #GroupData2=7, GroupData3=10
