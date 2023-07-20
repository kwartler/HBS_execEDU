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

# Transcripts supporting functions 
# install.packages("rvest", "httr")
#library(rvest)

parse_bubble <- function(bubble) {
  # Extracts speaker, title, and message content from a single speech bubble.
  
  speaker <- bubble %>% html_element(css = '.transcript-line-speaker .font-weight-bold') %>% html_text(trim=TRUE)
  title <- bubble %>% html_element(css = '.transcript-line-speaker .secondary-title') %>% html_text(trim=TRUE)
  paragraphs <- bubble %>% html_elements(css = '.pb-2') %>% html_text(trim=TRUE)
  msg <- paste(paragraphs, collapse = '\n\n')
  
  bubble_df <- data.frame(speaker = speaker, title = title, msg = msg, stringsAsFactors = FALSE)
  
  return(bubble_df)
}

get_transcript_from_url <- function(url) {
  # Given an URL, return a dataframe containing the speech bubbles.
  page <- read_html(url)
  
  speech_bubbles <- page %>% html_elements(css = ".transcript-line-left, .transcript-line-right")
  
  results_list <- lapply(speech_bubbles, parse_bubble)
  
  # Combine the list of dataframes into a single dataframe
  df <- do.call(rbind, results_list)
  
  # Print the resulting dataframe
  return(df)
}

get_btn_from_exchange <- function(ticker, exchange) {
  url <- paste('https://www.marketbeat.com/stocks', exchange, ticker, '/earnings', sep = '/')
  page <- read_html(url)
  
  button <- page %>% html_element(css = '#cphPrimaryContent_cphTabContent_pnlSummary .green-button.w-100')
  return(button)
}

ticker_to_transcript <- function(ticker) {
  button = get_btn_from_exchange(ticker, 'NASDAQ')
  
  if (!length(button)) {
    print("Trying NYSE instead...")
    button = get_btn_from_exchange(ticker, 'NYSE')
  }
  
  if (!length(button)) {
    print(paste("Warning: Ticker", ticker,"not found or no transcript available"))
    return(NULL) 
  }
  
  transcript_url <- button %>% html_attr("href")
  
  return( get_transcript_from_url(transcript_url) )
}

# Example usage:
#ticker_to_transcript('AAPL') # NASDAQ listed
#ticker_to_transcript('HAL') # NYSE listed
#ticker_to_transcript('TSM') # NYSE listed, but no transcript available
#ticker_to_transcript('COOL') # doesn't exist

getTranscripts <- function(pth = "~/Desktop/HBS_execEd/GroupData/"){
  library(rvest)
  library(httr)
  
  # Get all directories for each group
  allGrps <- dir(pth, full.names = T)
  
  for(i in 1:length(allGrps)){
    print(paste('Working on:',i))
    # Get the stock info
    companyInfo <- list.files(path = allGrps[i], 
                              pattern = '.csv',
                              full.names = T)
    companyStk <- read.csv(companyInfo)$Symbol
    print(companyStk[i])
    for(j in 1:length(companyStk)){
      oneTranscript <- ticker_to_transcript(companyStk[j])
      Sys.sleep(1)
      nam <- paste0(allGrps[i],'/',companyStk[j],'_transcript_',Sys.Date(),'.csv')
      write.csv(oneTranscript, nam, row.names = F)
    }
  }
}


# Get the Stock list
sp500 <- getSP500()

# Obtain the pairs for groups
spPairs <- getSP500pairs(sp500, factorLevel = 'GICS.Sub.Industry')
getHLOC(spPairs, pth = "~/Desktop/HBS_execEDU/GroupData/")

# Add recent transcripts
getTranscripts(pth = "~/Desktop/HBS_execEDU/GroupData/")

