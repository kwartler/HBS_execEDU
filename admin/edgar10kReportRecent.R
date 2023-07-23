# Need to make it performant but for now just run through it
# Also need to make it navigate each folder, read in the csv work in that directory
# Target
#https://www.sec.gov/edgar/search/#/dateRange=custom&entityName=AAPL&startdt=2022-01-01&enddt=2023-07-01&filter_forms=10-K

# Define the function to get the report
# symbol can be ticker or CIK code but would need some adjustment due to padding, for company CIK, can be less than 10 0s, leading 0s are added
# start_at  #query parameter
# end_at #query parameter
# savePth #where to save this
# binaryDownloadChk # checks latest binaries every rsDriver call, should be adjusted from time to time
getRecent10KReport <- function(symbol='0000320193', 
                               start_at='2022-01-01',
                               end_at='2023-07-01', 
                               savePth = '~/Desktop/HBS_execEDU/tmp/', 
                               binaryDownloadChk  = F) { 
  # Libs
  library(RSelenium)
  library(rvest)
  library(stringr)
  
  
  
  # Start a new driver
  driver <- rsDriver(port= sample(7600)[1], browser=c("firefox"), chromever = NULL, verbose = F,
                     check = binaryDownloadChk)
  
  # Go to the SEC Edgar search page for the specified symbol and date range
  baseURL  <- 'https://www.sec.gov/edgar/search/#/dateRange=custom&entityName='
  edgarURL <- paste0(paste0(baseURL, symbol, "&startdt=", start_at, "&enddt=", end_at, "&filter_forms=10-K"))
  driver$client$navigate(edgarURL)
  
  
  # Wait for the page to load
  Sys.sleep(2)
  
  # Find the table of reports on the search results page
  trs <- driver$client$findElements(using = "xpath", "/html/body/div[3]/div[2]/div[2]/table/tbody/tr")
  if(length(trs)==0){
    driver$server$stop()
    stop('No search results were found, could be the ticker, or the date range did not have a 10k filing.')
  }
  
  # Get the most recent report
  title <- trs[[1]]$findElement(using = "xpath", '//*[@id="hits"]/table/tbody/tr[1]/td[4]')
  title <- title$getElementText()
    
  # Find the modal click
  click <- trs[[1]]$findChildElement(using = 'xpath', '//*[@id="hits"]/table/tbody/tr[1]/td[1]/a')
  click$clickElement()
  Sys.sleep(2)
    
  # Get the link of the file to download
  link <- driver$client$findElement(using = 'id' , "open-file")
  link <- link$getElementAttribute('href')

  print(paste('working on', title))
  
  # Wait for the file to load
  Sys.sleep(2)
    
  # Navigate to the report
  pg <- driver$client$navigate(link[[1]])
    
  # Get the page source (HTML content) of the report
  pgSrc <- driver$client$getPageSource(pg)
    
  # Dynamic create file name
  title <- gsub('/','',title)
  nam <- gsub('[.]|[(]|[)]','', title)
  nam <- gsub(' ','_', nam)
  nam <- paste0(nam,'_.html')
    
  # Create path 
  fullName <- paste0(savePth, nam)

  # Save File
  writeLines(pgSrc[[1]], fullName)

  # Stop the driver and close the browser
  driver$server$stop()
}

# Test
#getRecent10KReport("ALB", start_at='2022-01-01', end_at='2023-07-01')
#getRecent10KReport('WRB', start_at='2022-01-01', end_at='2023-07-01',savePth = '~/Desktop/HBS_execEDU/admin/tmp/')

#tix <- c('AAPL','CRM')
#lapply(tix, getRecent10KReport,  start_at='2022-01-01', end_at='2023-07-01')

# In case you want to scrape all S&P 500 companies
getSP500 <- function(cleanUp = T){
  
  library(dplyr)
  library(rvest)
  library(stringr)
  x <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>%
    html_nodes('#constituents') %>% html_table()
  x <- x[[1]]
  x$Symbol <- gsub('[.]','-',x$Symbol)
  
  # Profile Summary
  #wikiURLs <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% 
  #  html_nodes(xpath = '//td') %>% html_nodes("a") %>% html_attr("href")
  #grep('/wiki/', wikiURLs)
  
  
  # Clean up names
  names(x) <-  make.names(names(x))
  
  if(cleanUp==T){
    x <- x[!duplicated(x$CIK),]
    x$CIK <- stringr::str_pad(x$CIK, 10, pad = '0')
  }
  
  
  return(x)
}
df <- getSP500()


# Apply to sp500
lapply(df$CIK, 
       getRecent10KReport,
       start_at='2022-01-01', 
       end_at='2023-07-01',
       savePth = '~/Desktop/HBS_execEDU/groupCompanyData/10k_reports/')

grep('AVY', df$Symbol)
lapply(df$CIK[57:nrow(df)], 
       getRecent10KReport,
       start_at='2022-01-01', 
       end_at='2023-07-01',
       savePth = '~/Desktop/HBS_execEDU/groupCompanyData/10k_reports/')



# End
