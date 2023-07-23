# Install required packages (if not already installed)
#install.packages("RSelenium")
#install.packages("rvest")
#install.packages("stringr")

# Load required libraries
library(RSelenium)
library(rvest)
library(stringr)

symbol='AAPL'
start_at='2023-01-01'
end_at='2023-07-01'
savePth = '~/Desktop/HBS_execEDU/tmp/'

# Define the function to get the report
get_report <- function(symbol='AAPL', start_at='2023-01-01', end_at='2023-07-01', savePth = '~/Desktop/HBS_execEDU/tmp/') {
  # Start a new driver
  driver <- rsDriver(port= sample(7600)[1], browser=c("firefox"), chromever = NULL)

  # Go to the SEC Edgar search page for the specified symbol and date range
  driver$client$navigate(paste0("https://www.sec.gov/edgar/search/#/q=", symbol, "&dateRange=custom&startdt=", start_at, "&enddt=", end_at, "&filter_forms=10-K"))

  # Wait for the page to load
  Sys.sleep(2)

  # Find the table of reports on the search results page
  trs <- driver$client$findElements(using = "xpath", "/html/body/div[3]/div[2]/div[2]/table/tbody/tr")

  # For each report, click on the link, get the title, and download the file
  for (i in 1:length(trs)) {
    title <- trs[[i]]$findElement(using = "xpath", '//*[@id="hits"]/table/tbody/tr[1]/td[4]')
    title <- title$getElementText()
    
    # Find the modal click
    #click <- trs[[i]]$findElement(using = "xpath", './/*[@id="hits"]/table/tbody/tr[1]/td[1]/a')
    click <- trs[[i]]$findChildElement(using = 'xpath', '//*[@id="hits"]/table/tbody/tr[1]/td[1]/a')
    #click$click()
    click$clickElement()
    Sys.sleep(2)
    
    # Get the link of the file to download
    link <- driver$client$findElement(using = 'id' , "open-file")
    link <- link$getElementAttribute('href')
    link
    
    # Click the element link
    #click$click()
    #click$clickElement()
    
    print(i)
    # Wait for the file to load
    Sys.sleep(2)
    
    # Navigate to the report
    pg <- driver$client$navigate(link[[1]])
    
    # Get the page source (HTML content) of the report
    pgSrc <- pg$client$getPageSource(pg)
    
    # Dynamic create file name
    nam <- paste0(symbol,'_',title,'_.html')

    # Create path 
    fullName <- paste0(savePth, nam)
    
    # Save File
    writeLines(pgSrc[[1]], fullName)
    
    # Navigate back to original page
    driver$client$navigate(paste0("https://www.sec.gov/edgar/search/#/q=", symbol, "&dateRange=custom&startdt=", start_at, "&enddt=", end_at, "&filter_forms=10-K"))
    
    Sys.sleep(2)
    
    # Close the report viewer modal
    #driver$client$close()

    # Close the modal dialog box
    #close <- driver$client$findElement(using = 'id', "close-modal")
    #close$click()
  }

  # Stop the driver and close the browser
  driver$client$stop()
}

# Get the report for the specified symbol and date range (e.g., "APP" from January 1, 2023, to July 1, 2023)
tmp <- get_report("APPL", "2023-01-01", "2023-07-01")

# In case you want to scrape all S&P 500 companies
# df <- read_csv("path/symbol.csv")
# for (symbol in df$symbol) {
#   get_report(symbol)
# }
