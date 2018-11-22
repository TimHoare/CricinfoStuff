library(tidyverse)
library(rvest)
library(rebus)

yearsUrl <- "http://stats.espncricinfo.com/ci/content/records/307847.html"

years <- read_html(yearsUrl) %>%
  html_nodes(".QuoteSummary") %>%
  html_attr("href") %>%
  as.character()

baseURL <- "http://stats.espncricinfo.com"


allData <- tibble()


for(year in years){
  Sys.sleep(1)
  constructedURL <- paste0(baseURL, year)
  print(constructedURL)
  
  yearData <- read_html(constructedURL) %>%
    html_node(".engineTable") %>%
    html_table()
  allData <- rbind(allData, yearData)
  
}

write_csv(allData, "AllTestMatches.csv")












