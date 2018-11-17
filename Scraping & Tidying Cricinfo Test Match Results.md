Cricinfo Test Match Result Data
=============
Background
----------------------

ESPNcricinfo is an online cricket news site, which covers matches all around the world live, and keeps a database of historical results and stats dating back to the 19th century. On its website, cricinfo stores the results and scorecard of every international cricket match ever (as far as i'm aware). A task I set for myself was to scrape out the results of all test matches (2325 at the time of writing this) into a a single data frame and manipulate the data into a tidy format. Cricinfo stores all test matches on their test match records [page](http://stats.espncricinfo.com/ci/engine/records/index.html?class=1) by year so the task is to obtain the urls for each year from the by year [page](http://stats.espncricinfo.com/ci/content/records/307847.html) and scrape out the relevant information as text.

Step 1 - Scraping 
----------

The first step is to obtain a URL for each year. The rvest package makes this pretty easy to do.
```r
library(rvest)
library(tidyverse)

#Store the URL we want to scrape
yearsUrl <- "http://stats.espncricinfo.com/ci/content/records/307847.html"

#Use selector gadget to find the nodes we want and get those URLs!
years <- read_html(yearsUrl) %>%
  html_nodes(".QuoteSummary") %>%
  html_attr("href") %>%
  as.character()
  ```
  The URLs, or at least the variations on the end part of the URL are now stored in a character vector called years. Next, we need to paste them onto the end of cricinfo's base URL and loop through and get the table of results for each year:
```r
baseURL <- "http://stats.espncricinfo.com"

#Create an emtpy data frame for us to fill
allData <- tibble()

#Now we write a for loop to iterate and get the data we want
for(year in years){
  Sys.sleep(1) #To prevent denial of service
  constructedURL <- paste0(baseURL, year) 
  print(constructedURL) 
  
  #Identify the name of the table class extract it as text
  yearData <- read_html(constructedURL) %>%
    html_node(".engineTable") %>%
    html_table()
  allData <- rbind(allData, yearData)  #Finally, add new set of rows to the end of our empty data frame
}
```
We now have a data frame with well over 2000 test matches which contains both teams, the winner and the margin of victory, the ground, the date and a column called scorecard which was originally a link but will serve well as a unique identifer for each match here. The next step is to save our data frame as a csv, so we don't have to scrape it again, and then tidy it up a bit. 
```r
write_csv(allData, "AllTestMatches.csv")
```
Step 2 - Tidying 
----------------
We have our data now but there a few issues that need to be dealt with if we want to do some analysis. Firstly, the 'Margin' probably needs to be split into a numerical and a categorical column i.e. a column for the margin of victory and one for the type of victory (runs wickets or by an innings). Drawn matches also need to have an NA in each of these columns rathern than just an empty string however parsing the data using read_csv will do this automatically. The largest issue is the 'Match Date' column, which represents dates in 3 different formats - matches which are within one month, matches which span 2 months and matches which span 2 years. Matches which span 2 years have two entries each, since they appear on two different pages, so those duplicates will have to be removed. Finally, there are a few less important things like capitalisations which could be tidyied up.

```r
#Load in the packages we will need and the data
library(tidyverse)
library(rebus)
library(lubridate)

Tests <- read_csv("AllTestMatches.csv")

#Remove spaces from column names
Tests <- Tests %>%
  rename(Team1 = `Team 1`,
         Team2 = `Team 2`,
         MatchDate = `Match Date`)
         
#Remove duplicate rows
Tests <- Tests %>%
  distinct()
  ```
  
Now we can separate the margin column into the margin of victory and the type (runs, wickets, innings). The challenge of this is that whereas innings and runs victories can easily be split on their space, 'inns' needs to be transferred from the left side of the margin column into a new column on the right whilst keeping the integer part on the left. The method I used to do this was to create a temporary column and extract the string 'inns' and place it in the equivalent row in the temp column. The margin column can then be split.

```r
#Create a temp column
Tests$Temp <- NA
#Find innings wins and copy the relevant part across
Tests$Temp <- str_extract(Tests$Margin, pattern = "inns & ")
#Remove from Margin column 
Tests$Margin <- str_remove(Tests$Margin, "inns & ")
```
Innings victories are now indistinguishable from those by runs but for the fact that we have an entry in our temp column, so we need to separate the margin column and transfer victory type to the temp column for runs and wickets only, so that it can be our new type column

```r
#Remove the & from inns in temp column
Tests$Temp <- str_replace(Tests$Temp, "inns & ", "Innings")
#Separate margin column into Margin and Type
Tests <- Tests %>%
  separate(Margin, c("Margin", "Type"), sep = " ")
  
#At this point we need to fill in our NAs in the temp column with runs and wickets (where applicable)

#Move type into Temp
Tests$Temp <- if_else(is.na(Tests$Temp), Tests$Type, Tests$Temp)
  
#Drop Type column and rename temp and a couple of other columns
Tests <- Tests %>%
  select(Team1, Team2, Winner, Margin, Temp, Ground, MatchDate, Scorecard) %>%
  rename(HomeTeam = Team1,
         AwayTeam = Team2,
         Type = Temp)
  
# A few capitalisations
Tests$Winner <- str_replace(Tests$Winner, "drawn", "Drawn")
Tests$Winner <- str_replace(Tests$Winner, "tied", "Tied")
Tests$Type <- str_replace(Tests$Type, "runs", "Runs")
Tests$Type <- str_replace(Tests$Type, "wickets", "Wickets")
```
We are now at this point:

![](https://github.com/TimHoare/CricinfoStuff/blob/master/cricinfogithubimg1.png)

The final challenge is to separate, reformat and parse the date column into a start date, and an end date. As mentioned earlier, the difficulty is dealing with the multiple date formats in this column. The method I have used here is to split the column into 6 parts: the day, month and year of the start and end date. 

Next follows quite a long pipe-chain of separates where I try to split the MatchDate column into as many new columns as possible. A comment is included under each to explain what is happening. The argument extra = "merge" ensures only the column is only split on the first occurence of the delimiter.
```r
Tests <- Tests %>%
  separate(MatchDate, c("StartMonth", "RoD"), sep = " ", extra = "merge") %>%
  #Split on the first space. Start (and end in most cases) month on the right, rest of date (RoD) on the left
  
  separate(RoD, c("RoD", "StartYear"), sep = ", ", extra = "merge") %>%
  #Split RoD on the first comma-space. Year is now on the far right and the rest in the middle
  
  separate(StartYear, c("StartYear", "YearWrap"), sep = " - ", extra = "merge") %>%
  #New year column split to move matches that span 2 years
  #The next 2 lines split those dates into 3
  
  separate(YearWrap, c("EndMonth", "Rest"), sep = " ", extra = "merge") %>%
  separate(Rest, c("EndDay", "EndYear"), sep = ", ", extra = "merge") %>% 
 
  separate(RoD, c("StartDay", "RoD"), sep = "-", extra = "merge")
  #Finally, we can return to our RoD column and get a start date column
```
Our data now looks like this:

![](https://github.com/TimHoare/CricinfoStuff/blob/master/cricinfogithubimg2.png)


  
  
  
  
  
  
  
  
  
  
  
  
  
