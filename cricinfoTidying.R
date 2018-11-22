options(warn = -1)
library(tidyverse)
library(rebus)
library(lubridate)

#Load in data
Tests <- read_csv("AllTestMatches.csv")

#Rename columns that contain spaces
Tests <- Tests %>%
  rename(Team1 = `Team 1`,
         Team2 = `Team 2`,
         MatchDate = `Match Date`)

#Remove duplicate rows
Tests <- Tests %>%
  distinct()

# ---------------------------------------------- Separate margin column ---------------------------------

#Create a temp column
Tests$Temp <- NA
#Find innings wins and copy the relevant part across
Tests$Temp <- str_extract(Tests$Margin, pattern = "inns & ")
#Remove from Margin column 
Tests$Margin <- str_remove(Tests$Margin, "inns & ")
#Remove & from temp
Tests$Temp <- str_replace(Tests$Temp, "inns & ", "Innings")
#Separate margin column
Tests <- Tests %>%
  separate(Margin, c("Margin", "Type"), sep = " ")
#Move type into Temp
Tests$Temp <- if_else(is.na(Tests$Temp), Tests$Type, Tests$Temp)
#Tidy up
Tests <- Tests %>%
  select(Team1, Team2, Winner, Margin, Temp, Ground, MatchDate, Scorecard) %>%
  rename(HomeTeam = Team1,
         AwayTeam = Team2,
         Type = Temp)


# Capitalise d in drawn in Winner column
Tests$Winner <- str_replace(Tests$Winner, "drawn", "Drawn")
Tests$Winner <- str_replace(Tests$Winner, "tied", "Tied")
Tests$Type <- str_replace(Tests$Type, "runs", "Runs")
Tests$Type <- str_replace(Tests$Type, "wickets", "Wickets")

#---------------------------------------- Dealing with date column ----------------------------------------------------------------
# Separate into a few columns and creata a temporary table
Tests <- Tests %>%
  separate(MatchDate, c("StartMonth", "RoD"), sep = " ", extra = "merge") %>%
  separate(RoD, c("RoD", "StartYear"), sep = ", ", extra = "merge") %>%
  separate(StartYear, c("StartYear", "YearWrap"), sep = " - ", extra = "merge") %>%
  separate(YearWrap, c("EndMonth", "Rest"), sep = " ", extra = "merge") %>%
  separate(Rest, c("EndDay", "EndYear"), sep = ", ", extra = "merge") %>%
  separate(RoD, c("StartDay", "RoD"), sep = "-", extra = "merge")

#Copy start year over to end year provided test doesn't span accross a year
Tests$EndYear <- if_else(is.na(Tests$EndYear), Tests$StartYear, Tests$EndYear)

#Create a temporary column containing NAs
Tests$Temp <- NA

#Find rows in RoD which contain a month and a day
Tests$Temp <- str_extract(Tests$RoD, pattern = START %R% WRD %R% WRD %R% WRD)

#Remove them from RoD
Tests$RoD <- str_remove(Tests$RoD, pattern = START %R% WRD %R% WRD %R% WRD %R% SPC)

#Move date parts into the right columns
Tests$EndMonth <- if_else(is.na(Tests$EndMonth), Tests$Temp, Tests$EndMonth)
Tests$EndMonth <- if_else(is.na(Tests$EndMonth), Tests$StartMonth, Tests$EndMonth)
Tests$EndDay <- if_else(is.na(Tests$EndDay), Tests$RoD, Tests$EndDay)

#Combine date parts into 2 columns and remove parts
Tests <- Tests %>%
  mutate(StartDate = paste(StartDay, StartMonth, StartYear),
         EndDate = paste(EndDay, EndMonth, EndYear)) %>%
  select(StartDate, EndDate, HomeTeam, AwayTeam, Winner, Margin, Type, Ground, Scorecard)

#Parse dates
Tests$StartDate <- dmy(Tests$StartDate)
Tests$EndDate <- dmy(Tests$EndDate)

#Sense check
Tests %>%
  mutate(DateDiff = EndDate - StartDate + 1) %>%
  count(DateDiff, sort = TRUE)

#------------------------------------------------------------------------------------------------------ 

TestsProcessed <- Tests

write_csv(TestsProcessed, "TestsProcessed.csv")