###LOAD LIBRARIES

library(tidyverse)
library(janitor)
library(tidyr)
library(lubridate)
library(geojsonsf)
library(sf)
library(rjson)
library(jsonlite)

#First need to load historical dataset locally or from Github

#Routinely will load from historicalcrime.csv that was exported during previous run of the script
historicalcrime <- read_csv("historicalcrime.csv",
                            col_types = cols(
                              display_time = col_character()
                            ))

#Next need to update the historical data

crime_file <- "https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/NIBRS_GroupA_Crime_Data/FeatureServer/0/query?where=%20(CrimeDateTime%20%3E%3D%20%272025-01-19%27%20AND%20CrimeDateTime%20%20%3C%3D%20%272025-01-26%27)%20&outFields=*&returnGeometry=false&outSR=4326&f=json"

#Get date of last record in historical data that occurs at least 30 times
crime_counts <- historicalcrime %>% count(crime_date, name = "n")
frequent_crime_dates <- crime_counts %>% filter(n >= 30) %>% pull(crime_date)
histdupes <- historicalcrime %>% filter(crime_date %in% frequent_crime_dates)

lastrechist <- max(histdupes$crime_date)

#json query for the seven days after last record
oneweekfstart <- lastrechist + days(1)
oneweekfend <- lastrechist + days(7)
oneweekf <- paste("https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/NIBRS_GroupA_Crime_Data/FeatureServer/0/query?where=%20(CrimeDateTime%20%3E%3D%20%27", oneweekfstart, "%27%20AND%20CrimeDateTime%20%20%3C%3D%20%27", oneweekfend, "%27)%20&outFields=*&returnGeometry=false&outSR=4326&f=json", sep="")
oneweekf <- fromJSON(oneweekf, flatten = TRUE)
oneweekf <- oneweekf$features 

# json query for the seven days after that (eight day after last record 14th day after last record)
twoweekfstart <- lastrechist + days(8)
twoweekfend <- lastrechist + days(14)
twoweekf <- paste("https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/NIBRS_GroupA_Crime_Data/FeatureServer/0/query?where=%20(CrimeDateTime%20%3E%3D%20%27", twoweekfstart, "%27%20AND%20CrimeDateTime%20%20%3C%3D%20%27", twoweekfend, "%27)%20&outFields=*&returnGeometry=false&outSR=4326&f=json", sep="")
twoweekf <- fromJSON(twoweekf, flatten = TRUE)
twoweekf <- twoweekf$features 

# json query for the day of the last record plus the previous six
oneweekbstart <- lastrechist - days(6)
oneweekbend <- lastrechist
oneweekb <- paste("https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/NIBRS_GroupA_Crime_Data/FeatureServer/0/query?where=%20(CrimeDateTime%20%3E%3D%20%27", oneweekbstart, "%27%20AND%20CrimeDateTime%20%20%3C%3D%20%27", oneweekbend, "%27)%20&outFields=*&returnGeometry=false&outSR=4326&f=json", sep="")
oneweekb <- fromJSON(oneweekb, flatten = TRUE)
oneweekb <- oneweekb$features 

#json query for the seven days before that (13 days before last record through seven days before last record)
twoweekbstart <- lastrechist - days(13)
twoweekbend <- lastrechist - days(7)
twoweekb <- paste("https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/NIBRS_GroupA_Crime_Data/FeatureServer/0/query?where=%20(CrimeDateTime%20%3E%3D%20%27", twoweekbstart, "%27%20AND%20CrimeDateTime%20%20%3C%3D%20%27", twoweekbend, "%27)%20&outFields=*&returnGeometry=false&outSR=4326&f=json", sep="")
twoweekb <- fromJSON(twoweekb, flatten = TRUE)
twoweekb <- twoweekb$features

#json query for the seven days before that (20 days before last record through 14 days before last record)
threeweekbstart <- lastrechist - days(20)
threeweekbend <- lastrechist - days(14)
threeweekb <- paste("https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/NIBRS_GroupA_Crime_Data/FeatureServer/0/query?where=%20(CrimeDateTime%20%3E%3D%20%27", threeweekbstart, "%27%20AND%20CrimeDateTime%20%20%3C%3D%20%27", threeweekbend, "%27)%20&outFields=*&returnGeometry=false&outSR=4326&f=json", sep="")
threeweekb <- fromJSON(threeweekb, flatten = TRUE)
threeweekb <- threeweekb$features

#json query for the seven days before that (27 days before last record through 21 days before last record)
fourweekbstart <- lastrechist - days(27)
fourweekbend <- lastrechist - days(21)
fourweekb <- paste("https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/NIBRS_GroupA_Crime_Data/FeatureServer/0/query?where=%20(CrimeDateTime%20%3E%3D%20%27", fourweekbstart, "%27%20AND%20CrimeDateTime%20%20%3C%3D%20%27", fourweekbend, "%27)%20&outFields=*&returnGeometry=false&outSR=4326&f=json", sep="")
fourweekb <- fromJSON(fourweekb, flatten = TRUE)
fourweekb <- fourweekb$features

#Combine the data
crime_data <-  bind_rows(oneweekb, oneweekf, twoweekb, twoweekf, threeweekb, fourweekb) %>% clean_names()
colnames(crime_data)<-gsub("attributes_","",colnames(crime_data))

#MAKE DATES USEABLE
#DISABLE SCIENTIFIC NOTATION
options(scipen = 999)
#DIVIDE DATES BY 1000 TO CONVERT TO UNIX TIMESTAMP
######ERROR BELOW OBJECT crime_date_time not found
crime_data <- crime_data %>% mutate(crime_date_time = crime_date_time / 1000)
#FORMAT DATES INTO READABLE FORMAT
crime_data$crime_date_time <- format(as_datetime(crime_data$crime_date_time), "%y-%m-%d %H:%M")
#CREATE SEPARATE DATE AND TIME COLUMNS
crime_data <- crime_data %>% separate(crime_date_time, c("crime_date", "crime_time"), " ")
#CONVERT DATE COLUMN TO DATE CLASS
crime_data$crime_date <- ymd(crime_data$crime_date)
#MAKE NEW DISPLAY TIME COLUMN IN 12-HOUR TIME
crime_data$crime_time <- strptime(crime_data$crime_time, format = "%H:%M")
crime_data$crime_time <- strftime(crime_data$crime_time, "%I:%M %p")
colnames(crime_data)[4] <- "display_time"
crime_data <- crime_data %>% relocate(display_time, .after=total_incidents)

#TAKE COLUMNS WE NEED
crime_data = select(crime_data, row_id,cc_number,crime_date,description,inside_outside,weapon,shooting,post,gender,age,race,ethnicity,location,old_district,new_district,neighborhood,latitude,longitude,premise_type,total_incidents,display_time)
#CONVERT WORKING DATA TIBBLE TO DATAFRAME
crime_data <- as.data.frame(crime_data)
#CHANGE COLUMN TYPES TO MATCH HISTORICAL DATA
crime_data$row_id <- as.numeric(crime_data$row_id)
crime_data$age <- as.numeric(crime_data$age)
crime_data$latitude <- as.numeric(crime_data$latitude)
crime_data$longitude <- as.numeric(crime_data$longitude)
crime_data$total_incidents <- as.numeric(crime_data$total_incidents)

#MAKE FILTERED VIEW OF HISTORICAL TO REMOVE LAST FOUR WEEKS
histfilter <- historicalcrime$crime_date < fourweekbstart 
hist_filtered <- historicalcrime %>% filter(histfilter)

#MATCH HIST_FILTERED COLUMN TYPES TO CRIME_DATA COLUMN TYPES
hist_filtered$cc_number<-as.character(hist_filtered$cc_number)
hist_filtered$post<-as.character(hist_filtered$post)
hist_filtered$display_time<-as.character(hist_filtered$display_time)

#NEXT, MERGE FILTERED HISTORICAL AND WORKING DATAFRAMES
merged_data <-  bind_rows(hist_filtered, crime_data)

#ACCOUNT FOR NEIGHBORHOOD NAMES THAT CHANGED AROUND OCT. 2024
merged_data <- merged_data %>%
  mutate(
    neighborhood = case_when(
      neighborhood == "PARKVIEW/WOODBROOK" ~ "AUCHENTOROLY-PARKWOOD",
      neighborhood == "NEW SOUTHWEST/MOUNT CLARE" ~ "MOUNT CLARE",
      neighborhood == "GLENHAM-BELHAR" ~ "HAMILTON",
      neighborhood == "BUTCHER'S HILL" ~ "BUTCHERS HILL",
      TRUE ~ neighborhood  # keep all other values unchanged
    )
  )

#STOP TO EXPORT MERGED DATA AS A CSV TO BE THE HISTORICAL DATA THE NEXT TIME THE SCRIPT RUNS
write.csv(merged_data, "historicalcrime.csv",  row.names=FALSE)

#FILTERING

#add violent crime column that's a Y if a record is a violent crime
merged_data$violent <- ifelse(merged_data$description == "HOMICIDE" | merged_data$description == "RAPE" | merged_data$description == "ROBBERY" | merged_data$description == "AGG. ASSAULT" | merged_data$description == "ROBBERY - COMMERCIAL" | merged_data$description == "ROBBERY - CARJACKING", "Y", "N")

#add property crime column that's a Y if a record is a property crime
merged_data$property <- ifelse(merged_data$description == "BURGLARY" | merged_data$description == "ARSON" | merged_data$description == "LARCENY FROM AUTO" | merged_data$description == "LARCENY" | merged_data$description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | merged_data$description == "SHOPLIFTING" | merged_data$description == "AUTO THEFT", "Y", "N")


#GET DATE OF MOST RECENT DATA TO BASE FILTERS ON (latest date that occurs at least 30 times)
crime_counts <- merged_data %>% count(crime_date, name = "n")
frequent_crime_dates <- crime_counts %>% filter(n >= 30) %>% pull(crime_date)
nowdupes <- merged_data %>% filter(crime_date %in% frequent_crime_dates)

lastrec <- max(nowdupes$crime_date)

#CREATE DATAFRAMES OF MOST RECENT TWO WEEKS AND TWO WEEKS PRECEDING THAT (after 7-day lag)
LastTwoWeeks = merged_data$crime_date >= lastrec - days(20) & merged_data$crime_date <= lastrec - days(7)
TwoWeeksPrior = merged_data$crime_date >= lastrec - days(34) & merged_data$crime_date <= lastrec - days(21)
recentnow <- merged_data %>% filter(LastTwoWeeks)
recentprior <- merged_data %>% filter(TwoWeeksPrior)

#CREATE DATAFRAME OF YEAR TO DATE AND THRU SAME DATE LAST YEAR (after 7-day lag )
thisyear <- format(as.Date(max(merged_data$crime_date)), "%Y")
YearToDate = merged_data$crime_date >= make_date(thisyear, 1, 1) & merged_data$crime_date <= lastrec - days(7)
prevyearstart <- make_date(as.numeric(thisyear)-1, 1, 1)
prevyearend <- make_date(as.numeric(thisyear)-1, format(lastrec - days(7),format="%m"), format(lastrec - days(7),format="%d"))
PriorYearToDate = merged_data$crime_date >= prevyearstart & merged_data$crime_date <= prevyearend

#THIS IS DF FOR CURRENT YEAR TO DATE
ytdnow <- merged_data %>% filter(YearToDate)

#THIS IS DF FOR PRIOR YEAR TO THE SAME POINT
ytdlast <- merged_data %>% filter(PriorYearToDate)


#PIVOTING

###LAST TWO WEEKS BY CATEGORY
rnowtbl <- recentnow %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING") %>% tabyl(description)
#remove the percentage column
rnowtbl <- rnowtbl[,-3]
#rename coumn with M/D-M/D for last two weeks minus 7 days for reporting lag
tempcolname <- paste(str_remove(format(as.Date(lastrec - days(20), '%Y-%m-%d'), "%m/%d"), "^0+"),"-",str_remove(format(as.Date(lastrec - days(7), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
tempcolname <- gsub("/0+", "/", tempcolname)
colnames(rnowtbl)[2] <- tempcolname

# Check if "ARSON" exists in the first column
if (!("ARSON" %in% rnowtbl[, 1])) {
  # Create a new row with the correct column names
  new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(rnowtbl)))
  names(new_row) <- names(rnowtbl) # Set the column names to match rnowtbl
  
  # Populate the new row
  new_row[1, 1] <- "ARSON"
  new_row[1, 2] <- "0"
  
  # Append the new row to the dataframe
  rnowtbl <- rbind(rnowtbl, new_row)
}


# Check if "HOMICIDE" exists in the first column
if (!("HOMICIDE" %in% rnowtbl[, 1])) {
  # Create a new row with the correct column names
  new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(rnowtbl)))
  names(new_row) <- names(rnowtbl) # Set the column names to match rnowtbl
  
  # Populate the new row
  new_row[1, 1] <- "HOMICIDE"
  new_row[1, 2] <- "0"
  
  # Append the new row to the dataframe
  rnowtbl <- rbind(rnowtbl, new_row)
}


# Check if "RAPE" exists in the first column
if (!("RAPE" %in% rnowtbl[, 1])) {
  # Create a new row with the correct column names
  new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(rnowtbl)))
  names(new_row) <- names(rnowtbl) # Set the column names to match rnowtbl
  
  # Populate the new row
  new_row[1, 1] <- "RAPE"
  new_row[1, 2] <- "0"
  
  # Append the new row to the dataframe
  rnowtbl <- rbind(rnowtbl, new_row)
}


#change second column back to number
rnowtbl[,2] <- as.numeric(rnowtbl[,2])

#sum component robberies and larcenies and overwrite "larceny" and "robbery" rows with composite totals
rnowtbl <- rnowtbl %>% arrange(description)
robttl <- sum(rnowtbl[10,2], rnowtbl[11,2], rnowtbl[12,2])
#overwrite robbery row
rnowtbl[10,2] <- robttl
#remove carjacking and commerical rows
rnowtbl <- rnowtbl[-c(11,12),]

larcttl <- sum(rnowtbl[6,2], rnowtbl[7,2], rnowtbl[8,2], rnowtbl[11,2])
#overwrite larceny row
rnowtbl[6,2] <- larcttl
#remove from auto, m/v parts, shoplifting rows
rnowtbl <- rnowtbl[-c(7,8,11),]

#change descriptions to sentence case
rnowtbl$description <- str_to_sentence(rnowtbl$description)

#add columns for total violent crime, shooting, total property crime, total crime
rnowtbl <- rnowtbl %>% arrange(description)
violttl <- sum(rnowtbl[1,2], rnowtbl[5,2], rnowtbl[7,2], rnowtbl[8,2])
rnowtbl[nrow(rnowtbl) + 1,] <- list('Violent crime total', violttl)
propttl <- sum(rnowtbl[2,2], rnowtbl[3,2], rnowtbl[4,2], rnowtbl[6,2])
rnowtbl[nrow(rnowtbl) + 1,] <- list('Property crime total', propttl)
ttl <- sum(rnowtbl[9,2], rnowtbl[10,2])
rnowtbl[nrow(rnowtbl) + 1,] <- list('Total', ttl)

#summing shootings
shootings <- length(which(recentnow$shooting == "Y"))
rnowtbl[nrow(rnowtbl) + 1,] <- list('Shooting', shootings)
#order columns for reader-facing table
x <- c("Agg. Assault", "Homicide", "Rape", "Robbery", "Violent crime total", "Shooting", "Arson", "Auto theft", "Burglary", "Larceny", "Property crime total", "Total")
rnowtbl <- rnowtbl %>% slice(match(x, description))

###PRIOR TWO WEEKS BY CATEGORY

rpriortbl <- recentprior %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING") %>% tabyl(description)
#remove the percentage column
rpriortbl <- rpriortbl[,-3]
tempcolname <- paste(str_remove(format(as.Date(lastrec - days(34), '%Y-%m-%d'), "%m/%d"), "^0+"),"-",str_remove(format(as.Date(lastrec - days(21), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
tempcolname <- gsub("/0+", "/", tempcolname)
#rename coumn with M/D-M/D for prior two weeks minus 7 days
colnames(rpriortbl)[2] <- tempcolname

# Check if "ARSON" exists in the first column
if (!("ARSON" %in% rpriortbl[, 1])) {
  # Create a new row with the correct column names
  new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(rpriortbl)))
  names(new_row) <- names(rpriortbl) # Set the column names to match rpriortbl
  
  # Populate the new row
  new_row[1, 1] <- "ARSON"
  new_row[1, 2] <- "0"
  
  # Append the new row to the dataframe
  rpriortbl <- rbind(rpriortbl, new_row)
}


# Check if "HOMICIDE" exists in the first column
if (!("HOMICIDE" %in% rpriortbl[, 1])) {
  # Create a new row with the correct column names
  new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(rpriortbl)))
  names(new_row) <- names(rpriortbl) # Set the column names to match rpriortbl
  
  # Populate the new row
  new_row[1, 1] <- "HOMICIDE"
  new_row[1, 2] <- "0"
  
  # Append the new row to the dataframe
  rpriortbl <- rbind(rpriortbl, new_row)
}


# Check if "RAPE" exists in the first column
if (!("RAPE" %in% rpriortbl[, 1])) {
  # Create a new row with the correct column names
  new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(rpriortbl)))
  names(new_row) <- names(rpriortbl) # Set the column names to match rpriortbl
  
  # Populate the new row
  new_row[1, 1] <- "RAPE"
  new_row[1, 2] <- "0"
  
  # Append the new row to the dataframe
  rpriortbl <- rbind(rpriortbl, new_row)
}



#change second column back to number
rpriortbl[,2] <- as.numeric(rpriortbl[,2])

#sum component robberies and larcenies and overwrite "larcey" and "robbery" rows with composite totals
rpriortbl <- rpriortbl %>% arrange(description)
robttl <- sum(rpriortbl[10,2], rpriortbl[11,2], rpriortbl[12,2])
#overwrite robbery row
rpriortbl[10,2] <- robttl
#remove carjacking and commerical rows
rpriortbl <- rpriortbl[-c(11,12),]
larcttl <- sum(rpriortbl[6,2], rpriortbl[7,2], rpriortbl[8,2], rpriortbl[11,2])
#overwrite larceny row
rpriortbl[6,2] <- larcttl
#remove from auto, m/v parts, shoplifting rows
rpriortbl <- rpriortbl[-c(7,8,11),]

#change descriptions to sentence case
rpriortbl$description <- str_to_sentence(rpriortbl$description)

#add columns for total violent crime, shooting, total property crime, total crime
rpriortbl <- rpriortbl %>% arrange(description)
violttl <- sum(rpriortbl[1,2], rpriortbl[5,2], rpriortbl[7,2], rpriortbl[8,2])
rpriortbl[nrow(rpriortbl) + 1,] <- list('Violent crime total', violttl)
propttl <- sum(rpriortbl[2,2], rpriortbl[3,2], rpriortbl[4,2], rpriortbl[6,2])
rpriortbl[nrow(rpriortbl) + 1,] <- list('Property crime total', propttl)
ttl <- sum(rpriortbl[9,2], rpriortbl[10,2])
rpriortbl[nrow(rpriortbl) + 1,] <- list('Total', ttl)

#summing shootings
shootings <- length(which(recentprior$shooting == "Y"))
rpriortbl[nrow(rpriortbl) + 1,] <- list('Shooting', shootings)

#order columns for reader-facing table
rpriortbl <- rpriortbl %>% slice(match(x, description))

###YEAR TO DATE BY CATEGORY

ytdnowtbl <- ytdnow %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING") %>% tabyl(description)
#remove the percentage column
ytdnowtbl <- ytdnowtbl[,-3]
#rename coumn with YYYY through M/D via lastrec - 7 days
tempcolname <- paste(thisyear," thru ",str_remove(format(as.Date(lastrec - days(7), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
tempcolname <- gsub("/0+", "/", tempcolname)
colnames(ytdnowtbl)[2] <- tempcolname

#sum component robberies and larcenies and overwrite "larcey" and "robbery" rows with composite totals
ytdnowtbl <- ytdnowtbl %>% arrange(description)
robttl <- sum(ytdnowtbl[10,2], ytdnowtbl[11,2], ytdnowtbl[12,2])
#overwrite robbery row
ytdnowtbl[10,2] <- robttl
#remove carjacking and commerical rows
ytdnowtbl <- ytdnowtbl[-c(11,12),]
larcttl <- sum(ytdnowtbl[6,2], ytdnowtbl[7,2], ytdnowtbl[8,2], ytdnowtbl[11,2])
#overwrite larceny row
ytdnowtbl[6,2] <- larcttl
#remove from auto, m/v parts, shoplifting rows
ytdnowtbl <- ytdnowtbl[-c(7,8,11),]
#change descriptions to sentence case
ytdnowtbl$description <- str_to_sentence(ytdnowtbl$description)

#add columns for total violent crime, shooting, total property crime, total crime
ytdnowtbl <- ytdnowtbl %>% arrange(description)
violttl <- sum(ytdnowtbl[1,2], ytdnowtbl[5,2], ytdnowtbl[7,2], ytdnowtbl[8,2])
ytdnowtbl[nrow(ytdnowtbl) + 1,] <- list('Violent crime total', violttl)
propttl <- sum(ytdnowtbl[2,2], ytdnowtbl[3,2], ytdnowtbl[4,2], ytdnowtbl[6,2])
ytdnowtbl[nrow(ytdnowtbl) + 1,] <- list('Property crime total', propttl)
ttl <- sum(ytdnowtbl[9,2], ytdnowtbl[10,2])
ytdnowtbl[nrow(ytdnowtbl) + 1,] <- list('Total', ttl)

#summing shootings
shootings <- length(which(ytdnow$shooting == "Y"))
ytdnowtbl[nrow(ytdnowtbl) + 1,] <- list('Shooting', shootings)
ytdnowtbl <- ytdnowtbl %>% slice(match(x, description))

###PRIOR YEAR TO SAME POINT BY CATEGORY
ytdlasttbl <- ytdlast %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING") %>% tabyl(description)
#remove the percentage column
ytdlasttbl <- ytdlasttbl[,-3]
#rename coumn with YYYY through M/D via lastrec -1 year - 7 days
tempcolname <- paste(format(as.Date(lastrec - years(1) - days(7), '%Y-%m-%d'), "%Y")," thru ",str_remove(format(as.Date(lastrec - days(7), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
tempcolname <- gsub("/0+", "/", tempcolname)
colnames(ytdlasttbl)[2] <- tempcolname
#sum component robberies and larcenies and overwrite "larcey" and "robbery" rows with composite totals
ytdlasttbl <- ytdlasttbl %>% arrange(description)
robttl <- sum(ytdlasttbl[10,2], ytdlasttbl[11,2], ytdlasttbl[12,2])
#overwrite robbery row
ytdlasttbl[10,2] <- robttl
#remove carjacking and commerical rows
ytdlasttbl <- ytdlasttbl[-c(11,12),]
larcttl <- sum(ytdlasttbl[6,2], ytdlasttbl[7,2], ytdlasttbl[8,2], ytdlasttbl[11,2])
#overwrite larceny row
ytdlasttbl[6,2] <- larcttl
#remove from auto, m/v parts, shoplifting rows
ytdlasttbl <- ytdlasttbl[-c(7,8,11),]

#change descriptions to sentence case
ytdlasttbl$description <- str_to_sentence(ytdlasttbl$description)

#add columns for total violent crime, shooting, total property crime, total crime
ytdlasttbl <- ytdlasttbl %>% arrange(description)
violttl <- sum(ytdlasttbl[1,2], ytdlasttbl[5,2], ytdlasttbl[7,2], ytdlasttbl[8,2])
ytdlasttbl[nrow(ytdlasttbl) + 1,] <- list('Violent crime total', violttl)
propttl <- sum(ytdlasttbl[2,2], ytdlasttbl[3,2], ytdlasttbl[4,2], ytdlasttbl[6,2])
ytdlasttbl[nrow(ytdlasttbl) + 1,] <- list('Property crime total', propttl)
ttl <- sum(ytdlasttbl[9,2], ytdlasttbl[10,2])
ytdlasttbl[nrow(ytdlasttbl) + 1,] <- list('Total', ttl)
#summing shootings
shootings <- length(which(ytdlast$shooting == "Y"))
ytdlasttbl[nrow(ytdlasttbl) + 1,] <- list('Shooting', shootings)

ytdlasttbl <- ytdlasttbl %>% slice(match(x, description))

###MERGE EACH PAIR OF TABLES

mergedtbl <- merge(rnowtbl,rpriortbl,by="description") 
mergedtbl <- merge(mergedtbl,ytdnowtbl,by="description") 
mergedtbl <- merge(mergedtbl,ytdlasttbl,by="description") 

#CALC AND ADD TWO PERCENTAGE CHANGE COLUMNS, THEN REMOVE TWO "CHANGED FROM" COLUMNS
mergedtbl$yoy <- apply(mergedtbl[,c(2, 3)], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
tempcolname <- paste("vs. ", colnames(mergedtbl[3]), sep="")
colnames(mergedtbl)[6] <- tempcolname

mergedtbl$yoy <- apply(mergedtbl[,c(4, 5)], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
tempcolname <- paste("vs. ", colnames(mergedtbl[5]), sep="")
colnames(mergedtbl)[7] <- tempcolname

#remove "changed from" columns
mergedtbl <- mergedtbl[, -3]
mergedtbl <- mergedtbl[, -4]
#reorder columns
mergedtbl <- mergedtbl[, c(1, 2, 4, 3, 5)]

#order columns for reader-facing table
mergedtbl <- mergedtbl %>% slice(match(x, description))

#split into two tables, one for ytd and one for last two weeks
mergedtblytd <- mergedtbl[, c(1,4,5)]
mergedtblrct <- mergedtbl[, c(1,2,3)]

###EXPORT TABLE
write.csv(mergedtblytd, "crime_table_ytd.csv")
write.csv(mergedtblrct, "crime_table_rct.csv")

###SWITCHING TO NEIGHBORHOODS

#last 2 weeks (recentnow)
nhoodsrnow <- recentnow %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING") %>% tabyl(neighborhood, description) %>% clean_names()


#Add any missing columns (arson, homicide, rape) and enter 0 for every neighborhood
if (!("arson" %in% colnames(nhoodsrnow))) {
  nhoodsrnow$arson <- 0
}
if (!("homicide" %in% colnames(nhoodsrnow))) {
  nhoodsrnow$homicide <- 0
}
if (!("rape" %in% colnames(nhoodsrnow))) {
  nhoodsrnow$rape <- 0
}

#sum component violent and property crimes and add new columns at end with totals
nhoodsrnow <- nhoodsrnow %>% mutate(Violent = rowSums(across(c(agg_assault,homicide,rape,robbery, robbery_carjacking, robbery_commercial))))
nhoodsrnow <- nhoodsrnow %>% mutate(Property = rowSums(across(c(burglary,auto_theft,larceny,arson, larceny_from_auto, larceny_of_motor_vehicle_parts_or_accessories,shoplifting))))
nhoodsrnow <- nhoodsrnow[, c(1,15, 16)]

#rename coumn with M/D-M/D for last two weeks minus 7 days for reporting lag
tempcolname <- paste('Violent ',str_remove(format(as.Date(lastrec - days(20), '%Y-%m-%d'), "%m/%d"), "^0+"),"-",str_remove(format(as.Date(lastrec - days(7), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
tempcolname <- gsub("/0+", "/", tempcolname)
colnames(nhoodsrnow)[2] <- tempcolname
colnames(nhoodsrnow)[3] <- str_replace(tempcolname, "Violent", "Property")

#two weeks before that (recentprior)
nhoodsrpri <- recentprior %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING") %>% tabyl(neighborhood, description) %>% clean_names()

#Add any missing columns (arson, homicide, rape) and enter 0 for every neighborhood
if (!("arson" %in% colnames(nhoodsrpri))) {
  nhoodsrpri$arson <- 0
}
if (!("homicide" %in% colnames(nhoodsrpri))) {
  nhoodsrpri$homicide <- 0
}
if (!("rape" %in% colnames(nhoodsrpri))) {
  nhoodsrpri$rape <- 0
}

#sum component violent and property crimes and add new columns at end with totals
nhoodsrpri <- nhoodsrpri %>% mutate(Violent = rowSums(across(c(agg_assault,homicide,rape,robbery, robbery_carjacking, robbery_commercial))))
nhoodsrpri <- nhoodsrpri %>% mutate(Property = rowSums(across(c(burglary,auto_theft,larceny,arson, larceny_from_auto, larceny_of_motor_vehicle_parts_or_accessories,shoplifting))))
nhoodsrpri <- nhoodsrpri[, c(1,15, 16)]
tempcolname <- paste('Violent ',str_remove(format(as.Date(lastrec - days(34), '%Y-%m-%d'), "%m/%d"), "^0+"),"-",str_remove(format(as.Date(lastrec - days(21), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
tempcolname <- gsub("/0+", "/", tempcolname)

#rename coumn with M/D-M/D for prior two weeks minus 7 days for reporting lag
colnames(nhoodsrpri)[2] <- tempcolname
colnames(nhoodsrpri)[3] <- str_replace(tempcolname, "Violent", "Property")

#combine tables
mergednhb <- merge(nhoodsrnow,nhoodsrpri,by="neighborhood") 
mergednhb <- mergednhb[, c(1, 2, 4, 3, 5)]

#EXPORT CSV FOR N/HOODS LAST TWO WEEKS
write.csv(mergednhb, "mergednhb.csv")

#year to date (ytdnow)
nhoodsytdnow <- ytdnow %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING") %>% tabyl(neighborhood, description) %>% clean_names()

#sum component violent and property crimes and add new columns at end with totals
nhoodsytdnow <- nhoodsytdnow %>% mutate(Violent = rowSums(across(c(agg_assault,homicide,rape,robbery, robbery_carjacking, robbery_commercial))))
nhoodsytdnow <- nhoodsytdnow %>% mutate(Property = rowSums(across(c(burglary,auto_theft,larceny,arson, larceny_from_auto, larceny_of_motor_vehicle_parts_or_accessories,shoplifting))))
nhoodsytdnow <- nhoodsytdnow[, c(1,15, 16)]

#rename coumn with YYYY through M/D via lastrec - 7 days for reporting lag
tempcolname <- paste('Violent ',thisyear," thru ",str_remove(format(as.Date(lastrec - days(7), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
tempcolname <- gsub("/0+", "/", tempcolname)
colnames(nhoodsytdnow)[2] <- tempcolname
colnames(nhoodsytdnow)[3] <- str_replace(tempcolname, "Violent", "Property")

#thru same point last year (ytdlast)
nhoodsytdlast <- ytdlast %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING") %>% tabyl(neighborhood, description) %>% clean_names()

#sum component violent and property crimes and add new columns at end with totals
nhoodsytdlast <- nhoodsytdlast %>% mutate(Violent = rowSums(across(c(agg_assault,homicide,rape,robbery, robbery_carjacking, robbery_commercial))))
nhoodsytdlast <- nhoodsytdlast %>% mutate(Property = rowSums(across(c(burglary,auto_theft,larceny,arson, larceny_from_auto, larceny_of_motor_vehicle_parts_or_accessories,shoplifting))))
nhoodsytdlast <- nhoodsytdlast[, c(1,15, 16)]

#rename coumn with YYYY through M/D via lastrec -1 year - 7 days
tempcolname <- paste('Violent ', format(as.Date(lastrec - years(1) - days(7), '%Y-%m-%d'), "%Y")," thru ",str_remove(format(as.Date(lastrec - days(7), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
tempcolname <- gsub("/0+", "/", tempcolname)
colnames(nhoodsytdlast)[2] <- tempcolname
colnames(nhoodsytdlast)[3] <- str_replace(tempcolname, "Violent", "Property")

#combine tables
mergednoy <- merge(nhoodsytdnow,nhoodsytdlast,by="neighborhood") 
mergednoy <- mergednoy[, c(1, 2, 4, 3, 5)]

#NEXT, CALC AND ADD TWO PERCENTAGE CHANGE COLUMNS, THEN REMOVE TWO "CHANGED FROM" COLUMNS
mergednoy$yoy <- apply(mergednoy[,c(2, 3)], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
tempcolname <- paste("vs. ", colnames(mergednoy[3]), sep="")
colnames(mergednoy)[6] <- tempcolname
mergednoy$yoy <- apply(mergednoy[,c(4, 5)], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
tempcolname <- paste("vs. ", colnames(mergednoy[5]), sep="")
colnames(mergednoy)[7] <- tempcolname

#round percentages
mergednoy[,6] <- round(mergednoy[,6])
mergednoy[,7] <- round(mergednoy[,7])

#append percentage signs
mergednoy[,6] <- paste0(mergednoy[,6], "%")
mergednoy[,7] <- paste0(mergednoy[,7], "%")

#prepend + sign on positive values
mergednoy[,6] <- ifelse(mergednoy[,6] > 0, paste0("+", mergednoy[,6]), mergednoy[,6])
mergednoy[,7] <- ifelse(mergednoy[,7] > 0, paste0("+", mergednoy[,7]), mergednoy[,7])

#replace NaN and Inf values
tempfiltr <- paste("0 in ", format(as.Date(lastrec - years(1) - days(7), '%Y-%m-%d'), "%Y")," thru ",str_remove(format(as.Date(lastrec - days(7), '%Y-%m-%d'), "%m/%d"), "^0+"), sep="")
mergednoy[,6] <- str_replace(mergednoy[,6], ".Inf.", tempfiltr)
mergednoy[,6] <- str_replace(mergednoy[,6], ".NaN.", tempfiltr)
mergednoy[,7] <- str_replace(mergednoy[,7], ".Inf.", tempfiltr)
mergednoy[,7] <- str_replace(mergednoy[,7], ".NaN.", tempfiltr)

mergednoy <- mergednoy[, c(1, 2, 6, 4, 7)]

#GET RID OF HARBOR EAST SINCE IT WAS ADDED FALL 2024
mergednoy <- mergednoy %>% filter(neighborhood != "HARBOR EAST")

#EXPORT CSV FOR N/HOODS YEAR TO DATE
write.csv(mergednoy, "mergednoy.csv")

#YEAR TO DATE MAP
ytdmap <- ytdnow

#filter ytdmap for last five weeks 
ytdmap <- ytdmap %>% filter(ytdmap$crime_date >= lastrec - days(34))

#change shootings to have a shooting description rather than robbery or agg assault
ytdmap$description[ytdmap$shooting == "Y"] <- "SHOOTING"

#filter for major crimes
ytdmap <- ytdmap %>% filter(description == "HOMICIDE" | description == "AGG. ASSAULT" | description == "ROBBERY" | description == "RAPE" | description == "BURGLARY" | description == "AUTO THEFT" | description == "LARCENY" | description == "ARSON" | description == "ROBBERY - CARJACKING" | description == "ROBBERY - COMMERCIAL" | description == "LARCENY FROM AUTO" | description == "LARCENY OF MOTOR VEHICLE PARTS OR ACCESSORIES" | description == "SHOPLIFTING" | description == "SHOOTING")

#take just columns we need
ytdmap <- ytdmap[, c(3,4,5,6,9,10,11,12,13,15,16,17,18,19,21)]

#column adjustments

#remove leading zeros form crime date
ytdmap$crime_date <- format(as.Date(ytdmap$crime_date, "%Y-%m-%d"), "%m/%d/%Y")
ytdmap$crime_date <- gsub("^0", "", ytdmap$crime_date)
ytdmap$crime_date <- gsub("/0", "/", ytdmap$crime_date)

ytdmap$inside_outside <- gsub("I", "Inside", ytdmap$inside_outside)
ytdmap$inside_outside <- gsub("O", "Outside", ytdmap$inside_outside)

ytdmap$weapon <- gsub("KNIFE_CUTTING_INSTRUMENT", "Knife/cutting instrument", ytdmap$weapon)
ytdmap$weapon <- str_to_sentence(ytdmap$weapon)
ytdmap$weapon <- gsub("_", " ", ytdmap$weapon)
ytdmap$weapon <- sub("^$", "None reported", ytdmap$weapon)

ytdmap$gender <- sub("NA", "None reported", ytdmap$gender)
ytdmap$gender <- gsub("M", "Male", ytdmap$gender)
ytdmap$gender <- gsub("F", "Female", ytdmap$gender)
ytdmap$gender <- gsub("U", "Unknown", ytdmap$gender)

ytdmap$age[is.na(ytdmap$age)] <- "None reported"

ytdmap$description <- str_to_sentence(ytdmap$description)
ytdmap$description <- gsub("Agg. Assault", "Aggravated assault", ytdmap$description)

ytdmap$race <- gsub("_", " ", ytdmap$race)
ytdmap$race <- str_to_title(ytdmap$race)
ytdmap$race <- gsub("Or", "or", ytdmap$race)
ytdmap$race <- gsub("Other", "other", ytdmap$race)
ytdmap$race[is.na(ytdmap$race)] <- "None reported"
ytdmap$race <- sub("^$", "None reported", ytdmap$race)

ytdmap$ethnicity <- sub("^ $", "None reported", ytdmap$ethnicity)
ytdmap$ethnicity <- gsub("_", " ", ytdmap$ethnicity)
ytdmap$ethnicity <- str_to_title(ytdmap$ethnicity)
ytdmap$ethnicity <- gsub("Or", "or", ytdmap$ethnicity)

ytdmap$new_district <- str_to_sentence(ytdmap$new_district)

ytdmap$premise_type <- str_to_sentence(ytdmap$premise_type)
ytdmap$premise_type <- gsub("Wholesaledisc. Stor", "Wholesaler/store", ytdmap$premise_type)

ytdmap$neighborhood <- str_to_title(ytdmap$neighborhood)

#display_time remove leading zeros
ytdmap$display_time <- sub("^0", "", ytdmap$display_time)
ytdmap$display_time <- gsub("AM", "a.m.", ytdmap$display_time)
ytdmap$display_time <- gsub("PM", "p.m.", ytdmap$display_time)

#trim trailing spaces in premises data

ytdmap$premise_type <- trimws(ytdmap$premise_type)

#preempt auto column type issue in Carto
ytdmap[c('zt', 'zap')] <-  str_split_fixed(ytdmap$display_time, ' ', 2)
ytdmap <- ytdmap[, c(-15)]

#write csvs for maps
write.csv(ytdmap %>% filter(description == "Homicide"), "Homicide.csv")
write.csv(ytdmap %>% filter(description == "Aggravated assault"), "Aggravated assault.csv")
write.csv(ytdmap %>% filter(grepl("Robbery", description)), "Robbery.csv")
write.csv(ytdmap %>% filter(description == "Shooting"), "Shooting.csv")
write.csv(ytdmap %>% filter(description == "Rape"), "Rape.csv")
write.csv(ytdmap %>% filter(description == "Burglary"), "Burglary.csv")
write.csv(ytdmap %>% filter(description == "Auto theft"), "Auto theft.csv")
write.csv(ytdmap %>% filter(grepl("Larceny", description) | grepl("Shoplifting", description)), "Larceny.csv")
write.csv(ytdmap %>% filter(description == "Arson"), "Arson.csv")
