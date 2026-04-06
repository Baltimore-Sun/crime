###LOAD LIBRARIES

library(tidyverse)
library(janitor)
library(tidyr)
library(lubridate)
library(geojsonsf)
library(sf)
library(rjson)
library(jsonlite)

#Replace file location below with that of most recently downloaded historical crime data
historicalcrime <- read_csv("/Users/steveearley/Downloads/NIBRS_GroupA_Crime_Data_-5958022609112572027.csv") %>% clean_names()

#The below processes historical data. Won't need to routinely do this.

#MAKE DATES USEABLE (FORMAT DATES INTO READABLE FORMAT, CREATE SEPARATE DATE AND TIME COLUMNS)
historicalcrime <- historicalcrime %>% separate(crime_date_time, c("crime_date", "crime_time_temp", "am_pm"), " ")
historicalcrime$crime_time = paste(historicalcrime$crime_time_temp, historicalcrime$am_pm, sep=" ")
#MAKE NEW DISPLAY TIME COLUMN
historicalcrime$display_time = format(strptime(historicalcrime$crime_time, format='%I:%M:%S %p'), '%I:%M %p')
#REMOVE UNWANTED COLUMNS
historicalcrime = select(historicalcrime, -crime_time_temp, -am_pm, -crime_time)
#PROPERLY FORMAT DATE COLUMN
historicalcrime <- mutate(historicalcrime, crime_date=mdy(crime_date))
#TAKE COLUMNS WE NEED
historicalcrime = select(historicalcrime, row_id,cc_number,crime_date,description,inside_outside,weapon,shooting,post,gender,age,race,ethnicity,location,old_district,new_district,neighborhood,latitude,longitude,premise_type,total_incidents,display_time)
#CONVERT HISTORICAL DATA TIBBLE TO DATAFRAME
historicalcrime <- as.data.frame(historicalcrime)
#CHANGE COLUMN TYPES TO MATCH WORKING DATA
historicalcrime$cc_number <- as.character(historicalcrime$cc_number)
historicalcrime$post <- as.character(historicalcrime$post)

#Export for manipulation by full crime_table_nhp script
write.csv(historicalcrime, "historicalcrime.csv", row.names=FALSE)