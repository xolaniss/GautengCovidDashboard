# Preliminary ----
rm(list = ls())
library(quantmod)
library(xts)
library(zoo)
library(dplyr)
library(tidyr)
library(readxl)
library(httr)
library(XML)
library(rvest)
library(stringr)
library(rebus)
library(filesstrings)
library(purrr)
library(lubridate)
library(RCurl)
library(curl)
library(jsonlite)
library(DT)
library(shiny)
library(rsconnect)

# Importing from API

data <-
  fromJSON(
    "https://covid-za-api.herokuapp.com/cases/confirmed?province=GP",
    simplifyDataFrame = T
  )

str(data)
data[, 2] <- as.Date(data$date, "%d-%m-%Y")
names(data) <-
  c(
    "ID",
    "Date",
    "Time_Stamp",
    "Country",
    "Provice",
    "Geo_subdivision",
    "Age",
    "Gender",
    "Transmission"
  )

data[, 8] <- data[, 8] %>% str_replace_all("^female", "Female") %>%
  str_replace_all("^male", "Male")

#Exporting to Excel

write.csv(data, file = "covidapi.csv", row.names = FALSE)

deployApp('/Users/xolanisibande/Desktop/Covid_Dashboard2/Covid_Dashboard2', forceUpdate = T)
