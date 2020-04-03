library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(stringr)
library(ggplot2)
library(DT)
library(plotly)
library(vroom)
library(here)

data <- vroom(
  here("data","covidapi.csv"),
  col_types =
    list(
      col_double(),
      col_date(),
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_double(),
      col_character(),
      col_character()
    )
)

data_total_cases <-
  data  %>%  select(-Time_Stamp, -Country, -Geo_subdivision) %>%
  group_by(Date) %>%
  transmute(Cases = n()) %>%
  unique()



header <-
  dashboardHeader(title = "Gauteng COVID-19 Cases")
sidebar <- dashboardSidebar(
  disable = TRUE,
  menuItem(
    "Provincial Comparison",
    tabName = "provinces",
    icon = icon("dashboard")
  ),
  menuItem(
    "Gauteng Individual Cases",
    tabName = "gauteng",
    icon = icon("dashboard")
  ),
  menuItem("About", tabName = "about", icon = icon("dashboard"))
)
body <- dashboardBody(
  #tabItems(
  #tabItem(
  #tabName = "gauteng",
  
  fluidRow(h1(align = "center", strong(
    paste(" GAUTENG INDIVIDUAL CASES: ", max(format(data$Date, "%d %B %Y")))
  ))),
  
  fluidRow(
    width = 12,
    align = "center",
    h4(
      paste(
        "This dashboard is focused on Gauteng specific individual cases. The data on indiviual cases has become less frequent since 25 March 2020. South Africa specific information can be found at"
      ),
      tags$a(href = "https://datastudio.google.com/s/qnBGVpdJlMQ", "https://datastudio.google.com/s/qnBGVpdJlMQ"),
      paste(
        ". The data was sourced from the Data Science for Social Impact Research Group @ University of Pretoria,
             #Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa and is available at"
      ),
      tags$a(href = "https://github.com/dsfsi/covid19za", "https://github.com/dsfsi/covid19za"),
      paste(". For inputs on Gauteng specific data email xolaniss@gmail.com. The information will update as soon as more data is released.")
    )
  ),
  br(),
  
  fluidRow(
    valueBoxOutput(width = 3, "total"),
    valueBoxOutput(width = 3, "most"),
    valueBoxOutput(width = 3, "avgage"),
    valueBoxOutput(width = 3, "oldest"),
    
  ),
  
  
  fluidRow(
    box(width = 7, h4("Gender Breakdown"), plotlyOutput("genderplot")),
    
    box(
      width = 5,
      height = 250,
      selectInput("gender", h4("Select Gender"), unique(data$Gender)),
      dateRangeInput(
        "date_gender",
        h4("Select Date Range"),
        start = "2020-01-01",
        end = Sys.Date()
      )
    )
    
  ),
  
  fluidRow(
    box(width = 7, h4("Age Breakdown"), plotlyOutput("ageplot")),
    box(
      width = 5,
      height = 250,
      sliderInput(
        "age",
        h4("Select Age Range"),
        min = 1,
        max = 100,
        value = c(1, 100)
      ),
      dateRangeInput(
        "date_age",
        h4("Select Date Range"),
        start = "2020-01-01",
        end = Sys.Date()
      )
      
    )
    
    
  ),
  
  fluidRow(
    box(
      width = 7,
      h4("Transmission Breakdown"),
      plotlyOutput("transplot")
    ),
    
    box(
      width = 5,
      height = 250,
      selectInput(
        "trans",
        h4("Select Transmission Type"),
        unique(data$Transmission)
      ),
      dateRangeInput(
        "date_trans",
        h4("Select Date Range"),
        start = "2020-01-01",
        end = Sys.Date()
      ),
      
      
      
      
    )
    
    
  )
  
  
)



#)#)


ui <- dashboardPage(header, sidebar,  body)
