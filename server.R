library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(stringr)
library(ggplot2)
library(DT)
library(plotly)
library(vroom)


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




server <- function(input, output) {
  #Gender Data
  Covid_GP_table_gender <- reactive({
    data %>%
      group_by(Date, Gender) %>%
      transmute(Cases = n()) %>%
      unique()
    
  })
  
  #Age Data
  Covid_GP_table_age <- reactive ({
    data %>%
      group_by(Date, Age) %>%
      transmute(Cases = n()) %>%
      unique()
    
  })
  
  #Gender Data
  output$genderplot <- renderPlotly({
    genderplot <-   Covid_GP_table_gender() %>%
      filter(Gender == input$gender) %>%
      filter(Date >= input$date_gender[1]) %>%
      filter(Date <= input$date_gender[2])
    
    ggplotly(
      ggplot(genderplot, aes(Date, Cases)) +
        geom_col(fill = "#1E8BC3") +
        labs("Date", "Cases") +
        theme_minimal() +
        theme(text = element_text(size = 6)) +
        coord_flip()
    )
  })
  
  #Transmission Data
  Covid_GP_table_trans <- reactive ({
    data %>%
      group_by(Date, Transmission) %>%
      transmute(Cases = n()) %>%
      unique()
  })
  
  #Age
  
  output$ageplot <- renderPlotly({
    ageplot <- Covid_GP_table_age() %>%
      filter(Age >= input$age[1]) %>%
      filter(Age <= input$age[2]) %>%
      filter(Date >= input$date_age[1]) %>%
      filter(Date <= input$date_age[2])
    
    ggplotly(
      ageplot %>% ggplot(aes(Date, Cases)) +
        geom_col(fill = "#1E6BC3") +
        labs("Date", "Cases") +
        theme_minimal() +
        theme(text = element_text(size = 6)) +
        coord_flip()
    )
    
  })
  
  #Transmission
  
  output$transplot <- renderPlotly({
    transplot <-  Covid_GP_table_trans() %>%
      filter(Transmission == input$trans) %>%
      filter(Date >= input$date_trans[1]) %>%
      filter(Date <= input$date_trans[2])
    
    ggplotly(
      ggplot(transplot, aes(Date, Cases)) +
        geom_col(fill = "#2574A9") +
        labs("Date", "Cases") +
        theme_minimal() +
        theme(text = element_text(size = 6)) +
        coord_flip()
    )
    
  })
  
  output$total <- renderValueBox({
    valueBox(
      sum(data_total_cases$Cases),
      "Total Number of Cases",
      color = "teal"
      #icon = icon("virus"),
    )
  })
  output$most <- renderValueBox({
    valueBox(
      max(data_total_cases$Cases),
      "Most Cases a Day",  color = "olive"#,
      #icon = icon("viruses", lib = "font-awesome")
      
    )
  })
  output$avgage <- renderValueBox({
    valueBox(
      round(mean(data$Age, na.rm = T), 0),
      "Average Age",
      color = "light-blue"#,
      #icon = icon("calendar-week", lib = "font-awesome")
    )
  })
  output$oldest <- renderValueBox({
    valueBox(
      max(data$Age, na.rm = T),
      "Oldest Infected Person",
      color = "blue"#,
      #icon = icon("calendar-week", lib = "font-awesome")
    )
  })
  output$sources <- renderText({
    paste(
      "This dashboard is focused on Gauteng specific individual cases. The data on indiviual cases has become less frequent since 25 March 2020. The data was sourced from the Data Science for Social Impact Research Group @ University of Pretoria,
             #Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa. Available at https://github.com/dsfsi/covid19za. This code for this dashboard can be found at . For inputs on Gauteng specific data email: xolaniss@gmail.com."
    )
    
  })
  
}

