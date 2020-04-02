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
    "/Users/xolanisibande/Desktop/Covid_Dashboard2/covidapi.csv",
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
    dashboardHeader(title = h3(strong("Gauteng COVID-19 Cases")))
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
             #Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa. Available at"
            ),
            tags$a(href = "https://github.com/dsfsi/covid19za", "https://github.com/dsfsi/covid19za"),
            paste("For inputs on Gauteng specific data email xolaniss@gmail.com.")
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
            icon = icon("virus"),
            color = "teal"
        )
    })
    output$most <- renderValueBox({
        valueBox(
            max(data_total_cases$Cases),
            "Most Cases a Day",
            icon = icon("viruses", lib = "font-awesome"),
            color = "olive"
        )
    })
    output$avgage <- renderValueBox({
        valueBox(
            round(mean(data$Age, na.rm = T), 0),
            "Average Age",
            color = "light-blue",
            icon = icon("calendar", lib = "font-awesome")
        )
    })
    output$oldest <- renderValueBox({
        valueBox(
            max(data$Age, na.rm = T),
            "Oldest Infected Person",
            color = "blue",
            icon = icon("calendar-week", lib = "font-awesome")
        )
    })
    output$sources <- renderText({
        paste(
            "This dashboard is focused on Gauteng specific individual cases. The data on indiviual cases has become less frequent since 25 March 2020. The data was sourced from the Data Science for Social Impact Research Group @ University of Pretoria,
             #Coronavirus COVID-19 (2019-nCoV) Data Repository for South Africa. Available at https://github.com/dsfsi/covid19za. This code for this dashboard can be found at . For inputs on Gauteng specific data email: xolaniss@gmail.com."
        )
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
