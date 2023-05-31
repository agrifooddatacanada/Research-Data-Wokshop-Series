library(shiny)
library(tidyverse)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(
    tagList(
      tags$b("My"),
      tags$em("cool"),
      tags$b("Dashboard")
      )
    ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h2("Upload your file here!"),
      fileInput(inputId = "file_input",
                label = "File Input",
                placeholder = "Nothing here yet..."),
      h2("Filter options"),
      dateRangeInput(inputId = "date_range",
                     label = "Date Filter"),
      sliderInput(inputId = "intake",
                  label = "Min Total Intake",
                  min = -1,
                  max = 5,
                  step = 0.1,
                  value = 0),
      actionButton(inputId = "submit",
                   label = "Apply Filters")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h2("Raw Data"),
      tableOutput("raw_data"),
      hr(),
      h2("Filtered Data"),
      
      textOutput("selected_dates"),
      textOutput("intake_msg"),
      
      tableOutput("filtered_data")
   )   
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$selected_dates <- renderText({
    paste("You have selected",
     input$date_range[1],
     "and",
     input$date_range[2]
    )
    })
  
  output$intake_msg <- renderText({
    paste("You have selected", input$intake, "Kg")
  })
  df <- reactive({
    req(input$file_input)
    read.csv(input$file_input$datapath)
  })
  
  output$raw_data <- renderTable({
    head(df())
  })
  
  filtered_data <- eventReactive(input$submit, {
    req(input$file_input)
    df() %>% 
      filter(date >= input$date_range[1],
             date <= input$date_range[2],
             intake_kg >= input$intake)
  })
  
  output$filtered_data <- renderTable({
    head(filtered_data())
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)