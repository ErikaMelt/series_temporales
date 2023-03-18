# Front page module

# UI function
tab_0_ui <- function(id) {

  # Basically not needed. Just kept here to preserve commonality across files.
  ns <- NS(id)

  # Main UI

  fluidPage(
    
    fluidRow(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ), 
    
    fluidRow(
      box(width = 3, textInput(ns("dat1"), value = "SERIE.csv", label = "Introduzca el nombre del dataset")),
      box(width = 3, textInput(ns("dat2"), value = "target", label = "Introduzca el nombre de la target")),
      box(width = 3, numericInput(ns("test1"), min = 0, max = 1, value =0.7, label = "Elija proporcion test - training"))
      ), # end: fluidRow
    
    
    
    
    fluidRow(
      box(width = 3, selectInput(ns("sel1"), choices = c("ARIMA", "Prophet"), label = "Elija su modelo"))
    ),
    
    fluidRow(
      actionButton("goButton", "Predecir", class = "btn-primary btn-lg")
    ), 
    
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )

  ) # end: fluidPage

} # end: tab_0_ui()

# Server function
tab_0_server <- function(input, output, session) {

  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })

} # end: tab_0_server()
