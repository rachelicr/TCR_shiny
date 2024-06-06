library(shiny)


ui <- fluidPage(
  titlePanel("Load file"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Select a file"),      
    ),
    mainPanel(
      DT::dataTableOutput("table"),
      tableOutput("files_df")
    ),
    
  )
)

server <- function(input, output) {

  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }    
    read.csv(file = input$file$datapath)
  })

  output$table <- DT::renderDataTable({    
    req(input_file())    
    data <- input_file()    
    data
  })  
}

shinyApp(ui, server)