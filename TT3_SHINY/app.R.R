library(shiny)
library(readxl)
library(ggplot2)
library(DT)

ui <- fluidPage(
  
  titlePanel("Aplikasi Visualisasi Data Interaktif Berbasis Shiny"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        "file",
        "Upload File CSV atau Excel",
        accept = c(".csv", ".xlsx")
      ),
      
      uiOutput("xvar"),
      
      uiOutput("yvar"),
      
      selectInput(
        "plotType",
        "Pilih Jenis Plot",
        choices = c(
          "Scatter Plot",
          "Line Plot",
          "Bar Plot"
        )
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Visualisasi",
          plotOutput("plot", height = "500px")
        ),
        
        tabPanel(
          "Data",
          DTOutput("table")
        )
        
      )
      
    )
    
  )
  
)

server <- function(input, output, session){
  
  data <- reactive({
    
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    if(ext == "csv") {
      
      read.csv(
        input$file$datapath,
        stringsAsFactors = FALSE
      )
      
    } else if(ext == "xlsx") {
      
      read_excel(
        input$file$datapath
      )
      
    }
    
  })
  
  output$xvar <- renderUI({
    
    req(data())
    
    selectInput(
      "x",
      "Pilih Variabel X",
      choices = names(data())
    )
    
  })
  
  output$yvar <- renderUI({
    
    req(data())
    
    selectInput(
      "y",
      "Pilih Variabel Y",
      choices = names(data())
    )
    
  })
  
  output$plot <- renderPlot({
    
    req(data())
    req(input$x)
    
    if(input$plotType == "Scatter Plot") {
      
      req(input$y)
      
      ggplot(
        data(),
        aes_string(
          x = input$x,
          y = input$y
        )
      ) +
        geom_point(color = "blue") +
        theme_minimal() +
        labs(
          title = "Scatter Plot",
          x = input$x,
          y = input$y
        )
      
    } else if(input$plotType == "Line Plot") {
      
      req(input$y)
      
      ggplot(
        data(),
        aes_string(
          x = input$x,
          y = input$y,
          group = 1
        )
      ) +
        geom_line(color = "red") +
        theme_minimal() +
        labs(
          title = "Line Plot",
          x = input$x,
          y = input$y
        )
      
    } else {
      
      ggplot(
        data(),
        aes_string(x = input$x)
      ) +
        geom_bar(fill = "forestgreen") +
        theme_minimal() +
        labs(
          title = "Bar Plot",
          x = input$x,
          y = "Frekuensi"
        )
      
    }
    
  })
  
  output$table <- renderDT({
    
    req(data())
    
    datatable(
      data(),
      options = list(
        pageLength = 10
      )
    )
    
  })
  
}

shinyApp(ui, server)

