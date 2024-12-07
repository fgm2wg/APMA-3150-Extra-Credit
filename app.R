library(shiny)

ui <- fluidPage(
  
  titlePanel("Exploring Normal Distributions - APMA 3150"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Customize Parameters"),
      sliderInput("mean", "Mean (μ):", min = -10, max = 10, value = 0),
      sliderInput("sd", "Standard Deviation (σ):", min = 1, max = 5, value = 1),
      numericInput("sample_size", "Sample Size:", value = 100, min = 1),
      actionButton("generate", "Generate New Sample"),
      
      br(),
      h4("About this App"),
      helpText("Created by Colin Henry."),
      helpText("This app demonstrates how varying the mean and standard deviation affects the normal distribution."),
      hr()
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Distribution Plot", 
                 plotOutput("distPlot")),
        tabPanel("Sample Data", 
                 tableOutput("sampleTable")),
        tabPanel("Summary Statistics", 
                 verbatimTextOutput("summaryStats"))
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    input$generate
    isolate(rnorm(input$sample_size, mean = input$mean, sd = input$sd))
  })
  
  output$distPlot <- renderPlot({
    hist(data(), breaks = 20, col = "lightblue", border = "white",
         main = "Histogram of Generated Data",
         xlab = "Value", ylab = "Frequency")
    abline(v = mean(data()), col = "red", lwd = 2, lty = 2)
  })
  
  output$sampleTable <- renderTable({
    head(data.frame(Sample = data()), n = 10)
  })
  
  output$summaryStats <- renderPrint({
    summary(data())
  })
}

shinyApp(ui = ui, server = server)
