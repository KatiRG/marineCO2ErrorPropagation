# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)

ui <- fluidPage(
  actionButton(inputId = "clicks",
    label='Click me'),

  sliderInput(inputId = "num",
    label = "Choose a number",
    value = 25, min = 1, max = 100),

  textInput(inputId = "title",
    label = "Write a title",
    value = "Histogram of Random Normal Values"
    ),

  plotOutput("hist"),

  verbatimTextOutput("stats")
)

server <- function(input, output) {

  # reactive expression
  data <- reactive({
    rnorm( input$num )
  })

  output$hist <- renderPlot({
    hist( data(), main = input$title )
  })

  output$stats <- renderPrint({
    summary( data() )
  })

  observeEvent(input$clicks, {
    print(as.numeric( input$clicks )) #prints to console
  })


}

shinyApp(ui = ui, server = server)
