# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)

ui <- fluidPage(
  actionButton(inputId = "norm",
    label='Normal'),

  actionButton(inputId = "unif",
    label='Uniform'),

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

  # # reactive expression
  # data <- reactive({
  #   rnorm( input$num )
  # })

  rv <- reactiveValues(data = rnorm( 100 ))

  observeEvent(input$norm, { rv$data <- rnorm( 100 ) })
  observeEvent(input$unif, { rv$data <- runif( 100 ) })

  output$hist <- renderPlot({
    hist( rv$data, main = input$title )
  })

  output$stats <- renderPrint({
    summary( rv$data )
  })




}

shinyApp(ui = ui, server = server)
