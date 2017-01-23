# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
    label = "Choose a number",
    value = 25, min = 1, max = 100),

  plotOutput("hist")
)

server <- function(input, output) {

  output$hist <- renderPlot({
    title <- "100 random normal values"
    hist( rnorm(input$num) )
    })

  

 

}

shinyApp(ui = ui, server = server)
