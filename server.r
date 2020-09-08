library(shiny)
server <- function(input,output,session){
  output$out <- renderText(
    paste(input$select," ")
  )
}