library(shiny)
 ui <- fluidPage(
   titlePanel("demonstration of select Input widgets"),
   sidebarLayout(
     sidebarPanel(
       selectInput("select","select your state",c("Tamil Nadu","Chattisgar","Andhrapradesh"))
     ),
     mainPanel("The selected status are:",
       textOutput("out")
     )
   )
 )