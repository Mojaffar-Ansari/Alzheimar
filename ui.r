
for (package in c('shiny', 'DT', 'shinythemes', 'htmlwidgets')) 
{
   if (!require(package, character.only=T, quietly=F)) 
   {
      install.packages(package)
      library(package, character.only=T)
   }
}
#library("shiny")
#library("DT")
#library("shinythemes")

#shinyUI(pageWithSidebar(

ui <-fluidPage(theme = shinytheme("cerulean"), #superhero
               headerPanel("Dataset Preprocessing"),
               sidebarPanel(fluid = FALSE,
                            tags$head(tags$script('var dimension = [0, 0];
                          $(document).on("shiny:connected", function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                          });
                          $(window).resize(function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                          });
                    ')),               
                            fileInput('file1', 'Choose Dataset1',
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.txt')),
                            #tags$hr(),
                            
                            fileInput(inputId = 'file2', 'Choose Dataset2',
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.txt')),
                            #tags$hr(),
                            
                            splitLayout(
                               radioButtons(inputId = 'sep', 'Separator',
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t'),
                                            'Comma'),
                               radioButtons(inputId = 'quote', 'Quote',
                                            c(None='',
                                              'Double Quote'='"',
                                              'Single Quote'="'"),
                                            'Double Quote')
                            ),
                            #splitLayout(cellWidths = c("75%", "25%"),
                            selectInput(inputId = "normalize",
                                        label = "Normalization",
                                        choices = c("Quantile", "Loess", "CyclicLoess","qspline", "RMA", "MASS5", "GCRMA")),
                            splitLayout(cellWidths = c("50%", "50%"),
                                        checkboxInput('header', 'Header', TRUE),
                                        checkboxInput('log2', 'Log2', TRUE)
                            ),
                            
                            actionButton("preprocess", "Preprocess"),
                            actionButton("next", "Next"),
                            p("Click the Next button for further processing...")
                            
               ),
               mainPanel(
                  tabsetPanel(
                     tabPanel("Dataset1",  DT::dataTableOutput('contents1')),
                     tabPanel("Dataset2",  DT::dataTableOutput('contents2')),
                     tabPanel("Output for file 1", DT::dataTableOutput('contents3')),
                     tabPanel("output for file 2", DT::dataTableOutput('contents4')),
                     tabPanel("Log2 value of normalized file 1", DT::dataTableOutput(('log2_value1'))),
                     tabPanel("Log2 value of normalized file 1", DT::dataTableOutput(('log2_value2')))
                  )
               )
)
#)