
for (package in c('shiny', 'DT', 'shinythemes', 'htmlwidgets')) 
{
  if (!require(package, character.only=T, quietly=F)) 	# checking whether ... packages are already installed
  {
    install.packages(package)		
    library(package, character.only=T)
  }
}

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")


if (!require("limma", character.only=T, quietly=F)) {
  BiocManager::install("limma")
  library("limma")
}

if (!require("affy", character.only=T, quietly=F)) {
  BiocManager::install("affy")
  library("affy")
}


if (!require("NormalyzerDE", character.only=T, quietly=F)) {
  BiocManager::install("NormalyzerDE")
  library("NormalyzerDE")
}

if (!require("preprocessCore", character.only=T, quietly=F)) {
  BiocManager::install("preprocessCore")
  library("preprocessCore")
}




#library("shiny")
#library("DT")
#library("preprocessCore")

#shinyServer(function(input, output,session) {
options(shiny.maxRequestSize=30*1024^2)
server <- function(input, output, session) {
  #output$contents <- DT::renderDataTable({
  
  output$contents1 <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile1 <- input$file1
    
    if (is.null(inFile1))
      return(NULL)
    
    #dat1<- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    else{
      
      dat1<- read.table(inFile1$datapath, header=input$header, sep=input$sep, quote=input$quote)
      DT::datatable(dat1, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=T, scrollX=TRUE, paging=TRUE))
    }
    
    
    
    #read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    #DT::datatable(dat1, options = list(pageLength = 25, paging=FALSE))
    
  })
  
  
  output$contents2 <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
    # columns. The 'datapath' column will contain the local filenames where the
    # data can be found.
    
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    
    #dat1<- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    else{
      
      dat2<- read.table(inFile2$datapath, header=input$header, sep=input$sep, quote=input$quote, row.names = 1)
      DT::datatable(dat2, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=T, scrollX=TRUE, paging=TRUE))
    }
    
    
    
    #read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    #DT::datatable(dat1, options = list(pageLength = 25, paging=FALSE))
    
  })
  #)
  
  observeEvent(c(input$preprocess,input$normalize), {
    
    
    inFile1 <- input$file1
    inFile2 <- input$file2
    
    if (is.null(inFile1))
      return(NULL)
    if (is.null(inFile2))
      return(NULL)
    
    #dat1<- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    else{
      
      dat1<- read.table(inFile1$datapath, header=input$header,  sep=input$sep, quote=input$quote, row.names = 1)
      dat2 <- read.table(inFile2$datapath, header = input$header, sep = input$sep, quote = input$quote,row.name = 1)
    }
    
    mat1 <- as.matrix(dat1)
    mat2 <- as.matrix(dat2)
    
    res1 <- mat1
    res2 <- mat2
    
    if(input$normalize == "Quantile"){
      
      res1<- normalize.quantiles(mat1)
      res2 <- normalize.quantiles(mat2)
    }
    
    
    
    if(input$normalize == "Loess"){
      
      res1<- normalize.loess(mat1)
      res2 <- normalize.loess(mat2)
    }
    
    if(input$normalize == "CyclicLoess"){
      
      res1 <- performCyclicLoessNormalization(mat1)
      res2 <- performCyclicLoessNormalization(mat2)
    }
    
    if(input$normalize == "RMA"){
      
      res1 = rma(mat1)
      res2 = rma(mat2)
    }
    log2_val1 <- log2(res1)
    log2_val2 <- log2(res2)
    #normalizeQuantiles
    output$contents3 <- DT::renderDataTable({
      DT::datatable(res1, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=FALSE, scrollX=TRUE, paging=TRUE)) 
    })
    output$contents4 <- DT::renderDataTable({
      DT::datatable(res2, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=FALSE, scrollX=TRUE, paging=TRUE)) 
    })
    output$log2_value1 <- DT::renderDataTable({
      DT::datatable(log2_val1,class = 'cell-border stripe',options = list(pageLength = 10,scrollY = F,scrollX = T,paging = T))
    })
    output$log2_value2 <- DT::renderDataTable({
      DT::datatable(log2_val2,class = 'cell-border stripe',options = list(pageLength = 10,scrollY = F,scrollX = T,paging = T))
    })
  })
}
#)