
for (package in c('shiny', 'DT', 'shinythemes', 'htmlwidgets')) 
{
  if (!require(package, character.only=T, quietly=F)) 	# checking whether ... packages are already installed
  {
    install.packages(package)		
    library(package, character.only=T)
  }
}

if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
  BiocManager::install("siggenes")
  }

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

if(!require("samr",character.only = T)){
  BiocManager::install("samr")
  library("samr")
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
  log2_val1 <- NULL
  log2_val2 <- NULL
  # dat1 <- NULL
  # dat2 <- NULL
  
  observeEvent(c(input$preprocess,input$normalize,input$log2), {
    
    
    inFile1 <- input$file1
    inFile2 <- input$file2
    
    if (is.null(inFile1))
      return(NULL)
    if (is.null(inFile2))
      return(NULL)
    
    #dat1<- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    else{
      
      dat1 <- read.table(inFile1$datapath, header=input$header,  sep=input$sep, quote=input$quote, row.names = 1)
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
    log2_val1 <<- log2(res1)
    log2_val2 <<- log2(res2)
    #normalizeQuantiles
    output$contents3 <- DT::renderDataTable({
      DT::datatable(res1, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=FALSE, scrollX=TRUE, paging=TRUE)) 
    })
    output$contents4 <- DT::renderDataTable({
      DT::datatable(res2, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=FALSE, scrollX=TRUE, paging=TRUE)) 
    })
    
    if(input$log2 == T){
    output$log2_value1 <- DT::renderDataTable({
      DT::datatable(log2_val1,class = 'cell-border stripe',options = list(pageLength = 10,scrollY = F,scrollX = T,paging = T))
    })
    output$log2_value2 <- DT::renderDataTable({
      DT::datatable(log2_val2,class = 'cell-border stripe',options = list(pageLength = 10,scrollY = F,scrollX = T,paging = T))
    })
    }
    
  })
observeEvent(input$next1,{
  output$page1 <- renderUI({
    fluidPage(
      titlePanel("Differential Gene expression"),
      sidebarLayout(
        sidebarPanel(
          selectInput("diff","Select a Method",c("SAM","T-test","Edger"),selected = "SAM"),
          actionButton("process","Process")
        ),
        mainPanel()
      )
    )
  })
})
<<<<<<< HEAD
observeEvent(c(input$process,input$diff),{
    
   if(input$diff == "SAM"){
=======
observeEvent(c(input$preprocess,input$diff),{
    
   # if(input$diff == "SAM"){
>>>>>>> 4c8e8b94bbb7bb24faeca106bbe4f8221f9aa20b
    
    file_1 <- input$file1
    file_2 <- input$file2
    if(is.null(file_1))
       return(NULL)
    if(is.null(file_2))
      return(NULL)
    else{
    data1 <- read.table(file_1$datapath, header=T, row.names = 1)
    data2 <- read.table(file_2$datapath, header=T, row.names = 1)
    }
    tab_ctrl_acute <- cbind(data1,data2)
    log2_ctrl_acute <- cbind(log2_val1,log2_val2)
    ncl = c(rep(1,ncol(data1)),rep(2,ncol(data2)));
    sam.data <- list(x=log2_ctrl_acute,y=ncl,geneid=row.names(tab_ctrl_acute),logged2=T,fdr.output=0.05)
    samr.object <- samr(sam.data,resp.type = "Two class unpaired",nperms=100,random.seed = 12345,testStatistic = "wilcoxon")
    delta.table <-samr.compute.delta.table(samr.object)
    View(delta.table)
    delta <- 0.55
    siggens.table <- samr.compute.siggenes.table(samr.object,delta,sam.data,delta.table,min.foldchange = 1.5)
    pvalues <- samr.pvalues.from.perms(samr.object$tt,samr.object$ttstar)
    ctrl_acute_upgenes <- siggens.table$genes.up
    ctrl_acute_downgenes <- siggens.table$genes.lo

    write.table(ctrl_acute_upgenes,file="ctrl_acute_upgenes.txt",sep="\t")
    write.table(ctrl_acute_downgenes,file="ctrl_acute_downgenes.txt",sep="\t")

    ctrl_acute_deg_rowID = sort(as.numeric(rbind(cbind(ctrl_acute_upgenes[,1]),cbind(ctrl_acute_downgenes[,1]))))

    write.table(ctrl_acute_deg_rowID,file = "ctrl_acute_deg_rowID.txt",sep = "\t")

    save.image("E:/NEW HIV conf/work/ctrl_acute.RData")
    save(ctrl_acute_deg_rowID,file = "E:/NEW HIV conf/ctrl_acute_deg_rowID")
    save(ctrl_acute_deg_rowID,file = "E:/NEW HIV conf/ctrl_acute_deg_rowID.RData")
    savehistory("E:/New HIV conf/work/his_ctrl_acute.txt")
    
<<<<<<< HEAD
    #volcano plot
    
    delta1 <- 0.55
    siggenes.table1 <- samr.compute.siggenes.table(samr.object, delta1, sam.data, delta.table, min.foldchange = 1.5, all.genes = TRUE)
    View(siggenes.table1)
    siggenes.table1[["genes.up"]]
    #View(siggenes.table1)
    up_df = siggenes.table1[["genes.up"]]
    dn_df = siggenes.table1[["genes.lo"]]
    View(dn_df)
    View(up_df)
    
    sig_up_df = siggenes.table1[["genes.up"]]
    View(sig_up_df)
    sig_up_df = as.data.frame(sig_up_df)
    up_df = as.data.frame(up_df)
    sig_dn_df = siggenes.table1[["genes.lo"]]
    sig_dn_df = as.data.frame(sig_dn_df)
    dn_df = as.data.frame(dn_df)
    
    
    dn_df1 = dn_df
    class(dn_df1)
    dn_df1 = dn_df
    head(dn_df1)
    head(dn_df1[,3])
    head(dn_df1[,c(3,7)])
    # dn_df1[,7] = -as.numeric(1 / dn_df1[,7])
    dn_df1[,7] = -as.numeric(1 / as.numeric(dn_df1[,7]))
    
    merge_df = rbind(up_df, dn_df1)
    merge_df = as.data.frame(merge_df)
    colnames(merge_df)
    colnames(merge_df) = c("Row", "GeneID", "GeneName", "Score(d)", "Numerator(r)", "Denominator(s+s0)", "FoldChange", "q-value(%)")
    merge_df1 = merge_df
    rownames(merge_df1) = merge_df1$GeneName
    merge_df2 = merge_df1[match(rownames(up_df), rownames(dn_df)), ]
    View(merge_df2)
    length(pvalues)
    
    fdr_bh = p.adjust(pvalues, method = "BH")
    merge_df3 = cbind(merge_df2, pvalues, fdr_bh)
    View(merge_df3)
    colnames(merge_df3)
    
    library(dplyr)
    
    mutateddf1 <- mutate(merge_df3, sig=ifelse(merge_df3$pvalues<0.05,"Significant DEG", "Non DEG")) #Will have different colors depending on significance
    sig_up_ids = which(mutateddf1$GeneName %in% sig_up_df$`Gene Name`)
    sig_up_ids
    mutateddf1$sig[sig_up_ids] = "Significant UpRegulated DEG"
    sig_dn_ids = which(mutateddf1$GeneName %in% sig_dn_df$`Gene Name`)
    sig_dn_ids
    mutateddf1$sig[sig_dn_ids] = "Significant DownRegulated DEG"
    write.table(mutateddf1, file = "mutated_df1.txt", sep = "\t", row.names = TRUE, col.names = NA)
    
    #rm(merge_df,merge_df1, merge_df2)
    
    
    mutated_df1 = read.table(file = "mutated_df1.txt", sep = "\t", header = TRUE, row.names = 1)
    View(mutated_df1)
    
    # Volcano Plot
    
    library("ggrepel") #Avoid overlapping labels
    library("ggplot2") #Best plots
    dim(merge_df3)
    library(dplyr)
    library("ggrepel") #Avoid overlapping labels
    library("ggplot2") #Best plots
    
    volc = ggplot(mutated_df1, aes(FoldChange, -log10(fdr_bh))) + #volcanoplot with log2Foldchange versus pvalue
      geom_point(aes(col = sig)) + #add points colored by significance
      scale_color_manual(values = c("black","blue", "orange", "red")) +
      ggtitle("Volcano Plot of Differential Expression") #e.g. 'Volcanoplot DESeq2'
    volc
    min(mutated_df1$FoldChange)
    max(mutated_df1$FoldChange)
    test = mutated_df1[which(mutated_df1$FoldChange < 0),]
    View(test)
    max(test$FoldChange)
    test2 = mutated_df1[which(mutated_df1$FoldChange > 0),]
    min(test2$FoldChange)
    volc = volc + labs(col = "Genes")
    volc = volc + theme(plot.title = element_text(hjust = 0.5))
    volc = volc + geom_vline(xintercept = 1.5, linetype="dashed", color = "red")
    volc = volc + geom_vline(xintercept = -1.5, linetype="dashed", color = "red")
    volc = volc + geom_hline(yintercept = 1.30103, linetype="dashed", color = "red")  # corresponding to FDR 0.05
    
    volc1 = volc+geom_text_repel(data=head(mutated_df1[mutated_df1$FoldChange >= 1.5,], 20), aes(label = GeneName)) #adding text for the top 20 Up regulated genes
    volc1 = volc1 + geom_text_repel(data=head(mutated_df1[mutated_df1$FoldChange <= -1.5,], 20), aes(label = GeneName)) #adding text for the top 20 Up regulated genes
    volc2 = volc1 + scale_x_continuous(breaks = seq(-15,15, by = 1 ))
    volc2 = volc2 + scale_y_continuous(breaks = c(0,2,seq(0,5, by = 1 )))
    
    volc2 = volc2 + xlab("log2FC")
    volc2 = volc2 + ylab("-log10(FDR)")
    volc2 = volc2 + ggtitle("")
    volc2 = volc2 + ggtitle("") + labs(col = "") + theme(legend.direction = "horizontal", legend.position = "top")
    
    volc2
    savehistory("E:/New HIV conf/SAM DEG/hist1.txt")
    } #if finished
=======
    #} #if finished
>>>>>>> 4c8e8b94bbb7bb24faeca106bbe4f8221f9aa20b
  }
) #observeEvent finished
} #function finished
#)