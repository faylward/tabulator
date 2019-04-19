

ui <- fluidPage(
  
  pageWithSidebar(headerPanel('Count Table Transformations'),
  sidebarPanel(
    fileInput("filename", "Choose Count Table File to Upload:", accept = c('.csv')),
    fileInput("metadata", "Choose Metadata File to Upload:", accept = c('.csv')),
    selectInput("transformation", "Transformation to apply:", c("Proportion", "log2", "DESeq2", "None")),
    selectInput("colorpal", "Color Palette:", c("default", "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn")),    
    selectInput("rowclust", "Cluster Rows?", c("TRUE", "FALSE")),
    selectInput("colclust", "Cluster Columns?", c("TRUE", "FALSE")),
    selectInput("numitems", "Number of features to show in heatmap:", c("50", "100", "500", "1000")),
    selectInput("distanceMethod", "Distance Metric for Heatmap:", c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "bray", "kulczynski", "jaccard", "gower", "morisita", "horn", "mountford")),
    selectInput("distanceMethod2", "Distance Metric for Anosim:", c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
    selectInput("agglomerationMethod", "Linkage Algorithm:", c("complete", "single", "average", "centroid", "median", "mcquitty", "ward.D", "ward.D2")),
    #selectInput("scale", "Apply Scaling:", c("row", "column", "none")),
    downloadButton("downloadHeatmap", "Download Heatmap")
  ),
  mainPanel(
    #plotOutput('plot1')
	      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Heatmap", plotOutput("heatmap")),
                  tabPanel("Rank-Abundance", plotOutput("rankabund")),
                  tabPanel("Mean-Variance", plotOutput("meanvar")),
                  tabPanel("ANOSIM", verbatimTextOutput("anosim"))
      )
  )
)
)

server <- function(input, output, session) {
library(data.table)
library(pheatmap)
library(DESeq2)
library(RColorBrewer)
library(vegan)
  
    datasetInput <- reactive({
    validate(
      need(input$filename != 0, "To begin drawing a heatmap, please select a file for input") 
    )
    inFile <- input$filename
    if (is.null(inFile)) return(NULL)
    datatable <- as.data.frame(fread(inFile$datapath))
    
    if(input$transformation == "DESeq2") {
      final <- varianceStabilizingTransformation(round(as.matrix(datatable[-1]+1)), fitType="mean")
    }
    if(input$transformation == "log2") {
      final <- log10(round(as.matrix(datatable[-1]+1)))
    }
    if (input$transformation == "Proportion") {
      final <- as.matrix(scale(datatable[-1], scale=colSums(datatable[-1]), center=F))
    } 
    if (input$transformation == "None") {
      final <- as.matrix(datatable[-1])
    } 
    
    print(input$numitems)
    if (as.numeric(dim(final)[1]) > as.numeric(input$numitems)) {
      sorted_final <- final[order(rowSums(final), decreasing=T),]
      newfinal <- sorted_final[1:as.numeric(input$numitems),]
    }
    else { newfinal <- final}
    
    return(as.matrix(newfinal))
})

    metadataInput <- reactive({    
    validate(
      need(input$metadata != 0, "To begin drawing a heatmap, please select your metadata file")
    )
      inFile2 <- input$metadata
      if (is.null(inFile2)) return(NULL)
      md <- read.table(inFile2$datapath, sep="\t", row.names=1, header=T)
   #   return(md)
  })
  
  # Combine the selected variables into a new data frame
  output$heatmap <- renderPlot({
    
    if (input$colorpal == "default") {
      palette <- colorRampPalette(rev(c("#D73027", "#FC8D59", "#FEE090", "#FFFFBF", "#E0F3F8", "#91BFDB", "#4575B4")))(100)
    }
    else {
      palette <- brewer.pal(9, input$colorpal)  
    }
    
    final <- datasetInput()
    
    if (input$distanceMethod %in% c("bray", "kulczynski", "jaccard", "gower", "morisita", "horn", "mountford")) {
      print(min(final))
      print(input$distanceMethod)
      distance1 <- vegdist(final, method=input$distanceMethod)
      distance2 <- vegdist(t(final), method=input$distanceMethod)
    }
    else {
      distance1 <- input$distanceMethod
      distance2 <- input$distanceMethod
    }
    
    par(mar = c(5.1, 4.1, 0, 1))
    metadata <- as.data.frame(metadataInput())
    print(colSums(final))
    print(min(final))
    pheatmap(final, 
             cluster_cols = as.logical(input$colclust), 
             cluster_row=as.logical(input$rowclust), 
             clustering_distance_rows=distance1,
             clustering_distance_cols=distance2,
             clustering_method=input$agglomerationMethod,
             border_color=FALSE,
             annotation_col=metadata,
             col=palette
    )
  })
  
  output$rankabund <- renderPlot({
    final <- datasetInput()
    par(mar = c(5.1, 4.1, 0, 1))
    plot(sort(apply(final, 1, mean), decreasing=T), lwd=3, col="dodgerblue", type="l")
    points(sort(apply(final, 1, mean), decreasing=T), col="dodgerblue"
    )
  })
  
  output$meanvar <- renderPlot({
    final <- datasetInput()
    print("working")
    par(mar = c(5.1, 4.1, 0, 1))
    mean <- apply(final, 1, mean)
    stand <- apply(final, 1, var)
    x1 <- as.numeric(mean[order(mean, decreasing=T)])
    x2 <- as.numeric(stand[order(mean, decreasing=T)])
    plot(x1, x2, col="dodgerblue", xlab="Variance", ylab="Mean Abundance")
  })
  
  output$anosim <- renderText({
    final <- datasetInput()
    metadata <- as.data.frame(metadataInput())
    ano <- anosim(t(final), grouping=as.factor(metadata$Value))
    call <- paste("Call:", ano$call, collapse=" ")
    significance <- paste("Significance:", ano$signif, collapse=" ")
    statistic <- paste("ANOSIM Statistic R:", ano$statistic, collapse=" ")
    perm <- paste("Permutations:", ano$permutations, collapse=" ")
    
    return(paste(c(call, significance, statistic, perm), collapse="\n"))
  })
  
  # static heatmap download								
#  output$downloadHeatmap <- downloadHandler(
#    filename <- function() {
#      paste0(basename(file_path_sans_ext(input$filename)), '_heatmap', '.png', sep='')
#    },
#    content <- function(file) {
#      png(file)
#      tiff(
#        file,
#        width = 4000,
#        height = 2000,
#        units = "px",
#        pointsize = 12,
#        res = 300
#      )
#      staticHeatmap()
#      dev.off()
#    }
#  )
  
}




shinyApp(ui = ui, server=server)