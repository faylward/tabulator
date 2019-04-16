
library(gplots)
ui <- fluidPage(
  
  pageWithSidebar(headerPanel('Count Table Transformations'),
  sidebarPanel(
    fileInput("filename", "Choose Count Table File to Upload:", accept = c('.csv')),
    fileInput("metadata", "Choose Metadata File to Upload:", accept = c('.csv')),
    selectInput("transformation", "Transformation to apply:", c("Proportion", "DESeq2", "None")),
    selectInput("lowColor", "Low Value:", c("dodgerblue", "green", "blue", "purple", "red", "orange", "yellow", "white", "black")),
    selectInput("midColor", "Mid Value:", c("none", "black", "green", "blue", "purple", "red", "orange", "yellow", "white")),
    selectInput("highColor", "High Value:", c("firebrick", "red", "orange", "yellow", "green", "blue", "purple", "orange", "white", "black")),    
    selectInput("dendrogram", "Apply Clustering:", c("none", "row", "column", "both")),
    selectInput("numitems", "Number of features to show in heatmap:", c("50", "100", "500", "1000")),
    selectInput("distanceMethod", "Distance Metric:", c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
    selectInput("agglomerationMethod", "Linkage Algorithm:", c("complete", "single", "average", "centroid", "median", "mcquitty", "ward.D", "ward.D2")),
    #selectInput("scale", "Apply Scaling:", c("row", "column", "none")),
    downloadButton("downloadHeatmap", "Download Heatmap")
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
)


server <- function(input, output, session) {
library(data.table)
library(gplots)
library(DESeq2)
library(WGCNA)
library(RColorBrewer)
  
    datasetInput <- reactive({
    validate(
      need(input$filename != 0, "To begin drawing a heatmap, please select a file for input") 
    )
    inFile <- input$filename
    if (is.null(inFile)) return(NULL)
    fread(inFile$datapath)
})

    metadataInput <- reactive({    
   validate(
      need(input$metadata != 0, "To begin drawing a heatmap, please select your metadata file")
    )
      inFile2 <- input$metadata
      if (is.null(inFile2)) return(NULL)
      read.table(inFile2$datapath, sep="\t", row.names=1)
  })
  
  # Combine the selected variables into a new data frame
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    datatable <- as.data.frame(datasetInput())
    meta <- as.data.frame(metadataInput())
    vector <- as.character(meta[,6])
    print(vector)
    print(length(vector))
    #print(row.names(meta))
    #print(colnames(datatable)[2:length(colnames(datatable))])
    features <- as.character(vector[match(row.names(meta), colnames(datatable)[2:length(colnames(datatable))])])
    print(features)
    colors <- labels2colors(features)
    print(colors)
    
    if(input$transformation == "DESeq2") {
      final <- varianceStabilizingTransformation(as.matrix(datatable[-1]+1))
      }
    else if (input$transformation == "Proportion") {
      final <- as.matrix(scale(datatable[-1], scale=colSums(datatable[-1]), center=F))
    } 
    else {
      final <- as.matrix(datatable[-1])
    } 
    print(length(colors))
    print(dim(final))
     
    heatmap.2(final,
    trace="none",
    scale = "none", #input$scale,
    dendrogram = input$dendrogram,
    hclustfun = function(x) hclust(x, method = input$agglomerationMethod),
    ColSideColors = colors,
    key=T,
    distfun = function(x) dist(x, method = input$distanceMethod),
    Rowv = if (input$dendrogram == "both" | input$dendrogram == "row") TRUE else FALSE, 
    Colv = if (input$dendrogram == "both" | input$dendrogram == "column") TRUE else FALSE,
    col = if (input$midColor == "none") colorpanel(256, low = input$lowColor, high = input$highColor) else colorpanel(256, low = input$lowColor, mid = input$midColor, high = input$highColor)
    )
    legend(1,1, legend=unique(features), fill=unique(colors))
  })
  
  # static heatmap download								
  output$downloadHeatmap <- downloadHandler(
    filename <- function() {
      paste0(basename(file_path_sans_ext(input$filename)), '_heatmap', '.png', sep='')
    },
    content <- function(file) {
      png(file)
      tiff(
        file,
        width = 4000,
        height = 2000,
        units = "px",
        pointsize = 12,
        res = 300
      )
      staticHeatmap()
      dev.off()
    }
  )
  
}



shinyApp(ui = ui, server=server)