
server <- function(input, output, session) {
library(data.table)
library(pheatmap)
library(DESeq2)
library(RColorBrewer)
  
    datasetInput <- reactive({
    validate(
      need(input$filename != 0, "To begin drawing a heatmap, please select a file for input") 
    )
    inFile <- input$filename
    if (is.null(inFile)) return(NULL)
    datatable <- as.data.frame(fread(inFile$datapath))
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
  output$plot1 <- renderPlot({

    datatable <- datasetInput()
    print(datatable)
    if(input$transformation == "DESeq2") {
      final <- varianceStabilizingTransformation(round(as.matrix(datatable[-1]+1)))
    }
    else if (input$transformation == "Proportion") {
      final <- as.matrix(scale(datatable[-1], scale=colSums(datatable[-1]), center=F))
    } 
    else {
      final <- as.matrix(datatable[-1])
    } 
    
    par(mar = c(5.1, 4.1, 0, 1))
    #final <- as.data.frame(datasetInput())
    print(dim(final))
    metadata <- as.data.frame(metadataInput())
    print(metadata)
    print(input$colclust)
    print(input$rowclust)
    pheatmap(final, 
             cluster_cols = as.logical(input$colclust), 
             cluster_row=as.logical(input$rowclust), 
             clustering_distance_rows=input$distanceMethod,
             clustering_method=input$agglomerationMethod,
             border_color=FALSE,
             annotation_col=metadata
    )
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

