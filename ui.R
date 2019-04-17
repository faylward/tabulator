ui <- fluidPage(
  
  pageWithSidebar(headerPanel('Count Table Transformations'),
  sidebarPanel(
    fileInput("filename", "Choose Count Table File to Upload:", accept = c('.csv')),
    fileInput("metadata", "Choose Metadata File to Upload:", accept = c('.csv')),
    selectInput("transformation", "Transformation to apply:", c("Proportion", "DESeq2", "None")),
    selectInput("lowColor", "Low Value:", c("dodgerblue", "green", "blue", "purple", "red", "orange", "yellow", "white", "black")),
    selectInput("midColor", "Mid Value:", c("none", "black", "green", "blue", "purple", "red", "orange", "yellow", "white")),
    selectInput("highColor", "High Value:", c("firebrick", "red", "orange", "yellow", "green", "blue", "purple", "orange", "white", "black")),    
    selectInput("rowclust", "Cluster Rows?", c("FALSE", "TRUE")),
    selectInput("colclust", "Cluster Columns?", c("FALSE", "TRUE")),
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
