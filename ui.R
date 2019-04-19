ui <- fluidPage(
  
  pageWithSidebar(headerPanel('Count Table Transformations'),
                  sidebarPanel(
                    fileInput("filename", "Choose Count Table (.tsv) File to Upload:", accept = c('.tsv', '.txt')),
                    fileInput("metadata", "Choose Metadata File (.tsv) to Upload:", accept = c('.tsv', '.txt')),
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