user_genes <- reactive({

    inFile <- input$user_genes

    if (is.null(inFile) & demo$start == FALSE){
        return(NULL)
    } else if (demo$start == TRUE){
        df <- read.csv('www/data/user_genes_examples.txt', col.names = 'Gene')
    } else{
        df <- read.csv(inFile$datapath, col.names = 'Gene')
    }
    return(df)
})
