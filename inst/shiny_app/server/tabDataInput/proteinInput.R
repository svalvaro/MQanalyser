proteoInput <- reactive({

    inFile <- input$proteinInput

    # If no file is loaded and no pressed demo
    if (is.null(inFile) & demo$start == FALSE){
        return(NULL)
    }
    # If a file is loaded (MaxQuant or spectronaut)
    if(!is.null(inFile)){

        # If the file ends in txt is from MaxQuant, read accordingly

        if (endsWith(as.character(inFile$datapath),suffix = '.txt') ) {

            df <- read.delim(inFile$datapath)

            # If the file ends in csv is from Spectronaut, read accordingly

        } else if (endsWith(as.character(inFile$datapath), suffix = '.csv')){

            df <- read_csv(inFile$datapath, na = 'NaN')

        }else if (endsWith(as.character(inFile$datapath),suffix = '.tsv') ){

            df  <- readr::read_delim(inFile$datapath,
                                           delim = "\t", escape_double = FALSE,
                                           trim_ws = TRUE)
        }

        # If they press DEMO
    } else if(demo$start == TRUE){

        df <- read.delim('www/data/proteinGroups_example.txt')

        # Change the "filtered" to 0

        df[df == "Filtered"] <- '0'
    }

    return(df)
})

# Infobox containing the matris RowsxColumns of the proteoInput

output$matrixDimensions <- renderInfoBox({

    if (is.null(proteoInput())) {
        return(NULL)
    }

    rows <- nrow(proteoInput())
    columns <- ncol(proteoInput())

    message(paste0('nrows proteoInput', rows))
    message(paste0('columns proteoInput', columns))

    icon <- "info"

    color <- 'green'


    info <- infoBox(
        'Dimensions data:',
        paste0('Rows: ', rows,'\n' ,'\nColumns: ',columns),
        #icon = icon("stats", lib = "glyphicon"))
        icon = icon(icon),
        color = color)
    return(info)
})
