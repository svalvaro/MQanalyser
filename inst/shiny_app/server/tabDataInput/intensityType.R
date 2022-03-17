# Radio buttons will appear in the UI depending on the software used.

output$intensity_selector  <- renderUI({

    if (is.null(software_used())) {
        return(NULL)
    }

    if (software_used() == 'MaxQuant') {

        radioButtons(inputId = "IntensityType",
                     h4("Intensity type to analyze:"),
                     choices = c("Raw Intensity" = 'Intensity',
                                 "LFQ" = 'LFQ',
                                 "iBAQ" = 'iBAQ'),
                     selected = 'LFQ')

    } else if ( software_used() == 'Spectronaut'){

        radioButtons(inputId = "IntensityType",
                     h4("Intensity type to analyze:"),
                     choices = c("LFQ" = 'LFQ',
                                 "iBAQ" = 'iBAQ'),
                     selected = 'LFQ')

    } else if (software_used() == 'MSFragger'){

        radioButtons(inputId = "IntensityType",
                     h4("Intensity type to analyze:"),
                     choices = 'intensity',
                     selected = 'intensity')
    }
})

# Check if the type of intensity is found in the proteoInput

IntensityFound <- reactive({

    intensityToUse <- input$IntensityType

    message(paste0('The user selects intensity:', intensityToUse))

    if (is.null(software_used())) {
        return(NULL)

        # If the software used was MaxQuant

    } else if (software_used() == 'MaxQuant') {

        columns = grep(paste0(input$IntensityType,'.'), colnames(proteoInput()))

        # For Spectronaut Software

    } else if (software_used() == 'Spectronaut'){

        # If the intensity selected is LFQ, the column name is PG.Quantity
        if (input$IntensityType == 'LFQ') {
            columns <-  grep('PG.Quantity', colnames(proteoInput()))

            # If IBAQ Intensity is selected
        }else{
            columns <-  grep('PG.IBAQ', colnames(proteoInput()))
        }
    } else if (software_used() == 'MSFragger'){


        df <- proteoInput() %>% select(contains('Intensity'),
                                          -contains(c('Total','Unique')))

        columns <- which(colnames(proteoInput()) %in% colnames(df) )
    }

    message(paste0("The columns with intensities are: ", columns))

    if (length(columns) == 0) {

        IntensityFound <- FALSE
    } else{

        IntensityFound <- TRUE
    }

    return(IntensityFound)
})


output$IntensityFound_message <- renderText({
    if (IntensityFound() == TRUE) {
        print(paste0(input$IntensityType, '  was found. \nYou can continue \nwith the analysis.'))
    }else{
        print(paste0(input$IntensityType, ' was not found. \nPlease select another intensity.'))
    }
})

output$intensityBox <- renderInfoBox({

    if (is.null(IntensityFound())) {
        return(NULL)
    }

    if (IntensityFound() == TRUE) {
        title_box <- paste0(input$IntensityType, '  was found.')
        subtitle_box <- ' You can continue with the analysis.'
        icon <- "check-square"
        color <- 'green'

    }else{
        title_box <- paste0(input$IntensityType, '  was not found.')
        subtitle_box <- ' You must select another \ntype of Intensity.'
        icon <- "exclamation-circle"
        color <- 'red'
    }

    info <- infoBox(
        title =  title_box,
        value = subtitle_box,
        #icon = icon("stats", lib = "glyphicon"))
        icon = icon(icon),
        color = color)
    return(info)
})
