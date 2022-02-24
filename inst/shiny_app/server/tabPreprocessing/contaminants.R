# If the user uploads a file from Spectronaut, I need to open a fasta file
# and remove the proteins from the proteoInput that match the contaminants
# inside the FASTA

contaminants_fasta <- reactive({

    if (software_used() == 'MaxQuant') {
        return(NULL)
    }

    inFile <- input$contaminantsFastaInput

    # If no file is loaded and no pressed demo
    if (input$fastaOptions == "Use MaxQuant default"){
        message('User uses the default contaminants.fasta')
        contaminants_fasta <- seqinr::read.fasta(file = 'www/data/MaxQuant_Contaminants_Default.fasta')
    }

    if(!is.null(inFile) && input$fastaOptions == "Upload Custom"){
        message('User uploads a fasta with contaminants')
        contaminants_fasta <- seqinr::read.fasta(file = inFile$datapath )
    }

    if(is.null(inFile) && input$fastaOptions == "Upload Custom"){
        message('Waiting for the user to upload a fasta with contaminants')
        return(NULL)
    }

    return(contaminants_fasta)
})

output$fastaSelection <- renderUI({

    if (software_used() == 'MaxQuant') {
        return(NULL)
    }

    radioGroupButtons(
        inputId = "fastaOptions",
        label = h4("Contaminants proteins"),
        choices = c("Use MaxQuant default",
                    "Upload Custom"),
        status = "success",
        checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon"),
            no = icon("remove",
                      lib = "glyphicon"))
    )

})

output$fastaInput <- renderUI({

    if (software_used() == 'MaxQuant') {
        return(NULL)
    }

    shiny::req(input$fastaOptions == 'Upload Custom')

    fileInput(inputId = 'contaminantsFastaInput',
              label = h4('Upload a FASTA file containing contaminant proteins'),
              multiple = FALSE,
              accept = 'txt')
})

proteoInputCombined <- reactive({

    df <- proteoInput()

    if (software_used() == 'MaxQuant') {

        if (c('Reverse', 'Only.identified.by.site',
              'Potential.contaminant') %in% names(df)) {

            df <- df[(df$Reverse == '')  & (df$Only.identified.by.site==''),]

        }
    }

    if (software_used() == 'Spectronaut') {

        if (is.null(contaminants_fasta())) {
            message('Waiting for the user to upload a fasta file')
            return(NULL)
        }

        # Change the name of some columns to make it like proteinGroups

        colnames(df)[colnames(df) %in% c("PG.Genes", "PG.ProteinGroups")] <- c("Gene.names", "Protein.IDs")

        # Add a new column with the contaminants

        df$Potential.contaminant <- ''

        # Compare the proteinIds, with the proteinIDs of the contaminants,
        # if they match, mark it as '+', to be removed later

        # df$Potential.contaminant[which(df$Gene.names %in% names(contaminants_fasta))] <- '+'
        df$Potential.contaminant[which(df$Gene.names %in% names(contaminants_fasta()))] <- '+'

    }

    return(df)
})

proteoInputClean <- reactive({

    df <- proteoInputCombined()

    if (input$removeContaminantsInput) {

        df <- df[(df$Potential.contaminant == ''),]
    }

    return(df)

})

output$contaminants_box <- renderInfoBox({

    if (is.null(proteoInputClean())) {
        return(NULL)
    }

    # Number of contaminants proteins
    #contaminants <- proteoInputClean()$Potential.contaminant
    contaminants <- proteoInputCombined()$Potential.contaminant

    total_contaminants <- length(contaminants[grep('.+',contaminants)])

    message(paste0('The number of contaminants is: ', total_contaminants))

    if(input$removeContaminantsInput == 1){
        icon <- "check-square"
        color <- 'green'
        message <- paste0(total_contaminants, ' contaminants proteins removed')
    }else{
        icon <- "exclamation-circle"
        color <- 'red'
        message <- paste0(total_contaminants, ' contaminants proteins present')
    }

    info <- infoBox(
        'Contaminant Proteins',
        message,
        icon = icon(icon),
        color = color)
    return(info)
})

output$contaminantsPlot <- renderPlotly(

    MQanalyser::plot_contaminants(proteoInput = proteoInputClean(),
                                  softwareUsed = software_used(),
                                  intensityType = input$IntensityType,
                                  interactive = TRUE)%>%
        layout(height = 800, width = 800)
)

contaminants_non_interactive <- reactive({
    if (is.null(proteoInputClean())) {
        return(NULL)
    }

    p <- MQanalyser::plot_contaminants(proteoInput = proteoInputClean(),
                                       softwareUsed = software_used(),
                                       intensityType = input$IntensityType,
                                       interactive = FALSE)
    return(p)
})

output$contaminantsPlotNonInteractive <- renderPlot(height = 800, width = 800,{

    contaminants_non_interactive()
})

output$contaminantsUI <- renderUI({

    if (software_used() == 'MaxQuant' && !'Potential.contaminant' %in% names(proteoInput())) {

        return('Contaminants Column is not present in the proteinGroups,
                   Contaminants might be present.
                   Did you modify the proteinGroups.txt?
                   ')
    }

    message(paste0('Value of the contaminants: ', input$contaminantsInteractive))

    if (input$contaminantsInteractive == FALSE) {
        return(
            shinycssloaders::withSpinner(

                plotOutput('contaminantsPlotNonInteractive'),
                image = 'images/logoTransparentSmall.gif',
                image.width = '200px'
            )
        )


    }else{

        return(
            shinycssloaders::withSpinner(
                plotlyOutput('contaminantsPlot'),
                image = 'images/logoTransparentSmall.gif',
                image.width = '200px'
            )
        )
    }
})
