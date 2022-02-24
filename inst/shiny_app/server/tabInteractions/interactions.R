output$comparisons_interaction  <- renderUI({
    selectInput(inputId = 'comparisons_inter',
                label = h4('Select the comparison:'),
                choices = unlist(comparisons()),
                selected = comparisons()[1])
})

data_interactions <- reactive({

    req(input$comparisons_inter)

    message(paste0('Comparison for interactiosn: ', input$comparisons_inter))
    # comparison FC

    comparisonFC <- paste0(input$comparisons_inter, '_ratio')

    message(paste0('Comparison: ', comparisonFC))

    # comparison pvalAdjusted

    comparisonPValAdj <- paste0(input$comparisons_inter, '_p.adj')

    message(paste0('Comparison: ', comparisonPValAdj))

    comparisonSignificant <- paste0(input$comparisons_inter, '_significant')

    diffExpressGenes <- data_results() %>% select(contains(
        c('name', comparisonFC, comparisonPValAdj, comparisonSignificant)
    ))

    diffExpressGenes <- diffExpressGenes[which(diffExpressGenes[, comparisonSignificant] == TRUE),]

    diffExpressGenes[,comparisonSignificant] <- NULL

    names(diffExpressGenes) <- c('gene', 'logFC','pvalue')

    message(paste0('nrows ', nrow(diffExpressGenes)))

    return(diffExpressGenes)
})

output$interactionResults <- DT::renderDataTable(server = FALSE,{

    DT::datatable(as.data.frame(data_interactions()),
                  extensions = c('Select', 'Buttons'),
                  options = list(
                      select = list(style = 'os', items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = c('selectAll', 'selectNone')
                  ),
                  selection = 'none',
                  width = '400px',
                  rownames = FALSE)
})


interactionDataSelected <- reactive({

    df <- data_interactions()

    print(paste0('Selected rows by the user: ', input$interactionResults_rows_selected))

    df <- df[input$interactionResults_rows_selected,]

    print(head(df))

    return(df)
})

interactionDataFinal <- reactiveValues(df = NULL)

observeEvent(input$refreshInteractionTable,{
    print(
        paste0('Rows selected: ', interactionDataSelected())
    )

    interactionDataFinal$df <- interactionDataSelected()
})

interactionsTrimmed <- reactive({

    if (is.null(interactionDataFinal$df)) {
        return(NULL)
    }

    string_db <- STRINGdb$new(version = "11.5", species=9606,
                              score_threshold = 200, input_directory="")

    # diffExpressGenes <- data.frame(gene = c('AAAS', 'AACS', 'AAMP', 'AASS', 'ABCB7', 'ABCD4'))

    mapped <- string_db$map(interactionDataFinal$df,
                            'gene',
                            removeUnmappedRows = TRUE)

    ids_trimmed <- mapped$STRING_id[1:input$numberofInteractions]

    return(ids_trimmed)
})

interactionResults <- reactive({

    string_db <- STRINGdb$new(version = "11.5", species=9606,
                              score_threshold = 200, input_directory="")

    plot <- string_db$plot_network(interactionsTrimmed())

    url <- string_db$get_link(interactionsTrimmed())

    string_db$get_link(interactionsTrimmed())

    interactionResults <- list("plot" = plot, "url" = url)
    return(interactionResults)
})

plotInteractions <- reactive({
    p <- interactionResults()$plot

    return(p)
})

output$stringPlot <- renderPlot(height = 800, width = 800,{
    if (!is.null(plotInteractions())) {
        return(NULL)
    }

    plotInteractions()

})

output$interactionsButton <- renderUI({

    url <- interactionResults()$url

    if (is.null(url)) {
        return(NULL)
    }

    message(paste0('The interactions url is: ', url))

    shiny::a(
        actionBttn(
            inputId = 'GoToSTRING',
            label = 'View Interactively',
            icon = NULL,
            style = "unite",
            color = "default",
            size = "md",
            block = FALSE,
            no_outline = TRUE
        ),
        target = "_blank",
        href = url)
})
