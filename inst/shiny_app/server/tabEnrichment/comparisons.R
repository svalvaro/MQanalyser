output$comparisons_enrichment  <- renderUI({
    selectInput(inputId = 'comparison_enrch',
                label = h4('Select the comparison:'),
                choices = unlist(comparisons()),
                selected = comparisons()[1])
})

output$selectUpregulatedEnrich <- renderUI({

    req(input$comparison_enrch)

    comparison <- input$comparison_enrch

    Option1 <- gsub(pattern = '_.*', '', comparison)
    Option2 <- gsub(pattern = '.*_', '', comparison)
    Option3 <- paste0('Combine: ', Option1, ' and ' , Option2)

    message(paste0('Option1: ' , Option1))
    message(paste0('Option2: ' , Option2))


    ## Unblock the other enrichment tabs
    shinyjs::runjs(
        '
                var tab = $(\'a[data-value="disease-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="network-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="pathway-tab"]\').parent();
                $(tab).removeClass("disabled");
                var tab = $(\'a[data-value="interactions-tab"]\').parent();
                $(tab).removeClass("disabled");
            '
    )

    return(
        selectInput(
            inputId = 'upregulatedSelection',
            label = h4('Upregulated:'),
            choices = c(Option1,
                        Option2,
                        Option3)
        )
    )
})
