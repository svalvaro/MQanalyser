# If pressed the button, it will open a new tab.

output$pathway_selector <- renderUI({

    selectInput(inputId = 'pathselec',
                label = h4('Select the pathway to check:'),
                choices = as.list(pathways_id()),
                selected = as.list(pathways_id()[1])
    )
})



output$pathwayButton <- renderUI({

    url <- paste0("http://www.kegg.jp/kegg-bin/show_pathway?",
                  input$pathselec,
                  '/',
                  kegg_react1()[input$pathselec, "geneID"])


    message(paste0('Pathway selected is: ',input$pathselec))

    message(paste0('The url is: ', url))

    shiny::a(
        actionBttn(
            inputId = 'GoToPathway',
            label = 'Browse Path',
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
