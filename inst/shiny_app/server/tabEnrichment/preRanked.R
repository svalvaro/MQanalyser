enriched_plot_preranked <- reactive({

    if (is.null(edo2())) {

        message('Can not plot, not enough proteins')
        return(NULL)
    }

    if(input$runscore == 'all'){
        p <- enrichplot::gseaplot2(edo2(), geneSetID = 1, pvalue_table = F)
        #enrichplot::gseaplot2(edo2, geneSetID = 1)
    } else{
        p <- enrichplot::gseaplot(edo2(), geneSetID = 1, by = input$runscore, pvalue_table = F)
    }

    return(p)
})



output$enr_gseaplot <- renderPlot(height = 800, {

    if (is.null(edo2())) {

        message('Can not plot, not enough proteins')
        return(NULL)
    }

    rows_edo2 <- nrow(as.data.frame(edo2()))

    message(paste0('The number of rows of edo2 is:', rows_edo2))


    if (rows_edo2 == 0) {

        message('Can not plot, not enough proteins')
        return(NULL)
    }

    if (is.null(enriched_plot_preranked())) {
        message('The plot is null')
        return(NULL)
    }

    message('edo2() is not null, can the gsea enrichment plot be created?')

    return(enriched_plot_preranked())
})

output$preRankedPlotUI <- renderUI({

    message('Plotting network enrichment')

    rows_edo2 <- nrow(as.data.frame(edo2()))
    message(paste0('Number of rows edo2: ', rows_edo2))

    if (rows_edo2 == 0) {
        print(
            'The Gene Set Enrichment Analysis did not found any gene
                enriched under the specific p-value cut-off'
        )
    }else{

        shinycssloaders::withSpinner(
            plotOutput('enr_gseaplot'),
            image = 'images/logoTransparentSmall.gif',
            image.width = '200px'
        )
    }
})
