# List containing the gene list and the failed to map number


geneListObject <- reactive({

    message(paste0('The comparison of the samples for enrichment is: ', input$comparison_enrch))

    message(paste0('The organism  for enrichment is: ', input$enrich_organism))
    # geneListObject <- MQanalyser::create_geneList(
    #     data_results = data_results,
    #     comparison_samples = 'HEK_vs_III',
    #     organism = 'org.Hs.eg.db')

    geneListObject <- MQanalyser::create_geneList(
        data_results = data_results(),
        comparison_samples = input$comparison_enrch,
        organism = input$enrich_organism)

    # geneListObject <- MQanalyser::create_geneList(data_results = data_results,
    #                             comparison_samples = 'HEK_vs_TM',
    #                             organism = 'org.Hs.eg.db')
})

geneList <- reactive({

    geneList <- geneListObject()$geneList$ratio

    names(geneList) <- geneListObject()$geneList$ENTREZID

    # geneList <- geneListObject$geneList$ratio
    #
    # names(geneList) <- geneListObject$geneList$ENTREZID

    # apply log2fc cut off:

    comparison <- input$comparison_enrch

    Option1 <- gsub(pattern = '_.*', '', comparison)
    Option2 <- gsub(pattern = '.*_', '', comparison)
    Option3 <- paste0('Combine: ', Option1, ' and ' , Option2)

    req(input$upregulatedSelection)


    if (input$upregulatedSelection == Option3) {

        geneList <- geneList[abs(geneList) > log2(input$fc_enrichment)]


        # The positive values for option 1, which is Ctrl_vs_Tumor
        # Means the upregulated in Ctrl
    }else if(input$upregulatedSelection == Option1){

        geneList <- geneList[geneList > log2(input$fc_enrichment)]
        # geneList <- geneList[geneList > log2(1.5)]

        #The negative values, which means the upregulated in Tumour or
        # option2
    }else if(input$upregulatedSelection == Option2){
        geneList <- geneList[geneList < log2(input$fc_enrichment)]

    }
})

diffExpress <- reactive({

    # diffExpress <- names(geneList)

    de <- names(geneList())

})

# enrich results disease

edo <- reactive({

    edo <- DOSE::enrichDGN(diffExpress())

    # edo <- DOSE::enrichDGN(diffExpress)

})

# Enrichment for gsea
edo2 <- reactive({
    # edo2 <- DOSE::gseDO(geneList)
    message('edo2 being generated')

    edo2 <- DOSE::gseDO(geneList())

    message('edo2 created:')
    print(as.data.frame(edo2))

    return(edo2)
})


# Edox is the same as edo but changing the geneID for the gene Names.

edox <- reactive({
    edox <- clusterProfiler::setReadable(edo(),
                                         input$enrich_organism,
                                         'ENTREZID')

    # edox <- clusterProfiler::setReadable(edo2,
    #                                      'org.Hs.eg.db',
    #                                      'ENTREZID')
    return(edox)
})

# Output box with the number of proteins

output$differentiallyExpressedProteins <- renderInfoBox({

    a <- length(diffExpress())

    icon = "info"
    color = 'aqua'

    if (a == 0) {
        color = 'red'
        icon = "exclamation-triangle"
    }


    info <- infoBox(
        'Proteins Enriched',
        paste0(a, ' proteins used for enrichment.'),
        #icon = icon("stats", lib = "glyphicon"))
        icon = icon(icon),
        color =  color)
    return(info)
})

# Infobox with the genes tha failed to map
output$failedToMapGenes <- renderInfoBox({

    failedToMap <- geneListObject()$failedToMap

    info <- infoBox(
        'Unmapped Proteins',
        paste0(failedToMap, '% were not mapped.'),
        icon = icon("exclamation-triangle"),
        color = 'yellow')
    return(info)
})
