# Gene Ontology --------------------------

geneOntology_report <- reactive({

    if ("Gene Ontology" %in% input$enrichmentReport) {
        message('Gene Ontology added to the report')
        return(geneOntologyReactive())
    }else{
        return(NULL)
    }
})

# Preranked --------------------------

preRanked_report <- reactive({
    if ("GSEA Enrichment" %in% input$enrichmentReport) {
        message("Preranked Plot added to the report")
        return(enriched_plot_preranked())
    }else{
        return(NULL)
    }
})

# Network Enrichment --------------------------
networkEnrichment_report <- reactive({
    if ("Network" %in% input$enrichmentReport) {
        message("Network Enrichment Plot added to the report")
        return(networkEnrichment_reactive())
    }else{
        return(NULL)
    }
})
