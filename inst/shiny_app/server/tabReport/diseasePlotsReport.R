# Disease Enrichment -----------------------
disEnrichment <- reactive({
    if ("Enrichment" %in% input$diseaseReport) {
        message("Disease Enrichment Plot added to the report")
        return(disEnrichmentReact())
    }else{
        return(NULL)
    }
})

# Disease GSEA -----------------------
disGSEA_report <- reactive({
    if ("GSEA" %in% input$diseaseReport) {
        message("Disease GSEA Plot added to the report")
        return(disGSEAReact())
    }else{
        return(NULL)
    }
})


# Disease Heatmap -----------------------
disHeatMap_report <- reactive({
    if ("Heatmap" %in% input$diseaseReport) {
        message("Disease Heatmap Plot added to the report")
        return(disHeatMapReact())
    }else{
        return(NULL)
    }
})

# Disease Density -----------------------
disOverlap_report <- reactive({
    if ("Density" %in% input$diseaseReport) {
        message("Disease Density Plot added to the report")
        return(disOverlapReact())
    }else{
        return(NULL)
    }
})

# Disease Association -----------------------
disUpset_report <- reactive({
    if ("Association" %in% input$diseaseReport) {
        message("Disease Association Plot added to the report")
        return(disUpsetReact())
    }else{
        return(NULL)
    }
})

# Disease Circus  -----------------------
disCircus_report <- reactive({
    if ("Circus Plot" %in% input$diseaseReport) {
        message("Disease Circus Plot Plot added to the report")
        return(disCircusReact())
    }else{
        return(NULL)
    }
})

# Disease Network -----------------------
disNetwork_report <- reactive({
    if ("Network" %in% input$diseaseReport) {
        message("Disease Network Plot added to the report")
        return(disNetworkReact())
    }else{
        return(NULL)
    }
})

# Disease Map -----------------------
disEnrichMap_report <- reactive({
    if ("Map" %in% input$diseaseReport) {
        message("Disease Map Plot added to the report")
        return(disEnrichMapReact())
    }else{
        return(NULL)
    }
})
