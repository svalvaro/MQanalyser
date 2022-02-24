# Disease Enrichment

disEnrichmentReact <- reactive({

    if (length(diffExpress()) == 0) {
        return(NULL)
    }

    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    p <- enrichplot::dotplot(edo(),showCategory = 25)

    return(p)

})

output$enr_dotplot <- renderPlot(height = 1000,{

    disEnrichmentReact()

})

# Disease GSEA

disGSEAReact <- reactive({
    if (length(diffExpress()) == 0) {
        return(NULL)
    }

    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    message('Disease GSEA plot')
    message(paste0('Number of rows of edo2:', nrow(edo2())))

    if(nrow(edo2()) <1){
        print('Cant print')
    }else{
        p <- dotplot(edo2(), showCategory=20) + ggtitle("dotplot for GSEA")

        return(p)
    }

})

output$enr_gseadotplot <- renderPlot(height = 1000,{

    disGSEAReact()
})

# Disease plot of enriched terms

disHeatMapReact <- reactive({

    if (length(diffExpress()) == 0) {
        return(NULL)
    }

    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    p <- enrichplot::heatplot(edox() ,foldChange=geneList())

    return(p)
})

output$heatmapnrich <- renderPlotly({

    ggplotly(disHeatMapReact()) %>%

        layout(height = 800, width = 1400)
})

#Output overlapping distributions

disOverlapReact <- reactive({
    if (length(diffExpress()) == 0) {
        return(NULL)
    }

    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    if(nrow(edo2()) <1){
        print('Cant print')
    }else{

        p <- enrichplot::ridgeplot(edo2())
        return(p)
    }


})

output$enr_ridgeplot <- renderPlot(height = 800, width =1200,{

    disOverlapReact()

})

# Disease Association

disUpsetReact <- reactive({
    if (length(diffExpress()) == 0) {
        return(NULL)
    }

    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    p <- enrichplot::upsetplot(edo())

    return(p)
})

output$upset <- renderPlot(height = 800, width = 1200,{

    disUpsetReact()

})

# Circus PLot

disCircusReact <- reactive({

    if (length(diffExpress()) == 0) {
        return(NULL)
    }

    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    p <- cnetplot(edox(),  circular = TRUE, colorEdge = TRUE)

    return(p)
})

output$enr_circusplot <- renderPlot(height = 1000,{

    disCircusReact()

})

#Gene Network

disNetworkReact <- reactive({
    if (length(diffExpress()) == 0) {
        return(NULL)
    }

    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    p <- cnetplot(edox(), node_label = "all")

    return(p)
})

output$enr_networkplot <- renderPlot(height = 900, width = 800, {

    disNetworkReact()
})

#Enrichment Map

disEnrichMapReact <- reactive({
    if (length(diffExpress()) == 0) {
        return(NULL)
    }

    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    p  <- enrichplot::emapplot(pairwise_termsim(edo())#, node_scale=input$enrich_nodes
                               ,layout="kk")

    return(p)
})

output$enr_mapplot <- renderPlot(height = 1000, width = 900, {

    disEnrichMapReact()
})

# Disease Tabular format

output$diseaseTable <- DT::renderDataTable({
    shiny::req(input$enrich_organism == 'org.Hs.eg.db')

    DT::datatable(as.data.frame(edox()),
                  extensions = 'Scroller',

                  options = list(scrollY=500,
                                 scrollX=30),
                  width = '400px')
})

# Download the table button

output$download_disease_table <- downloadHandler(


    filename = function(){ 'disease_results.csv'},
    content = function(fname){
        write.csv(edox(), fname)
    }
)
