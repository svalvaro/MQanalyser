# Table
network_enrich_table <- reactive({
    message('Creating the Gene Network in enrichment tab')

    bp <- enrichplot::pairwise_termsim(enrichGO(diffExpress(),
                                                ont=input$go_ontology,
                                                OrgDb = input$enrich_organism))
    return(bp)
})

# Plot

networkEnrichment_reactive <- reactive({
    p <- enrichplot::emapplot(network_enrich_table())
    return(p)
})

output$network_renrichment <- renderPlot(height = 900, {

    networkEnrichment_reactive()

})

# Table to Visualize

# Download the table button

netEnrichReadable <- reactive({
    message('Creating the Gene Network in enrichment tab for downloading')

    bp <- clusterProfiler::setReadable(network_enrich_table(),
                                       OrgDb = input$enrich_organism)

    bp <- as.data.frame(bp)
    return(bp)
})


output$netEnrichReadableOut <- DT::renderDataTable({

    DT::datatable(netEnrichReadable(),
                  extensions = 'Scroller',

                  options = list(scrollY=300,
                                 scrollX=30),
                  width = '600px',height = '1500px')
})


output$download_network_table <- downloadHandler(

    filename = function(){ 'network_results.csv'},
    content = function(fname){
        write.csv(x = netEnrichReadable(),
                  fname,
                  row.names = FALSE)
    }
)


output$networkEnrichmentUI <- renderUI({

    if (length(diffExpress()) == 0) {

        print('The Gene Set Enrichment Analysis did not found any gene
                enriched under the specific p-value cut-off')
    }else{

        fluidRow(
            column(width = 8,
                   shinycssloaders::withSpinner(
                       plotOutput('network_renrichment'),
                       image = 'images/logoTransparentSmall.gif',
                       image.width = '200px'
                   )
            ),
            column(
                width = 4,
                DT::dataTableOutput('netEnrichReadableOut'),
                br(),
                downloadButton(outputId = 'download_network_table',
                               label = 'Download table')

            )
        )
    }

})
