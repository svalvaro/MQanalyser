#KEGG analysis1

kegg_react1 <- reactive({

    if (input$enrich_organism == 'org.Hs.eg.db' ) {
        organism <-  'hsa'
    }

    if (input$enrich_organism == 'org.Mm.eg.db' ) {
        organism <-  'mmu'
    }


    kk <- clusterProfiler::enrichKEGG(gene=diffExpress(),
                                      organism = organism,
                                      pvalueCutoff = 0.05,
    )

    return(kk)
})

pathway_plot <- reactive({
    p <- dotplot(kegg_react1(), showCategory =20)

    return(p)
})

output$enr_kegg1 <- renderPlot(height = 900,{


    print(pathway_plot())
})

pathways_id <- reactive({

    patwaisID_vec <- kegg_react1()$ID

    names(patwaisID_vec) <- kegg_react1()$Description

    return(patwaisID_vec)

})


pathway_table <- reactive({
    # Change the ids to the names
    kk <- clusterProfiler::setReadable(kegg_react1(),
                                       input$enrich_organism,
                                       'ENTREZID')
})




output$pathwayTable <- DT::renderDataTable({

    DT::datatable(as.data.frame(pathway_table()),
                  extensions = 'Scroller',

                  options = list(scrollY=500,
                                 scrollX=30),
                  width = '400px', rownames = FALSE)
})

# Download the table button

output$download_pathway_table <- downloadHandler(


    filename = function(){ 'pathway_results.csv'},
    content = function(fname){
        write.csv(pathway_table(), fname)
    }
)
