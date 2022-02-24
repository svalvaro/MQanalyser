output$geneOntologyDataTable <- DT::renderDataTable({

    DT::datatable(geneOntologyTable(),
                  extensions = 'Scroller',

                  options = list(scrollY=500,
                                 scrollX=30),
                  width = '400px')
})

# Download the table button

output$download_enrichment_table <- downloadHandler(

    filename = function(){ 'enrichment_results.csv'},
    content = function(fname){
        write.csv(geneOntologyTable(), fname)
    }
)
