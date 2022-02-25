output$generateReport <- downloadHandler(

    # For PDF output, change this to "report.pdf"
    filename = reactive({
        paste0(
            "Proteomics_report_", Sys.Date(),'.',
               input$formatReport)

    }),



    content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("www/report/report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document

        params <- list(
            experimentDesign = expdesign_report(),
            contaminants = contaminants_report(),
            missingValues = missingValues_report(),
            normalization = normalization_report(),
            imputation = imputation_report(),
            scatter = scatter_report(),
            correlation = correlation_report(),
            PCA = PCA_report(),
            heatMap = heatmap_report(),
            heatMapTopContributors = input$topContInput ,
            volcano = volcano_report(),
            profile = profile_report(),
            geneOntology = geneOntology_report(),
            GSEAenrichment = preRanked_report(),
            networkEnrichment = networkEnrichment_report(),
            diseaseaseEnrinchment = disEnrichment(),
            diseaseaseGSEA = disGSEA_report(),
            diseaseaseHeatmap = disHeatMap_report(),
            diseaseaseDensity = disOverlap_report(),
            diseaseaseAssociation = disUpset_report(),
            diseaseaseCircus = disCircus_report(),
            diseaseaseNetwork = disNetwork_report(),
            diseaseaseMap = disEnrichMap_report(),
            pathway = pathway_report(),
            interactions = interactions_report()
        )

        id <- shiny::showNotification(
            ui ="Rendering report... it can take a couple of minutes",
            duration = NULL,
            type = "message",
            closeButton = FALSE
        )
        on.exit(shiny::removeNotification(id), add = TRUE)


        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          output_format = switch(input$formatReport,
                                                 "PDF"= rmarkdown::pdf_document(),
                                                 'HTML'= rmarkdown::html_document()
                                                 ),
                          envir = new.env(parent = globalenv())
        )
    }
)
